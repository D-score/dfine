# This script refits the core model 293_0 for the phase-1 data with
# the following changes:
# + The sample size is slighly lower due to updated duplicate removal
# + The data are read from the DuckDB database
# + Responses from inter-rater scores (vist_type 5) are ignored
#
# Dependencies:
# + Environmental variable "LOCAL_DUCKDB" must be set to the directory
#   containing database "fixed.duckdb" containing fixed administration
#   phase1 & phase2 data
#
# Created   20250618 SvB
# Modified  20250625 SvB

plot_pdf <- FALSE

if (nchar(Sys.getenv("LOCAL_DUCKDB")) == 0L) {
  stop("Environmental variable LOCAL_DUCKDB not set.", call. = FALSE)
}

# Load required packages
library("DBI", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("dscore")
library("dfine")
library("tidyr", quietly = TRUE, warn.conflicts = FALSE)
library("testthat", quietly = TRUE, warn.conflicts = FALSE)

if (packageVersion("gsedread") < "0.13.0") stop("Needs gsedread 0.13.0")

#
#  A.  Read fixed form Phase 1 data responses and visits
#
dbfile <- file.path(Sys.getenv("LOCAL_DUCKDB"), "fixed.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = dbfile)
dbListTables(con)
visits <- dbReadTable(con, "visits")
responses <- dbReadTable(con, "responses")
dbDisconnect(con)

#
#  B.  Subset to phase 1 LF and SF data
#      Ignore any inter-rater scores (vist_type 5) to prevent duplicate matches
#

visits <- visits |>
  filter(phase == 1L & ins %in% c("lf", "sf") & vist_type != 5L)
responses <- semi_join(
  responses,
  visits,
  by = c("subjid", "agedays", "vist_type"))

#
#  C. Identify pairs of LF-SF records occurring within four days
#

visits <- visits |>
  select(subjid, agedays, ins, vist_type) |>
  arrange(subjid, agedays, ins, vist_type) |>
  group_by(subjid) |>
  mutate(sf_order = if_else(ins == "sf", row_number(), NA_integer_)) |>
  mutate(sf_order = if_else(ins == "sf", cumsum(ins == "sf"), NA_integer_)) |>
  ungroup()

# Find pair number (0, 1, 2, 3) for each SF record
sf_rows <- visits |>
  filter(ins == "sf") |>
  select(subjid, sf_agedays = agedays, sf_order)
lf_rows <- visits |>
  filter(ins == "lf") |>
  select(subjid, lf_agedays = agedays)
pairs <- sf_rows |>
  left_join(lf_rows, by = "subjid", relationship = "many-to-many") |>
  mutate(diff = abs(sf_agedays - lf_agedays)) |>
  group_by(subjid, sf_order) |>
  slice_min(order_by = diff, n = 1L, with_ties = FALSE) |>
  ungroup() |>
  mutate(pair = ifelse(diff > 4L | is.na(diff), 0L, sf_order))

# Merge pair number with with response
items_sf <- get_itemnames(ins = "gpa", order = "indm")
items_lf <- get_itemnames(ins = "gto")
responses <- responses |>
  filter(item %in% c(items_sf, items_lf))
responses_sf <- responses |>
  filter(item %in% items_sf) |>
  left_join(pairs, by = join_by(subjid, agedays == sf_agedays)) |>
  mutate(ins = "sf") |>
  select(subjid, agedays, pair, ins, item, response)
responses_lf <- responses |>
  filter(item %in% items_lf) |>
  left_join(pairs |> filter(pair > 0),
            by = join_by(subjid, agedays == lf_agedays)) |>
  mutate(pair = ifelse(is.na(pair), -1L, pair),
         ins = "lf") |>
  select(subjid, agedays, pair, ins, item, response)

# Check for zero duplicate matches
nrow(responses_sf) + nrow(responses_lf) - nrow(responses) == 0

# Recreate with the new pair number
responses <- bind_rows(responses_sf, responses_lf)

# table(responses$pair, useNA = "al")
#
#     -1      0      1      2      3    <NA>
#   1126   3421 362196 129924   7483      0
#
# -1 : Only LF
#  0 : Only SF
#  1-3: Pair number of SF-LF match

#
#  D. Create wide format that puts items from a SF-LF pair on the same row
#

data <- responses |>
  select(subjid, agedays, pair, ins, item, response) |>
  pivot_wider(names_from = c(item), values_from = response,
              id_cols = c(subjid, pair)) |>
  arrange(subjid, pair)
agedays_info <- responses |>
  distinct(subjid, pair, ins, agedays) |>
  pivot_wider(names_from = ins, values_from = agedays,
              names_prefix = "agedays_")
data <- data |>
  left_join(agedays_info, by = c("subjid", "pair")) |>
  select(subjid, pair, starts_with("agedays_"), any_of(items_sf), any_of(items_lf))

# wide: 5961 visits (measurements), 297 columns (2 + 2 + 138 + 155)
# wide: 4374 unique subjid (=children)
cat("dim(data):", dim(data), "\n")

#
#  E. Select items with at least 10 observation in both categories
#

min_n <- 10
id_cols <- c("subjid", "pair", "agedays_sf", "agedays_lf")
items <- setdiff(colnames(data), id_cols)
counts <- sapply(data[items], function(x) {
  c(count_0 = sum(x == 0, na.rm = TRUE),
    count_1 = sum(x == 1, na.rm = TRUE))
})
counts_df <- as.data.frame(t(counts))
counts_df$item <- rownames(counts_df)
valid_items <- counts_df |>
  filter(count_0 >= min_n, count_1 >= min_n) |>
  pull(item)
data <- data |>
  select(all_of(id_cols), all_of(valid_items))

#
#  F. Estimate difficulty of SF and LF items by a single group design
#

fit <- rasch(data = data, visit_var = c("subjid", "pair"), items = valid_items)

#
#  G. Specify anchors to define the D-score scale
#

# Define agedays
data <- data |>
  mutate(agedays = rowMeans(across(c(agedays_sf, agedays_lf)), na.rm = TRUE))

# 20: Lift head 45 degrees
# 40: Moves from lying to sitting
model <- calculate_dmodel(data = data,
                          fit = fit,
                          name = "phase1_wide",
                          anchors = c(gtogmd001 = 20, gtogmd026 = 40))
