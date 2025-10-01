#' @rdname rasch
#' @name rasch_wide
#' @examples
#' \dontrun{
#' library("sirt")
#' # Simulate data from the Rasch model
#' base::set.seed(9765)
#' N <- 50000    # number of persons
#' I <- 11       # number of items
#' b <- base::seq(-2, 2, length = I)
#' dat <- sirt::sim.raschtype(stats::rnorm(N), b)
#' base::colnames(dat) <- base::paste0("I", 1:I)
#'
#' # Do the conventional analysis
#' fit1 <- rasch_wide(dat)
#' (fit1$b-b) - mean(fit1$b-b) # should be close to zero
#'
#' # split items I6 and I9 into two parts
#' dat2 <- dat[, -c(6, 9)]
#' dat2 <- cbind(dat2,
#'            c(dat[1:(N/2),"I6"], rep(NA, N/2)),
#'            c(rep(NA, N/2), dat[(N/2+1):N,"I6"]),
#'            c(dat[1:(N/4),"I9"], rep(NA, 3*N/4)),
#'            c(rep(NA, N/4), dat[(N/4+1):N,"I9"]))
#' colnames(dat2)[10:13] <- c("I6a", "I6b","I9a","I9b")
#'
#' # no equating, parameters close but not identical
#' fit2 <- rasch_wide(dat2)
#' fit2$item
#'
#' # equate I6a and I6b, and I9a and I9b
#' # parameters identical AND close to conventional analysis
#' fit3 <- rasch_wide(dat2,
#'               equate = list(cube = c("I6a", "I6b"), wave = c("I9a", "I9b")))
#' fit3$item
#' }
#' @noRd
rasch_wide <- function(
  wide,
  visit_var = c("subjid", "agedays"),
  items = NULL,
  equate = NULL,
  b_fixed = NULL,
  b_init = NULL,
  pairs = NULL,
  zerosum = FALSE,
  conv = .00001,
  maxiter = 3000,
  progress = FALSE,
  save_pairs = FALSE,
  save_wide = FALSE
) {
  call <- match.call()

  # Set the items vector
  if (is.null(colnames(wide))) {
    stop("'wide' has no column names; cannot subset using names.")
  }
  if (is.null(items)) {
    items <- colnames(wide)
  } else {
    if (any(!items %in% colnames(wide))) {
      stop(
        "Items not found: ",
        paste(items[!items %in% colnames(wide)], collapse = ", ")
      )
    }
  }

  # Check for duplicated item names
  if (any(duplicated(items))) {
    stop(
      "Duplicate item names found: ",
      paste(items[duplicated(items)], collapse = ", ")
    )
  }

  # Select items from the data, convert to integer matrix
  wide <- as.matrix(wide[, items, drop = FALSE])
  wide <- matrix(
    as.integer(wide),
    nrow = nrow(wide),
    ncol = ncol(wide),
    dimnames = list(rownames(wide), colnames(wide))
  )

  # # Check for values > 1
  # out_of_range_max <- sapply(wide, function(x) max(x, na.rm = TRUE) > 1)
  # if (any(out_of_range_max)) {
  #   stop(paste("Some items have scores > 1:",
  #              paste(colnames(wide)[out_of_range_max], collapse = ", ")))
  # }
  #
  # # Check for values < 0
  # out_of_range_min <- sapply(wide, function(x) min(x, na.rm = TRUE) < 0)
  # if (any(out_of_range_min)) {
  #   stop(paste("Some items have scores < 0:",
  #              paste(colnames(wide)[out_of_range_min], collapse = ", ")))
  # }

  # Convert to numeric matrix if needed

  # Check max > 1
  out_of_range_max <- matrixStats::colMaxs(wide, na.rm = TRUE) > 1
  if (any(out_of_range_max)) {
    stop(paste(
      "Some items have scores > 1:",
      paste(colnames(wide)[out_of_range_max], collapse = ", ")
    ))
  }

  # Check min < 0
  out_of_range_min <- matrixStats::colMins(wide, na.rm = TRUE) < 0
  if (any(out_of_range_min)) {
    stop(paste(
      "Some items have scores < 0:",
      paste(colnames(wide)[out_of_range_min], collapse = ", ")
    ))
  }

  # Create pairs matrix
  if (!is.null(pairs)) {
    pairs_items <- colnames(pairs)
    missing_items <- setdiff(items, pairs_items)
    if (length(missing_items) > 0L) {
      stop(paste(
        "The following items are missing from 'pairs':",
        paste(missing_items, collapse = ", ")
      ))
    }
    pairs <- pairs[items, items, drop = FALSE]
  } else {
    wide2 <- wide
    wide2[is.na(wide2)] <- 9
    pairs <- crossprod(wide2 == 0, wide2 == 1)
    rm("wide2")
  }

  # Identify orphans (zero counts) and stop
  orphans <- NULL
  flags <- rowSums(pairs) == 0
  if (any(flags)) {
    orphans <- dimnames(pairs)[[1L]][flags]
    cat("Orphans found: ", orphans, "\n")
  }

  # Initialize
  p <- colMeans(wide, na.rm = TRUE)
  n <- colSums(1 - is.na(wide))
  I <- ncol(wide)
  if (is.null(b_init)) {
    b_init <- -stats::qlogis(p)
  }
  b <- b_init
  if (!is.null(b_fixed)) {
    b_fixed <- b_fixed[names(b_fixed) %in% names(b_init)]
    b[names(b_fixed)] <- b_fixed
    exp_b_fixed <- exp(b_fixed)
    zerosum <- FALSE
  }
  nij <- pairs + t(pairs)
  eps0 <- eps <- exp(b)
  max.change <- 10
  iter <- 1

  if (is.null(orphans)) {
    while (max.change > conv & iter <= maxiter) {
      b0 <- b
      eps0 <- eps
      m1 <- matrix(eps0, I, I, byrow = TRUE) + matrix(eps0, I, I)
      g1 <- rowSums(nij / m1)
      eps <- rowSums(pairs) / g1
      b <- log(eps)

      # put item parameter constraints
      if (!is.null(b_fixed)) {
        eps[names(exp_b_fixed)] <- exp_b_fixed
      }

      # equate estimates
      if (length(equate) > 0) {
        for (i in seq_along(equate)) {
          pos <- match(equate[[i]], names(eps))
          if (anyNA(pos)) {
            cat(
              "\n Equate ",
              names(equate)[i],
              "  Item not found: ",
              equate[[i]][is.na(pos)]
            )
          }
          eps[pos] <- weighted.mean(x = eps[pos], w = n[pos])
        }
      }

      if (zerosum) {
        b1 <- -log(eps)
        b2 <- b1 - mean(b1)
        eps <- exp(-b2)
      }

      dif <- abs(b - b0)
      if (any(is.na(dif))) {
        pars <- names(b)[[is.na(dif)]]
        stop("Cannot estimate parameter: ", paste(pars))
      }
      max.change <- max(dif)
      if (progress) {
        cat(
          "PL Iter.",
          iter,
          ": max. parm. change = ",
          round(max.change, 6),
          "\n"
        )
        flush.console()
      }
      iter <- iter + 1
    }
  }
  item <- data.frame("n" = n, "p" = p, "b" = log(eps))

  res <- list(
    items = items,
    visit_var = visit_var,
    item_var = NULL,
    response_var = NULL,
    ability_var = "d",
    shape = "wide",
    equate = equate,
    b_fixed = b_fixed,
    b_init = b_init,
    orphans = orphans,
    zerosum = zerosum,
    iter = iter,
    convergence = conv,
    item = item,
    betapar = -log(eps),
    call = call
  )
  if (save_pairs) {
    res$pairs <- pairs
  }
  if (save_wide) {
    res$wide <- wide
  }
  return(res)
}
