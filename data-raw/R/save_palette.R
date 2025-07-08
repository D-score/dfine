# Defines and stores the color palettes for the gsed data

require("RColorBrewer")

# color_study
color_study <- c(
  brewer.pal(9, "Greens")[8],        # Bangladesh
  brewer.pal(12, "Paired")[c(5, 4)], # Brazil 1/2
  brewer.pal(12, "Paired")[c(3, 7)], # Chile 1/2
  brewer.pal(12, "Paired")[6],       # China
  brewer.pal(8,  "Dark2")[4],        # Columbia 1/2
  brewer.pal(12, "Paired")[2],       # Columbia 2
  brewer.pal(8, "Set2")[1],          # Ecuador
  brewer.pal(8, "YlOrRd")[3],        # Ethiopia
  brewer.pal(8, "Set2")[c(6, 4)],    # Jamaica 1/2
  brewer.pal(12, "Paired")[12],      # Madagascar
  brewer.pal(12, "Paired")[c(8)],    # Netherlands 1
  brewer.pal(12, "Paired")[c(1)],    # Netherlands 2
  brewer.pal(9, "BuPu")[7],          # South Africa

  brewer.pal(11, "BrBG")[1],         # CREDI BGD
  brewer.pal(11, "BrBG")[c(3,8)],    # CREDI BRA 1/2
  brewer.pal(11, "BrBG")[9],         # CREDI CHL
  brewer.pal(11, "PiYG")[1],         # CREDI COL
  brewer.pal(11, "PiYG")[11],        # CREDI GHA
  brewer.pal(11, "PiYG")[3],         # CREDI GTM
  brewer.pal(11, "PiYG")[8],         # CREDI IND
  brewer.pal(11, "PiYG")[2],         # CREDI JOR
  brewer.pal(11, "BrBG")[11],        # CREDI KHM
  brewer.pal(11, "PRGn")[1],         # CREDI LAO
  brewer.pal(11, "PRGn")[2],         # CREDI LBN
  brewer.pal(11, "PRGn")[10],        # CREDI NPL
  brewer.pal(11, "PuOr")[1],         # CREDI PAK
  brewer.pal(11, "PuOr")[11],        # CREDI PHL
  brewer.pal(11, "PuOr")[c(2,10)],   # CREDI TZA 1/2
  brewer.pal(11, "PuOr")[c(9,3)],    # CREDI USA 1/2
  brewer.pal(11, "RdBu")[c(1,10)],   # CREDI ZMB 1/2

  brewer.pal(11, "Spectral")[3],     # IYCD BGD
  brewer.pal(11, "Spectral")[1],     # IYCD BRA
  brewer.pal(11, "Spectral")[9],     # IYCD CRI
  brewer.pal(11, "Spectral")[2],     # IYCD IDN
  brewer.pal(11, "Spectral")[4],     # IYCD IND
  brewer.pal(11, "PuOr")[c(2,10)],   # IYCD KEN 1/2
  brewer.pal(11, "PuOr")[c(9,3)],    # IYCD MWI 1/2
  brewer.pal(8, "YlOrRd")[3],        # IYCD NIC
  brewer.pal(12, "Paired")[10],      # IYCD PAK
  brewer.pal(11, "Spectral")[10:11], # IYCD PER 1/2
  brewer.pal(8,  "Dark2")[4],        # IYCD PRY

  "#A6001A",                         # RSF-BANGLADESH
  "#489033",                         # RSF-PAKISTAN
  "#47A1D8",                         # RSF-TANZANIA

  "#A6001A",  # deeper red (rich crimson/burgundy)
  "#002776",  # navy blue (flag globe)
  "#DE2010",  # bright red (saturated)
  "#1BAF5F",  # cool green (teal-shifted for distinctiveness)
  "#F77F33",  # Dutch orange
  "#166A2F",  # forest green
  "#47A1D8"   # light blue
)
names(color_study) <- c("GCDG-BGD-7MO", "GCDG-BRA-1","GCDG-BRA-2",
                        "GCDG-CHL-1", "GCDG-CHL-2", "GCDG-CHN",
                        "GCDG-COL-LT45M", "GCDG-COL-LT42M",
                        "GCDG-ECU", "GCDG-ETH", "GCDG-JAM-LBW", "GCDG-JAM-STUNTED",
                        "GCDG-MDG", "GCDG-NLD-SMOCC", "GCDG-NLD-2",
                        "GCDG-ZAF",
                        "CREDI-BGD", "CREDI-BRA-ONLINE", "CREDI-BRA-SP", "CREDI-CHL",
                        "CREDI-COL", "CREDI-GHA", "CREDI-GTM", "CREDI-IND-ONLINE",
                        "CREDI-JOR", "CREDI-KHM", "CREDI-LAO", "CREDI-LBN",
                        "CREDI-NPL", "CREDI-PAK", "CREDI-PHL", "CREDI-TZA-MALARIA",
                        "CREDI-TZA-NEOVITA", "CREDI-USA-BOS", "CREDI-USA-ONLINE", "CREDI-ZMB-CHIPATA",
                        "CREDI-ZMB-CHOMA",
                        "IYCD-BGD-ASQVAL", "IYCD-BRA-FPS2017", "IYCD-CRI-PRIDI", "IYCD-IDN-ASQ",
                        "IYCD-IND-ASQ", "IYCD-KEN-DID", "IYCD-KEN-DMC", "IYCD-MWI-FPS2017",
                        "IYCD-MWI-MDAT", "IYCD-NIC-PRIDI", "IYCD-PAK-FPS2017", "IYCD-PER-ASQ",
                        "IYCD-PER-PRIDI", "IYCD-PRY-PRIDI",
                        "RSF-BANGLADESH", "RSF-PAKISTAN", "RSF-TANZANIA",
                        "GSED-BGD", "GSED-BRA", "GSED-CHN", "GSED-CIV",
                        "GSED-NLD", "GSED-PAK", "GSED-TZA")

color_cohort <- c(
  "GSED-BGD" = "#A6001A",  # deeper red (rich crimson/burgundy)
  "GSED-BRA" = "#002776",  # navy blue (flag globe)
  "GSED-CHN" = "#DE2010",  # bright red (saturated)
  "GSED-CIV" = "#1BAF5F",  # cool green (teal-shifted for distinctiveness)
  "GSED-NLD" = "#F77F33",  # Dutch orange
  "GSED-PAK" = "#166A2F",  # forest green
  "GSED-TZA" = "#47A1D8"   # light blue
)

# color_country
color_country <- c(
  BGD = "#A6001A",  # deeper red (rich crimson/burgundy)
  BRA = "#002776",  # navy blue (flag globe)
  CHN = "#DE2010",  # bright red (saturated)
  CIV = "#1BAF5F",  # cool green (teal-shifted for distinctiveness)
  NLD = "#F77F33",  # Dutch orange
  PAK = "#166A2F",  # forest green
  TZA = "#47A1D8",   # light blue
  BD = brewer.pal(9, "Greens")[8],
  BR = brewer.pal(12, "Paired")[4],
  CL = brewer.pal(12, "Paired")[6],
  CN = brewer.pal(12, "Paired")[12],
  CO = brewer.pal(12, "Paired")[2],
  EC = brewer.pal(8, "Set2")[6],
  ET = brewer.pal(8, "GnBu")[6],
  JM = brewer.pal(8, "Greys")[8],
  MG = brewer.pal(8, "Reds")[6],
  NL = brewer.pal(12, "Paired")[8],
  ZA = brewer.pal(9, "BuPu")[9]
)


# color_wave
color_wave <- rep(brewer.pal(4, "Set1"), 5)[1:13]
names(color_wave) <- as.character(1:13)


# color_instrument
color_instrument <- c(
  brewer.pal(12, "Paired")[10], # asi
  brewer.pal(12, "Paired")[7],  # bar
  brewer.pal(12, "Paired")[12], # bat
  brewer.pal(6, "Blues")[3:5],  # by1-3
  brewer.pal(11, "PuOr")[1],    # cro
  brewer.pal(12, "Paired")[6],  # denver
  brewer.pal(12, "Paired")[8],  # ddi
  brewer.pal(12, "Paired")[4],  # gri
  brewer.pal(11, "Spectral")[9],# iyo
  brewer.pal(12, "Paired")[3],  # mac
  brewer.pal(12, "Paired")[3],  # peg
  brewer.pal(12, "Paired")[9],  # sbi
  brewer.pal(12, "Paired")[5],  # sgr
  brewer.pal(11, "Spectral")[3],# tep
  brewer.pal(11, "Spectral")[5],# vin
  brewer.pal(8, "Accent")[7]    # rap
)
names(color_instrument) <- c("asi", "bar", "bat",
                             "by1", "by2", "by3",
                             "cro",
                             "den", "ddi", "gri",
                             "iyo",
                             "mac", "peg", "sbi",
                             "sgr", "tep", "vin",
                             "rap")

# color_domain
color_domain <- c(
  brewer.pal(5, "Set2")[2],   # Cognitive
  brewer.pal(5, "Set2")[5],   # Expressive
  brewer.pal(5, "Set2")[3],   # Fine Motor
  brewer.pal(5, "Set2")[4],   # Gross Motor
  brewer.pal(5, "Set2")[1],   # Receptive
  rgb(0, 0, 0)                # NA
)
names(color_domain) <-
  c("Cognitive", "Expressive", "Fine Motor", "Gross Motor", "Receptive", "")


gsed_palettes <- list(cohort = color_cohort,
                      study = color_study,
                      country = color_country,
                      domain = color_domain,
                      instrument = color_instrument,
                      wave = color_wave)

# save to /data
usethis::use_data(gsed_palettes, overwrite = TRUE)
