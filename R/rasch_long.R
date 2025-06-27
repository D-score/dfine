#' `rasch_long()` estimates item parameters using the pairwise conditional
#' likelihood estimation for the Rasch model from long data.
#'
#' @rdname rasch
#' @name rasch_long
#' @examples
#' long <- dmetric::small_data$itm[1:10000, ]
#' names(long) <- c("id", "age", "item", "response")
#' result <- rasch_long(long, response_var = "value")
#' @noRd
rasch_long <- function(long,
                       visit_var = c("subjid", "agedays"),
                       item_var = "item",
                       response_var = "response",
                       items = NULL,
                       equate = NULL,
                       b_fixed = NULL,
                       b_init = NULL,
                       zerosum = FALSE,
                       pairs = NULL,
                       conv = .00001,
                       maxiter = 3000,
                       progress = FALSE,
                       save_pairs = FALSE,
                       save_wide = FALSE) {
  call <- match.call()

  # check if the data is a data frame
  if (!is.data.frame(long)) {
    stop("'long' must be a data frame.")
  }

  # check if the visit_var are present
  if (!all(visit_var %in% colnames(long))) {
    stop("Some columns not found in long: ", paste(visit_var, collapse = ", "))
  }
  # check if item and response columns are present
  item_var <- as.character(item_var[1L])
  response_var <- as.character(response_var[1L])
  if (!item_var %in% colnames(long)) {
    stop("Column '", item_var, "' not found in long.")
  }
  if (!response_var %in% colnames(long)) {
    stop("Column '", response_var, "' not found in long.")
  }

  # Select items
  if (is.null(items)) {
    items <- unique(long[[item_var]])
  }
  long <- long[long[[item_var]] %in% items, , drop = FALSE]

  # Check 0/1 range
  x_range <- range(long[[response_var]], na.rm = TRUE)
  if (x_range[2] > 1)
    stop("Some items have scores > 1")
  if (x_range[1] < 0)
    stop("Some items have scores < 0")

  # Check for duplicate item names
  if (anyDuplicated(items))
    stop("Duplicate item names found")

  # Ensure item_var and response_var are character strings
  item_vals <- long[[item_var]]
  response_vals <- long[[response_var]]
  items_f <- factor(item_vals, levels = items)
  p <- tapply(response_vals, items_f, function(x) mean(x, na.rm = TRUE))
  n <- tapply(response_vals, items_f, function(x) sum(!is.na(x)))
  names(p) <- names(n) <- levels(items_f)
  k <- length(p)
  if (is.null(b_init)) {
    b_init <- -stats::qlogis(p)
  }

  # Initialize b
  b <- b_init
  if (!is.null(b_fixed)) {
    b_fixed <- b_fixed[names(b_fixed) %in% names(b_init)]
    b[names(b_fixed)] <- b_fixed
    exp_b_fixed <- exp(b_fixed)
    zerosum <- FALSE
  }

  # Calculate pairs matrix
  # using sparse matrix with score + 1
  id <- as.factor(do.call(paste, c(long[visit_var], sep = "_")))
  items_f <- factor(long[[item_var]], levels = items)
  sm <- sparseMatrix(
    i = as.numeric(id),
    j = as.numeric(items_f),
    x = as.integer(long[[response_var]] + 1L),
    dimnames = list(
      levels(id),
      levels(items_f)
    )
  )
  pairs <- as.matrix(crossprod(sm == 1L, sm == 2L))
  dimnames(pairs) <- list(levels(items_f), levels(items_f))

  # Identify orphans (zero counts) and stop
  orphans <- NULL
  rsp <- rowSums(pairs)
  flags <- rsp == 0
  if (any(flags)) {
    orphans <- dimnames(pairs)[[1]][flags]
    cat("Orphans found: ", orphans, "\n")
  }

  # Prepare for loop
  nij <- pairs + t(pairs)
  eps0 <- eps <- exp(b)
  max.change <- 10
  iter <- 1

  if (is.null(orphans)) {
    while (max.change > conv & iter <= maxiter) {
      b0 <- b
      eps0 <- eps
      m1 <- matrix(eps0, k, k, byrow = TRUE) + matrix(eps0, k, k)
      eps <- rsp / rowSums(nij / m1)
      b <-  log(eps)

      # Put item parameter constraints
      if (!is.null(b_fixed)) {
        eps[names(exp_b_fixed)] <- exp_b_fixed
      }

      # Equate estimates
      if (length(equate) > 0) {
        for (i in seq_along(equate)) {
          pos <- match(equate[[i]], names(eps))
          if (anyNA(pos))
            cat("\n Equate ", names(equate)[i],
                "  Item not found: ", equate[[i]][is.na(pos)])
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
        cat("PL Iter.", iter, ": max. parm. change = ",
            round( max.change , 6 ), "\n")
        flush.console()
      }
      iter <- iter + 1
    }
  }
  item <- data.frame("n" = n,
                     "p" = p ,
                     "b" =  log(eps))

  # return fitted object
  res <- list(items = items,
              visit_var = visit_var,
              item_var = item_var,
              response_var = response_var,
              ability_var = "d",
              shape = "long",
              equate = equate,
              b_fixed = b_fixed,
              b_init = b_init,
              orphans = orphans,
              zerosum = zerosum,
              iter = iter,
              convergence = conv,
              item = item,
              betapar = -log(eps),
              call = call)
  if (save_pairs) res$pairs <- pairs
  if (save_wide) res$wide <- as.data.frame(as.matrix(sm))
  return(res)
}
