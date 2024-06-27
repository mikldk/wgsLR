check_x <- function(x) {
  stopifnot(length(x) >= 1L)
  
  if (any(abs(x - as.integer(x)) > 1e-12)) {
    stop("x must be integers")
  }
  
  x <- as.integer(x)
  
  if (length(setdiff(x, c(0L, 1L, 2L))) > 0L) {
    stop("x must contain entries with 0, 1 and 2")
  }
  
  return(x)
}

check_w <- function(w) {
  stopifnot(length(w) == 1L)
  stopifnot(is.numeric(w))
  stopifnot(w >= 0)
  stopifnot(w < 1)
}

check_p <- function(p) {
  for (i in seq_along(p)) {
    pi <- p[[i]]
    stopifnot(length(pi) == 3L)
    stopifnot(all(pi > 0))
    stopifnot(all(pi < 1))
    stopifnot(abs(sum(pi) - 1) < 1e-12)
  }
}

check_tab <- function(tab) {
  d <- dim(tab)
  stopifnot(!is.null(d))
  stopifnot(isTRUE(all.equal(d, c(3L, 3L))))
  stopifnot(all(tab >= 0L))
}

