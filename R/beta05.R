

#' @rdname beta05
#' @importFrom stats rbeta
#' @export
rbeta05 <- function(n, shape1, shape2) {
  rbeta(n, shape1, shape2) / 2
}

#' @rdname beta05
#' @importFrom stats pbeta
#' @export
pbeta05 <- function(x, shape1, shape2) {
  pbeta(2 * x, shape1, shape2)
}

#' @rdname beta05
#' @importFrom stats qbeta
#' @export
qbeta05 <- function(x, shape1, shape2) {
  qbeta(x, shape1, shape2) / 2
}

#' Beta distribution on (0, 0.5)
#' 
#' @param x number between 0 and 0.5
#' @param n number of random samples to draw
#' @param shape1 first shape parameter
#' @param shape2 second shape parameter
#' @param log return result on log scale
#' 
#' @rdname beta05
#' 
#' @examples
#' dbeta05(0.2, 1, 5)
#' dbeta05(0.2, 1, 5, log = TRUE)
#' dbeta05(0.2, 1, 5) |> log()
#' 
#' pbeta05(0.5, 1, 5)
#' 
#' qbeta05(1, 1, 5)
#' 
#' #rbeta05(100, 1, 5) |> hist(probability = TRUE)
#' #curve(dbeta05(x, 1, 5), from = 0, to = 0.5, add = TRUE)
#' 
#' @importFrom stats dbeta
#' 
#' @export
dbeta05 <- function(x, shape1, shape2, log = FALSE) {
  if (log) {
    d <- log(2) + dbeta(2 * x, shape1, shape2, log = TRUE)
    d
    return(d)
  }
  
  d <- 2 * dbeta(2 * x, shape1, shape2, log = log)
  d
}


if (FALSE) {
  x <- rbeta05(1000, 5, 1)
  hist(x, probability = TRUE)
  curve(dbeta05(x, 5, 1), from = 0, to = 1, add = TRUE)
  integrate(dbeta05, lower = 0, upper = 0.5, shape1 = 5, shape2 = 1)
  integrate(dbeta05, lower = 0, upper = 1, shape1 = 5, shape2 = 1)
  
  integrate(dbeta05, lower = 0, upper = 0.25, shape1 = 5, shape2 = 1)
  pbeta05(0.25, 5, 1)
  qbeta05(0.03125, 5, 1)
  
  integrate(dbeta05, lower = 0, upper = 0.1, shape1 = 5, shape2 = 1)
  pbeta05(0.1, 5, 1)
  qbeta05(0.00032, 5, 1)
}
