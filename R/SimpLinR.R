Rcpp::sourceCpp("src/SimpLinCpp.cpp")

#' Perform simple linear regression analysis.
#' 
#' @param x A numeric vector of length n.
#' @param y A numeric vector of length n.
#' @returns A named list with five elements: estimated parameter coefficients, standard errors for these coefficients, 95\% confidence intervals for these coefficients, predicted values, and residuals.
#' @examples
#' x <- c(1,5,6)
#' y <- c(1,2,3)
#' fit <- SimpLinR(x, y)
#' # estimated coefficients, with intercept always first
#' fit$coefs
#' # standard errors of both coefficients
#' fit$std_errors
#' # 95% confidence interval for both coefficients; 2.5% is the first column and 97.5% is the second column
#' fit$conf_int
#' # predicted values
#' fit$preds
#' # residuals
#' fit$resids

SimpLinR <- function(x, y) {
  if (!is.vector(x) | !is.vector(y)) {
    stop("Incorrect data type. Both inputs must be vectors.")
  } else if (length(x) != length(y)) {
    stop("Inputs are of different length. Both vectors must be the same length.")  
  } else if (!is.numeric(x) | !is.numeric(y)) {
    stop("Incorrect vector class(es). Both vectors must be numeric.")
  } else {
    return(SimpLinCpp(x, y))
  }
}

