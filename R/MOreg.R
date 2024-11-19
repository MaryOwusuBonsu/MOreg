#' MOreg: Fits a Linear Regression Model
#'
#' This function fits a linear regression model.
#'
#' @param Y A numeric vector representing the response variable (dependent variable).
#' @param X A numeric matrix or data frame containing the predictor variables (independent variables).
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{beta}{A numeric vector of estimated regression coefficients.}
#'   \item{residuals}{A numeric vector of residuals (differences between observed and fitted values).}
#'   \item{sigma_sq_hat}{Estimated variance of the residuals (error term).}
#'   \item{std_err}{A numeric vector of the standard errors of the regression coefficients.}
#'   \item{tval}{A numeric vector of the t-values for the regression coefficients.}
#'   \item{pval}{A numeric vector of the p-values for the regression coefficients.}
#' }
#'
#' @examples
#' Y <- c(2.5, 6.8, 8.7, 9)
#' X <- c(15, 25, 30, 29)
#' model <- MOreg(Y, X)
#'
#' @importFrom stats pt
#'
#' @export
MOreg <- function(Y, X) {
  # Ensure X is a matrix
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  # Check that Y is a numeric vector and has the correct length
  if (!is.numeric(Y)) stop("Y must be a numeric vector.")
  if (length(Y) != nrow(X)) stop("Length of Y must match the number of rows in X.")

  # Add intercept (this could be moved outside of this function if needed)
  X <- cbind(1, X)

  # Compute the estimate of beta
  XtX <- t(X) %*% X
  XtY <- t(X) %*% Y

  # Check if XtX is singular or near-singular using a small tolerance
  if (det(XtX) == 0 || min(eigen(XtX)$values) < 1e-10) {
    stop("Matrix XtX is singular or nearly singular, regression cannot be performed.")
  }

  # Calculate beta
  beta <- solve(XtX) %*% XtY

  # Calculate residuals
  Y_hat <- X %*% beta
  residuals <- Y - Y_hat

  # Sum of squares of errors
  SSE <- sum(residuals^2)

  # Degrees of freedom
  df <- nrow(X) - ncol(X)

  # Estimated sigma^2
  sigma_sq_hat <- SSE / df

  # Calculate standard errors of beta and p-values
  std_err <- sqrt(diag(sigma_sq_hat * solve(XtX)))
  tval <- beta / std_err
  pval <- 2 * (1 - pt(abs(tval), df))

  # Return a list of results
  results <- list(
    beta = beta,
    residuals = residuals,
    sigma_sq_hat = sigma_sq_hat,
    std_err = std_err,
    tval = tval,
    pval = pval
  )

  return(results)
}
