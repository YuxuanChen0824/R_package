#' dffit
#'
#' This function is used to calculate the standardized difference in fits. Please use it with function `linear_model`.
#'
#' @param mod the fitted linear regression
#'
#' @return a vector of DFFIT_i for each observation.
#'
#' @examples
#' mod <- linear_model(Depression ~ Fatalism + R_E + Sex, Patients)
#' dffit(mod)
#'
#' @export

dffit <- function(mod) {
  res <- as.vector(mod$residuals)

  ## internal studentized
  X <- mod$X
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  H_ii <- diag(H)
  internall.student <- as.vector(res/(mod$sigma_hat[[1]] * sqrt(1 - H_ii)))

  # external studentized
  df <- mod$sigma_hat[[2]]
  stand.res.sq <- internall.student ^ 2
  one_remove <- (df - 1)/(df - stand.res.sq)
  r <- internall.student * sqrt(one_remove)

  DFFIT <- r * sqrt(H_ii/(1 - H_ii))

  return(DFFIT)
}
























