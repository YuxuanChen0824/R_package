#' Cooks
#'
#' Calculate the Cook's distance, a summary of subject i' influence on beta estimates overall. Use this function with `linear_model`.
#'
#' @param mod the fitted linear regression
#'
#' @return a vector of each observation's Cook's distance
#'
#' @examples
#' mod <- linear_model(Depression ~ Fatalism + R_E + Sex, Patients)
#' Cooks(mod)
#'
#' @export



Cooks <- function(mod) {
  res <- as.vector(mod$residuals)

  ## internal studentized
  X <- mod$X
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  H_ii <- diag(H)
  internall.student <- res/(mod$sigma_hat[[1]] * sqrt(1 - H_ii))

  p <- ncol(mod$X)
  D <- (internall.student^2 * H_ii) / (p * ( 1 - H_ii))

  return(D)
}
