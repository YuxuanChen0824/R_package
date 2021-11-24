residuals <- function(mod) {
  library(Rcpp)
  res <- as.vector(mod$residuals)
  stand.res <- as.vector(res/mod$sigma_hat[[1]])

  # internal studentized
  X <- mod$X
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  H_ii <- diag(H)
  internall.student <- as.vector(res/(mod$sigma_hat[[1]] * sqrt(1 - H_ii)))

  # external studentized
  SSE <- (mod$sigma_hat[[1]])^2 * mod$sigma_hat[[2]]
  ydata <- cbind(mod$Y, mod$fitted_values)
  cppFunction('
      vector<double>

  ')

  residual_data <- data.frame("residuals" = res,
                          "standardized residuals" = stand.res,
                          "internal studentize" = internall.student)

  return(residual_data)
}






















