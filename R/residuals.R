#'residuals
#'
#'Calculate four types of residuals for a fitted linear regression. Use it with linear_model function.
#'
#'@param mod the fitted linear regression
#'
#'@return a table containing four types of residuals
#'
#'@examples
#'mod.davis <- linear_model(weight ~ repwt, data=Davis)
#'residuals(mod.davis)
#'
#'@export



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
  df <- mod$sigma_hat[[2]]
  stand.res.sq <- internall.student ^ 2
  cppFunction('
      NumericVector one_remove(NumericVector stand_res_sq, double df) {
         return (df - 1)/(df - stand_res_sq);
      }
  ')
  external.student <- as.vector(internall.student * sqrt(one_remove(stand.res.sq,df)))

  residual_data <- data.frame("residuals" = res,
                          "standardized residuals" = stand.res,
                          "internal studentize" = internall.student,
                          "external studentize" = external.student)

  return(residual_data)
}












