#'GLH
#'
#'Conduct general linear hypothesis test. Use this function with function \code{linear_model}
#'
#'@param model the fitted linear model results
#'@param T matrix giving linear combinations of coefficients by rows
#'@param c a vector containing the results of linear combinations of coefficients,defaulting to 0.
#'
#'@return The results of F test including the F statistics and p value
#'
#'@examples
#'mod <- linear_model(Depression ~ Fatalism + R_E + Sex, Patients)
#'GLH(mod, diag(4))
#'
#'@export
#'


GLH <- function(model, T, c = rep(0,nrow(T))) {
  beta_hat <- model$Coefficients
  df1 <- qr(T)$rank
  df2 <- model$sigma_hat[2]
  main <- T %*% beta_hat - c

  Fstatistics <- t(main) %*% solve(T %*% model$Cov_beta %*% t(T)) %*% main / df1
  pvalue <- pf(Fstatistics, df1, df2, lower.tail = FALSE)

  return(c('F' = Fstatistics, 'Pr(>F)' = pvalue))
}
