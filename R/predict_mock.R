predict_mock <- function(mod, newobs) {
  library(modelr)

  beta_hat <- mod$Coefficients
  newX <- data.matrix(model_matrix(newobs, mod$formula))

  predicts <- newX %*% beta_hat

  return(predicts)
}














