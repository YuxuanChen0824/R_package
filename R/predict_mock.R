library(tidyverse)

predict_mock <- function(mod, newobs) {
  beta_hat <- mod$Coefficients
  col <- rownames(beta_hat)[-1]
  newX <- newobs[,col] %>% cbind(1,.) %>% data.matrix()

  predicts <- newX %*% beta_hat

  return(predicts)
}














