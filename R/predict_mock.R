#'predict_mock
#'
#'Used to get the predictions from the results of a fitted linear model. Please use it with function \code{linear_model}.
#'
#'@param mod the fitted linear model results
#'@param newobs the new observations, for which you would like to make predictions
#'
#'@return a vector of predict values
#'
#'@examples
#'ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#'trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#'group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#'weight <- c(ctl, trt)
#'data <- data.frame(cbind(weight, group))
#'lm.D9 <- linear_model(weight ~ group, data)
#'
#'predict_mock(lm.D9, data.frame("group" = c(1,2,2,1)))
#'
#'@export
#'

predict_mock <- function(mod, newobs) {
  # prepare newX
  yname <- toString(mod$formula[[2]])
  newobs <- cbind(newobs, y = 0)
  colnames(newobs)[ncol(newobs)] <- yname
  newX <- data.matrix(modelr::model_matrix(newobs, mod$formula))

  # prepare betas
  beta_hat1 <- mod$Coefficients
  beta_hat2 <- beta_hat1[rownames(beta_hat1) %in% colnames(newX)]

  predicts <- newX %*% beta_hat2

  return(predicts)
}














