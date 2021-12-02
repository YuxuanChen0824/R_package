#'linear_model
#'
#'Used to fit linear models.
#'
#'@param formula a symbolic description of the model to be fitted
#'@param data a data frame containing the variables in the model
#'
#'
#'@return a list containing following components:
#'@return \code{formula}: the symbolic description of the model
#'@return \code{Coefficients}: the estimated coefficients for each predictor
#'@return \code{s.e}: the standard error of coefficient estimates
#'@return \code{Cov_beta}: the variance-covariance matrix of estimated coefficients
#'@return \code{t_value}: the t statistics of each coefficient
#'@return \code{p_value}: the p value of corresponding coefficients' t test
#'@return \code{sigma_hat}: the square root of MSE
#'@return \code{R2}: R square
#'@return \code{F_statistics}: the F statistics for the full model
#'@return \code{fitted_values}: the fitted mean values
#'@return \code{residuals}: the residuals, observed values mins fitted
#'@return \code{X}: the model matrix used
#'@return \code{Y}: the response used
#'
#'@examples
#'ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#'trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#'group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#'weight <- c(ctl, trt)
#'data <- data.frame(cbind(weight, group))
#'lm.D9 <- linear_model(weight ~ group, data)
#'
#'@export
#'


linear_model <- function(formula, data) {
  # get x and y matrix
  X <- data.matrix(modelr::model_matrix(data, formula))
  ystring <- toString(formula[[2]])
  val_names <- all.vars(formula)
  y <- data.matrix(dplyr::select(stats::na.omit(data[,val_names]),tidyselect::all_of(ystring)))

  # get n and p values
  n <- nrow(X)
  p <- ncol(X)

  # check singularity
  fullrank_check <- tryCatch(solve(t(X)%*%X), error = function(e) e)
  if(any(class(fullrank_check) == "error")) {
    print("X'X is not invertable")
  } else {
    # fitted values
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
    hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
    y_hat <- hat_matrix %*% y
    colnames(beta_hat) <- "Estimate"

    # s.e for betas
    residuals <- y - y_hat
    colnames(residuals) <- "residuals"
    SSE <- sum((residuals)^2)
    MSE <- SSE / (n - p)
    sigma_hat <- c("sigma_hat" = sqrt(MSE), "df" = n - p)
    var_cov <- MSE * solve(t(X) %*% X)
    se.beta <- sqrt(diag(var_cov))

    # t statistics
    t_stat <- beta_hat / se.beta
    colnames(t_stat) <- "t value"

    # p value
    p_values <- unlist(sapply(t_stat, function(x) 2*(1-stats::pt(abs(x),n-p))))

    # multiple R square and adjusted R square
    SSR <- sum((y_hat - mean(y))^2)
    SSY <- sum((y - mean(y))^2)
    R_sq <- SSR/SSY
    R_sq_adj <- 1 - (SSE/(n - p))/(SSY/(n - 1))
    RSquare <- c("Multiple R-squared" = R_sq, "Adjusted R-squared" = R_sq_adj)

    # F statistics
    F_stat <- (SSR/(p - 1))/MSE
    df1 <- p - 1
    df2 <- n - p
    P_f <- 1 - stats::pf(F_stat, df1, df2)
    Fdata <- c("F statistics" = F_stat, "df1" = df1, "df2" = df2, "pvale" = P_f)

    # fit in results
    results <- list()
    results$formula <- formula
    results$Coefficients <- beta_hat
    results$s.e <- se.beta
    results$Cov_beta <- var_cov
    results$t_value <- t_stat
    results$p_value <- p_values
    results$sigma_hat <- sigma_hat
    results$R2 <- RSquare
    results$F_statistics <- Fdata
    results$fitted_values <- y_hat
    results$residuals <- residuals
    results$X <- X
    results$Y <- y
    invisible(results)


    print(results[1:9])

    return(results)
  }

}




















