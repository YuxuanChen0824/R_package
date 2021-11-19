library(dplyr)
library(modelr)

linear_model <- function(formula, data) {
  # get x and y matrix
  X <- data.matrix(model_matrix(data, formula))
  y_name <- formula[[2]]
  y <- data.matrix(select(data, y_name))

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
    SSE <- sum((y-y_hat)^2)
    MSE <- SSE / (n - p)
    sigma_hat <- c("sigma_hat" = sqrt(MSE), "df" = n - p)
    var_cov <- MSE * solve(t(X) %*% X)
    se.beta <- sqrt(diag(var_cov))

    # t statistics
    t_stat <- beta_hat / se.beta
    colnames(t_stat) <- "t value"

    # p value
    p_values <- unlist(sapply(t_stat, function(x) 2*(1-pt(abs(x),n-p))))

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
    P_f <- 1 - pf(F_stat, df1, df2)
    Fdata <- c("F statistics" = F_stat, "df1" = df1, "df2" = df2, "pvale" = P_f)

    # fit in results
    results <- list()
    results$Coefficients <- beta_hat
    results$s.e <- se.beta
    results$t_value <- t_stat
    results$p_value <- p_values
    results$sigma_hat <- sigma_hat
    results$R2 <- RSquare
    results$F_statistics <- Fdata
    results$fitted_values <- y_hat

    return(results)
  }

}




















