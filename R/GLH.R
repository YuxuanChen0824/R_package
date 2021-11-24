GLH <- function(model, T, c = rep(0,nrow(T))) {
  beta_hat <- model$Coefficients
  df1 <- qr(T)$rank
  df2 <- model$sigma_hat[2]
  main <- T %*% beta_hat - c

  Fstatistics <- t(main) %*% solve(T %*% model$Cov_beta %*% t(T)) %*% main / df1
  pvalue <- pf(Fstatistics, df1, df2, lower.tail = FALSE)

  return(c('F' = Fstatistics, 'Pr(>F)' = pvalue))
}
