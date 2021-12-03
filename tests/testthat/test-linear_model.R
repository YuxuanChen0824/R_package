test_that("test four functions", {
  library(car)

  ## test linear_model function
  mod <- linear_model(Sepal.Length ~ Sepal.Width + Species, data = iris)
  mod2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

  # test the fitted coefficients
  expect_equal(as.vector(mod$Coefficients), as.vector(mod2$coefficients))
  # test the value of se
  expect_equal(as.vector(mod$s.e), as.vector(summary(mod2)$coefficients[,2]))
  # test the t value
  expect_equal(as.vector(mod$t_value), as.vector(summary(mod2)$coefficients[,3]))
  # test p value
  expect_equal(as.vector(mod$p_value), as.vector(summary(mod2)$coefficients[,4]))
  # test sigma
  expect_equal(mod$sigma_hat[[1]], summary(mod2)$sigma)
  # test r square
  expect_equal(mod$R2[[1]], summary(mod2)$r.squared)
  # test adjusted r square
  expect_equal(mod$R2[[2]], summary(mod2)$adj.r.squared)

  ### test special case in which x is singular
  data_mock <- data.frame("pred1" = c(1,1),
                          "pred2" = c(1,1),
                          "response" = c(3,4))

  expect_equal(linear_model(response ~ pred1 + pred2, data_mock), "Please check your predictors")


  ## test predict function
  # generate new observation
  newobs <- data.frame("Sepal.Length" = runif(10, min = 4, max = 7),
                       "Sepal.Width" = runif(10, min = 2, max = 5),
                       "Species" = sample(c("setosa", "versicolor"), 10, replace = T))
  pred1 <- predict_mock(mod, newobs)
  pred2 <- predict(mod2, newobs)
  expect_equal(as.vector(pred1), as.vector(pred2))


  ## test GLH function
  T <- matrix(c(0,0,1,0,0,1,0,-1), nrow = 2)
  test1 <- GLH(mod, T)
  test2 <- linearHypothesis(mod2, T)
  expect_equal(test1[[1]], test2[2,5])

  ## test residuals function
  res <- residuals(mod)
  # general residuals
  expect_equal(as.vector(res$residuals), as.vector(mod2$residuals))
  # standardized residuals
  expect_equal(as.vector(res$standardized.residuals), as.vector(mod2$residuals/summary(mod2)$sigma))
  # internal studentized
  expect_equal(as.vector(res$internal.studentize), as.vector(rstandard(mod2)))
  # external studentized
  expect_equal(as.vector(res$external.studentize), as.vector(rstudent(mod2)))

  ## test dffit function
  expect_equal(as.vector(dffit(mod)), as.vector(dffits(mod2)))

  ## test cooks function
  expect_equal(as.vector(Cooks(mod)), as.vector(cooks.distance(mod2)))
  })
