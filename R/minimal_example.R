library(lavaan)
model <- '
  y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8
  x =~ x1 + x2 + x3
  y ~ x
'
fit <- cfa(model, PoliticalDemocracy, estimator = "WLSMV")
summary(fit, fit = TRUE, std = TRUE)
