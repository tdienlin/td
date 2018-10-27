library(lavaan)
model <- '
  y_1 =~ y1 + y2 + y3 + y4
  y_2 =~ y5 + y6 + y7 + y8
  x =~ x1 + x2 + x3
  y_1 ~ a*x
  y_2 ~ a*x
'
# fit <- cfa(model, PoliticalDemocracy, estimator = "WLSMV")
# summary(fit, fit = TRUE, std = TRUE)
