#### STANDARDIZED BIAS TESTS
stdbias <- function(formula, data) {

  # note: we assume the first covariate is the treatment indicator
  reg <- lm( formula = formula , data = data)
  sdbias <- reg$coeff[2]/sd(reg$model[,1]) #<-std deviation of 'y'
  mean1 <- round(reg$coeff[1] + reg$coeff[2], digits = 4)
  mean0 <- round(reg$coeff[1], digits = 4)
  variable <- names(reg$model[2])
  ci <- confint.lm(reg, variable , level = 0.95)
  out <- data.frame(
    Name = names(reg$model[1]),
    "Standardized.bias" = sdbias,
    Treatment = mean1,
    Control = mean0,
    "Confidence Interval" = paste0("(",round(ci[1], digits = 4),", ", round(ci[2], digits = 4),")"),
    pvalue = round(coef(summary(reg))[2,4],digits = 4),
    N = length(reg$residuals)
  )
  return(out)

}
