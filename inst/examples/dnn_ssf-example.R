fit <- dnn_ssf(
   data = mydata,
  formula = case_ ~ sl_ + hab,
  batchsize = 64
 )
summary(fit)
predict(fit, newdata = mydata)


library(cito)
?cito
?citoMove
