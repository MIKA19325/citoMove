
formula = case_~Distance_others
batch_size = 500

SSF_Data$InteractData$case_ = as.integer(SSF_Data$InteractData$case_ )

SSF_Data$InteractData = SSF_Data$InteractData[1:5000,]
SSF_Data$InteractData$Distance_others = scale(SSF_Data$InteractData$Distance_others)[,1]

clogit_mod = mgcv::gam(case_ ~ Distance_others
                       + strata(step_id_),
                       data=SSF_Data$InteractData,
                       family = binomial(link = "logit"),
                       method = "REML")
coef(clogit_mod)[2]
summary(clogit_mod)
fits <- predict(clogit_mod)
table(apply(as_array(torch_tensor(predict(clogit_mod))$reshape(list(-1L, 20L))$softmax(dim = 2L)), 1, which.max))
exp(-44.4515)

m = fitDeepSSF(formula = formula,
               data = SSF_Data,
               lr= 10.1,
               epochs = 1000L,
               burnin = 1000L,
               hidden = NULL,
               batchsize=nrow(SSF_Data$InteractData))

ACE = cito::conditionalEffects(m)
conditionalEffects(m)

apply(matrix(ACE[[1]]$result, ncol = 20L, byrow = TRUE), 2, mean)
coef(m)

cito::ALE(m)
summary(m)

table(apply(as_array(torch_tensor(predict(m))$reshape(list(-1L, 20L))$softmax(dim = 2L)), 1, which.max))

