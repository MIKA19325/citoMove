library(citoMove)
library(torch)

# Example formula for fitDeepSSF function

# 1 Focal interaction with environment

# formula = c(0,0,0) = effect of the environment but no effect of predator and no interaction of pred*env
data = simulateSSF(indi = 1, betaEnv = 3, formula = c(1, 0, 0))

fitDNN <- fitDeepSSF(case~env , data = data, lr = 1.1, epochs = 30, batchsize = 100 )


ALE(fitDNN)
#summary(fit_1F1O_NoEnv)


# Calculate average conditional effects
ACE = cito::conditionalEffects(fitDNN)

# Average conditional effects of each step_id for Biotic interaction
apply(matrix(ACE[[1]]$result, ncol = 20L, byrow = TRUE), 2, mean)
coef(fitDNN)


# ignore : not working yet
# with embeddings

table(SSF_Data$InteractData$individual)

SSF_Data$InteractData$individual = ifelse(SSF_Data$InteractData$individual == 3, 2, SSF_Data$InteractData$individual)

fit2 <- fitDeepSSF(case_~ Distance_others+ e(focal, lambda = 0.0001) + e(opponent, lambda = 0.0001), data = SSF_Data, lr = 1.2, epochs = 100L, batchsize = 1080L, burnin = 100L)
ce = coef(fit2)
ce[[1]]$e_1.weight

effs = conditionalEffects(fit2)
mean(effs[[1]]$result[,1,1][SSF_Data$InteractData$individual == 1])
mean(effs[[1]]$result[,1,1][SSF_Data$InteractData$individual == 2])



