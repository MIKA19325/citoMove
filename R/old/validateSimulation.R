#' @title statistics of DNN and logistic regression models on simulation model.
#'
#' @export
returnStatistics <- function(betaEnv = control,
                             env = GroupMovement$environment,
                             steps = steps,
                             focalID = 1,
                             OpponentIDs = NULL,
                             n_control = 19,
                             iSFF = iSFF){

  simulateData = citoMove::simulateMovement(env = env, betaEnv = betaEnv, steps = steps, iSFF =iSFF)

  MoveDat <- createMovementData(x = simulateData$Data$x,
                                     y = simulateData$Data$y,
                                     t = simulateData$Data$t,
                                     ID = simulateData$Data$indivID)

  environmentData <- createEnvironment(env)

  SSF_Data <- prepareSSFData(movement = MoveDat,
                             environment = environmentData,
                             focalID = focalID,
                             n_control = n_control,
                             OpponentIDs = OpponentIDs,
                             BioticI = F)
  SSF_Data$Data$z[is.na(SSF_Data$Data$z)] <- 0

  fittedssf <- fit_issf(case_ ~ z + strata(step_id_), data=SSF_Data$Data)
  #fitgam =  mgcv::gam(case_ ~ z + strata(step_id_), data=SSF_Data$Data, method = "REML", family = binomial(link = "logit"))
  DNNModel = fitDeepSSF(formula = case_ ~ z, data = SSF_Data, lr = 1.1, epochs= 30, plot = F, verbose = F)

  mod = "SSF"
  est <- coef(fittedssf)[[1]]
  ci <- confint(fittedssf$model)
  pvalue = summary(fittedssf)$coefficients[5]
  se = summary(fittedssf)$coefficients[3]

  out = data.frame(mod, est, se, pvalue, ci)
  names(out)[5:6] = c("lw", "up")

  # out[2,"mod"] = "GamBinomLogit"
  # out[2,"est"] = fitgam$coefficients[[2]]
  # out[2,"se"] = summary(fitgam)$se[2]
  # out[2,"lw"] = fitgam$coefficients[[2]] - qnorm(0.975) * summary(fitgam)$se[2]
  # out[2,"up"] = fitgam$coefficients[[2]] + qnorm(0.975) * summary(fitgam)$se[2]
  # out[2,"pvalue"] = summary(fitgam)$p.pv[[2]]

  out[2,"mod"] = "DNN"
  out[2,"est"] = cito::conditionalEffects(DNNModel)[[1]]$mean

  return(out)
}


#' @title Repetition of Simulation
#' @description  run Simulation multiple times and return number of statistics, typically estimates, p-values and confidence interval.
#'
#' @export

runSimulation <- function(nRep = 10, control = 3, iSFF = FALSE, steps = 500){
  Simulationstat = data.frame()
  for(rep in 1:nRep){
    stat_i = returnStatistics(betaEnv = control, iSFF = iSFF, steps = steps)
    stat_i[1,"control"] = ifelse(any(control > stat_i$lw[1] & control < stat_i$up[1]), "YES", "NO")
    stat_i[1,"signif"] = ifelse(any(0.05 > stat_i$pvalue[1]), "*", "")
    # stat_i[2,"control"] = ifelse(any(control > stat_i$lw[2] & control < stat_i$up[2]), "YES", "NO")
    # stat_i[2,"signif"] = ifelse(any(0.05 > stat_i$pvalue[2]), "*", "")
    stat_i$rep = as.factor(rep)
    Simulationstat = rbind(Simulationstat, stat_i)
    message(paste("Repetition :",rep))
  }
  rownames(Simulationstat) = 1:nrow(Simulationstat)
  out = list()
  out$Simulationstat = Simulationstat
  out$trueEstimate = control
  class(out) = "citoMoveBenchmark"
  return(out)
}

#' @title plot statistics of Data with class citoMoveBenchmark
#'
#' @export
citoMoveBenchmark <- function(x){
  ggplot(x$Simulationstat, aes(y = est, x = rep))+
    geom_hline(yintercept = x$trueEstimate, color = "grey75", linewidth = .9) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = .5,linetype = "dashed",)+
    geom_point()+
    facet_grid(cols = vars(mod)) +
    geom_errorbar(aes(ymin=lw,ymax=up,width=0.2))+
    geom_text(aes(y = 5,label = signif))+
    theme_classic()
}






