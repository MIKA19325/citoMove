
#' @title Simulate Space Use from Step selection function.
#'
#' @description Does not allow integration of the movement process.
#'
#' @param indi Number of individuals in simulation.
#' @param steps Number of time steps.
#' @param randomSteps Number of Steps of which true step is generated
#' @param nEnv Number of environment covariates.
#' @param betaEnv beta coefficient for environment Can be a vector with multiple betas.
#' @param formula Activating effect of environment `formula[1]`, predator `formula[2]` and interaction `formula[3]` of both. Zero is the absence and One is the presence of each effect.
#' @param sd standard deviation of betaEnv for different individuals
#' @param func Equation of model. Relationship between variables. Can be linear, quadratic and exponential.
#'
#' @export
#'

simulateSSF = function(indi = 1,
                       species = 1,
                       steps = 1000,
                       randomSteps = 20,
                       nEnv = 1,
                       betaEnv = 3,
                       betaPred = 0,
                       formula = c(1,0,0),
                       sd = 0,
                       func = "linear"){

  if (nEnv > 1) betaEnvSp_matri = sapply(1:species, function(i) sample(rep(betaEnv, length.out =  nEnv)))
  if (nEnv <= 1) betaEnvSp_matri = matrix(betaEnv, ncol = species)

  Spvec = rep(1:species, length.out = indi)

  betaEnvIndi = sapply(1:indi, function(i) {
    sapply(1:nEnv, function(j) rnorm(1, mean = betaEnvSp_matri[j,Spvec[i]], sd = sd))
    })
betaEnv
  betaPred_vec = rep(betaPred, length.out =  length(betaEnv))
  betaPredIndi = sapply(1:indi, function(i) sapply(1:length(betaPred_vec), function(j) rnorm(1, mean = betaPred_vec[j], sd = sd)))

  betaEnvIndi = matrix(betaEnvIndi, ncol = indi) # col = individual, row = environement
  betaPredIndi = matrix(betaPredIndi, ncol = indi)

  if(func == "linear") Equation = function(i, env,betaEnvIndi_i, Pred, betaPredIndi_i){
    formula[3] * betaPredIndi_i[,] * Pred[,] * env[,i]*betaEnvIndi_i[,i] +
      formula[1] * env[,i]*betaEnvIndi_i[,i] +
      formula[2] * Pred[,]*betaPredIndi_i[,]
  }

  if(func == "quadratic") Equation = function(i, env,betaEnvIndi_i, Pred, betaPredIndi_i) (betaEnvIndi_i[,i]) * env[,i]^2
  if(func == "cubic") Equation = function(i, env,betaEnvIndi_i, Pred, betaPredIndi_i) (betaEnvIndi_i[,i]) * env[,i]^3
  if(func == "logarithmic") Equation = function(i, env,betaEnvIndi_i, Pred, betaPredIndi_i) betaEnvIndi_i[,i]* log(env[,i])
  if(func == "sin") Equation = function(i, env,betaEnvIndi_i, Pred, betaPredIndi_i) betaEnvIndi_i[,i] * sin(env[,i]+1)


  SSFdf_indiv = lapply(1:indi, function(indiv_i) {

    betaEnvIndi_i = matrix(betaEnvIndi[,indiv_i],nrow = 1)
    betaPredIndi_i = matrix(betaPredIndi[,indiv_i],nrow = 1)

    SSFdf_step = lapply(1:steps, function(step) {

      env = sapply(1:nEnv, function(i) runif(randomSteps, 0,1))
      Pred = sapply(1:1, function(i) runif(randomSteps, 0,1))

      rawResponse = sapply(1:nEnv, function(i) Equation(i, env,betaEnvIndi_i, Pred, betaPredIndi_i))

      sumCovariates = sapply(1:randomSteps, function(i) sum(rawResponse[i,]))

      selectionFunction = exp(sumCovariates)/sum(exp(sumCovariates))

      draws = rmultinom(randomSteps, 1, selectionFunction)[,1]

      return(data.frame(env = env,
                        Pred = Pred,
                        rawResponse = sumCovariates,
                        case = draws,
                        step = step,
                        indivID = indiv_i,
                        speciesID = Spvec[indiv_i]))

    })

    return(do.call(rbind, SSFdf_step))

  })

  out = list()
  out$Data = SSFdf = do.call(rbind, SSFdf_indiv)
  out$truth = data.frame(rbind(betaEnvIndi, matrix(betaPredIndi, ncol = indi)))
  colnames(out$truth) = c(paste0("Indiv",1:indi,"Sp",Spvec))
  rownames(out$truth) = c(paste0("env",1:nEnv), paste0("pred"))
  out$StrataSteps = randomSteps

  class(out) = c("citoMoveSimulation")
  return(out)

}




