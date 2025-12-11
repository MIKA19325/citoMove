
#' @title Simulate movement of Predator and create Stacked raster Layer

movePred = function(env, steps, slShape, slScale){
  xi_pred_step = c()
  yi_pred_step = c()
  xi = 50
  yi = 50

  for(step in 1:steps){
    sl_pred = rgamma(1, shape = slShape, scale = slScale)
    ta_pred = suppressWarnings(circular::rvonmises(20, mu = 0, kappa = 0))

    potential_xi = xi + sl_pred * cos(ta_pred)
    potential_yi = yi + sl_pred * sin(ta_pred)


    df_step = data.frame(potential_xi, potential_yi)
    df_step = df_step[potential_xi < 100 & potential_xi > 0 & potential_yi < 100 & potential_yi > 0,]
    df_step = df_step[sample(nrow(df_step),1),]

    xi_pred_step = append(xi_pred_step, df_step[,1])
    yi_pred_step = append(yi_pred_step, df_step[,2])

    xi = xi_pred_step[step]
    yi = yi_pred_step[step]
  }
  dfPred = data.frame(x = xi_pred_step, y= yi_pred_step,
                      t = c(1:steps))
  rasti = raster(terra::rast(terra::ext(env), resolution=res(env)))

  for (k in 1:nrow(dfPred)){
    pred_i <- subset(dfPred, dfPred$t == k)
    curr_ras_toadd <- raster::distanceFromPoints(rasti, pred_i[,c("x","y")])
    rasti <- addLayer(rasti,curr_ras_toadd)
  }
  return(rasti)

}

#' @title Simulate movement data
#'
#' @param indiv Number of individuals in simulation
#' @param steps Number of time steps
#' @param startXY Starting coordinates. A list with two elements : x and y coordinates. Starting coordinates should be within landscape.Add starting coordinates for each individual. Otherwise the given coordinates will be repeated to the number of individuals.
#' @param startTA Starting turning angles. Add starting turning angles for each individual. Otherwise the given turning angle will be repeated to the number of individuals.
#' @param randomSteps Number of Steps of which true step is generated
#' @param vonMisesK Kappa parameter of vonMises distribution used for generating random steps
#' @param slShape Shape parameter of Gamma distribution used for generating random steps
#' @param slScale Scale parameter of Gamma distribution used for generating random steps
#' @param betaEnv beta coefficient for Habitat
#' @param betaPred beta coefficient for Predator
#' @param env Environment Raster Layer
#' @param Predator Logical if TRUE then a Predator Occurrence raster is simulated in addition to the Habitat Raster
#' @param iSFF Logical if TRUE the true steps is sampled based on turning angle and step lengths distributions. Otherwise the true step is only sampled based on environment in simulation.
#'
#' @export
#' @example /inst/examples/simulateMovementHelp.R
#'


simulateMovement <- function(indiv = 1,
                             startXY = list(x = NULL, y = NULL),
                             startTA = NULL,
                             steps = 500,
                             randomSteps = 10000,
                             vonMisesK = 1,
                             slShape = 1,
                             slScale = 0.5,
                             betaEnv = 5,
                             betaPred = 0,
                             env,
                             Predator = F,
                             iSFF = F,
                             PredInt = F){


  listbetaEnv = rep(betaEnv, length.out = max(indiv))
  listbetaPred = rep(betaPred, length.out = max(indiv))

  startXY = suppressWarnings(list(x = rep(startXY$x, length.out = max(indiv)), y = rep(startXY$y, length.out = max(indiv))))
  startTA = suppressWarnings(rep(startTA, length.out = max(indiv)))

  out = list()

  if(Predator == T) {
    predatorRaster = movePred(env, steps, slShape, slScale)
    message("Predator Raster created")
    out$PredatorRaster = predatorRaster
  }
  simulateMove = data.frame()

  for (indiv_i in 1:indiv){

    betaEnv_i = listbetaEnv[indiv_i]
    betaPred_i = listbetaPred[indiv_i]

    simulateMove_i = data.frame()

    if (is.null(startXY$x) | is.null(startXY$y)){
      xi = env@extent@xmax/2 + runif(1,-env@extent@xmax/5,env@extent@xmax/5)
      yi = env@extent@ymax/2 + runif(1,-env@extent@ymax/5,env@extent@ymax/5)
    }else{
      xi = startXY$x[indiv_i]
      yi = startXY$y[indiv_i]
    }

    if (is.null(startTA)) ta_i = circular::rcircularuniform(1) else ta_i = startTA[indiv_i]


    for (step in 1:steps){

      # Create random steps based on uniformly distributed steplength and turning angle around current xy position

      slga = rgamma(n = 1000, shape = slShape, scale = slScale)
      maxsl = mean(slga)+ 2*sd(slga)
      sl_rs = runif(randomSteps,0.1,maxsl)
      ta_rs = sapply(circular::rcircularuniform(randomSteps), function(obj) obj[[1]])

      xi_potential_step = xi + sl_rs * cos(ta_rs)
      yi_potential_step = yi + sl_rs * sin(ta_rs)

      df = data.frame(rs_id = c(1:randomSteps), x = xi_potential_step, y= yi_potential_step,
                      t = step, indivID = indiv_i,
                      ta_rad = ta_rs , sl_gamma = sl_rs)

      # Calculate Movement free Selection Kernel : #xp(β1x1(s) + · · · + βk xk (s)).

      df$env_cov = raster::extract(env, df[,c("x","y")])
      if(betaEnv_i == 0) df$env_cov = runif(randomSteps)
      if(Predator == T) {
        df$pred_cov = raster::extract(predatorRaster[[step]], df[,c("x","y")])
        df$pred_cov = df$pred_cov/100
        }

      #df$pred_cov = dnorm(pred_cov,mean = 0, sd = 15)

      df = na.omit(df)

      if(Predator == T) {
        selection_function =
          betaEnv_i * df$env_cov +
          betaPred_i * df$pred_cov
        if(PredInt == T){
          selection_function =
            df$pred_cov * (betaEnv_i * df$env_cov) +
            betaPred_i * df$pred_cov
        }

      } else {selection_function = betaEnv_i * df$env_cov}

      denominator = sum(exp(selection_function))
      df$envKernel = exp(selection_function)/denominator


      if(iSFF == T){

        # Calculate Selection Free Movement Kernel

        ta_diff <- (df$ta_rad - ta_i)
        ta_diff <- ifelse(ta_diff > pi, ta_diff - 2*pi, ta_diff)
        suppressWarnings({ta_likeli <- circular::dvonmises(ta_diff, mu = 0, kappa = vonMisesK)})

        sl_likeli <- dgamma(x = df$sl_gamma, shape = slShape, scale = slScale)



        movement_kernel = ta_likeli * sl_likeli
        df$moveKernel = movement_kernel/sum(movement_kernel)

        #Redistribution Kernel = product of the movement kernel and the selection function

        step_kernel = df$envKernel * df$moveKernel
        df$stepKernel = step_kernel / sum(step_kernel)

      } else{

        df$stepKernel = df$envKernel

      }


      #ggplot(df) + geom_point(aes(x = x, y = y, color = stepKernel))

      # Sample step based on conditional probability

      true_step = sample(x =  df$rs_id, 1, replace = T, prob = df$stepKernel)

      # Update Data frame and t-1 xy coordinates and turning angle

      simulateMove_i = rbind(simulateMove_i, df[df$rs_id == true_step,])

      xi = simulateMove_i[simulateMove_i$t == step,"x"]
      yi = simulateMove_i[simulateMove_i$t == step,"y"]
      ta_i = simulateMove_i[simulateMove_i$t == step,"ta_rad"]


    }

    simulateMove = rbind(simulateMove, simulateMove_i)
    rownames(simulateMove) = 1:nrow(simulateMove)

  }

  out$Data = simulateMove
  out$betaEnvs = listbetaEnv
  out$betaPred = listbetaPred
  class(out) = c("citoMoveSimulation")
  return(out)
}

