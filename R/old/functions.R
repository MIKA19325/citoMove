#' @title Create movement data
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param t Time stamp
#' @param ID Identification of animal
#' @param species Species identification of animal
#'
#'
#' @export
#' @example /inst/examples/prepareDataHelp.R
#'
createMovementData <- function(x,y, t, ID = 1,species=1){
  if (missing(x) | missing(y) | missing(t)) {
    stop("x, y and t are required.")
  }

  out = list()
  out$data = data.frame(x = x, y=y, t = t, ID = ID, species = species)
  out$n_of_individuals = length(unique(out$data$ID))
  out$n_of_species = length(unique(out$data$species))
  class(out) = "citoMovementData"
  return(out)
}

# plot.citoMovementData <- function(x,...) {
#   plot(x=x$x, y=x$y, main='Movement Track',type = 'p', col=x$ID)
# }

#' @title Prepare environmental data
#'
#' @export
#' @example /inst/examples/prepareDataHelp.R
createEnvironment <- function(env){
  out = list()

  if(class(env) == "RasterLayer"){
    out$env = env
  }

  if(class(env) == "data.frame"){
    if (missing(x) | missing(y) | missing(z)) {
      stop("x, y and z are required.")
    }else{
      out$env = raster::rasterFromXYZ(env)
    }
  }

  if(class(env) != "data.frame" & class(env) != "RasterLayer"){
    stop("env needs to be either a data.frame or a RasterLayer")
  }

  out$creationdate = Sys.Date()
  class(out) = "citoMoveEnvironment"
  return(out)
}



# createRandomData <- function(movement, focalID, randomPoints){
#   error("not implement yet")
# }

#' @title Generate random steps
#' @description  Generate random steps/pseudo-abscences and create a binomial data set for conditional logistic regression.
#' @param ... arguments to be forwarded to \code{\link{amt::random_steps}}

randomResampleTrack <- function(){
  out = list()
  out$method = "ResampleTrack"
  out$createRandomResampleTrack<- function(movement, focalID,n_control,...){
    focal_track <-  amt::make_track(movement[movement$ID == focalID, ] ,x,y,t, species)
    focal_step_track <- amt::steps(focal_track)
    RTrack <-  amt::random_steps(focal_step_track,n_control=n_control,...)
    RTrack$FocalID = focalID
    RTrack$FocalSpecies = unique(focal_track$species)
    return(RTrack)
  }
  class(out) = c("citoMoveRandomMethod")
  return(out)
}


# randomUserDefined <- function(x,y,t){
#   out = list()
#   out$method = "UserDefined"
#   out$createPAtrack <- function(movement, focalID){
#   }
# }

#' @title Calculate other individual covariates.
#' @param distance Method of calculating the distance between focal individual and other individuals
#' @param kernel Method of calculating the kernel of distance between focal individual and other individuals
#'
interactionKernel <- function(distance = "Euclidian", kernel = function(x, sd)dnorm(x, sd = 1)){
  out = list()
  out$method = "kernel"
  out$distance = "Euclidian"
  if(out$distance == "Euclidian"){
    out$distanceFunction = function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  }

  out$createInteractPredictors <- function(focalData, movement, OpponentIDs, n.points = 0){
    focalprecitData = data.frame()
    for (other in OpponentIDs){
      dist_list <- c()
      other_data = movement[which(movement$ID %in% other),]

      for (k in unique(focalData$step_id_)){
        focal <- focalData[which(focalData$step_id_ == k),]
        others <- subset(other_data, other_data$t %in% unique(focalData$step_id_)[c(((k+1)-n.points):(k+1))])
        for (i in 1:nrow(focal)){
          focal_i = focal[i,]
          focal_i = focal_i[rep(1, nrow(others)),]
          dist_list <- append(dist_list, min(out$distanceFunction(focal_i[,c("x2_","y2_")], others[,c("x","y")])))
        }
      }

      focalprecitData_i <- focalData
      focalprecitData_i[,paste0("OpponentID_S", unique(other_data$species))] <- other
      focalprecitData_i[,paste0("BI_S",unique(other_data$species))] <- dist_list
      #focalprecitData_i[,"Kernel_others"] <- kernel(dist_list, sd = 10)

      focalprecitData = rbind(focalprecitData, focalprecitData_i)

    }
    return(focalprecitData)
  }

  class(out) = c("citoMoveInteractionMethod")
  return(out)
}

#' @title Create citoMoveData needed for \code{\link{fitDeepSSF}}
#' @param movement object of class citoMovementData created by \code{\link{createMovementData}}
#' @param environment Raster environment
#' @param focalID ID of focal individual of interest. Analyse if focal individual is interacting with other individuals.
#' @param OpponentsID ID of other individuals of interest, to which focal individual might be interacting with.
#' @param randomMethod object of class citoMoveRandomMethod created by \code{\link{randomResampleTrack}}
#' @param interactionMethod object of class citoMoveInteractionMethod created by \code{\link{interactionKernel}}
#' @param BioticI logical if true interactionMethod is used to calculate biotic covariates. For now Euclidian distance is calculated between individuals.
#'
#' @export
#' @example /inst/examples/prepareDataHelp.R
#'
prepareSSFData <- function(movement,
                           environment,
                           focalID,
                           OpponentIDs = NULL,
                           randomMethod = randomResampleTrack(),
                           interactionMethod = interactionKernel(),
                           BioticI = FALSE,
                           n_control = 19,
                           ...){


  # check classes / inputs
  move_dat = movement$data
  env_dat = environment$env

  if (length(OpponentIDs) > 0){
    if(FALSE %in% (OpponentIDs %in% move_dat$ID)) {
      missingOpponent = OpponentIDs[!OpponentIDs %in% move_dat$ID]
      for (i in 1:length(missingOpponent)) message(paste("Opponent ID", missingOpponent[i], "does not exist in Movement Data"))
      stop("Please use OpponentIDs present in MovementData")
    }
  }

  nInd_per_Species = aggregate(ID ~ species , move_dat, FUN = function(x) length(unique(x)))
  names(nInd_per_Species)[2] <- "nIndiv"


  out = list()

  if(BioticI == T){
    InteractData = data.frame()
    for (focal_i in focalID){
      opponent_i = OpponentIDs[!focal_i == OpponentIDs]
      PAtrack = randomMethod$createRandomResampleTrack(movement = move_dat, focalID = focal_i, n_control = n_control,...)

      if(length(opponent_i) == 0) {
        PAtrack[setdiff(names(InteractData), names(PAtrack))] <- NA
        InteractData = rbind(InteractData,PAtrack)

        message(paste("Focal ID", focal_i, "does not have an biotic interaction with another individual."))
      }else{
        BioticIData = interactionMethod$createInteractPredictors(focalData = PAtrack, movement = move_dat,
                                                   OpponentIDs = opponent_i)
        BioticIData[setdiff(names(InteractData), names(BioticIData))] <- NA
        try(InteractData[setdiff(names(PAtrack), names(InteractData))] <- NA, silent=TRUE)

        InteractData = rbind(InteractData,BioticIData)

        }
    }
    out$Data = InteractData[!InteractData$step_id_ == max(InteractData$step_id_),]

  }else{
    if (length(OpponentIDs) > 0) message("No biotic interaction will be calculated between the focalIDs and the selected opponendIDs")
    nonBioticData = data.frame()
    for (focal_i in focalID){
      PAtrack = randomMethod$createRandomResampleTrack(movement = move_dat, focalID = focal_i, n_control = n_control,...)
      nonBioticData = rbind(nonBioticData, PAtrack)
    }
    out$Data = nonBioticData
  }

  out$Data = amt::extract_covariates(out$Data, env_dat, where = "end")
  out$Data$log_sl_ =  log(out$Data$sl_)
  out$Data$cos_ta_ =  cos(out$Data$ta_)
  out$Data$case_ = as.integer(out$Data$case_)
  out$Data$z[is.na(out$Data$z)] <- 0
  out$StrataSteps = n_control + 1
  out$nSpecies = movement$n_of_species
  out$nIndividuals = movement$n_of_individuals
  out$nInd_per_Species = nInd_per_Species
  class(out) = "citoMoveSSFData"
  return(out)

}

