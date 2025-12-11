library(citoMove)

# this uses the dataset independentMovement


env = GroupMovement$environment
dat = simulateMovement(env = env)
class(dat)

# if no ID is provided, ID and species is set to 1 for all observations
# better to provide ID  (and possibly species)for each observation
movementData <- createMovementData(x = dat$Data$x,
                                     y = dat$Data$y,
                                     t = dat$Data$t,
                                   ID = dat$Data$indivID)

head(movementData$data)

# this creates the RasterLayer of the environement

environmentData <- createEnvironment(env)

# this creates the SSF Data set on which the DeepSSF can be fitted. Random steps are resampled from the provided track.

SSF_Data <- prepareSSFData(movement = movementData,
                           environment = environmentData,
                           focalID = 1,
                           OpponentIDs = 1,
                           n_control =19)


# Random steps can also be resampled from a theoretical track.
crw = crwMovement$crwMoveSteps

SSF_Data <- prepareSSFData(movement = movementData,
                           environment = environmentData,
                           focalID = 1,
                           OpponentIDs = 1,
                           n_control =19,
                           sl_distr = fit_distr(crw$sl_, "gamma"),
                           ta_distr = fit_distr(crw$ta_, "vonmises"))





