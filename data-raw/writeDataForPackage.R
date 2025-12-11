library(amt)
library(dplyr)
library(citoMove)
## Save SSF Data

envRaster = GroupMovement$environment
dat = citoMove::simulateMovement(steps = 1000, env = envRaster)

movementData <- createMovementData(x = dat$Data$x,
                                   y = dat$Data$y,
                                   t = dat$Data$t,
                                   ID = dat$Data$indivID)

environmentData <- createEnvironment(envRaster)

SSF_Data <- prepareSSFData(movement = movementData,
                           environment = environmentData,
                           focalID = 1,
                           OpponentIDs = 1,
                           n_control = 19)

SSF_Data$SSFData$z[is.na(SSF_Data$SSFData$z)] <- 0

Move1FEnv = SSF_Data
usethis::use_data(Move1FEnv, overwrite = TRUE)

## make predator raster for simulation

env = GroupMovement$environment
pred = independentMovement$movement
pred = pred[pred$ID == 1,]

rasti = raster(terra::rast(terra::ext(env), resolution=res(env)))
crs(rasti) = 3035

for (k in 1:nrow(pred)){
  pred_i <- subset(pred, pred$t == k)
  curr_ras_toadd <- raster::distanceFromPoints(rasti, pred_i[,c("x","y")])
  rasti <- addLayer(rasti,curr_ras_toadd)
}
usethis::use_data(predatorRaster, overwrite = TRUE)



