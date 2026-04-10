

#Example: Simulating 2 animal tracks in an environment with 3 habitat variable layers

layer1 <- simulateLayer(size = 200,
                        resolution = 1,
                        clustering  = 15,
                        ratio = 0.3,
                        colour = "darkgreen")

plotSimulation(layer1)

# für alles schon mal auch testthat funktionen schreiben

track2 <- simulateTrack(xStart = 0,
                        yStart = 0,
                        tStart = as.POSIXct("2026-01-01 00:00:00",tz = "UTC"),
                        angleStart = 0,
                        nSteps = 100,
                        nChoiceSet = 10,
                        scaleSL = 2,
                        shapeSL = 2,
                        kappaTA = 2,
                        numericLayers = list(layer1, layer2, layer3),
                        betas = c(-5, 2, 3),
                        colours = c("darkgreen", "darkblue", "darkred"))


a <- simulateLayer(colour = "darkgreen")
plotSimulation(a)

b <- simulateLayer(colour = "darkblue")
plotSimulation(b)
plotSimulation(a, b)

t1 <- simulateTrack(numericLayers = list(a,b), betas = c(-3,3), colour = "black")
plotSimulation(t1)

t2 <- simulateTrack(numericLayers = list(a,b), betas = c(0,3), colour = "darkred")
plotSimulation(t2)
plotSimulation(t1, t2)
sim <- mergeTracks(t1, t2)

plotSimulation(sim)
