library(terra)
library(amt)

# simulate layers
layer1 <- simulateLayer(size = 200, clustering = 15, ratio = 0.5, colour = "darkgreen")
layer2 <- simulateLayer(size = 200, clustering = 20, ratio = 0.3, colour = "steelblue")

# simulate tracks
track1 <- simulateTrack(xStart = 0,
                        yStart = 0,
                        nSteps = 100,
                        colour = "black",
                        numericLayers = list(layer1, layer2),
                        betas = c(-5, -3))

track2 <- simulateTrack(xStart = 10,
                        yStart = 10,
                        nSteps = 100,
                        colour = "red",
                        numericLayers = list(layer1, layer2),
                        betas = c(-5, 3))

# merge tracks
simulation <- mergeTracks(track1, track2)

# plot
plot(simulation)
