

# Default simulates movement of one individual within heterogeneous landscape to which it is attracted. Available steps are emerging from the position of the previous step. Based on covariates of landscape the one true step is then selected.

env = GroupMovement$environment

# Only Movement free selection function

simulateSF = simulateMovement(env = env)

#If iSSF == T then the turning angle and step lengths is integrated in the selection of the true step.
# Redistribution Kernel = product of the movement kernel and the selection function

simulateRK = simulateMovement(env = env, iSFF = T)


# if needed a predator can also be simulated. The simulated individual can then express an avoidance or attraction towards this predator. The movement of the predator is converted into a in a stacked raster layer with distances of the predator to all other cells in the landscape. Thus extracting the covariate from the raster will be the euclidean distance of the simulated individual to the predator.

simulatePR = simulateMovement(env = env, Predator = T, betaPred = 0)
