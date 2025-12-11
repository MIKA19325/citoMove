
movement = citoMove::independentMovement$movement

head(movement)

m = movement[movement$ID == 1,]

focal_track <-  amt::make_track(m ,x,y,Date)
focal_step_track <- amt::steps(focal_track)

plot(focal_track)
class(focal_track)
plot(focal_track)
class(focal_step_track)

x = createRandomSteps(focal_step_track = focal_step_track)
head(x)
