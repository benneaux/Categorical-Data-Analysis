######
library("ca")

######
data("smoke")
ca(smoke)

######
names(ca(smoke))

######
ca(smoke)$rowcoord

######
summary(ca(smoke))

######
summary(ca(smoke, supcol = 1))

######
data(wg93)
mjca(wg93[,1:4])

######
summary(mjca(wg93[,1:4], lambda = "Burt"))

######
plot(ca(smoke, supcol = 1))

######
plot(ca(smoke), mass = TRUE, contrib = "absolute", 
     map = "rowgreen", arrows = c(FALSE, TRUE))

######
plot3d(ca(smoke, nd=3))
