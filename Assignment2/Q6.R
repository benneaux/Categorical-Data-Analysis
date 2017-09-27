library(rgl)
library(ca)
library(reshape)

sib.dat <- c(15,31,35,18,34,60,45,14,36,46,30,3,22,25,13,3,61,26,8,4,
             17,60,63,15,53,96,74,15,70,45,39,9,67,40,24,2,79,31,7,1,
             7,5,5,2,20,12,10,1,23,11,4,2,16,12,4,0,36,7,3,1)
sib.dat <- array(sib.dat, dim = c(4,5,3))

dimnames(sib.dat) <- list("Years Of Schooling"=c("<12","12","13-16","17+"),
                          "Number of Siblings"=c("0-1","2-3","4-5","6-7","8+"),
                          Happiness=c("Not Too Happy","Pretty Happy","Very Happy"))
sib.dat


tab.long<-melt(sib.dat)
colnames(tab.long) <- c("Schooling","Siblings","Happiness","Freq")
expand.sibs <- tab.long[rep(row.names(tab.long), tab.long$Freq),1:3]

ca_sibs1 <- ca(sib.dat[,,1], nd = 3)
ca_sibs2 <- ca(sib.dat[,,2], nd = 3)
ca_sibs3 <- ca(sib.dat[,,3], nd = 3)

mjca_sibs <- mjca(expand.sibs)
plot3d.ca(ca(as.data.frame(sib.dat)), dim = c(1,2,3), what = c("all","all","supplementary"),
            contrib = c("absolute","absolute"), arrows = c(TRUE,FALSE))

plot3d.ca(ca_sibs1)
plot3d.ca(ca_sibs2)
plot3d.ca(ca_sibs3)

data("wg93")
mjca(wg93)
# data("smoke")
# ca(smoke, nd=3)
# plot3d.ca(ca(smoke,nd=3))
