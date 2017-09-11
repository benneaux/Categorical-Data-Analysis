# Galtons fingerprint data
library(broom)
galton.dat <- matrix(c(5,4,1,12,42,14,2,15,10),nrow = 3)
dimnames(galton.dat) <- list(c("Arches","Loops","Whorls"),
                             c("Arches","Loops","Whorls"))
galton.dat

chisq.test(galton.dat, simulate.p.value = TRUE, B = 10000)
test <- tidy(chisq.test(galton.dat, simulate.p.value = TRUE, B = 10000))
