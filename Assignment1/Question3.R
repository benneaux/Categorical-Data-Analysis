#Question 3

drug.dat <- matrix(c(5,5,10,7,1,3,6,12,10,3,12,8,8,8,3,1,6,12,0,1), nrow = 4)
dimnames(drug.dat) = list(c("A","B","C","D"),c("Poor","Fair","Good","Very_Good","Excellent"))

drug.dat
str(drug.dat)

chisq.test(drug.dat)


(drug.dat.10    <- chisq.test(drug.dat, simulate.p.value = TRUE, B=10))
(drug.dat.100   <- chisq.test(drug.dat, simulate.p.value = TRUE, B=100))
(drug.dat.1000  <- chisq.test(drug.dat, simulate.p.value = TRUE, B=1000))
(drug.dat.10000 <- chisq.test(drug.dat, simulate.p.value = TRUE, B=10000))

q3ii <- matrix(c(drug.dat.10$p.value,
                 drug.dat.100$p.value,
                 drug.dat.1000$p.value,
                 drug.dat.10000$p.value),
               nrow = 4)
dimnames(q3ii) <- list(c("10","100","1000","10000"),c("p.value"))
q3ii
