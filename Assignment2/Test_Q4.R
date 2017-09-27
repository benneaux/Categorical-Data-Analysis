naples.dat <- c(16,8,1,1,2,1,4,1,2,2,2,2,1,2,1,3,
            14,26,4,2,6,2,1,2,2,10,6,2,1,6,4,2,
            6,4,36,4,2,14,4,8,1,2,14,2,1,0,82,14,
            4,16,44,10,2,22,22,24,0,10,24,98,4,4,50,382)
naples.dat <- array(naples, dim = c(4,4,4))

dimnames(naples.dat) <- list("Satisfaction"=c("1 (Poor)","2 (Fair)","3 (Good)","4 (Excellent)"),
                         "Cleanliness"=c("1 (Poor)","2 (Fair)","3 (Good)","4 (Excellent)"),
                         "QOM"=c("1 (Low)","2","3","4 (High)"))
naples.dat

ChisqFunc3D.exe <- function(df){
  options(scipen=10)
  options(digits = 4)
  XIJ = apply(df, c(1,2), sum)
  XIK = apply(df, c(1,3), sum)
  XJK = apply(df, c(2,3), sum) 
  
  XIJK = rbind(df[,,1],
               df[,,2],
               df[,,3], 
               df[,,4])
  
  chisqIJ  = chisq.test(XIJ)
  chisqIK  = chisq.test(XIK)
  chisqJK  = chisq.test(XJK)
  chisqIJK = chisq.test(XIJK)
  
  statIJ  = chisqIJ$statistic
  statIK  = chisqIK$statistic
  statJK  = chisqJK$statistic
  statIJK = chisqIJK$statistic
  statSum = sum(statIJ, statIK, statJK, statIJK)
  
  dfIJ  = chisqIJ$parameter
  dfIK  = chisqIK$parameter
  dfJK  = chisqJK$parameter
  dfIJK = chisqIJK$parameter
  
  pIJ  = round(chisqIJ$p.value,2)
  pIK  = round(chisqIK$p.value,2)
  pJK  = round(chisqJK$p.value,2)
  pIJK = round(chisqIJK$p.value,2)
  
  df = array(c(statIJ, statIK,statJK,statIJK, statSum,
              dfIJ,dfIK,dfJK,dfIJK, 27,
              pIJ,pIK,pJK,pIJK, NA), dim = c(5,3))
  dimnames(df) = list(c("XIJ","XIK","XJK","XIJK","X2"),c("X2","DF","Pvalue"))

df
}
options(scipen=10)

df2 = ChisqFunc3D.exe(naples.dat)

# apply(naples, c(1,2), sum)
# apply(naples, c(1,3), sum)
# apply(naples, c(2,3), sum)

naples.conc.dat = rbind(naples.dat[,,1],
                        naples.dat[,,2],
                        naples.dat[,,3], 
                        naples.dat[,,4])

naples.conc.dat
chisq.test(naples.conc.dat)

df = naples.dat
  