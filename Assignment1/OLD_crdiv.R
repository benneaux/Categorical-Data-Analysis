
selikoff.dat <- matrix(c(310,212,21,25,7,36,158,35,102,35,0,9,17,49,51,0,0,4,18,28), nrow = 5)
dimnames(selikoff.dat) <- list(c("0-9","10-19","20-29","30-39","40+"),c("None","Grade 1", "Grade 2", "Grade 3"))

galton.dat <- matrix(c(5,4,1,12,42,14,2,15,10),nrow = 3)
dimnames(galton.dat) <- list(c("Arches","Loops","Whorls"),
                             c("Arches","Loops","Whorls"))
# x2 <- as.numeric(chisq.test(selikoff.dat)$statistic)
# rr <- rowSums(selikoff.dat)
# cc <- colSums(selikoff.dat)
#
# B = 10000
# dum = vector(mode = "numeric", length = B)
# for(i in 1:B) {
#   dum[i] = chisq.test(r2dtable(1,rr,cc)[[1]])$statistic
# }
# MC.p.value <- length(dum[dum > x2])/B
# MC.p.value
# hist(dum)

#
# divergeCR.exe <- function(ctable,
#                           correct = TRUE,
#                           correct.val = 0.5,
#                           plot = TRUE){
#
#   # ctable: a contingency table
#   # lambda: the value for lambda to be used by the CR divergence. Defaults to 2/3.
#   # correct: if 0 values present, should they be replaced to avoid errors? Defaults to TRUE.
#   # correct.val: if correct = TRUE, what value should be used to replace 0? Defaults to 0.5.
#   options(digits = 15, warn = FALSE)
#   rows = nrow(ctable)
#   cols = ncol(ctable)
#   ctsum = sum(ctable)
#
#   if(correct){
#     for(m in 1:rows){
#       for(n in 1:cols){
#         if(ctable[m,n] == 0){
#           ctable[m,n] = correct.val
#         }
#       }
#     }
#   }
#
#   ptable = prop.table(ctable)
#   rr = rowSums(ptable)
#   cc = colSums(ptable)
#   # Pearson
#   pearsonmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       pearsonmatrix[m,n] = ((ptable[m,n] - rr[m]*cc[n])^2)/(rr[m]*cc[n])
#     }
#   }
#
#   pearsonstat = ctsum*sum(pearsonmatrix)
#   p.pearsonstat = 1 - pchisq(pearsonstat, 1)
#
#   # Log-Likelihood
#   llmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       llmatrix[m,n] = ptable[m,n]*log(ptable[m,n]/(rr[m]*cc[n]))
#     }
#   }
#
#   llstat = 2*ctsum*sum(llmatrix)
#   p.llstat = 1 - pchisq(llstat, 1)
#
#   # Freeman-Tukey
#   ftmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       ftmatrix[m,n] = (sqrt(ptable[m,n]) - sqrt(rr[m]*cc[n]))^(2)
#     }
#   }
#
#   ftstat = 4*ctsum*sum(ftmatrix)
#   p.ftstat = 1 - pchisq(ftstat, 1)
#
#   # Neyman's Modified
#   nmmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       nmmatrix[m,n] = ((ptable[m,n] - (rr[m]*cc[n]))^(2))/ptable[m,n]
#     }
#   }
#
#   nmstat = ctsum*sum(nmmatrix)
#   p.nmstat = 1 - pchisq(nmstat, 1)
#
#
#   # Cressie-Read
#   crmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       crmatrix[m,n] = ptable[m,n]*((ptable[m,n]/(rr[m]*cc[n]))^(0.67) -1)
#     }
#   }
#
#   crstat = (2 * ctsum/((0.67)*(0.67 + 1)))*sum(crmatrix)
#   p.crstat = 1 - pchisq(crstat, 1)
#
#   # Modified Log-Likelihood
#   mllmatrix = matrix(NA, nrow = rows, ncol = cols)
#   for(m in 1:rows){
#     for(n in 1:cols){
#       mllmatrix[m,n] = (rr[m]*cc[n])*log((rr[m]*cc[n])/ptable[m,n])
#     }
#   }
#
#   mllstat = 2*ctsum*sum(mllmatrix)
#   p.mllstat = 1 - pchisq(mllstat, 1)
#
#   # Output
#   out = matrix(c(-2,-1,-1/2,0,0.67,1,
#                  nmstat,  mllstat,  ftstat,  llstat,  crstat,  pearsonstat,
#                  p.nmstat,p.mllstat,p.ftstat,p.llstat,p.crstat,p.pearsonstat),
#                ncol = 3)
#
#   dimnames(out) = list(c("Neyman's Modified","Modified Log-Likelihood",
#                          "Freeman-Tukey","Log-Likelihood",
#                          "Cressie-Read","Pearson"),
#                        c("Lambda","Stat","p-value"))
#   options(digits = 7)
#
#   if(plot){
#     # par(mfrow = c(2,2))
#     # x = nmstat
#     # curve(pchisq(x, df=1), col='blue', main = "Chi-Square Density Graph",
#     #        from=-0.001,to=x)
#     # x = mllstat
#     # curve(pchisq(x, df=1), col='green',
#     #       from=-0.001,to=x)
#     # x = ftstat
#     # curve(pchisq(x, df=1), col='orange',
#     #       from=-0.001,to=x)
#     # x = crstat
#     # curve(pchisq(x, df=1), col='red',
#     #       from=-0.001,to=x)
#   }
#   return(out)
# }
#
# divergeCR.exe(selikoff.dat)

# plotCR.exe <- function(ctable, resolution = 1000, correct = TRUE, correct.val = 0.5){
#   options(digits = 7, warn = FALSE)
#   x = seq(from = -1, to = 1, length.out = resolution)
#   vcases = c(-1,-0.5,0,1)
#   hcases = vector(mode = "numeric", length = length(vcases))
#   for(i in 1:length(vcases)){
#     sqvals = abs(x-vcases[i])^2
#     val = min(abs(x + vcases[i])^2)
#     hcases[i] = which(sqvals == val)
#   }
#   crstat = vector(mode = "numeric", length = resolution)
#   crstat = cbind("lambda" = x,"CR(lambda)" = crstat)
#   rows = nrow(ctable)
#   cols = ncol(ctable)
#
#   if(correct){
#     for(m in 1:rows){
#       for(n in 1:cols){
#         if(ctable[m,n] == 0){
#           ctable[m,n] = correct.val
#         }
#       }
#     }
#   }
#
#   ctsum = sum(ctable)
#   ptable = prop.table(ctable)
#   rr = rowSums(ptable)
#   cc = colSums(ptable)
#
#   for(i in 1:resolution){
#     lambda = crstat[i,1]
#     crmatrix = matrix(NA, nrow = rows, ncol = cols)
#     for(m in 1:rows){
#       for(n in 1:cols){
#         crmatrix[m,n] = ptable[m,n]*((ptable[m,n]/(rr[m]*cc[n]))^(lambda) - 1)
#       }
#     }
#     crstat[i,2] = (2 * ctsum/(lambda*(lambda + 1)))*sum(crmatrix)
#   }
#   plot(crstat, type = "l", lwd = 2, ylim = c(min(crstat[,2]*0.99),max(crstat[2:nrow(crstat),2])*1.01))
#   abline(v = 0.67, col = "red")
#   abline(h = min(crstat[,2]), col = "red")
#   abline(h = c(crstat[2,2],crstat[hcases[2:length(hcases)],2]), col = "lightgray", lty = c(3,4,5,6))
#   abline(v = cases, col = "lightgray", lty = c(3,4,6,5))
# }
# plotCR.exe(selikoff.dat)
options(scipen=999)
monte.study.exe <- function(ctable,
                            B = 10000,
                            correct = TRUE,
                            correct.val = 0.5){

  # ctable: a contingency table
  # lambda: the value for lambda to be used by the CR divergence. Defaults to 2/3.
  # correct: if 0 values present, should they be replaced to avoid errors? Defaults to TRUE.
  # correct.val: if correct = TRUE, what value should be used to replace 0? Defaults to 0.5.
  options(digits = 6, warn = FALSE)
  rows = nrow(ctable)
  cols = ncol(ctable)
  dof = min(nrow(ctable), ncol(ctable))-1
  ctsum = sum(ctable)
  if(correct){
    for(m in 1:rows){
      for(n in 1:cols){
        if(ctable[m,n] == 0){
          ctable[m,n] = correct.val
        }
      }
    }
  }


  ptable = prop.table(ctable)
  pvec = vector(mode = "numeric", length = B)
  rr = rowSums(ctable)
  cc = colSums(ctable)
  for(i in 1:B){
    pvec[i] = chisq.test(r2dtable(1,rr,cc)[[1]])$statistic
  }
  rr = rowSums(ptable)
  cc = colSums(ptable)
  # Pearson's Phi
  pphimatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      pphimatrix[m,n] = ((ptable[m,n] - rr[m]*cc[n])^2)/(rr[m]*cc[n])
    }
  }

  pphistat   = sum(pphimatrix)
  p.pphistat = mean(pvec > pphistat)/B


  # Cramer

  cstat   = sqrt(pphistat/dof)
  p.cstat = length(pvec[pvec/ctsum > cstat])/B

  # Chi-Square

  xsstat   = pphistat*ctsum
  p.xsstat = length(pvec[pvec > xsstat])/B

  # Tchouproff

  tstat   = sqrt(pphistat/((nrow(ctable)-1)*(ncol(ctable)-1)))
  pvec0 = pvec
  p.tstat = length(pvec0[pvec0 > tstat])/B

  # Pearson's

  pstat   = sqrt(pphistat/(1 + pphistat))
  
  p.pstat = length(pvec[pvec > pstat])/B

   # Sakoda's

   sstat   = sqrt(pphistat/(1 + pphistat)*((dof+1)/dof))
   p.sstat = length(pvec[pvec > sstat])/B

   # Belson
   bmatrix = matrix(NA, nrow = rows, ncol = cols)
   for(m in 1:rows){
     for(n in 1:cols){
       bmatrix[m,n] = (ptable[m,n] - rr[m]*cc[n])^2
     }
   }

  bstat   = (ctsum^2)*sum(bmatrix)
  p.bstat = length(pvec[pvec > bstat])/B

  # Jordan
  jmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      jmatrix[m,n] = ptable[m,n]*(ptable[m,n] - rr[m]*cc[n])^2
    }
  }

  jstat   = ctsum*sum(jmatrix)
  p.jstat = length(pvec[pvec > jstat])/B

  # Variation of Squares
  vosmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      vosmatrix[m,n] = (ptable[m,n]-rr[m]*cc[n])*(ptable[m,n] + rr[m]*cc[n])
    }
  }

  vosstat   = (ctsum^2)*sum(vosmatrix)
  p.vosstat = length(pvec[pvec > (ctsum*sqrt(vosstat)*2)])/B


  # Output
  out = matrix(c(xsstat,   bstat,   jstat,   vosstat,   pphistat,   sstat,   tstat,   cstat,
                 p.xsstat, p.bstat, p.jstat, p.vosstat, p.pphistat, p.sstat, p.tstat, p.cstat),
               ncol = 2)

  dimnames(out) = list(c("X-square","Belson's","Jordan's",
                         "Var. of Squares","Pearson's Phi", "Sakoda's",
                         "Tchouproff's","Cramer's"),
                       c("Stat","MC.p-value"))
  return(out)
}

monte.study.exe(galton.dat, B = 10000,correct = FALSE)




## Simulate permutation test for independence based on the maximum
## Pearson residuals (rather than their sum).

