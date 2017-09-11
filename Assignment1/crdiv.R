
selikoff.dat <- matrix(c(310,212,21,25,7,36,158,35,102,35,0,9,17,49,51,0,0,4,18,28), nrow = 5)
dimnames(selikoff.dat) <- list(c("0-9","10-19","20-29","30-39","40+"),c("None","Grade 1", "Grade 2", "Grade 3"))

divergeCR.exe <- function(ctable,
                          correct = TRUE,
                          correct.val = 0.5,
                          B = 10000,
                          plot = TRUE){

  # ctable: a contingency table
  # lambda: the value for lambda to be used by the CR divergence. Defaults to 2/3.
  # correct: if 0 values present, should they be replaced to avoid errors? Defaults to TRUE.
  # correct.val: if correct = TRUE, what value should be used to replace 0? Defaults to 0.5.

  options(digits = 4, warn = FALSE)
  rows = nrow(ctable)
  cols = ncol(ctable)
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
  ctsum = sum(ctable)
  ptable = prop.table(ctable)
  rrc = rowSums(ctable)
  ccc = colSums(ctable)
  rr = rowSums(ptable)
  cc = colSums(ptable)

  # Pearson
  pearsonmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      pearsonmatrix[m,n] = ((ptable[m,n] - rr[m]*cc[n])^2)/(rr[m]*cc[n])
    }
  }

      # Monte-Carlo Sims
      pvecP = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsP = matrix(NA, nrow = rows, ncol = cols)
        pmatsP = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsP[m,n] = ((pmatsP[m,n] - rr[m]*cc[n])^2)/(rr[m]*cc[n])
          }
        }
        pvecP[i] = (ctsum)*sum(matsP)
      }

      # Calculate Score and p value

      pearsonstat = ctsum*sum(pearsonmatrix)
      p.pearsonstat = sum(pvecP >= pearsonstat)/B

  # Log-Likelihood
  llmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      llmatrix[m,n] = ptable[m,n]*log(ptable[m,n]/(rr[m]*cc[n]))
    }
  }
      # Monte-Carlo Sims
      pvecL = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsL = matrix(NA, nrow = rows, ncol = cols)
        pmatsL = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsL[m,n] = pmatsL[m,n]*log(pmatsL[m,n]/(rr[m]*cc[n]))
          }
        }
        pvecL[i] = 2*ctsum*sum(matsL)
        if(is.na(sum(matsL))){
          pvecL[i] = pvecL[i-1]
        }
      }

      llstat = 2*ctsum*sum(llmatrix)
      p.llstat = sum(pvecL >= llstat)/B

  # Freeman-Tukey
  ftmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      ftmatrix[m,n] = (sqrt(ptable[m,n]) - sqrt(rr[m]*cc[n]))^(2)
    }
  }

      # Monte-Carlo Sims
      pvecF = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsF = matrix(NA, nrow = rows, ncol = cols)
        pmatsF = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsF[m,n] = (sqrt(pmatsF[m,n]) - sqrt(rr[m]*cc[n]))^(2)
          }
        }
        pvecF[i] = 4*(ctsum)*sum(matsF)
      }

      ftstat = 4*ctsum*sum(ftmatrix)
      p.ftstat = sum(pvecF >= ftstat)/B

  # Neyman's Modified
  nmmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      nmmatrix[m,n] = ((ptable[m,n] - (rr[m]*cc[n]))^(2))/ptable[m,n]
    }
  }

      # Monte-Carlo Sims
      pvecN = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsN = matrix(NA, nrow = rows, ncol = cols)
        pmatsN = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsN[m,n] = ((pmatsN[m,n] - (rr[m]*cc[n]))^(2))/pmatsN[m,n]
          }
        }
        pvecN[i] = (ctsum)*sum(matsN)
      }

      nmstat = ctsum*sum(nmmatrix)
      p.nmstat = sum(pvecN >= nmstat)/B


  # Cressie-Read
  crmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      crmatrix[m,n] = ptable[m,n]*((ptable[m,n]/(rr[m]*cc[n]))^(0.67) -1)
    }
  }

      # Monte-Carlo Sims
      pvecCR = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsCR = matrix(NA, nrow = rows, ncol = cols)
        pmatsCR = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsCR[m,n] = pmatsCR[m,n]*((pmatsCR[m,n]/(rr[m]*cc[n]))^(0.67) -1)
          }
        }
        pvecCR[i] = (2 * ctsum/((0.67)*(0.67 + 1)))*sum(matsCR)
      }
      crstat = (2 * ctsum/((0.67)*(0.67 + 1)))*sum(crmatrix)
      p.crstat = sum(pvecCR >= crstat)/B

  # Modified Log-Likelihood
  mllmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      mllmatrix[m,n] = (rr[m]*cc[n])*log((rr[m]*cc[n])/ptable[m,n])
    }
  }
      # Monte-Carlo Sims
      pvecML = vector(mode = "numeric", length = B)

      for(i in 1:B){
        matsML = matrix(NA, nrow = rows, ncol = cols)
        pmatsML = r2dtable(1,rrc,ccc)[[1]]/ctsum
        for(m in 1:rows){
          for(n in 1:cols){
            matsML[m,n] = (rr[m]*cc[n])*log((rr[m]*cc[n])/pmatsML[m,n])
          }
        }
        pvecML[i] = 2*ctsum*sum(matsML)
        if(is.na(sum(matsML))){
          pvecML[i] = pvecML[i-1]
        }
      }
      mllstat = 2*ctsum*sum(mllmatrix)
      p.mllstat = sum(pvecML >= mllstat)/B

  # Output
  out = matrix(c(-2,-1,-1/2,0,0.67,1,
                 nmstat,  mllstat,  ftstat,  llstat,  crstat,  pearsonstat,
                 p.nmstat,p.mllstat,p.ftstat,p.llstat,p.crstat,p.pearsonstat),
               ncol = 3)

  dimnames(out) = list(c("Neyman's Modified","Modified Log-Likelihood",
                         "Freeman-Tukey","Log-Likelihood",
                         "Cressie-Read","Pearson"),
                       c("Lambda","Stat","p-value"))
  if(plot){
    dev.new()
    par(mfrow = c(3,2))
    x <- rchisq(B, 12)
    ylims = c(0, max(dchisq(x, df=12))*1.25)
    hist(pvecN, prob = TRUE, breaks = 40, main = "Neyman's Modified", ylim = ylims)
    curve( dchisq(x, df=12), col='green', add=TRUE)
    hist(pvecML, prob = TRUE, breaks = 40, main = "Modified Log-Likelihood", ylim = ylims)
    curve(dchisq(x, df=12), col='green', add=TRUE)
    hist(pvecF, prob = TRUE, breaks = 40, main = "Freeman-Tukey", ylim = ylims)
    curve(dchisq(x, df=12), col='green', add=TRUE, ylim = ylims)
    hist(pvecL, prob = TRUE, breaks = 40, main = "Log-Likelihood", ylim = ylims)
    curve(dchisq(x, df=12), col='green', add=TRUE, ylim = ylims)
    hist(pvecCR, prob = TRUE, breaks = 40, main = "Cressie-Read", ylim = ylims)
    curve(dchisq(x, df=12), col='green', add=TRUE, ylim = ylims)
    hist(pvecP, prob = TRUE, breaks = 40, main = "Pearson", ylim = ylims)
    curve(dchisq(x, df=12), col='green', add=TRUE, ylim = ylims)
    par(mfrow = c(1,1))
  }
  return(out)
}

divergeCR.exe(drug.dat)
