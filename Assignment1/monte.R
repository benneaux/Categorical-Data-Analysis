options(scipen=999)
monte.study.exe <- function(ctable,
                            B = 10000,
                            correct = TRUE,
                            correct.val = 0.5){

  # ctable: a contingency table
  # lambda: the value for lambda to be used by the CR divergence. Defaults to 2/3.
  # correct: if 0 values present, should they be replaced to avoid errors? Defaults to TRUE.
  # correct.val: if correct = TRUE, what value should be used to replace 0? Defaults to 0.5.
  options(digits = 3, warn = FALSE)
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
  rrc = rowSums(ctable)
  ccc = colSums(ctable)

  for(i in 1:B){
    pvec[i] = chisq.test(r2dtable(1,rrc,ccc)[[1]])$statistic
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
  p.pphistat = sum(pvec >= ctsum*pphistat)/B


  # Cramer

  cstat   = sqrt(pphistat/dof)
  p.cstat = sum(pvec >= (cstat^2)*ctsum*dof)/B

  # Chi-Square

  xsstat   = chisq.test(ctable)$statistic
  p.xsstat = sum(pvec >= xsstat)/B

  # Tchouproff

  tstat   = sqrt(pphistat/((nrow(ctable)-1)*(ncol(ctable)-1)))
  p.tstat = sum(pvec >= (tstat^2)*ctsum*(rows - 1)*(cols - 1))/B

  # Pearson's

  pstat   = sqrt(pphistat/(1 + pphistat))
  p.pstat = length(pvec[pvec > pstat])

  # Sakoda's

  sstat   = sqrt(pphistat/(1 + pphistat)*((dof+1)/dof))
  sstatadj = ctsum*((1-(sstat^2 * dof)/(dof + 1))^(-1) - 1)
  p.sstat = length(pvec[pvec > sstatadj])/B

  # Belson
  bmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      bmatrix[m,n] = (ptable[m,n] - rr[m]*cc[n])^2
    }
  }
  pvecB = vector(mode = "numeric", length = B)

  for(i in 1:B){
    matsB = matrix(NA, nrow = rows, ncol = cols)
    pmatsB = r2dtable(1,rrc,ccc)[[1]]/ctsum
    for(m in 1:rows){
      for(n in 1:cols){
        matsB[m,n] = (pmatsB[m,n] - rr[m]*cc[n])^2
      }
    }
    pvecB[i] = (ctsum^2)*sum(matsB)
  }
  bstat   = (ctsum^2)*sum(bmatrix)
  p.bstat = sum(pvecB >= bstat)/B


  # Jordan
  jmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      jmatrix[m,n] = ptable[m,n]*(ptable[m,n] - rr[m]*cc[n])^2
    }
  }

  pvecJ = vector(mode = "numeric", length = B)

  for(i in 1:B){
    matsJ = matrix(NA, nrow = rows, ncol = cols)
    pmatsJ = r2dtable(1,rrc,ccc)[[1]]/ctsum
    for(m in 1:rows){
      for(n in 1:cols){
        matsJ[m,n] = pmatsJ[m,n]*(pmatsJ[m,n] - rr[m]*cc[n])^2
      }
    }
    pvecJ[i] = (ctsum)*sum(matsJ)
  }
  jstat   = ctsum*sum(jmatrix)
  p.jstat = sum(pvecJ >= jstat)/B

  # Variation of Squares
  vosmatrix = matrix(NA, nrow = rows, ncol = cols)
  for(m in 1:rows){
    for(n in 1:cols){
      vosmatrix[m,n] = (ptable[m,n]-rr[m]*cc[n])*(ptable[m,n] + rr[m]*cc[n])
    }
  }
  pvecV = vector(mode = "numeric", length = B)

  for(i in 1:B){
    matsV = matrix(NA, nrow = rows, ncol = cols)
    pmatsV = r2dtable(1,rrc,ccc)[[1]]/ctsum
    for(m in 1:rows){
      for(n in 1:cols){
        matsV[m,n] = (pmatsV[m,n]-rr[m]*cc[n])*(pmatsV[m,n] + rr[m]*cc[n])
      }
    }
    pvecV[i] = (ctsum^2)*sum(matsV)
  }
  vosstat   = (ctsum^2)*sum(vosmatrix)
  p.vosstat = sum(pvecV >= vosstat)/B


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

monte.study.exe(galton.dat,correct = FALSE)
