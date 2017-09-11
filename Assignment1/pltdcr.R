plotCR.exe <- function(ctable, resolution = 1000, correct = TRUE, correct.val = 0.5){
  options(digits = 7, warn = FALSE)
  x = seq(from = -1, to = 1, length.out = resolution)
  vcases = c(-1,-0.5,0,1)
  hcases = vector(mode = "numeric", length = length(vcases))
  for(i in 1:length(vcases)){
    sqvals = abs(x-vcases[i])^2
    val = min(abs(x + vcases[i])^2)
    hcases[i] = which(sqvals == val)
  }
  crstat = vector(mode = "numeric", length = resolution)
  crstat = cbind("lambda" = x,"CR(lambda)" = crstat)
  rows = nrow(ctable)
  cols = ncol(ctable)

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
  rr = rowSums(ptable)
  cc = colSums(ptable)

  for(i in 1:resolution){
    lambda = crstat[i,1]
    crmatrix = matrix(NA, nrow = rows, ncol = cols)
    for(m in 1:rows){
      for(n in 1:cols){
        crmatrix[m,n] = ptable[m,n]*((ptable[m,n]/(rr[m]*cc[n]))^(lambda) - 1)
      }
    }
    crstat[i,2] = (2 * ctsum/(lambda*(lambda + 1)))*sum(crmatrix)
  }
  plot(crstat, type = "l", lwd = 2, ylim = c(min(crstat[,2]*0.99),max(crstat[2:nrow(crstat),2])*1.01))
  abline(v = 2/3, col = "red")
  abline(h = min(crstat[,2]), col = "red")
  abline(h = c(crstat[2,2],crstat[hcases[2:length(hcases)],2]), col = "lightgray", lty = c(3,4,5,6))
  abline(v = vcases, col = "lightgray", lty = c(3,4,6,5))
  points(vcases,c(crstat[2,2],crstat[hcases[2:length(hcases)],2]),col = "blue")
}
plotCR.exe(drug.dat)
