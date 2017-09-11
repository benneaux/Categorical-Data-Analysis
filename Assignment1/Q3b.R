drug.dat <- as.table(rbind(c(5,1,10,8,6),
                           c(5,3,3,8,12),
                           c(10,6,12,3,0),
                           c(7,12,8,1,1)))
dimnames(drug.dat) <- list(Drug = c("A","B","C","D"),
                           Effect = c("Poor","Fair","Good","VeryGood","Excellent"))
options(scipen=999)
monte.carlo.p.exe <- function(ctable,
                              B = 10000,
                              plot = TRUE){
  options(digits = 7, warn = FALSE)
  dof  = (nrow(ctable)-1)*(ncol(ctable)-1)
  pvec = vector(mode = "numeric", length = B)
  rr   = rowSums(ctable)
  cc   = colSums(ctable)
  x2 <- as.numeric(chisq.test(ctable)$statistic)
  
  for(i in 1:B) {
    pvec[i] = chisq.test(r2dtable(1,rr,cc)[[1]])$statistic
  }
  
  MC.p.value = sum(pvec >= x2)/B
  
  print(c("Simulated p-value" = MC.p.value))
  
  if(plot){
    dev.new()
    x <- rchisq(B, dof)
    ylims = c(0, max(dchisq(x, df=12))*1.25)
    xlims = c(0, x2*1.01)
    hist(pvec, 
         prob = TRUE, 
         breaks = 40, 
         xlim = xlims, 
         ylim = ylims,
         xlab = NULL,
         main = "Monte-Carlo p-value \n",
         sub = "Simulations (histogram); theoretical chi-squared (green), chi-squared statistic for data (red)")
    curve(dchisq(x, df=dof), col='green', add=TRUE)
    abline(v = x2, col = "red")
  }

}
monte.carlo.p.exe(drug.dat)