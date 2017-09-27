
ainsworth.dat <- matrix(c(62,24,3,19,
                          29,210,9,26,
                          14,14,10,10,
                          11,39,6,62),
                        ncol = 4)

dimnames(ainsworth.dat) <- list(c("Avoidant","Secure","Resistant","Disorganised"),c("Dismissing", "Autonomous","Preoccupied","Unresolved"))
ainsworth.dat
N <- sum(ainsworth.dat)

# Circular

P <-  selikoff.dat/N
dI <-  diag(apply(P, 1, sum),nrow(P) ,nrow(P))
dJ <-  diag(apply(P, 2, sum),ncol(P) ,ncol(P))
rr <-  rowSums(P)
cc <-  colSums(P)
Z = solve(sqrt(dI))%*%(P - rr%*%t(cc))%*%solve(sqrt(dJ))
svdZ = svd(Z)
A = svdZ$u # row scores
dimnames(A)[1] = dimnames(P)[1]
B = svdZ$v # col scores
dimnames(B)[1] = dimnames(P)[2]


dlambda = diag(svdZ$d)
F = sqrt(solve(dI))%*%A%*%dlambda
dimnames(F)[1] = dimnames(P)[1]
F
G = sqrt(solve(dJ))%*%B%*%dlambda
dimnames(G)[1] = dimnames(P)[2]
G
(df <- as.data.frame(rbind(F[,1:2], G[,1:2])))

rRval <- matrix(nrow = nrow(P), ncol = 2)
for(i in 1:nrow(P)){
  rRval[i,1] = sqrt(qchisq(0.95,2)/sum(ainsworth.dat[i,]))
  rRval[i,2] = sqrt(qchisq(0.95,2)/sum(ainsworth.dat[i,]))
}
rCval <- matrix(nrow = ncol(P), ncol = 2)
for(i in 1:ncol(P)){
  rCval[i,1] = sqrt(qchisq(0.95,2)/sum(ainsworth.dat[,i]))
  rCval[i,2] = sqrt(qchisq(0.95,2)/sum(ainsworth.dat[,i]))
}
rVal <- rbind(rRval,rCval)
(df2 <- cbind(df,rVal))

ellipse <- function(hlaxa = 1,
                    hlaxb = 1,
                    theta = 0, 
                    xc = 0, 
                    yc = 0, 
                    npoints = 100){
  xp <- NULL
  yp <- NULL
  for(i in 0:npoints){
    a  = (2 * pi * i)/npoints
    x  = hlaxa * cos(a)
    y  = hlaxb * sin(a)
    xp = c(xp, x + xc)
    yp = c(yp, y + yc)
  }
  lines(xp, yp, type = "l")
  invisible()
}
plot(c(-1,1), c(-1,1), 
     xlab = paste("First Principal Axis:", round(prininert[1]*100/totinert,2),"%"),
     ylab = paste("Second Principal Axis:", round(prininert[2]*100/totinert,2),"%"),
     main = "Circular Conf.Int.",
     type="n") +
  abline(v = 0, col = "grey") +
  abline(h = 0, col = "grey") +
  points(df2[,1:2], col = "red")
for(i in 1:nrow(df2)){
  ellipse(hlaxa = df2[i,3],
          hlaxb = df2[i,4],
          xc = df2[i,1],
          yc = df2[i,2])
}

## Basic Eliptical

P <-  ainsworth.dat/N
dI <-  diag(apply(P, 1, sum),4 ,4)
dJ <-  diag(apply(P, 2, sum),4 ,4)
rr <-  rowSums(P)
cc <-  colSums(P)
Z = solve(sqrt(dI))%*%(P - rr%*%t(cc))%*%solve(sqrt(dJ))
svdZ = svd(Z)
A = svdZ$u # row scores
dimnames(A)[1] = dimnames(P)[1]
B = svdZ$v # col scores
dimnames(B)[1] = dimnames(P)[2]


dlambda = diag(svdZ$d)
F = sqrt(solve(dI))%*%A%*%dlambda
dimnames(F)[1] = dimnames(P)[1]
F
G = sqrt(solve(dJ))%*%B%*%dlambda
dimnames(G)[1] = dimnames(P)[2]
G
(prininert <- svdZ$d^2)
(totinert <- sum(svdZ$d^2))
chi2 <- totinert*N 
(df <- as.data.frame(rbind(F[,1:2], G[,1:2])))

rRval <- matrix(nrow = nrow(P), ncol = 2)
for(i in 1:nrow(P)){
  rRval[i,1] = sqrt(prininert[1])*sqrt((qchisq(0.95,9)/chi2)*(1/rowSums(P)[i]))
  rRval[i,2] = sqrt(prininert[2])*sqrt((qchisq(0.95,9)/chi2)*(1/rowSums(P)[i]))
}
rCval <- matrix(nrow = ncol(P), ncol = 2)
for(i in 1:ncol(P)){
  rCval[i,1] = sqrt(prininert[1])*sqrt((qchisq(0.95,9)/chi2)*(1/colSums(P)[i]))
  rCval[i,2] = sqrt(prininert[2])*sqrt((qchisq(0.95,9)/chi2)*(1/colSums(P)[i]))
}
rVal <- rbind(rRval,rCval)
(df2 <- cbind(df,rVal))

ellipse <- function(hlaxa = 1,
                    hlaxb = 1,
                    theta = 0, 
                    xc = 0, 
                    yc = 0, 
                    npoints = 100){
  xp <- NULL
  yp <- NULL
  for(i in 0:npoints){
    a  = (2 * pi * i)/npoints
    x  = hlaxa * cos(a)
    y  = hlaxb * sin(a)
    xp = c(xp, x + xc)
    yp = c(yp, y + yc)
  }
  lines(xp, yp, type = "l")
  invisible()
}
plot(c(-1,1), c(-1,1), 
     xlab = paste("First Principal Axis:", round(prininert[1]*100/totinert,2),"%"),
     ylab = paste("Second Principal Axis:", round(prininert[2]*100/totinert,2),"%"),
     main = "Elliptical Conf.Int.",
     sub = "Exc. Inf from > Second dim",
     type="n") +
  abline(v = 0, col = "grey") +
  abline(h = 0, col = "grey") +
  points(df2[,1:2], col = "red")
for(i in 1:nrow(df2)){
  ellipse(hlaxa = df2[i,3],
          hlaxb = df2[i,4],
          xc = df2[i,1],
          yc = df2[i,2])
}

### Complex Elliptical

P <-  ainsworth.dat/N
dI <-  diag(apply(P, 1, sum),4 ,4)
dJ <-  diag(apply(P, 2, sum),4 ,4)
rr <-  rowSums(P)
cc <-  colSums(P)
Z = solve(sqrt(dI))%*%(P - rr%*%t(cc))%*%solve(sqrt(dJ))
svdZ = svd(Z)
A = svdZ$u # row scores
dimnames(A)[1] = dimnames(P)[1]
B = svdZ$v # col scores
dimnames(B)[1] = dimnames(P)[2]


dlambda = diag(svdZ$d)
F = sqrt(solve(dI))%*%A%*%dlambda
dimnames(F)[1] = dimnames(P)[1]
F
G = sqrt(solve(dJ))%*%B%*%dlambda
dimnames(G)[1] = dimnames(P)[2]
G
(prininert <- svdZ$d^2)
(totinert <- sum(svdZ$d^2))
chi2 <- totinert*N 
(df <- as.data.frame(rbind(F[,1:2], G[,1:2])))

rRval <- matrix(nrow = nrow(P), ncol = 2)
for(i in 1:nrow(P)){
  rRval[i,1] = sqrt(prininert[1])*sqrt((qchisq(0.95,9)/chi2)*(1/rowSums(P)[i] - (F[i,3]/sqrt(prininert[3]))^2))
  rRval[i,2] = sqrt(prininert[2])*sqrt((qchisq(0.95,9)/chi2)*(1/rowSums(P)[i] - (F[i,3]/sqrt(prininert[3]))^2))
}
rCval <- matrix(nrow = ncol(P), ncol = 2)
for(i in 1:ncol(P)){
  rCval[i,1] = sqrt(prininert[1])*sqrt((qchisq(0.95,9)/chi2)*(1/colSums(P)[i] - (G[i,3]/sqrt(prininert[3]))^2))
  rCval[i,2] = sqrt(prininert[2])*sqrt((qchisq(0.95,9)/chi2)*(1/colSums(P)[i] - (G[i,3]/sqrt(prininert[3]))^2))
}
rVal <- rbind(rRval,rCval)
(df2 <- cbind(df,rVal))

ellipse <- function(hlaxa = 1,
                    hlaxb = 1,
                    theta = 0, 
                    xc = 0, 
                    yc = 0, 
                    npoints = 100){
  xp <- NULL
  yp <- NULL
  for(i in 0:npoints){
    a  = (2 * pi * i)/npoints
    x  = hlaxa * cos(a)
    y  = hlaxb * sin(a)
    xp = c(xp, x + xc)
    yp = c(yp, y + yc)
  }
  lines(xp, yp, type = "l")
  invisible()
}
plot(c(-1,1), c(-1,1), 
     xlab = paste("First Principal Axis:", round(prininert[1]*100/totinert,2),"%"),
     ylab = paste("Second Principal Axis:", round(prininert[2]*100/totinert,2),"%"),
     main = "Elliptical Conf.Int.",
     sub = "Inc. Inf from > Second dim",
     type="n") +
  abline(v = 0, col = "grey") +
  abline(h = 0, col = "grey") +
  points(df2[,1:2], col = "red")
for(i in 1:nrow(df2)){
  ellipse(hlaxa = df2[i,3],
          hlaxb = df2[i,4],
          xc = df2[i,1],
          yc = df2[i,2])
}
