
# values of Z_a for a = 0.1,0.05,0.01,0.005,0.001
z_scores <- c(1.28155,1.64485,2.32635,2.57583,3.09023)
v_values <- c(2,5,10,20,50)

X2_approx_i <- matrix(nrow = 5,ncol = 5)
dimnames(X2_approx_i) <- list(c(2,5,10,20,50),c(0.1,0.05,0.01,0.005,0.001))
for(i in 1:5){
  for(j in 1:5){
    X2_approx_i[i,j] = v_values[i] + z_scores[j]*sqrt(2*v_values[i])
  }
}
X2_approx_i

X2_approx_ii <- matrix(nrow = 5,ncol = 5)
dimnames(X2_approx_ii) <- list(c(2,5,10,20,50),c(0.1,0.05,0.01,0.005,0.001))
for(i in 1:5){
  for(j in 1:5){
    X2_approx_ii[i,j] = v_values[i] + z_scores[j]*sqrt(2*v_values[i])
  }
}
X2_approx_ii

X2_approx_iii <- matrix(nrow = 5,ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    X2_approx_iii[i,j] = v_values[i]*(1-(2/(9*v_values[i])) + z_scores[j]*sqrt(2/(9*v_values[i])))^3
  }
}
X2_approx_iii

