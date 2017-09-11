
drug.dat <- as.table(rbind(c(5,1,10,8,6),
                         c(5,3,3,8,12),
                         c(10,6,12,3,0),
                         c(7,12,8,1,1)))
dimnames(drug.dat) <- list(Drug = c("A","B","C","D"),
                      Effect = c("Poor","Fair","Good","VeryGood","Excellent"))

(TabXsq <- chisq.test(tab))
TabXsq$observed   # observed counts (same as M)
TabXsq$expected   # expected counts under the null
TabXsq$residuals  # Pearson residuals
TabXsq$stdres     # standardized residuals


## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals