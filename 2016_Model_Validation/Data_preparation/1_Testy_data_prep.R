
N <- 200
X <- rnorm(N-1,0,1)
X <- c(X,99)
write.table(X, 'normality_tests.csv', row.names = F,col.names=F)
