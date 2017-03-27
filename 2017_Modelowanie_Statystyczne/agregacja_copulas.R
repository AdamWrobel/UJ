
library(Matrix);library(rgl);library(CDVine);



##########################################
## Intrduction to the concept of Copula ## 
##########################################

sample_size = 15000

# sampling three indpendent uniformly distibutied variables/vectors
uniform_distibutions <- matrix(runif(3*sample_size),sample_size,3) 
hist(uniform_distibutions[,1])

# converting them into normally distibuted with use of inverse cumulative function techniqe (analitical transformation)
normal_distibutions <- qnorm(uniform_distibutions)
plot(density(normal_distibutions[,1]))

# defining dependency parameter
linear_correlation =0.4

# inputing it into correlation matrix as in multivariat distibution
cor_matrix <- Matrix(linear_correlation,3,3, sparse = TRUE)
diag(cor_matrix) <- 1
cor_matrix

# cholesky decomposition - techniqe for conversion of correlation matrix
chol <- chol(cor_matrix)

# incorporating dependency between normal vectors
n_cor <- normal_distibutions %*% chol

# convering normal vectors with dependency into uniform vector
u_cor <- pnorm(n_cor %>% as.matrix)

# convering uniform vectors with dependency into specifc marginal distibution
normal_with_dependency <- qnorm(u_cor) # normal
student_t_with_dependency <- qt(u_cor,df=6) # student-t with 4-degree of freedom

# check their linear correlation
cor(normal_with_dependency)
cor(student_t_with_dependency)

# check kendall tau independence from margins - reduce number of simulations
#cor(student_t_with_dependency, method = 'kendall')


##########################
## Copula Vizualization ##
##########################

plot(normal_distibutions[,1],normal_distibutions[,2]) # no dependency
plot(n_cor[,1],n_cor[,2]) # multivariat normal distibution
plot(normal_with_dependency[,1],normal_with_dependency[,2]) # gaussian copula with normal margins
plot(student_t_with_dependency[,1],student_t_with_dependency[,2]) # gaussian copula with student t margins



###########################################
## Dependency between those risk drivers ##
###########################################
## we want to capture dependency not just in body, but also in tails and linear correlation will not capture this ##

# plot dependency
plot(risk_drivers_trans$YR1_log_return_N, risk_drivers_trans$WIG_log_return_N, pch = 19,cex = 1.2,
     xlim = c(-4,4), ylim = c(-4,4), xlab = 'YR1 log return - normalized', ylab = 'EQ_WIG log return - normalized')
grid()

# tranformation into uniform vectors - since in that implementation copulas are fitted on uniformly distibuted margins
# we are using previously perfomred distibution fitting
uniform_IR_1 <- risk_drivers_trans %>% select(YR1_log_return) %>% pghyp(YR1_NIG)
uniform_WIG <- risk_drivers_trans %>% select(WIG_log_return) %>% pghyp(EQ_WIG_NIG)
hist(uniform_IR_1) # since we are transforming empirical data - it will be as good as our distibution fitting
hist(uniform_WIG)

# selecting most appropriate copula based on AIC criterion
(fitted_copula <- BiCopSelect(u1 = uniform_IR_1,u2 = uniform_WIG))

# since rotated Gumbel copula was choosen we will simulated from this type of copula to get intuition on its shape
simulated_rotated_Gumbel_copula <- BiCopSim(10000,family = fitted_copula$family, par= -3)
plot(qnorm(simulated_rotated_Gumbel_copula))

# simulating copula based of choosen copula and its parameter
simulated_from_fitted_copula<- BiCopSim(2000,family = fitted_copula$family, par= fitted_copula$par, par2 =fitted_copula$par2)
simulated_IR <- simulated_from_fitted_copula[,1] %>% qghyp(YR1_NIG)
simulated_WIG <- simulated_from_fitted_copula[,2] %>% qghyp(EQ_WIG_NIG)


# ploting both empirical data and margins simulated from fitted copula
plot(simulated_IR,simulated_WIG, xlab = 'log returns of IR month to month', ylab = 'log returns of equity index WIG month to month')
points(risk_drivers_trans$YR1_log_return, risk_drivers_trans$WIG_log_return, col = 'red', pch = 19)
grid()
legend('bottomleft', c('empirical data', 'simulated from fitted copula'), col = c('red','black'), pch = c(19,1))

# 3D density plot
open3d()
mfrow3d(1, 2)
den3d_emp <- kde2d(risk_drivers_trans$YR1_log_return, risk_drivers_trans$WIG_log_return)
persp3d(den3d_emp,col="lightblue", box = T, ticktype = 'detailed',xlim=c(-0.2,0.2),ylim =c(-0.2,0.2))
next3d()
den3d_sim <- kde2d(simulated_IR, simulated_WIG)
persp3d(den3d_sim,col="chartreuse2",add = F,xlim=c(-0.2,0.2),ylim =c(-0.2,0.2))
