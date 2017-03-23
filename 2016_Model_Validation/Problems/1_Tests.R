#############
### Tests ###
#############

# reading data #
X <- read.table('1_normality_tests.csv', header = F) %>% as.matrix

# tests #
library(nortest) # load package with tests for normality
ad.test(X)
shapiro.test(X)
cvm.test(X)
sf.test(X)
lillie.test(X)
pearson.test(X)

# plot data
plot(X, ylim = c(-5,5))

# plot density function #
X %>% density %>% plot(xlim = c(-5,5))

# add theoretical density of standard normal distribution
qnorm(seq(from=0, to = 1, by = 0.01)) %>% 
  density %>% lines(col = 'blue', lty = 2)

# basic statistisc
summary(X)

# calcluate mean and standard deviation
mean_X <- mean(X)
sd_X <- sd(X)

# plot density function on whole range and add line for fitted normal distribution
X %>% density %>% plot(xlim = c(-100,100))
qnorm(seq(from=0, to = 1, by = 0.01), mean = mean_X, sd = sd_X) %>% 
  density %>% lines(col = 'blue', lty = 2)

# plot the vector
plot(X)

# remove outlier
X_without_outlier <- X[X < 5*sd_X]
plot(X_without_outlier)

# re-run the tests
ad.test(X_without_outlier)
shapiro.test(X_without_outlier)
cvm.test(X_without_outlier)
sf.test(X_without_outlier)
lillie.test(X_without_outlier)
pearson.test(X_without_outlier)
