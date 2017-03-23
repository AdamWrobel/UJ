
N <- 2000
year_of_default <- rnorm(N, 2007, 5) %>% round %>% pmin(2014) %>% pmax(1991)
year_of_default[year_of_default > 2010] <- rnorm(N, 2008, 5) %>% round %>% pmin(2014) %>% pmax(1991)
year_of_default[year_of_default > 2000 & year_of_default < 2007] <- rnorm(N, 2008, 5) %>% round %>% pmin(2014) %>% pmax(1991)
hist(year_of_default)

LTV <- runif(N,min = 0.2, max = 1)
State <- c(rep('New York',60*20),rep('Colorado',40*20),rep('Texas',60*20), rep('Ohio',50*20), rep('Michigan',50*20)) %>% sample(N)
df <- data.frame(State, ran = runif(N)) %>% arrange(ran) %>% select(-ran)

indiosyncratic <- data.frame(Year = year_of_default, State = df$State, LTV)
colnames(macroeconomic_data_USA)[1] <- 'Year'
full <- indiosyncratic %>% left_join(macroeconomic_data_USA)

cor(full %>% select(-Year, -State))
attach(full)

z <- 1 + 0.5*LTV+0.5*GDP_growth_annual - 0.2*Unemployment + 0.1 * Inflation
pr <- 1/(1+exp(-z))
LGD_empirical <- dnorm(z)
hist(LGD_empirical)

plot(LGD_empirical)
full$LGD_empirical <- LGD_empirical
full %>% group_by(GDP_growth_annual) %>% summarize(mean_LGD = mean(LGD_empirical))

write.csv(full %>% arrange(Year),'LGD_data.csv', row.names = F)

write.csv(full %>% select(State, LTV) %>% mutate(ID = 1:N),'LGD_data_not_defaulted.csv', row.names = F)
