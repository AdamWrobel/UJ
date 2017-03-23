
# assess the fit
plot(fitted(probit),LGD_dataset$LGD_empirical)

# benchmark model
linear <- lm(LGD_empirical ~ LTV + GDP_growth_annual + Unemployment + Inflation  ,data = LGD_dataset)
summary(linear)
plot(fitted(linear),LGD_dataset$LGD_empirical)



# downturn LGD
probit_downturn <- glm(LGD_empirical ~ LTV + GDP_growth_annual + Unemployment + Inflation ,data = LGD_dataset %>% 
                       filter(Year %in% c(2007:2009)),family = binomial(link = "probit"))
summary(probit_downturn)

# compare the mean
fitted(probit_downturn) %>% mean
fitted(probit) %>% mean

# manipulate data to create a plot
out_data_set <- LGD_dataset
out_data_set$LGD_modelled <- fitted(probit)
out_data_set$downturn_LGD_modelled <- NA
out_data_set[out_data_set$Year %in% c(2007:2009),]$downturn_LGD_modelled <- fitted(probit_downturn) %>% as.numeric

gathered <- out_data_set %>% gather('type','value',LGD_modelled:downturn_LGD_modelled)

# comparison plots
ggplot(gathered %>% filter(Year %in% c(2007:2009)), aes(value, group = type, fill = type)) + geom_density(alpha = 0.3)
ggplot(gathered %>% filter(Year %in% c(2007:2009)), aes(value, group = type, fill = type)) + geom_histogram(alpha = 0.3, bins = 20)

