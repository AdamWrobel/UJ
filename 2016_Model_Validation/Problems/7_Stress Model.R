############################
##### stress model #########
############################

# we will use LGD model from previous exmaple
probit

# read scenario data
severly.adverse <- read.csv('7_Table 3A Supervisory severely adverse scenario Domestic.csv')
relevant_variables <- severly.adverse %>% select(Date,
                                                 GDP_growth_annual = Nominal.GDP.growth,
                                                 Unemployment = Unemployment.rate,
                                                 Inflation = CPI.inflation.rate,
                                                 House_Price_Index =House.Price.Index..Level.) %>% 
  mutate(Year = Date %>% as.character %>% substr(4,7) %>% as.numeric) %>%
  filter(Year >= 2015)

# read portfolio data
portfolio <- read.csv('7_LGD_data_not_defaulted.csv')

# stress idiosyncratic variables (LTV)
portfolio_stressed_1 <- portfolio %>% mutate(LTV_stressed = LTV * 1.2) 

# stress the economy
portfolio_stressed_2 <- portfolio_stressed_1 %>% mutate(i = 1) %>% left_join(relevant_variables %>% mutate(i = 1))

# compute the LGD on stressed input
par <- coefficients(probit)
portfolio_stressed_3 <- portfolio_stressed_2 %>% mutate(LGD_modelled = 
                                                        dnorm(par['(Intercept)']+
                                                              par['LTV'] * LTV_stressed +
                                                              par['GDP_growth_annual'] * GDP_growth_annual +
                                                              par['Unemployment'] * Unemployment +
                                                              par['Inflation'] * Inflation
                                                        )) %>% arrange(ID) 

portfolio_stressed_4 <- portfolio_stressed_3 %>% mutate(LTV_group = ifelse(LTV_stressed > 1, 'LTV > 1',
                                                                           ifelse(LTV_stressed > 0.8, 'LTV > 0.8',
                                                                                  ifelse(LTV_stressed > 0.6, 'LTV > 0.6',
                                                                                         'LTV < 0.6'))))

aggregated <- portfolio_stressed_4 %>% group_by(LTV_group, Date) %>% 
  summarize(median_LGD = median(LGD_modelled),
            quantile_0.1 = quantile(LGD_modelled,0.1),
            quantile_0.9 = quantile(LGD_modelled,0.9))
aggregated$Date <- factor(aggregated$Date, levels = c('Q1 2015', 'Q2 2015','Q3 2015','Q4 2015',
                                                      'Q1 2016', 'Q2 2016','Q3 2016','Q4 2016',
                                                      'Q1 2017', 'Q2 2017','Q3 2017','Q4 2017',
                                                      'Q1 2018', 'Q2 2018','Q3 2018','Q4 2018',
                                                      'Q1 2019'))
ggplot(aggregated, aes(x = Date, group = LTV_group, colour = LTV_group, fill = LTV_group)) + 
  geom_line(aes(y = median_LGD)) +
  geom_ribbon(aes(ymin = quantile_0.1, ymax = quantile_0.9), alpha = 0.3)

# stressing LTV based on Home Price Index
severly.adverse %>% filter(Date == 'Q4 2014') %>% select(House.Price.Index..Level.)
portfolio_stressed_2$LTV_stressed_2 <- portfolio_stressed_2$LTV / (1+(portfolio_stressed_2$House_Price_Index-174.5)/174.5)

portfolio_stressed_3a <- portfolio_stressed_2 %>% mutate(LGD_modelled = 
                                                          dnorm(par['(Intercept)']+
                                                                  par['LTV'] * LTV_stressed_2 +
                                                                  par['GDP_growth_annual'] * GDP_growth_annual +
                                                                  par['Unemployment'] * Unemployment +
                                                                  par['Inflation'] * Inflation
                                                          )) %>% arrange(ID) 
portfolio_stressed_4a <- portfolio_stressed_3a %>% mutate(LTV_group = ifelse(LTV_stressed > 1, 'LTV > 1',
                                                                           ifelse(LTV_stressed > 0.8, 'LTV > 0.8',
                                                                                  ifelse(LTV_stressed > 0.6, 'LTV > 0.6',
                                                                                         'LTV < 0.6'))))

aggregated_a <- portfolio_stressed_4a %>% group_by(LTV_group, Date) %>% 
  summarize(median_LGD = median(LGD_modelled),
            quantile_0.1 = quantile(LGD_modelled,0.1),
            quantile_0.9 = quantile(LGD_modelled,0.9))
aggregated_a$Date <- factor(aggregated_a$Date, levels = c('Q1 2015', 'Q2 2015','Q3 2015','Q4 2015',
                                                      'Q1 2016', 'Q2 2016','Q3 2016','Q4 2016',
                                                      'Q1 2017', 'Q2 2017','Q3 2017','Q4 2017',
                                                      'Q1 2018', 'Q2 2018','Q3 2018','Q4 2018',
                                                      'Q1 2019'))
ggplot(aggregated_a, aes(x = Date, group = LTV_group, colour = LTV_group, fill = LTV_group)) + 
  geom_line(aes(y = median_LGD)) +
  geom_ribbon(aes(ymin = quantile_0.1, ymax = quantile_0.9), alpha = 0.3)
