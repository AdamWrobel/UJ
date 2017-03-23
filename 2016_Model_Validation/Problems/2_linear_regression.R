##############################
### PD - linear regression ###
##############################

# read data
retail_credits <- read.csv('2_retail_credits.csv', header = T)

# vizualize modelled variable default


# vizualize relation between modelled variable and other variables (use table() function)


# fit linear regression
liner_model <- lm(default~loan_size+income, data = retail_credits)

# assess the model (for intance use function summary() on lm object)


# vizualize fitted values (use function fitted() on lm object to obtain fitted values)


# calculate the cut-off and assing 0-1 value
retail_credits %>% select(default) %>% table
cut_off <- quantile(fit, 4823/5000)
fit[fit>cut_off] <- 1
fit[fit<=cut_off] <- 0
table(fit, retail_credits$default)

# vizualize the residuals
residuals(liner_model) %>% hist
residuals(liner_model) %>% density %>% plot(xlim = c(-1,1))
plot(residuals(liner_model))

# fit logit model
logit_model <-
  glm(default~loan_size+income, family = "binomial", data = retail_credits)

# assess the model


# assing 1 if probability >= 50% and 0 if < 50%
fit_logit <- fitted(logit_model)
fit_logit[fit_logit>=0.5] <- 1
fit_logit[fit_logit<0.5] <- 0
table(fit_logit, retail_credits$default)

# vizualize the residuals
residuals(logit_model) %>% density %>% plot(xlim = c(-0.1,0.1))
plot(residuals(logit_model))


