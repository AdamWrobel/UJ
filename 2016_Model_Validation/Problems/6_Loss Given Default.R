##########################
### Loss Given Default ###
##########################

# read data
LGD_dataset <- read.csv("6_LGD_data.csv")
head(LGD_dataset)

# fit probit model
probit <- glm(LGD_empirical ~ LTV + GDP_growth_annual + Unemployment + Inflation ,data = LGD_dataset, 
              family = binomial(link = "probit"))
summary(probit)

# is this model conceptually sound?


# is fit quality good enough?


# create benchmark model and compare with base model


# model downturn LGD (LGD in crisis) and compare with base model

