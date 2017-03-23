##########################
## data processing in R ##
######### dplyr ##########

#install.packages('dplyr')
library(dplyr)

getwd()

# read some data
sim_data <- read.csv('0_for_dplyr_practice.csv')
vector <- c(1,2,3)

# base notation 
# function(input)
mean(vector)

# pipe notation
# input %>% function()
vector %>% mean

# look into the data
sim_data %>% head

# assinging result
first_five_obserations <- sim_data %>% head()
first_five_obserations

# printing out results while defining the processing
# base notation
(first_five_obserations <- head(sim_data))

# pipe notation
first_five_obserations <- sim_data %>% head %>% print

### dplyr functions ###

## select() - selecting variables/columns ##
sim_data %>% select(N1) %>% head
sim_data %>% select(contains('N'),P1) %>% head

## filter() - filtering on condition ##
sim_data %>% filter(P1 >= 2) %>% head
sim_data %>% filter(factor_variable == 'type 1') %>% head
sim_data %>% filter(factor_variable %in% c('type 2','type 3')) %>% head # %in% notation for vector of values
sim_data %>% filter(!factor_variable %in% c('type 2','type 3')) %>% head # ! notatnion for negation of whole expression
sim_data %>% filter(P1 > 2, N1 > 0) %>% head  # AND: (condition 1, condition 2)
sim_data %>% filter(P1 > 2 & N1 > 0) %>% head # AND: (condition 1 & condition 2)
sim_data %>% filter(P1 > 2 | N1 > 0) %>% head # OR: (condition 1 | condition 2)

## mutate() - defining new variables ##
sim_data %>% mutate(new_variable = N1 + P1) %>% head
sim_data %>% mutate(monthly_diff = N1 - lag(N1)) %>% head
sim_data %>% mutate(yearly_diff = N1 - lag(N1,4)) %>% head
sim_data %>% mutate(N1_mean = mean(N1)) %>% head
sim_data %>% mutate(N1_quantile_0.9 = quantile(N1,0.9)) %>% head
sim_data %>% mutate(factor_variable = gsub(factor_variable, pattern = 'type',replace = 'typ')) %>% head

## group_by() - grouping by some variables ##
sim_data %>% group_by(factor_variable) %>% mutate(mean_in_type = mean(N1)) %>% data.frame
sim_data %>% group_by(factor_variable) %>% summarize(mean_in_type = mean(N1))

#install.packages('ggplot2')
library(ggplot2)
library(MASS)
library(StMoMo)
library(readxl)

