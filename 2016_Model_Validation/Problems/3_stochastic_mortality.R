############################
### Stochastic Mortality ###
############################

# load needed library
library(StMoMo)

# read data
mortality_table <- read.csv("3_mortality_table_Poland.csv")
deaths_matrix <- read.csv("3_deaths_matrix.csv")
exposure_matrix <- read.csv("3_exposure_matrix.csv")

# get some basic understanding of the mortality_table dataset


# fit Lee-Carter model
wxt <- genWeightMat(ages = 25:85, years = 1958:2014, clip = 3)
LCfit <- fit(lc(link = "logit"),
             Dxt = deaths_matrix,
             Ext = exposure_matrix,
             ages = 0:100,
             years = 1958:2014,
             ages.fit = 25:85,
             wxt = wxt)
plot(LCfit, nCol = 3)

# plot residuals of Lee-Carter model
# use function residuals() on LCfit object to get residuals


# use function plot() with parameter type = 'colourmap' to get heatmap


# use function plot() with parameter type = 'scatter' to get scatter plot


# fit PLAT (more complex model of a same class)
source('PLAT_implementation.R')
PLATfit <- fit(extended_PLAT,
             Dxt = deaths_matrix,
             Ext = exposure_matrix,
             ages = 0:100,
             years = 1958:2014,
             ages.fit = 25:85,
             wxt = wxt)
plot(PLATfit, nCol = 3)

# plot residuals of PLAT model
# use function residuals() on LCfit object to get residuals


# use function plot() with parameter type = 'colourmap' to get heatmap


# use function plot() with parameter type = 'scatter' to get scatter plot

