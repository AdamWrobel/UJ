library(MASS)
library(ggplot2)
library(dplyr)

# generate one path for for a given period of time 
# S-starting price
# K-strike
# vol - yearly volatility
# r - yearly interest rates
MCSimOnePath<-function(from,to,r,vol,S,K)
{
  days_in_year<-360
  df<-data.frame()
  days<-to-from
  
  # generate a vector of brownians from normal distribution 'rnorm(    )'
  
  sprices<-numeric() # can also be done as data.frame
  for (day in 1:(days+1))
  {
    # 
  }
  sprices
}


# generate nPaths paths for for a given period of time 
# S-starting price
# K-strike
# vol - yearly volatility
# r - yearly interest rates
MCSim<-function(nPaths,from,to,r,vol,S,K)
{
  dates<-seq.Date(from=from,to=to,by=1)
  df<-data.frame()
  prices<-numeric()
  for (path in 1:nPaths)
  {
    # generate all the path and combine them in a single data frame
    # It is more convienient to do it as a three column frame with Date, Price and
    # a factor variable assigning data to a given path
  }
  df
}

# function that returns the data frame containing variousa measures of a trade distribuiton
# freq - how often we want to calculate the measures (e.g. every 10 days)
# K    - strike price
makeProfile<-function(df,freq,K)
{
  sdf<-data.frame()
  now<-from+freq
  while(now<to)
  {
    # use 'filter' function to get measurements for a given day
    # use 'fitdistr' to fit a proper distribution to date
    # use the fitted distribution to calculate the quantiles 95% and 5%, mean value
    # calculate Expected Positive Exposure

    # It is more convienient for the data frame to be a 3 column set containing the date, exposure
    # and a factor variable with a description of the measure type
    nQ95<-data.frame(Date=now,ValueType="Q95",Exposure=Q95)
    nEPE<-data.frame(Date=now,ValueType="EPE",Exposure=EPE)
    nMEAN<-data.frame(Date=now,ValueType="MEAN",Exposure=MEAN)
    nQ05<-data.frame(Date=now,ValueType="Q05",Exposure=Q05)
    sdf<-rbind( ... ) # adding all new rows to the data frame
    
    now<-now+freq  # next data slice
  }
  sdf
}


from<-as.Date("2015-01-01")
to<-as.Date("2015-12-31")
nPaths<-1000

S<-100
K<-100    # 
r<-0.01   # yearly interest rates 
vol<-0.01 # yearly volatility

data<-MCSim(nPaths,from,to,r,vol,S,K)
sdf<-makeProfile(data,from,to,10,K)

# plot few paths , use 'ggplot()' and 'geom_line()'
# plot the exposure profiles 

df<-read.csv("./5_LGDnPD.dat",TRUE," ") # load data with LGD and PD
# combine with LGD and PD data to calculate the CVA
