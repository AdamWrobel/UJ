library(ghyp)
library(ggplot2)
library(dplyr)
library(MASS)
# library(forecast)

TSGeneration<-function(NPoints,NOutliers,mean, sigma, distr="GAUSS")
{
  distrib<-NULL
  if(distr=="GAUSS")
  {
    alpha<-100000 #tail heavyness
    beta<-0  # asymetry
    delta<-sigma^2 * alpha
    mu<-mean-delta*beta/sqrt(alpha^2-beta^2)
    distrib<-NIG.ad(mu=mu, delta = sigma*sigma*alpha, alpha=alpha, beta=beta, data=NULL)
  }
  else
  {
    alpha<-2 #tail heavyness
    beta<-1 # asymetry
    delta<-sigma^2 * alpha
    mu<-mean-delta*beta/sqrt(alpha^2-beta^2)
    distrib<-NIG.ad(mu=mu, delta = sigma*sigma*alpha, alpha=alpha, beta=beta, data=NULL)
  }
  brownians<-rghyp(NPoints, distrib)
  brownians
}

MCSimP<-function(from,to,r,vol,S,nOutliers,distr="GAUSS")
{
  days_in_year<-360
  days<-to-from
  dates<-seq.Date(from=from,to=to,by=1)
  
  brownians<-TSGeneration(days+1,nOutliers,0,1,distr)
  sprices<-numeric()
  
  for (day in 1:(days+1))
  {
    if(day==1)
    {
      sprices[day]<-S
    }
    else
    {
      p<-sprices[day-1]
      sprices[day]<-p*(1+vol/sqrt(days_in_year)*brownians[day]+r/days_in_year)
    }
  }
  
  outliers<-sample(1:days,nOutliers,replace=F)
  for(day in outliers)
  {
    if(day!=1)
    {
      p<-sprices[day-1]
      sprices[day]<-p*(1+vol/sqrt(days_in_year)*brownians[day]*10+r/days_in_year)
    }
  }
  
  df<-data.frame(Date=dates,Price=sprices)
  df
}

from<-as.Date("2005-01-01")
to<-as.Date("2015-12-31")

S<-100
r<-0.05
vol<-0.01
nOutliers<-0

df<-MCSimP(from,to,r,vol,S,nOutliers,distr="PTA")
df%>%write.table("DataGaussWithSkew.dat", row.names=F)
# df%>%write.table("DataGaussWithOutliers.dat", row.names=F)
# df%>%write.table("DataGauss.dat", row.names=F)

# binWidth=0.2
# nPoints=18000
# data<-TSGeneration(nPoints,0,0,1,"PTA")
# 
# fitResults<-data$Brownian %>% fitdistr("normal")
# fitResults%>%print
# tdata<-data.frame(Gaussian=qnorm(seq(from=0, to = 1, by = 0.001), mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]]))
# 
# (data%>%ggplot(aes(Brownian))+geom_histogram(aes(y=..density..,fill="Data"),binwidth=0.2, alpha=0.4, fill="darkgreen")+
#   geom_density(data=tdata,aes(Gaussian),kernel="gaussian", size=1.2, adjust=2,color="Red"))%>%print


