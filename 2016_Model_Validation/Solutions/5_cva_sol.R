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
    alpha<-10 #tail heavyness
    beta<-9 # asymetry
    delta<-sigma^2 * alpha
    mu<-mean-delta*beta/sqrt(alpha^2-beta^2)
    distrib<-NIG.ad(mu=mu, delta = sigma*sigma*alpha, alpha=alpha, beta=beta, data=NULL)
  }
  
  outliers<-sample(1:NPoints,NOutliers,replace=F)
  brownians<-rghyp(NPoints, distrib)
  for (i in outliers)
  {
    brownians[i]<-brownians[i]*10
  }
  brownians
}

MCSimP<-function(from,to,r,vol,S,K)
{
  days_in_year<-360
  df<-data.frame()
  days<-to-from
  
  
  brownians<-TSGeneration(days+1,0,0,1,distr="GAUSS")
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
  sprices
}

MCSim<-function(nPaths,from,to,r,vol,S,K)
{
  dates<-seq.Date(from=from,to=to,by=1)
  df<-data.frame()
  prices<-numeric()
  for (path in 1:nPaths)
  {
    prices<-MCSimP(from,to,r,vol,S,K)
    ndf<-data.frame(Date=dates,Path=paste0("Path ",path),Price=prices)
    df<-rbind(df,ndf)
  }
  df
}

makeProfile<-function(df,from,to,freq,K)
{
  sdf<-data.frame()
  now<-from+freq
  while(now<to)
  {
    ndf<-df%>%filter(Date==now)
    pdf<-df%>%filter(Date==now & Price>K)
    fitResults<-ndf$Price %>% fitdistr("log-normal")
    MLEh<- qlnorm(0.95,meanlog=fitResults$estimate[[1]],sdlog=fitResults$estimate[[2]])-K
    MLEl<- qlnorm(0.05,meanlog=fitResults$estimate[[1]],sdlog=fitResults$estimate[[2]])-K
    EPE<- (pdf$Price%>%mean)-K
    nMLEh<-data.frame(Date=now,ValueType="Q95",Exposure=MLEh)
    nEPE<-data.frame(Date=now,ValueType="EPE",Exposure=EPE)
    nMEAN<-data.frame(Date=now,ValueType="MEAN",Exposure=mean(ndf$Price)-K)
    nMLEl<-data.frame(Date=now,ValueType="Q05",Exposure=MLEl)
    sdf<-rbind(sdf,nMLEh,nEPE,nMLEl,nMEAN)
    
    now<-now+freq
  }
  sdf
}


from<-as.Date("2015-01-01")
to<-as.Date("2015-12-31")
nPaths<-1000

S<-100
K<-100
r<-0.01
vol<-0.01

data<-MCSim(nPaths,from,to,r,vol,S,K)
(data%>%ggplot(aes(Date,Price, group=Path, color=Path))+geom_line())%>%print
(data%>%filter(Date=="2015-12-01")%>%ggplot(aes(Price))+geom_histogram(aes(y=..density..),binwidth=0.1))%>%print
sdf<-makeProfile(data,from,to,10,K)
(sdf%>%ggplot(aes(Date,Exposure, group=ValueType, color=ValueType))+geom_line())%>%print
