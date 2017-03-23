rollWindow<-function(data, width,horizon)
{
  from<-min(data$Date)
  to<-max(data$Date)
  now<-from+width+2
  rdf<-data.frame()
  while (now<to)
  {
    widthdf<-data%>%filter(Date>now-width,Date<now)
    nowdf<-data%>%filter(Date==now)
    
    regres.lm<-lm(RelDiff~Day,data=widthdf)
    coeffs<-coefficients(regres.lm)
    predict<-coeffs[1]+coeffs[2]*(nowdf$Day[1]+horizon)
    
    newrow<-data.frame(Day=nowdf$Day[1]+horizon, RelDiffPred=predict)
    rdf<-rbind(rdf,newrow)
    
    now<-now+1
  }
  rdf<-left_join(data,rdf,by="Day")
  rdf<-rdf%>%mutate(PredPrice=lag(Price)*(1+RelDiffPred))
  rdf
}


df<-read.csv("./DataGauss.dat",TRUE," ")
# df<-read.csv("./DataGaussWithSkew.dat",TRUE," ")
# df<-read.csv("./DataGaussWithOutliers.dat",TRUE," ")

df<-df%>%mutate(Date=as.Date(Date))%>%mutate(RelDiff=(Price-lag(Price))/lag(Price))%>%mutate(Day=1:n())


regdata<-rollWindow(df,30,1)
regdata<-regdata%>%mutate(RegDiff=RelDiffPred-RelDiff)

(regdata%>%ggplot(aes(Day,RelDiff))+geom_point()+geom_line(aes(Day,RelDiffPred),color="Red"))%>%print
(regdata%>%ggplot(aes(Day,Price))+geom_point()+geom_line(aes(Day,PredPrice),color="Red"))%>%print


regdata<-regdata[complete.cases(regdata),]
fitResults<-regdata$RegDiff%>%fitdistr("normal")
fitResults%>%print
tdata<-data.frame(Gaussian=qnorm(seq(from=0, to = 1, by = 0.001), mean = fitResults$estimate[[1]], sd = fitResults$estimate[[2]]))

(regdata%>%ggplot(aes(RegDiff))+geom_histogram(aes(y=..density..,fill="Data"),binwidth=0.0002, alpha=0.4, fill="darkgreen")+
  geom_density(data=tdata,aes(Gaussian),kernel="gaussian", size=1.2, adjust=2,color="Red",linetype=2))%>%print
