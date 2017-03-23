
library(ggplot2)
library(dplyr)

from<-as.Date("2015-01-01")
to<-as.Date("2015-12-31")

days<-to - from
dates<-seq.Date(from=from,to=to,by=1)
LGD<-0.4
sPD<-0.001
jPD<-0.00001

df<-data.frame()
for (day in 1:(days+1))
{
  PD<-sPD+day*jPD
  nr<-data.frame(Date=dates[day], LGD=LGD, PD=PD)
  df<-rbind(df,nr)
}

(df%>%ggplot(aes(Date,PD))+geom_line())%>%print

df%>%write.table("5_LGDnPD.dat", row.names=F)
