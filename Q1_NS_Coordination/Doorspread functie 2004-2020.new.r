#### estimating the algorithms for doorspread and wing spread for filling in gaps in the datras flexfile.  

library(icesDatras)
library(mgcv)


hh<-getDATRAS("HH","NS-IBTS",c(2003:2020),c(1)) ## HH data downloaded from the ICES data
hh<-hh[hh$HaulVal=="V",]

hh.NL<-hh[hh$Country=="NED",]
hh.NL<-hh.NL[hh.NL$DoorSpread>0,]

# correcting errors not yet changed in Datras
hh.NL[hh.NL$DoorSpread==48,]$DoorSpread<-74
hh.NL[hh.NL$Year==2005 & hh.NL$HaulNo==20,]$DoorSpread<-91
hh.NL[hh.NL$Year==2010 & hh.NL$DoorSpread>98,]$DoorSpread<--9
hh.NL<-hh.NL[hh.NL$DoorSpread>50,]
hh.NL<-hh.NL[hh.NL$Warplngt>0,]
hh.NL<-hh.NL[hh.NL$Depth>1,]

hh.NL$Doorcal.Den<-79.254-(34.368*exp(-0.029*hh.NL$Depth)) ## Danish algorithm for doorspread
hh.NL$Doorcal.MAF<-(33.251*log10(hh.NL$Depth))+ 15.744 ## Algorithm proposed by the EU project MAFCONS


plot(hh.NL$DoorSpread~hh.NL$Depth, ylim=c(30,140))
lines(hh.NL$Doorcal.Den~hh.NL$Depth, col="blue")
lines(hh.NL$Doorcal.MAF~hh.NL$Depth, col="red")
# clear underestimation of the Dutch values

mm<-lm(hh.NL$Warplngt~hh.NL$Depth)

hh.NL<-hh.NL[is.na(hh.NL$Depth)==F,]
hh.NL<-hh.NL[is.na(hh.NL$Warplngt)==F,]

DoorSpread<- hh.NL$DoorSpread
Depth<- hh.NL$Depth
Warplngt<- hh.NL$Warplngt

t1<-nls(hh.NL$DoorSpread~a*log10(hh.NL$Depth)+d*log10(hh.NL$Warplngt)+ b, start = list(a = 16, b = -30, d = -15))

test1<-data.frame(Depth=seq(0,200,by=1))
test1$Warplngt<-mm$coefficients[1]+test1$Depth*mm$coefficients[2]

test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
lines(test1$nls.fun~test1$Depth, col="gold")
## better fit
summary(t1)$parameters


###forcing to a doorspread of zero
zero<-hh.NL[1,]
zero$DoorSpread<-45
zero$Depth<-10
zero$Warplngt<-mm$coefficients[1]+zero$Depth*mm$coefficients[2]

max<-hh.NL[1,]
max$DoorSpread<-100
max$Depth<-180
max$Warplngt<-mm$coefficients[1]+max$Depth*mm$coefficients[2]



hh.NL1<-rbind(zero,hh.NL,max)
we = rep(1, dim(hh.NL1)[1])
we[1]<-1000         
we[length(we)]<-1000 

d1<-hh.NL1$DoorSpread
d2<-hh.NL1$Depth
d3<-hh.NL1$Warplngt

t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b,weights=we, start = list(a = 16, b = -30, d = -15))
test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
lines(test1$nls.fun~test1$Depth, col="green")




##### year effect

plot(hh.NL$DoorSpread~hh.NL$Depth, ylim=c(30,140))

for(i in unique(hh.NL$Year)){
d1<-hh.NL[hh.NL$Year==i,]$DoorSpread
d2<-hh.NL[hh.NL$Year==i,]$Depth
d3<-hh.NL[hh.NL$Year==i,]$Warplngt

t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b, start = list(a = 16, b = -30, d = -15))
test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]

lines(test1$nls.fun~test1$Depth, col=i)
Sys.sleep(3)
}

#### groups 2003+2004, 2005-2014, 2015+2016, 2017-2020
plot(hh.NL$DoorSpread~hh.NL$Depth, ylim=c(30,120), xlab="Depth (m)",ylab="Doorspread (m)")

zero<-hh.NL[1,]
zero$DoorSpread<-55
zero$Depth<-10
zero$Warplngt<-mm$coefficients[1]+zero$Depth*mm$coefficients[2]

max<-hh.NL[1,]
max$DoorSpread<-110
max$Depth<-180
max$Warplngt<-mm$coefficients[1]+max$Depth*mm$coefficients[2]

hh.NL1<-rbind(zero,hh.NL[hh.NL$Year %in% c(2003,2004),],max)
we = rep(1, dim(hh.NL1)[1])
we[1]<-5         
we[length(we)]<-50

  d1<-hh.NL1$DoorSpread
  d2<-hh.NL1$Depth
  d3<-hh.NL1$Warplngt
  
  t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b,weights=we, start = list(a = 16, b = -20, d = 35))
  test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
  
 # plot(hh.NL1$DoorSpread~hh.NL1$Depth, ylim=c(30,140))
    lines(test1$nls.fun~test1$Depth, col="red")
  summary(t1)$parameters

#####2005-2014
  
  zero<-hh.NL[1,]
  zero$DoorSpread<-55
  zero$Depth<-10
  zero$Warplngt<-mm$coefficients[1]+zero$Depth*mm$coefficients[2]
  
  max<-hh.NL[1,]
  max$DoorSpread<-100
  max$Depth<-180
  max$Warplngt<-mm$coefficients[1]+max$Depth*mm$coefficients[2]
  
  hh.NL1<-rbind(zero,hh.NL[hh.NL$Year %in% c(2005:2014),],max)
  we = rep(1, dim(hh.NL1)[1])
  we[1]<-50         
  we[length(we)]<-50
  
  d1<-hh.NL1$DoorSpread
  d2<-hh.NL1$Depth
  d3<-hh.NL1$Warplngt
  
  t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b,weights=we, start = list(a = 16, b = -20, d = 35))
  test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
  
 # plot(hh.NL1$DoorSpread~hh.NL1$Depth, ylim=c(30,140))
  lines(test1$nls.fun~test1$Depth, col="blue")
  summary(t1)$parameters

  #####2015-2016
  
  zero<-hh.NL[1,]
  zero$DoorSpread<-45
  zero$Depth<-10
  zero$Warplngt<-mm$coefficients[1]+zero$Depth*mm$coefficients[2]
  
  max<-hh.NL[1,]
  max$DoorSpread<-100
  max$Depth<-180
  max$Warplngt<-mm$coefficients[1]+max$Depth*mm$coefficients[2]
  
  hh.NL1<-rbind(zero,hh.NL[hh.NL$Year %in% c(2015:2016),],max)
  we = rep(1, dim(hh.NL1)[1])
  we[1]<-50         
  we[length(we)]<-50
  
  d1<-hh.NL1$DoorSpread
  d2<-hh.NL1$Depth
  d3<-hh.NL1$Warplngt
  
  t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b,weights=we, start = list(a = 16, b = -20, d = 35))
  test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
  
 #plot(hh.NL1$DoorSpread~hh.NL1$Depth, ylim=c(30,140))
  lines(test1$nls.fun~test1$Depth, col="green")
  summary(t1)$parameters

  ### 2017-2020
  zero<-hh.NL[1,]
  zero$DoorSpread<-55
  zero$Depth<-10
  zero$Warplngt<-mm$coefficients[1]+zero$Depth*mm$coefficients[2]
  
  max<-hh.NL[1,]
  max$DoorSpread<-100
  max$Depth<-180
  max$Warplngt<-mm$coefficients[1]+max$Depth*mm$coefficients[2]
  
  hh.NL1<-rbind(zero,hh.NL[hh.NL$Year %in% c(2017:2020),],max)
  we = rep(1, dim(hh.NL1)[1])
  we[1]<-50         
  we[length(we)]<-50
  
  d1<-hh.NL1$DoorSpread
  d2<-hh.NL1$Depth
  d3<-hh.NL1$Warplngt
  
  t1<-nls(d1~a*log10(d2)+d*log10(d3)+ b,weights=we, start = list(a = 16, b = -20, d = 35))
  test1$nls.fun<-summary(t1)$parameters[1]*log10(test1$Depth)+summary(t1)$parameters[3]*log10(test1$Warplngt)+ summary(t1)$parameters[2]
  
 #plot(hh.NL1$DoorSpread~hh.NL1$Depth, ylim=c(30,140))
  lines(test1$nls.fun~test1$Depth, col="gold")
  summary(t1)$parameters
  

