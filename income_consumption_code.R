#################################################################################################
#                 TIME SERIES PROJECT - RELATION BETWEEN INCOME AND CONSUMPTION                 #
#                                                                                               #
#                               Thibault LAUGEL - Ilan BOURGINE                                #
##################################################################################################


### Libraries

library(tseries)
library(stats)
library(fUnitRoots)
library(urca)
library(forecast)




### Definition of the series and visual representation

Conso<-read.table("C://Users/Thibault/Desktop/ENSAE/RESSOURCES/2A/seriestemp/conso2.csv",sep=";",header=FALSE)
conso_mois2 = Conso[1:394,3]+Conso[2:395,3]+Conso[3:396,3] 
conso_true = conso_mois2[c(3*(0:131)+1)]
conso = data.frame(Conso[c(3*(0:131)+1),1],Conso[c(3*(0:131)+2),2]/3,conso_true)
Revenu<-read.table("C://Users/Thibault/Desktop/ENSAE/RESSOURCES/2A/seriestemp/revenu2.csv",sep=";",header=FALSE)

TSConso<-ts(conso[,3],frequency=1,names="Consommation")
TSRevenu<-ts(Revenu[,3],frequency=1,names="Revenus")
TSlnConso<-ts(log(Conso[,3]),frequency=1,names="Consommation")
TSlnRevenu<-ts(log(Revenu[,3]),frequency=1,names="Revenus")

plot(TSConso)
lines(TSRevenu, col="red")
plot(TSRevenu)
plot(TSRevenu, TSConso)
plot(TSlnConso)
plot(TSlnRevenu)




### Stationnarity tests and differenciation of the series

# Augmented Dickey-Füller
#H0:Non-Stationnary
adfTest(TSRevenu, type="ct")
adfTest(TSConso, type="ct")

# Philippe perron test
PP.test(TSRevenu, lshort=TRUE)
PP.test(TSConso,lshort=TRUE)

#DFGLS test
summary(ur.ers(TSRevenu,type="DF-GLS",model="trend"))
summary(ur.ers(TSConso,type="DF-GLS",model="trend"))

#Elliot-Rothenberg-Stock test
summary(ur.ers(TSRevenu,type="P-test",model="trend"))
summary(ur.ers(TSConso,type="P-test",model="trend"))

#KPSS test
kpss.test(TSRevenu, null="T") #T=Trend
kpss.test(TSRevenu, null="L") #L=Level
kpss.test(TSConso, null="T")
kpss.test(TSConso, null="L")
#H0: stationnary

plot(TSlnRevenu)
lines(TSlnConso,col="red")


#d=1?
DRevenu<-diff(TSRevenu)
DConso<-diff(TSConso)
plot(DRevenu,col="blue")
lines(DConso,col="red")
plot(DConso)

A<-arima(TSConso,c(0,1,0))
B<-arima(DConso,c(0,0,0))
A


#d=2?
DDRevenu<-diff(DRevenu)
DDConso<-diff(DConso)
plot(DDRevenu,col="blue")
lines(DDConso,col="red")
plot(DDConso)

#tests on differenciated series
#d=1 consumption
adfTest(DConso,type="c") #p-value: 0.01
PP.test(DConso,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DConso,type="DF-GLS",model="constant"))
summary(ur.ers(DConso,type="P-test",model="constant"))
kpss.test(DConso,null="L") #p-value > 0.1

#d=1 income
adfTest(DRevenu,type="c") #p-value: 0.01
PP.test(DRevenu,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DRevenu,type="DF-GLS",model="constant"))
summary(ur.ers(DRevenu,type="P-test",model="constant"))
kpss.test(DRevenu,null="L") #p-value > 0.1

_______________________________
#d=2 consumption
adfTest(DDConso,type="c") #p-value: 0.01
PP.test(sDDConso,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DDConso,type="DF-GLS",model="constant"))
summary(ur.ers(DDConso,type="P-test",model="constant"))
kpss.test(DDConso,null="L") #p-value > 0.1

#d=2 income
adfTest(DDRevenu,type="c") #p-value: 0.01
PP.test(DDRevenu,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DDRevenu,type="DF-GLS",model="constant"))
summary(ur.ers(DDRevenu,type="P-test",model="constant"))
kpss.test(DDRevenu,null="L") #p-value > 0.1

#d=3 consumption
adfTest(DDDConso,type="c") #p-value: 0.01
PP.test(DDDConso,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DDDConso,type="DF-GLS",model="constant"))
summary(ur.ers(DDDConso,type="P-test",model="constant"))
kpss.test(DDDConso,null="L") #p-value > 0.1

#d=3 income
adfTest(DDDRevenu,type="c") #p-value: 0.01
PP.test(DDDRevenu,lshort=TRUE) #p-value: 0.01
summary(ur.ers(DDDRevenu,type="DF-GLS",model="constant"))
summary(ur.ers(DDDRevenu,type="P-test",model="constant"))
kpss.test(DDDRevenu,null="L") #p-value > 0.1

tsdisplay(DConso)



### Orders of the ARMA

#autocorrelations
acf(DConso)
acf(DDRevenu)
#partials
pacf(DConso, lag.max=40)
pacf(DDRevenu, lag.max=30)

#Au vu des ACF et PACF, on peut dire
#Conso: pmax= 4 ou 6 qmax=3 ou 6 dfsdffsf
#Revenu: pmax=2 ou 3 qmax= 1 ou 5


plot(DConso)

#Selection of the best model in term of AIC, AICc, BIC

AIC.DRevenu<-matrix(0,6,6)
AICC.DConso<-matrix(0,6,6)
AICC.DRevenu<-matrix(0,6,6)
AIC.DConso<-matrix(0,6,6)
for(p in 1:6){
for(q in 1:6){
ARMA.DRevenu<-arima(DDRevenu,c(p-1,0,q-1))
ARMA.DConso<-arima(DConso,c(p-1,0,q-1))
AIC.DRevenu[p,q]<-ARMA.DRevenu$aic
AIC.DConso[p,q]<-ARMA.DConso$aic
AICC.DConso[p,q]<-AIC.DConso[p,q]+2*(p+q-1)*(p+q)/(length(DConso)-p-q)
AICC.DRevenu[p,q]<-AIC.DRevenu[p,q]+2*(p+q-1)*(p+q)/(length(DConso)-p-q)
}}
print(AIC.DRevenu)
AIC.DRevenu<-data.frame(AIC.DRevenu,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))
AIC.DConso<-data.frame(AIC.DConso,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))

AICC.DConso<-data.frame(AICC.DConso,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))
AICC.DRevenu<-data.frame(AICC.DRevenu,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))

names(AIC.DRevenu)<-c("q=0","q=1","q=2","q=3","q=4","q=5")
names(AIC.DConso)<-c("q=0","q=1","q=2","q=3","q=4","q=5")

names(AICC.DConso)<-c("q=0","q=1","q=2","q=3","q=4","q=5")
names(AICC.DRevenu)<-c("q=0","q=1","q=2","q=3","q=4","q=5")

print(AIC.DRevenu)


int(AIC.DCons#Selection of the best model in term of AIC, AICc, BIC@coefficients

.DConso==min(AIC.DConso) #p=0, q=0
AICC.DConso==min(AICC.DConso)#p=0,q=0
AICC.DRevenu==min(AICC.DRevenu)#q=2, p=1


#BIC criterion
BIC.fun<-function(series,order){
model<-arima(series,order)
BIC<- -2*model$loglik+(order[1]+order[3]+1)*log(length(series)-order[2])
return(BIC)
}
BIC.DRevenu<-matrix(0,6,6)
BIC.DConso<-matrix(0,6,6)
for(p in 1:6){
for(q in 1:6){
BIC.DRevenu[p,q]<-BIC.fun(DDRevenu,c(p-1,0,q-1))
BIC.DConso[p,q]<-BIC.fun(TSConso,c(p-1,1,q-1))
}}
BIC.DRevenu<-data.frame(BIC.DRevenu,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))
BIC.DConso<-data.frame(BIC.DConso,row.names=c("p=0","p=1","p=2","p=3","p=4","p=5"))
names(BIC.DRevenu)<-c("q=0","q=1","q=2","q=3","q=4","q=5")
names(BIC.DConso)<-c("q=0","q=1","q=2","q=3","q=4","q=5")
print(BIC.DRevenu)
print(BIC.DConso)
BIC.DRevenu==min(BIC.DRevenu) #p=1, q=2
BIC.DConso==min(BIC.DConso) #p=0, q=0

#verification tests
Aconso<-arima(TSConso, c(1,1,1))
Arevenu<-arima(TSRevenu, c(1,2,3))
Arevenu2<-arima(TSRevenu,c(0,2,1))

Aconso$coef/sqrt(diag(Aconso$var.coef))

#Autocorrelations
tsdiag(Arevenu2)
t criterion
tsdiag(Arevenuverification testsrevenu$residuals


# Calculation of the residuals +normality tests
RArevenu<-Arevenu$residuals
jarque.bera.test(RArevenu)
jarque.bera.test(RArevenu2)
