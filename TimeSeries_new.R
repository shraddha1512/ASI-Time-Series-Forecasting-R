library('ggplot2')
library('forecast')
library('tseries')
library('vars')
library('urca')
library('lmtest')
library('tsDyn')
###########################################################
asi<-read.csv("time_series_data_annual_survey_of_industries_2018.csv")
###########################################################
netValue<-ts(asi$Net.Value.added..Rs.in.Crores.)
adf.test(netValue)
netValueLogged<-log(netValue)
adf.test(netValueLogged)
netValuediffed<-diff(diff(netValueLogged))
adf.test(netValuediffed)
View(cbind(netValue, netValueLogged, netValuediffed))
plot.ts(cbind(netValue, netValueLogged, netValuediffed))
###########################################################
output<-ts(asi$Value.of.Output..Rs.in.Crores.)
adf.test(output)
outputLogged<-log(output)
adf.test(outputLogged)
outputDiffed<-diff(diff(outputLogged))
adf.test(outputDiffed)
View(cbind(output, outputLogged, outputDiffed))
plot.ts(cbind(output, outputLogged, outputDiffed))
###########################################################
asi1<-cbind(output, netValue)
plot.ts(asi1)
asits<-cbind(outputLogged,netValueLogged)
colnames(asits) <- c("outputL","netValueL")
View(asits)
plot.ts(asits, plot.type="single", col=1:2)
legend("bottomleft", legend=colnames(asits),col=1:2, lty=1:2, cex=0.5)
asits<-na.trim(asits)
View(asits)

###################################################################
lags.select(asits, lag.max=10, include="trend", sameSample=TRUE)
var<- VARselect(asits)
var
###################################################################
#Conduct Trace Test
cointest1<-ca.jo(asits, type="trace", ecdet="trend", K=7)
summary(cointest1)
#Conduct Eigen Test
cointest1<-ca.jo(asits, type="eigen", ecdet="trend", K=7)
summary(cointest1)
###################################################################
#Data is Cointegrated, so Vector Error Correction model is applied.
grangertest(output~netValue, order=6, data=asi1)
grangertest(netValue~output, order=6, data=asi1)
###################################################################
#VECM begins here....
vec=VECM(asits, lag=6, r=1, estim="ML")
summary(vec)
predict(vec, n.ahead=5)

# Q.d
# estimate unrestriced VEC model

vec1<-cajools(cointest1)
summary(vec1)

# Q.d
# estimate unrestriced VEC model
y.VEC <- cajorls(y.CA, r=1)
y.VEC

#Q.E
# to see t-statistics and p-values
summary(vec1$rlm)




#Q.F
# test for restricted adjustment parameters alpha
rest.alpha <- matrix(c(1,0), c(2,1))
cointest1.ralpha <- alrtest(cointest1, A=rest.alpha, r=1)
summary(cointest1.ralpha)

# Q.G

# vec2varX supports restricted VEC models
source("vec2varX.r")
y.VAR <- vec2varX(y.CA, r=1)
y.VAR.rest <- vec2varX(y.CA, A=rest.alpha, r=1)

# forecast using VAR in levels
y.VAR.fcst <- predict(y.VAR.rest, n.ahead=76, ci=0.95)
par( mar=c(4,4,2,1), cex=0.75)
plot(y.VAR.fcst)

















#require(forecast)
#a<-tsclean(a)
#tsclean(a)
#z<-a[complete.cases(a[, c(1, 3)]), ]


#VAR begins.........
require(vars)

asiVar<-VAR(a , lag.max=22)
asiVar$p
names(asiVar$varresult)

class(asiVar$varresult$Value.of.Input..Rs.in.Crores.)
class(asiVar$varresult$Value.of.Output..Rs.in.Crores.)
class(asiVar$varresult$Net.Value.added..Rs.in.Crores.)

#head(coef(asiVar$varresult$S..No.))
#head(coef(asiVar$varresult$Year))
#head(coef(asiVar$varresult$Factories.covered..No..))
#head(coef(asiVar$varresult$All.Employees...000..Nos.))
head(coef(asiVar$varresult$Value.of.Input..Rs.in.Crores.))
head(coef(asiVar$varresult$Value.of.Output..Rs.in.Crores.))
head(coef(asiVar$varresult$Net.Value.added..Rs.in.Crores.))

require(coefplot)

coefplot(asiVar$varresult$Net.Value.added..Rs.in.Crores.)
predict(asiVar, n.ahead=5)
final<-predict(asiVar, n.ahead=5)
plot(final, plot.type="single", col=1:3)

summary(final)

