setwd('D:/FDU/毕业论文/正式/Empirical Tests')

library(readxl)
library(forecast)
library(tseries)
library(timeDate)
library(timeSeries)

#---------------1.导入usdcarr+简单的统计量------------
rdata = read_excel('Corrected Fml Single Currency Pair Results.xlsx')
usdcarr = rdata$usdcarr

usdskew = skewness(usdcarr)
usdskewts = usdskew / sqrt(6/length(usdcarr)) 
usdskewts

#--------------2.MA(1) & AR(1) Model------------
adf.test(usdcarr) #usdcarr不是单位根
auto.arima(usdcarr,d=0) #usdcarr定阶ma(1)
ma1 = arima(usdcarr, order = c(0, 0, 1), include.mean = T) #估计ma(1)模型
ma1
pacf(usdcarr)
m2 = arima(usdcarr, order = c(1, 0, 0), include.mean = T) #估计ar(1)模型
m2
Box.test(m2$residuals, lag = 12, type = 'Ljung')

#--------------3.GARCH类模型------------
usdmares = ma1$residuals #均值方程的残差
Box.test(usdmares^2,lag=12,type='Ljung') #检验是否存在ARCH效应
library(fGarch)
library(rugarch)
garchinm = ugarchfit(ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(0,0),archm=T)),usdcarr)
#对usdcarr建立GARCH-M模型
show(garchinm)
# model checking
garchinmres=residuals(garchinm,standardize=T)
Box.test(garchinmres,lag=6,type='Ljung')
Box.test(garchinmres^2,lag=6,type='Ljung')
ugarchforecast(garchinm,n.ahead=6)

#------------4.OLS方程中解释变量的adf检验-------------
stok = read_excel('stok中美股市对数收益率差.xlsx',sheet='diff')
sp = stok$splgrt
sh = stok$szlgrt

intervention = read_excel('外汇市场干预指标_abs version.xlsx')
mintv = intervention$...5
adf.test(mintv)

tsavg = read_excel('tsavg.xlsx',sheet = 2)
fxvol = tsavg$fxvol
adf.test(fxvol)

auto.arima(fxvol,d=0)

#-------------5.LSTAR Model by "tsDyn" package-----------
install.packages("tsDyn")
library(tsDyn)
historicalvolatility = read_excel('usdspotvol.xlsx',sheet = 2)
vol = historicalvolatility$volatility

modlstar = lstar(usdcarr,m=1,thVar=vol)
summary(modlstar)
lstarres = modlstar$residuals
lstarrmse = sqrt(sum(lstarres^2)/length(lstarres)) #计算RMSE
plot(modlstar)

#Nonlinear test
install.packages("fNonlinear")
library(fBasics)
library(fNonlinear)
bds.test(usdcarr,m=2,eps=0.01) #检验usdcarr是否存在非线性相依性
bds.test(m2$residuals,m=5)
bds.test(modlstar$residuals,m=2,eps=0.01)

#------------6.LSTAR Model by "twinkle" package----------
library(zoo)
library(xts)
library(rugarch)
library(parallel)
library(twinkle)
#Step 1. Data Input
lstar = read_excel('LSTAR.xlsx')
str(lstar)
dates = lstar$yrmon
usdcarrxts = xts(x=lstar$usdcarr,order.by=dates)
colnames(usdcarrxts) = 'usdcarr'
fxvolxts = xts(x=lstar$fxvol,order.by=dates)
colnames(fxvolxts) = 'fxvol'

#Step 2. Specifying Model
usdcarrspec = starspec(mean.model=list(states=2,include.intercept=c(1,1),
                                       arOrder=c(1,1),statevar='s',
                                       s=fxvolxts))

#Step 3. Nonlinear Test
stnonlit = nonlinearTest(usdcarrspec, data = usdcarrxts)
rbnonlit = nonlinearTest(usdcarrspec, data = usdcarrxts, robust = TRUE) #异方差稳健an option for 
#also testing with the robust assumption (to heteroscedasticity):
#搭建报告结果矩阵
testrs = matrix(NA, ncol = 4, nrow = 2, dimnames = list(c('Standard', 'Robust'),
                                                       c('F.stat', 'p.value', 'Chisq.stat', 'p.value')))
testrs[1, ] = c(stnonlit$F.statistic, stnonlit$F.pvalue, stnonlit$chisq.statistic, stnonlit$chisq.pvalue)
testrs[2, ] = c(rbnonlit$F.statistic, rbnonlit$F.pvalue, rbnonlit$chisq.statistic, rbnonlit$chisq.pvalue)
print(testrs, digit = 5)

#Step 4. Model Estimation
control=list(maxit=10000,reltol=1e-12,trace=1,method="BFGS",parsearch=TRUE)
modlstar = starfit(usdcarrspec, data = usdcarrxts, out.sample = 0, solver = 'strategy',
                   n = 8, solver.control = control)
show(modlstar)
plot(modlstar)

#------------7.LSTR Model----------
#Step 1. Data Input
exogenous = read_excel('Exogenous SP & TB.xlsx')
exo = xts(x=exogenous,order.by=dates)

#Step 2. Specifying Model
usdcarrspec2 = starspec(mean.model=list(states=2,include.intercept=c(1,1),
                                       arOrder=c(1,1),statevar='s',s=fxvolxts,
                                       xreg=exo))

#Step 3. Nonlinear Test
stnonlit = nonlinearTest(usdcarrspec2, data = usdcarrxts)
rbnonlit = nonlinearTest(usdcarrspec2, data = usdcarrxts, robust = TRUE) #异方差稳健an option for 
#also testing with the robust assumption (to heteroscedasticity):
#搭建报告结果矩阵
testrs = matrix(NA, ncol = 4, nrow = 2, dimnames = list(c('Standard', 'Robust'),
                                                        c('F.stat', 'p.value', 'Chisq.stat', 'p.value')))
testrs[1, ] = c(stnonlit$F.statistic, stnonlit$F.pvalue, stnonlit$chisq.statistic, stnonlit$chisq.pvalue)
testrs[2, ] = c(rbnonlit$F.statistic, rbnonlit$F.pvalue, rbnonlit$chisq.statistic, rbnonlit$chisq.pvalue)
print(testrs, digit = 5)

#Step 4. Model Estimation
control=list(maxit=10000,reltol=1e-12,trace=1,method="BFGS",parsearch=TRUE)
modlstr = starfit(usdcarrspec2, data = usdcarrxts, out.sample = 0, solver = 'strategy',
                   n = 8, solver.control = control)
show(modlstr)
plot(modlstr)

#Step 5. Model Check
lstrres = residuals(modlstr)
#Nonlinear test
library(fBasics)
library(fNonlinear)
bds.test(lstrres,m=5)
Box.test(lstrres,lag=12,type='Ljung')
Box.test(lstrres^2,lag=12,type='Ljung')

#Plot transition function
fxvol = lstar$fxvol
logst = 1/(1+exp(-2.350673*(fxvol-5.364351)))
logstplot = data.frame(fxvol,logst)
library(ggplot2)
ggplot(logstplot,aes(fxvol,logst))+
  geom_point(size = 1.5,color='tan2')+
  labs(title = 'Logistic Transition',x = 'fxvol',y = 'G(fxvol)')+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust=0.5,size=14,face='bold'))
ggsave('logistic transition.png')

library(RColorBrewer)
mycolors=brewer.pal(3,'Dark2')
gammacomp = read_excel('usdspotvol.xlsx',sheet=3)
gammacomp$gamma = factor(gammacomp$gamma)
ggplot(gammacomp,aes(volatility,Gfunc,color=gamma))+
  geom_point(size=1.5)+
  labs(title = 'Logistic Transition (with varying gamma)',x = 'fxvol',y = 'G(fxvol)')+
  scale_colour_manual(values=mycolors)+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust=0.5,size=14,face='bold'),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.position = 'right')
ggsave('logistic transition_2.png')

#-------------8.LSTR w/ CC-----------
#Step 1. Data Input
exogenouscc = read_excel('Exogenous SP & TB.xlsx',sheet = 2)
exocc = xts(x=exogenouscc,order.by=dates)

#Step 2. Specifying Model
usdcarrspec3 = starspec(mean.model=list(states=2,include.intercept=c(1,1),
                                        arOrder=c(1,1),statevar='s',s=fxvolxts,
                                        xreg=exocc))

#Step 3. Nonlinear Test
stnonlit = nonlinearTest(usdcarrspec3, data = usdcarrxts)
rbnonlit = nonlinearTest(usdcarrspec3, data = usdcarrxts, robust = TRUE) #异方差稳健an option for 
#also testing with the robust assumption (to heteroscedasticity):
#搭建报告结果矩阵
testrs = matrix(NA, ncol = 4, nrow = 2, dimnames = list(c('Standard', 'Robust'),
                                                        c('F.stat', 'p.value', 'Chisq.stat', 'p.value')))
testrs[1, ] = c(stnonlit$F.statistic, stnonlit$F.pvalue, stnonlit$chisq.statistic, stnonlit$chisq.pvalue)
testrs[2, ] = c(rbnonlit$F.statistic, rbnonlit$F.pvalue, rbnonlit$chisq.statistic, rbnonlit$chisq.pvalue)
print(testrs, digit = 5)

#Step 4. Model Estimation
control=list(maxit=10000,reltol=1e-12,trace=1,method="BFGS",parsearch=TRUE)
modlstrcc = starfit(usdcarrspec3, data = usdcarrxts, out.sample = 0, solver = 'strategy',
                  n = 8, solver.control = control)
show(modlstrcc)
plot(modlstrcc)

#-------------8.LSTR w/ CC & mtint-----------
#exogenous vars inc:sp_t,tb_t,cc_t,mtintve_t
#Step 1. Data Input
exogenousccm = read_excel('Exogenous SP & TB.xlsx',sheet = 3)
exoccm = xts(x=exogenousccm,order.by=dates)

#Step 2. Specifying Model
usdcarrspec4 = starspec(mean.model=list(states=2,include.intercept=c(1,1),
                                        arOrder=c(1,1),statevar='s',s=fxvolxts,
                                        xreg=exoccm))

#Step 3. Nonlinear Test
stnonlit = nonlinearTest(usdcarrspec4, data = usdcarrxts)
rbnonlit = nonlinearTest(usdcarrspec4, data = usdcarrxts, robust = TRUE) #异方差稳健an option for 
#also testing with the robust assumption (to heteroscedasticity):
#搭建报告结果矩阵
testrs = matrix(NA, ncol = 4, nrow = 2, dimnames = list(c('Standard', 'Robust'),
                                                        c('F.stat', 'p.value', 'Chisq.stat', 'p.value')))
testrs[1, ] = c(stnonlit$F.statistic, stnonlit$F.pvalue, stnonlit$chisq.statistic, stnonlit$chisq.pvalue)
testrs[2, ] = c(rbnonlit$F.statistic, rbnonlit$F.pvalue, rbnonlit$chisq.statistic, rbnonlit$chisq.pvalue)
print(testrs, digit = 5)

#Step 4. Model Estimation
control=list(maxit=10000,reltol=1e-12,trace=1,method="BFGS",parsearch=TRUE)
modlstrccm = starfit(usdcarrspec4, data = usdcarrxts, out.sample = 0, solver = 'strategy',
                    n = 8, solver.control = control)
show(modlstrccm)
plot(modlstrccm)
#r.squared = 56.97%

#Plot transition function
fxvol = lstar$fxvol
logst = 1/(1+exp(-1.460231*(fxvol-4.904747)))
logstplot = data.frame(fxvol,logst)
library(ggplot2)
ggplot(logstplot,aes(fxvol,logst))+
  geom_point(size = 1.5,color='tan2')+
  labs(title = 'Logistic Transition',x = 'fxvol',y = 'G(fxvol)')+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust=0.5,size=14,face='bold'))
ggsave('logistic transition_v2.png')

library(RColorBrewer)
mycolors=brewer.pal(3,'Dark2')
gammacomp = read_excel('usdspotvol.xlsx',sheet=4)
gammacomp$gamma = factor(gammacomp$gamma)
ggplot(gammacomp,aes(volatility,Gfunc,color=gamma))+
  geom_point(size=1.5)+
  labs(title = 'Logistic Transition (with varying gamma)',x = 'fxvol',y = 'G(fxvol)')+
  scale_colour_manual(values=mycolors)+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust=0.5,size=14,face='bold'),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.position = 'right')
ggsave('logistic transition_v2.png')





#-------------9.LSTR w/ lagged sp,tb & CC & mtint-----------
#exogenous vars inc:sp_t-1,tb_t-1,cc_t,mtintve_t
#Step 1. Data Input
exogenousccml = read_excel('Exogenous SP & TB.xlsx',sheet = 4)
exoccml = xts(x=exogenousccml,order.by=dates)

#Step 2. Specifying Model
usdcarrspec5 = starspec(mean.model=list(states=2,include.intercept=c(1,1),
                                        arOrder=c(1,1),statevar='s',s=fxvolxts,
                                        xreg=exoccml))

#Step 3. Nonlinear Test
stnonlit = nonlinearTest(usdcarrspec5, data = usdcarrxts)
rbnonlit = nonlinearTest(usdcarrspec5, data = usdcarrxts, robust = TRUE) #异方差稳健an option for 
#also testing with the robust assumption (to heteroscedasticity):
#搭建报告结果矩阵
testrs = matrix(NA, ncol = 4, nrow = 2, dimnames = list(c('Standard', 'Robust'),
                                                        c('F.stat', 'p.value', 'Chisq.stat', 'p.value')))
testrs[1, ] = c(stnonlit$F.statistic, stnonlit$F.pvalue, stnonlit$chisq.statistic, stnonlit$chisq.pvalue)
testrs[2, ] = c(rbnonlit$F.statistic, rbnonlit$F.pvalue, rbnonlit$chisq.statistic, rbnonlit$chisq.pvalue)
print(testrs, digit = 5)

#Step 4. Model Estimation
control=list(maxit=10000,reltol=1e-12,trace=1,method="BFGS",parsearch=TRUE)
modlstrccml = starfit(usdcarrspec5, data = usdcarrxts, out.sample = 0, solver = 'strategy',
                     n = 8, solver.control = control)
show(modlstrccml)
plot(modlstrccml)
#r.squared = 56.97%






