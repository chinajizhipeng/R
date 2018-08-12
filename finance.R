re_simple <- diff(ts)/lag(ts, k =-1)*100 #��������
re_cont   <- diff(log(ts))*100 #��������������
library(quantmod)
getSymbols("code",from = "2000-01-20",to = "2012-02-20")
code.rtn <- diff(log(code$code.Adjusted))
install.packages("zoo")
install.packages("xts")
library(zoo)
library(xts)
install.packages("quantmod") #��������ר�û�ͼ��
data <- seq(as.Date("2018-01-01"), as.Date("2018-01-31"),by = 1)
ts() #����ʱ�����
tsales <- ts(sales, start=c(2003, 1), frequency=12) 
plot() #����ʱ������ͼ
start() #���ؿ�ʼ��ʱ��
end() #���ؽ�����ʱ��
source(ma) #�������ݷ�������
ma(ts,21) #21�յ��ƶ�ƽ����
frequency() #����ʱ���ĸ���
window() #��ʱ�����ȡ�Ӽ�
ma() #���һ���򵥵��ƶ�ƽ��ģ��
plot(ma(ts,k)) #kΪƽ���۲�ֵ�ĸ���
names <- zoo(x,dt) #xΪ�����������ݿ�dtΪ������Ӧ���ڻ�����ʱ�������
names <- xts(x,dt)
as.zoo(name) #ת��ΪZOO��
as.xts(name)
coredata(names) #��ȡʱ����������
index(names) #��ȡʱ��
read.zoo() #ר��������ȡASCII�ļ��е�ʱ����������
first(as.xts(names),"3 weeks") #�鿴ǰ3�����ڵ�����
last(as.xts(names),"month") #�鿴��һ���µ�����
ts[i] #�ӵ�һʱ��������ѡ���i���۲�ֵ
ts[j, i] #ѡ���jʱ�����еĵ�i���۲�ֵ
ts[as.Date("yyyy-mm-dd")] #����tsΪDATA����
window(ts,start = , end = ) #ѡ��һ��ʱ�䷶Χ�ڵ�ʱ����������
tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
#�ϲ�����
na.locf(merge(A, B)) #������һ�ڵĹ۲�ֵ������ʱ�����е�ȱʧֵ
#ȱʧʱ�����е����
#ʱ�����е��ͺ������ǰ�ƶ�
lag(ts, k, na.pad = T) #��Ϊ��ǰ����Ϊ�ͺ� + ��- ,TʱȱʧֵΪNA��FʱNAֵɾ��
diff(tx,lag = ) #���� #���趨��ֵĽ���
#�����ƶ�ƽ��
names <- rollmean(ts, k, align = "right") #ֻӦ�ø����ܹ��õ�����ʷ����
#������ʱ�䷶Χ��Ӧ�ú��� #ʹ��xts����
apply.daily(tx, fun)
apply.weekly(tx, fun)
apply.monthly(tx, fun)
apply.quarterly(tx, fun)
apply.yearly(tx, fun)
#Ӧ�ù�������
rollapply(ts, width, f, align = "right") #align���⺯��Ӧ���ڼ�����в��ɵõ���ʷ����
###�����Էֽ�
stl(ts, s.window = , t.window = ) #��ʱ��ֽ�Ϊ�����������������.s.window���Ƽ���ЧӦ�仯���ٶȣ�t.window����������仯���ٶ�
name1 <- stl(ts, s.window="period")   #period������ЧӦ�޶�Ϊÿ�궼һ�� 
plot(fit) #�����Էֽ�ͼ
fit$time.series  #�ֽ�ֵ                               
exp(fit$time.series) #ָ��������Ϊԭʼ�߶�
monthplot() #����ʱ���еļ�����
seasonplot() #���ɼ���ͼ
###ָ��Ԥ��ģ��
forecast::ets
ets(ts, model = "ZZZ") #��ĸ˳��ֱ�Ϊ����������������ѡ��ĸΪ�����ģ��A�����ģ��M����N���Զ�ѡ��Z
#��ָ��ANN��˫ָ��AAN����ָ��AAA
forecast(ets(ts, model = "ZZZ"), k) #Ԥ��k��
accuracy(ets(ts, model = "ZZZ")) #�õ�׼ȷ�Զ���
HoltWinters() #���ָ��ƽ��ģ��
plot(forecast(model)) #Ԥ��ͼ
###########ARIMA(ƽ��)
#1��ȷ��ʱ����ƽ�ȵģ�������ȶ�����������BOX-COX�任�����������ƣ����в��ȷ��d����ͼ��ndiffs(ts)
#2���ҵ������ģ�ͣ�ȷ��p\q��Acf(p) Pacf(q).��ͼ���𽥼��ٵ�o.ARIMA(p, d, q)
#3�����ģ�͡�
fit <- arima(ts,order = c(p, d, q))
#4��ģ������qqnorm() ��ģ�͵Ĳв�Ҫ������̬�Լ���Ͳв��������ϵ���ļ���
qqnorm(fit$residuals)
qqline(fit$residuals)
#5��Ԥ��ģ��
#����ʱ�����е�����أ�MA�� 
library('tseries')
acf(ts)
Box.test(ts) #�Ƿ���������,ģ�� ho �����ϵ���Ƿ�Ϊ0
Box.test(ts,type = "Ljung-Box") #�ж��������������Ի���˵������Ƿ����,HO���ݶ��Ƕ��������ϵ��Ϊ0��H0������
adf.test() #����ƽ���� DF����AR(1)���� ADF����AR(P)���̵�ƽ���� 
#ƫ���(AR)
pacf(ts)
#����ʱ�����е��ͺ������
ccf(ts1, ts2)
#�޳�ʱ�����е�����
m <- lm(coredata(ts)~index(ts))
detr <- zoo(resid(m),index(ts)) #detrΪ�޳����ƺ��
#���ARIMAģ��
install.packages("forecast")
library(forecast)
auto.arima(ts) #�Զ�ѡ����ʵ�ARIMAģ��
ARIMA(p, d, q) #pΪ�Իع�ϵ���ĸ�����dΪ��ֵĽ�����qΪ�ƶ�ƽ��ϵ���ĸ���
confint(arima()) #�������� ���޳�Ϊ����0�ı���
#���ARIMAģ��
tsdiag(arima())
#��ARIMA����Ԥ��
predict(arima())
#��ֵ�ع�ļ���
install.packages("tseries")
library(tseries) 
adf.test(coredata(ts))#p<.05 ���ھ�ֵ�ع�
#ʱ�����е�ƽ��
install.packages("KernSmooth")
library(KernSmooth)
gridsize <- length(y)
bw <- dpill(t, y, gridsize = gridsize)
lp <- locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize)
smooth <- lp$y #yΪƽ���������
###########�ʲ�������ģ��
###ARCHģ��
library(fGarch)
ts <- intc - mean(intc) #intcΪ����������
Box.test(ts^2, type = "Ljung-Box",lag = 20) #H0:����������ء�0Ϊ����ARCHЧӦ
ArchTest(ts)  #���ּ��鶼�����Ƿ����ARCH��Ӧ�����������ڡ� ho:NO ARCH EFFECT
Box.test(v^2,type = "Ljung-Box") #P������0 ARCHЧӦ��ȡ�����Ľ���
pacf(v^2) #����
source("archTest.R") #P������0 ����ARCHЧӦ��ȡ*�Ľ���
m1=garchFit(~1+garch(3,0),data=intc,trace=F)                  # ��̬�ֲ�
m1=garchFit(~1+garch(3,0),data=intc,trace=F,con.dist = "std") # ѧ��t�ֲ�
summary(m1)
#ARCHģ�� Rt = mu+at  ��^t=omega+alpha1*a^t-1+alpha2*a^t-2����
#ȥ����������alpha
#��ģ�͵Ĳв���м��飨����ģ�͵���������
resi=residuals(m1,standardize=T) #�õ��в�
acf(resi,lag=20)
pacf(resi^2,lag=20)  #����ģ�������
Box.test(resi^2,lag=10,type='Ljung') #�����Ƿ���������
plot(resi) #�в����е�ʱ��ͼ
plot(m1) #�鿴ģ�͵ĸ���ͼ
predict(m1,10)
###GARCHģ��
m2 = garchFit(~1 + garch(1,1),data=intc,trace=F) #GARCH(1,1)�㹻Ӧ�Դ󲿷����
#Ԥ�� ����������
m3 = arima(ts^2,order = c(1,0,1)) #������ARCH GARCHһ��
m3
mean(intc)
fit = ts^2 - m3$residuals
v3 = volatility(m2)
cor(v3,sqrt(fit)) #��������ϵĲ�������GARCH��1��1���������ϵ��
predict(m3,10)
###########Э������(��ƽ��)��ECM�������ģ��
##Э��������ECM���������ģ�ͣ�
library("urca")
v1 <- ur.df(V$v1,type = "none")  #ADF���飬���Ƿ�ƽ�� HO����ƽ��  ��Tֵ�� 1pct  5pct 10pct�Ƚϣ�ȫ���ڽ���ԭ���衣
v2 <- ur.df(V$v2,type = "none")  #ADF���飬���Ƿ�ƽ��
summary(v1)
summary(v2)
lm1 <- summary(lm(V$v1~V$v2,data = v)) #EG���� �����ھ��⣩
#ģ�� y = ��x+��
error <- residuals(lm1)
error_adf <- ur.df(error, type = "none") #��������ƽ�ȣ�����в��Ƿ�ƽ��.Ҫ��ƽ�ȡ� ƽ�ȴ���Э����ϵ
po.coint <- po.test(y, demean = TRUE, lshort = TRUE) #HO���������в�Э����y����2��ʱ������
#p-value smaller than printed p-value �ܾ�ԭ����
summary(error_adf)
#ECM�����ڲ�����
diff_x <- diff(v1)
diff_y <- diff(v2)
error_term <- head(error,-1)
data.ecm<-data.frame(dy=diff_x,dx=diff_y,error_term=error_term)
model.ecm<-lm(diff_x~diff_y+error_term,data=data.ecm)
summary(model.ecm)
#��y = ��x+ECM(t-1)+��
############GARCH(VARģ��)
#������������
#Ȼ��Կ��������ʵľ�ֵ�Ƿ�Ϊ0��
t.test(tx_return) #H0Ϊ0 ��Ϊ����ն��������ʵ�ƽ������
                  #����Ϊ�㣬���ն��������ʶ���
acf(tx_return)
pacf(tx_return)
Box.test(tx_return) #�Ƿ�Ϊ������
ArchTest(tx_return) #�Ƿ���ARCHЧӦ
library(rugarch)
garch11.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(0, 0))) #��Ϊ��ֵΪ0 ��ֵ���̿���Ϊ��0��0��
mod.garch <- ugarchfit(spec = garch11.spec, data=tx_return) 
mod.garch #���������������VaR
garch11.roll <- ugarchroll(garch11.spec, tx_return, n.start = 120, refit.every = 1, 
                           refit.window = "moving", calculate.VaR = TRUE, 
                           solver = "hybrid", VaR.alpha = 0.05) #����ˮƽ =1-VaR.alpha
report(garch11.roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.99)
garch11.fcst <- ugarchforecast(mod.garch, n.ahead = 6)
garch11.fcst
ret.fcst <- - qnorm(0.95) * garch11.fcst@forecast$sigmaFor
ret.fcst


############cAPM(�ʲ�����ģ��)
library(devtools)
install_github("joshuaulrich/quantmod")
library(quantmod) 
library(PerformanceAnalytics)
getSymbols(c("code1","code2","^GSPC"), from = '2010-01-01') #���Ż���ȡ��Ʊ�������� ,codeΪ��Ʊ����
chartSeries(code) #�ɼ�ͼ
barChart(code) #��ȡcode��K��ͼ
Cl(MSFT) 	#closing prices
Op(MSFT)	#open prices
Hi(MSFT)	#daily highest price
Lo(MSFT)	#daily lowest price
ClCl(MSFT)	#close-to-close daily return
Ad(MSFT)	#daily adjusted closing price
chartSeries(ClCl(code)) #�����̼۵���������
ret <- dailyReturn(Cl(SNP), type='log') #�ն���������
names(code)<-c("open","high","low","close","volume","adjusted") #�޸�����
dat=merge(IBM$adjusted,GE$adjusted,GSPC$adjusted) #����֧��Ʊ�����һ��
names(dat)<-c('IBM','GE','SP500') 
code_ret=dailyReturn(code)   #����ÿ��������
Rf<-0.04/12 #�����޷�������Ϊ4%.
results<-table.AnnualizedReturns(code_ret,Rf=Rf);results #�õ�ƽ���껯������
table.Stats(dat_ret) #����ͳ��ֵ
chart.Bar(dat_ret[,1], main="IBM Daily Returns") #ÿ������
chart.Bar(monthlyReturn(IBM), main="IBM Monthly Returns") #ÿ������
chart.CumReturns(dat_ret,main="Total Returns",legend.loc="topleft") #�ۼ�����
chart.Correlation(dat_ret, histogram=TRUE, pch="+") #����Է���
CAPM.alpha(dat_ret[,1:2],dat_ret[,3],Rf=Rf) #### 1-2��Ϊ������Ʊ��3Ϊ�г����� alphaֵ
CAPM.beta(dat_ret[,1:2],dat_ret[,3],Rf=Rf) ### betaֵ
#alphaֵͶ����ϵĳ������棬�������˵�������>0�͹��������룬<0�߹���������
#betaֵΪ�г����գ���ӳ�˵���֤ȯ���г��������ԡ�>1���г�����ʱ�Ƿ���<0�г��½�ʱ����С

