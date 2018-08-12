re_simple <- diff(ts)/lag(ts, k =-1)*100 #简单收益率
re_cont   <- diff(log(ts))*100 #连续复合收益率
library(quantmod)
getSymbols("code",from = "2000-01-20",to = "2012-02-20")
code.rtn <- diff(log(code$code.Adjusted))
install.packages("zoo")
install.packages("xts")
library(zoo)
library(xts)
install.packages("quantmod") #金融数据专用绘图包
data <- seq(as.Date("2018-01-01"), as.Date("2018-01-31"),by = 1)
ts() #生成时序对象
tsales <- ts(sales, start=c(2003, 1), frequency=12) 
plot() #画出时序折线图
start() #返回开始的时间
end() #返回结束的时间
source(ma) #金融数据分析导论
ma(ts,21) #21日的移动平均线
frequency() #返回时间点的个数
window() #对时序对象取子集
ma() #拟合一个简单的移动平均模型
plot(ma(ts,k)) #k为平均观测值的个数
names <- zoo(x,dt) #x为向量或者数据框，dt为储存相应日期或日期时间的向量
names <- xts(x,dt)
as.zoo(name) #转化为ZOO类
as.xts(name)
coredata(names) #提取时间序列数据
index(names) #提取时间
read.zoo() #专门用来读取ASCII文件中的时间序列数据
first(as.xts(names),"3 weeks") #查看前3个星期的数据
last(as.xts(names),"month") #查看后一个月的数据
ts[i] #从单一时间序列中选择第i个观测值
ts[j, i] #选择第j时间序列的第i个观测值
ts[as.Date("yyyy-mm-dd")] #假设ts为DATA对象
window(ts,start = , end = ) #选择一个时间范围内的时间序列数据
tsales.subset <- window(tsales, start=c(2003, 5), end=c(2004, 6))
#合并数据
na.locf(merge(A, B)) #用最新一期的观测值来代替时间序列的缺失值
#缺失时间序列的填充
#时间序列的滞后或者向前移动
lag(ts, k, na.pad = T) #正为向前，负为滞后 + ，- ,T时缺失值为NA，F时NA值删除
diff(tx,lag = ) #求差分 #可设定差分的阶数
#计算移动平均
names <- rollmean(ts, k, align = "right") #只应用改日能够得到的历史数据
#在日历时间范围内应用函数 #使用xts对象
apply.daily(tx, fun)
apply.weekly(tx, fun)
apply.monthly(tx, fun)
apply.quarterly(tx, fun)
apply.yearly(tx, fun)
#应用滚动函数
rollapply(ts, width, f, align = "right") #align避免函数应用在计算点尚不可得的历史数据
###季节性分解
stl(ts, s.window = , t.window = ) #将时序分解为季节项，趋势项和随机项.s.window控制季节效应变化的速度，t.window控制趋势项变化的速度
name1 <- stl(ts, s.window="period")   #period将季节效应限定为每年都一样 
plot(fit) #季节性分解图
fit$time.series  #分解值                               
exp(fit$time.series) #指数化，换为原始尺度
monthplot() #画出时序中的季节项
seasonplot() #生成季节图
###指数预测模型
forecast::ets
ets(ts, model = "ZZZ") #字母顺序分别为误差项、趋势项、季节项。可选字母为，相加模型A，相乘模型M，无N，自动选择Z
#单指数ANN，双指数AAN，三指数AAA
forecast(ets(ts, model = "ZZZ"), k) #预测k期
accuracy(ets(ts, model = "ZZZ")) #得到准确性度量
HoltWinters() #拟合指数平滑模型
plot(forecast(model)) #预测图
###########ARIMA(平稳)
#1、确保时序是平稳的，若方差不稳定，对数化或BOX-COX变换，若存在趋势，进行差分确定d。看图，ndiffs(ts)
#2、找到理想的模型，确定p\q。Acf(p) Pacf(q).看图，逐渐减少到o.ARIMA(p, d, q)
#3、拟合模型。
fit <- arima(ts,order = c(p, d, q))
#4、模型评价qqnorm() 。模型的残差要满足正态性假设和残差零自相关系数的假设
qqnorm(fit$residuals)
qqline(fit$residuals)
#5、预测模型
#检验时间序列的自相关（MA） 
library('tseries')
acf(ts)
Box.test(ts) #是否存在自相关,模型 ho 自相关系数是否为0
Box.test(ts,type = "Ljung-Box") #判断序列总体的相关性或者说随机性是否存在,HO数据都是独立的相关系数为0。H0白噪声
adf.test() #检验平稳性 DF检验AR(1)过程 ADF检验AR(P)过程的平稳性 
#偏相关(AR)
pacf(ts)
#两个时间序列的滞后相关性
ccf(ts1, ts2)
#剔除时间序列的趋势
m <- lm(coredata(ts)~index(ts))
detr <- zoo(resid(m),index(ts)) #detr为剔除趋势后的
#拟合ARIMA模型
install.packages("forecast")
library(forecast)
auto.arima(ts) #自动选择合适的ARIMA模型
ARIMA(p, d, q) #p为自回归系数的个数，d为差分的阶数，q为移动平均系数的个数
confint(arima()) #置信区间 ，剔除为包含0的变量
#诊断ARIMA模型
tsdiag(arima())
#用ARIMA进行预测
predict(arima())
#均值回归的检验
install.packages("tseries")
library(tseries) 
adf.test(coredata(ts))#p<.05 存在均值回归
#时间序列的平滑
install.packages("KernSmooth")
library(KernSmooth)
gridsize <- length(y)
bw <- dpill(t, y, gridsize = gridsize)
lp <- locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize)
smooth <- lp$y #y为平滑后的数据
###########资产波动率模型
###ARCH模型
library(fGarch)
ts <- intc - mean(intc) #intc为对数收益率
Box.test(ts^2, type = "Ljung-Box",lag = 20) #H0:不存在自相关。0为存在ARCH效应
ArchTest(ts)  #两种检验都检验是否存在ARCH相应。《量化金融》 ho:NO ARCH EFFECT
Box.test(v^2,type = "Ljung-Box") #P趋向于0 ARCH效应，取显著的阶数
pacf(v^2) #定阶
source("archTest.R") #P趋向于0 存在ARCH效应，取*的阶数
m1=garchFit(~1+garch(3,0),data=intc,trace=F)                  # 正态分布
m1=garchFit(~1+garch(3,0),data=intc,trace=F,con.dist = "std") # 学生t分布
summary(m1)
#ARCH模型 Rt = mu+at  σ^t=omega+alpha1*a^t-1+alpha2*a^t-2……
#去除不显著的alpha
#对模型的残差进行检验（检验模型的拟合情况）
resi=residuals(m1,standardize=T) #得到残差
acf(resi,lag=20)
pacf(resi^2,lag=20)  #检验模型相关性
Box.test(resi^2,lag=10,type='Ljung') #检验是否存在自相关
plot(resi) #残差序列的时序图
plot(m1) #查看模型的各个图
predict(m1,10)
###GARCH模型
m2 = garchFit(~1 + garch(1,1),data=intc,trace=F) #GARCH(1,1)足够应对大部分情况
#预测 两步法估计
m3 = arima(ts^2,order = c(1,0,1)) #阶数与ARCH GARCH一致
m3
mean(intc)
fit = ts^2 - m3$residuals
v3 = volatility(m2)
cor(v3,sqrt(fit)) #两步法拟合的波动率与GARCH（1，1）的相关性系数
predict(m3,10)
###########协整分析(非平稳)和ECM误差修正模型
##协整分析和ECM（误差修正模型）
library("urca")
v1 <- ur.df(V$v1,type = "none")  #ADF检验，看是否平稳 HO：不平稳  看T值与 1pct  5pct 10pct比较，全大于接受原假设。
v2 <- ur.df(V$v2,type = "none")  #ADF检验，看是否平稳
summary(v1)
summary(v2)
lm1 <- summary(lm(V$v1~V$v2,data = v)) #EG方法 （长期均衡）
#模型 y = βx+ε
error <- residuals(lm1)
error_adf <- ur.df(error, type = "none") #若两个非平稳，检验残差是否平稳.要求平稳。 平稳存在协整关系
po.coint <- po.test(y, demean = TRUE, lshort = TRUE) #HO：两个序列不协整。y里有2个时间序列
#p-value smaller than printed p-value 拒绝原假设
summary(error_adf)
#ECM（短期波动）
diff_x <- diff(v1)
diff_y <- diff(v2)
error_term <- head(error,-1)
data.ecm<-data.frame(dy=diff_x,dx=diff_y,error_term=error_term)
model.ecm<-lm(diff_x~diff_y+error_term,data=data.ecm)
summary(model.ecm)
#▽y = ▽x+ECM(t-1)+ε
############GARCH(VAR模型)
#先求日收益率
#然后对看日收益率的均值是否为0，
t.test(tx_return) #H0为0 若为零对日对数收益率的平方定阶
                  #若不为零，对日对数收益率定阶
acf(tx_return)
pacf(tx_return)
Box.test(tx_return) #是否为白噪声
ArchTest(tx_return) #是否有ARCH效应
library(rugarch)
garch11.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(0, 0))) #因为均值为0 均值方程可以为（0，0）
mod.garch <- ugarchfit(spec = garch11.spec, data=tx_return) 
mod.garch #利用这个函数计算VaR
garch11.roll <- ugarchroll(garch11.spec, tx_return, n.start = 120, refit.every = 1, 
                           refit.window = "moving", calculate.VaR = TRUE, 
                           solver = "hybrid", VaR.alpha = 0.05) #置信水平 =1-VaR.alpha
report(garch11.roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.99)
garch11.fcst <- ugarchforecast(mod.garch, n.ahead = 6)
garch11.fcst
ret.fcst <- - qnorm(0.95) * garch11.fcst@forecast$sigmaFor
ret.fcst


############cAPM(资产定价模型)
library(devtools)
install_github("joshuaulrich/quantmod")
library(quantmod) 
library(PerformanceAnalytics)
getSymbols(c("code1","code2","^GSPC"), from = '2010-01-01') #从雅虎获取股票交易资料 ,code为股票代码
chartSeries(code) #股价图
barChart(code) #获取code的K线图
Cl(MSFT) 	#closing prices
Op(MSFT)	#open prices
Hi(MSFT)	#daily highest price
Lo(MSFT)	#daily lowest price
ClCl(MSFT)	#close-to-close daily return
Ad(MSFT)	#daily adjusted closing price
chartSeries(ClCl(code)) #日收盘价的日收益率
ret <- dailyReturn(Cl(SNP), type='log') #日对数收益率
names(code)<-c("open","high","low","close","volume","adjusted") #修改名称
dat=merge(IBM$adjusted,GE$adjusted,GSPC$adjusted) #将多支股票组合在一起
names(dat)<-c('IBM','GE','SP500') 
code_ret=dailyReturn(code)   #计算每日收益率
Rf<-0.04/12 #假设无风险利率为4%.
results<-table.AnnualizedReturns(code_ret,Rf=Rf);results #得到平均年化收益率
table.Stats(dat_ret) #具体统计值
chart.Bar(dat_ret[,1], main="IBM Daily Returns") #每日收益
chart.Bar(monthlyReturn(IBM), main="IBM Monthly Returns") #每月收益
chart.CumReturns(dat_ret,main="Total Returns",legend.loc="topleft") #累计收益
chart.Correlation(dat_ret, histogram=TRUE, pch="+") #相关性分析
CAPM.alpha(dat_ret[,1:2],dat_ret[,3],Rf=Rf) #### 1-2列为其他股票，3为市场收益 alpha值
CAPM.beta(dat_ret[,1:2],dat_ret[,3],Rf=Rf) ### beta值
#alpha值投资组合的超额收益，即管理人的能力。>0低估建议买入，<0高估建议卖空
#beta值为市场风险，反映了单个证券与市场的联动性。>1，市场上升时涨幅大，<0市场下降时跌幅小


