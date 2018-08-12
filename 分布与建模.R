#随机数
simple(n) #返回一个1到n的自然数排列
simple(n,m) #返回m个1和n之间的随机数
simple(n, m, replace = T) #允许重复
sample(colors(),5)
sample(a,1,prob = )
runif(5) #5个介于0-1之间均匀分布的随机数
runif(5,1,10) #5个介于1-10之间均匀分布的随机数
rnorm(5,3,7) #均值为3，标准差为7的5个正太分布的随机数
set.seed() #设定种子值，保证运行同一段代码能生成相同的随机数
#分布：概率密度函数、累计分布函数、反函数，分别以d\p\q开头
dnorm() #密度
plot(x, dnorm(x), type = "l") #正态分布图
curve(dnorm(x),from = -4, to = 4) #x <- c(-10,10  )
pnorm() #累计分布函数
qnorm() #p分位数
rnorm() #n个服从正态分布的随机数
summary(M) #提供最小值、最大值、四分位数、数值型变量，以及因子向量和逻辑型向量的频数统计
install.packages("Hmisc")
library(Hmisc)
describe() #变量和观测的数量、缺失值、唯一值的数目、平均值和分位数
library(fBasics)
basicStats() #统计量描述
install.packages("pastecs")
library(pastecs)
stat.des(data, basic = T, desc = T, norm = F, p = 0.95) #含大量统计性描述
describe() #含大量的统计性描述 
install.packages("Hmisc")
Hmisc::describe(x, v1 ~ v2 + v3) #注意缺失值处理的情况，返回个数、值的列表，v1
mean() #均值na.omit = T
sd() #标准差
var() #方差
median() #中位数
quantile() #分位数
mystats <- function(x, na.omit = FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x - m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
library(doBy)
summaryBY(A + B ~ c, data = names, FUN = mystats) #以c为分组变量，查看AB的统计性描述
#描述性统计图表
#直方图
#经验累计分布
#QQ图
#箱式图
#频数表和列联表
#条形图
#饼图
table(A,B) #以AB为分组的频数表
prop.table(A, B)  #换算呈比例
A <- xtabs( ~a + b, data = names) #二维频数
margin.table(A, 1) #查看A中第一个变量
prop.table(A,1) #比例
#正太分布检验
shapiro.test() #大于0.05
qqnorm() #QQ图
#单样本t检验
t.test(data,mu = ) #mu为均值，检验data的值是否符合均值为A的正态假设
#单样本Wilcoxon检验(不依赖于正态分布的假设)
wilcox.test(data,mu = ) #检验data的值是否符合均值为A的假设
#双样本t检验
t.test(A$a ~ A$b, var.equal = T) #b为分组变量，目的是查看两组的水平是否有差异,假设方差相等
#双样本同方差检验
#双样本Wilcoxon检验
#独立性检验
A <- xtabs( ~a + b, data = names) #先生成频数表
chisq.test(A) #卡方独立性检验 ho:独立
chisq.test(A,B) #检验B的差异是否由A导致
fisher.test(A) #Fisher精确检验，用于2*2以上的列联表
B <- xtabs( ~a + b + c, data = names)
mantelhaen.test(B) #a和b在c中是否独立， ho：独立
#独立性→相关性
cov(data, use = , method = )
#偏相关是指在控制其他定量变量后，两个变量之间的相互关系
install.packages("ggm")
library(ggm)
colnames(data) #获取变量下标
pcor(c(1, 4, 3), cov(data)) #控制3后，1和4的偏相关系数
#相关的显著性
install.packages("psych")
library(psych)
corr.test(datas,use = ,method = )
cor.test(A, B, method = ) #A和B之间的相关性，p值
cor(A, B, method = ) #AB之间的相关性
pcor.text() #偏相关的显著性
###t检验
#独立样本
t.test(y ~ x, data) #y为被解释变量，x为解释变量，二元变量 #2者相同
t.test(y1, y2) #2个变量为数值型
#非独立样本
t.test(y1, y2, paired = TRUE)
#组间差异的非参数检验 变量在本质上发生严重偏倚，或者存在有序关系
#若独立
wilcox.test(y ~ x,data)
wilcox.test(y1, y2)
#多于两组
kruskal.test(y ~ A, data) #A的分类多于两组，各组之间独立
#进行多组比较
##################回归
#大多数模型满足一个通用的模式：左边指定响应变量，右边制定自变量，两者由波浪线隔开
#普通最小二乘法
complete.cases() #筛选出没有缺失值的观测
M <- lm(A ~ B + C + D) #A为被解释变量，BCD为解释变量
~ #分隔符
+ #分割解释变量
: #解释变量的交互项
* #所有交互项的简洁方式
^ #交互项的次数
- #除去该变量
-1 #删除截距项
bM <- dlply(df,.(var),M) #对每组观测值分别回归


##广义线性模型（放宽被解释变量正态分布的假设） LOG回归，泊松回归，生存分析
#检查是否存在过度离势
M <- glm(formula = A ~ . ,
         family = "binomial",
         data = data1)
f <- fitted(m) #得出模型的拟合值
ifelse(f > .5, 1, 0) == as.numeric(data$A) #查看拟合值与实际情况
fit <- glm(y ~ A+ ..., data = names, family = binomial())
anova(fit, fit_full, test = "Chisq") #若不显著则说明两个模型拟合程度类似，删除的变量不影响模型
exp(coef(fit)) #对模型指数化进行解释，如age= 1.1 ，说明年龄增加一岁，婚外情的优势比*1.106
data$prob <- predict(fit, newdata = data, type = "response")
##泊松回归
#检查是否存在过度离势
library(qcc)
qcc.overdispersion.test(y, type="poisson") #若存在过度离势，将"quasipoisson"替换"poisson"
fit <- glm(y ~ A + B, data = names, family = poisson()) 

##多项逻辑回归模型
nnet::multinom(
  formula = ,
  data = 
)
fitted() #显示模型如何拟合训练数据，返回各行数据属于各分类的概率
predict(object = , data, type = c("class", "probs")) #使用多项逻辑回归模型进行预测,class返回种类，probs返回概率
xtabs( ~ pre + acutal) #返回列连表
######
#其他函数
summary() #拟合模型的详细结果
coefficients() #列出拟合模型的参数
confint() #提供置信区间
fitted() #提供模型的预测值
residuals() #残差值，回归值与观测值之差
anova() #方差分析表
vcoc() #协方差矩阵
AIC() #赤池信息准则
plot() #生成拟合型的诊断图
predict() #预测被解释变量
scale(data[1:4]) #数据标准化,center = T,scale = T 
################方差分析（需要满足正态性和同方差性假设）
fit <- aov(y ~ A) #单因素ANOVA,需要满足正态分布假设
library(gplots)
plotmeans(y ~ A, xlab=" ", ylab=" ", 
          main="Mean Plot\nwith 95% CI")   #看图均值图
TukeyHSD(fit) #查看各组之间的差异性及显著性
#检验正态分布
library(car)
qqPlot(lm(y ~ A, data=names), 
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)
#方差齐性检验(还包含假定回归斜率相同)
bartlett.test(y ~ A, data=names) #H0:同方差
aov(y ~ x + A) #含单个协变量的单因素ANOVA88  x为协变量，A为因素
library(effects)
effect("A", fit) #查看去除协变量效应后的组均值
library(multcomp)
fit2 <- aov(y ~ x*A, data=names)
summary(fit2) #检验回归斜率，若交互项不显著，支持斜率相等的假设
library(HH)
ancova(y ~ x + A, data=names) #斜率可视化 ,看每组的斜率是否平行

aov(y ~ A * B) #双因素ANOVA
interaction.plot(A, B, y, type="b", 
                 col=c("red","blue"), pch=c(16, 18),
                 main = " ")  #结果可视化
aov(y ~ x1 + x2 + A * B) #含两个协变量的双因素ANOVA
aov(y ~ B + A) #随机化区组ANOVA
aov(y ~ A + Error(Subject / A)) #单因素组内ANOVA
aov(y ~ B * W + Error(Subject / W)) #含单个组内因子（W）和单个组间因子（B）的重复测量ANOVA
manova(y ~ A) #多元方差分析,需要检验多元正态性和方差-协方差同质性
summary(fit)
summary.aov(fit)
#####变量选择
#一、过滤器方法
#1、近零方差（剔除近零方差变量）
caret::nearZeroVar(data)
data1 <-  data[,-nearZeroVar(data)] #剔除近方差变量
#2、相关系数。（剔除相关系数较大的解释变量）
caret::findCorrelation(data, cutoff = 0.5) 
findCorrelation(cor(subset(data, select = -c(A)))) #剔除不用的变量A，返回系数高的变量索引
data1 <- data[, -c(返回索引)]
#3、卡方检验（剔除独立变量）
M <- FSelector::chi.squared( #别忘了安装JAVA 
  A ~ . , #与A的所有变量的独立性检验
  data = DATA 
) 
cutoff(M, 3) #选出重要的三个变量
#4、使用模型评估变量的重要性
caret::varImp(object) #回归或分类模型
###模型评估方法
#评估指标
caret::confusionMatrix(predicted, actual) #混淆矩阵
#ROC曲线
a <- ROCP::prediction( #创建prediction对象
  predict, #预测值
  acutal   #实际值
)
plot(ROCP::performance(a,"tpr","fpr"))
############主成分分析
a <- princomp(data,
              cor = T) #F为协方差矩阵，T为相关矩阵
summary(a) #查看结果COMP
a$scores[,1:2] #查看前两个主成分的得分
###########分类与回归树
#先设定训练集和验证集
train <- sample(nrow(data), 0.7*nrow(data))
data.train <- data[train,] #训练集
data.validate <- data[-train,] #验证集
##逻辑回归
fit.logit <- glm(y~., data=data.train, family=binomial())
summary(fit.logit)
prob <- predict(fit.logit, data.validate, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), 
                     labels=c("benign", "malignant")) #和验证集分类一样
logit.perf <- table(data.validate$class, logit.pred,
                    dnn=c("Actual", "Predicted")) ##与验证集的分类验证
logit.perf #查看验证结果 
##决策树(经典决策树)
rpart::rpart(
  formula = ,
  data = ,
  method="class",      
  parms=list(split="information")
)
dtree$cptable
plotcp(dtree) #选择虚线下最左侧CP值对应的树
dtree.pruned <- prune(dtree, cp=.0125)  #选择CP值进行剪枝
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,  
    fallen.leaves = TRUE, main="Decision Tree") #绘制决策树
dtree.pred <- predict(dtree.pruned, df.validate, type="class") ##利用验证集预测
dtree.perf <- table(df.validate$class, dtree.pred, 
                    dnn=c("Actual", "Predicted"))  #验证预测结果
dtree.perf #查看验证结果
####条件推断决策树（缓解RPART过度拟合的问题），变量和分割的选取是基于显著性检验的
party::ctree( #创建决策树模型
  formula,
  data
)
party::predict.BinaryTree(
  obj,
  newdata,
  type = c("response", "node" ,"prob")
)
plot(m) #可视化模型
library(party)
#步骤
fit.ctree <- ctree(class~., data=df.train) #使用训练集
plot(fit.ctree, main="Conditional Inference Tree") #查看决策树，对于条件推断树来说，剪枝不是必需的。

ctree.pred <- predict(fit.ctree, df.validate, type="response") #预测验证集
ctree.perf <- table(df.validate$class, ctree.pred, 
                    dnn=c("Actual", "Predicted")) #验证预测
ctree.perf #查看预测

####随机森林
randomForest::randomForest( #创建随机森林
  formula,
  data,
  ntree = n, #n个决策树
  mtry,
  importance = T
)
randomForest::predict.randomForest( #预测
  obj,
  newdata,
  type = 
)
randomForest::varImpPlot( #为变量的重要性绘制图形
  obj,
  type = NULL
)
#实例
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~., data=df.train,        
                           na.action=na.roughfix, #将缺失值变为中位数
                           importance=TRUE)       #度量变量的重要性          
fit.forest 
importance(fit.forest, type=2)                    #得到相对重要性就是该变量时节点不存度下降总量对所有树取平均     
forest.pred <- predict(fit.forest, data.validate)         
forest.perf <- table(data.validate$class, forest.pred, 
                     dnn=c("Actual", "Predicted"))
forest.perf
#参数调优
###神经网络
nnet::nnet(formula, data, size = n) #n为隐匿层接点的个数
predict(m, newdata = ,type = "") #预测新数据的分类
###支持向量机(二元分类问题)
kernlab::ksvm(formula, data= ) #创建向量机
predict(m, newdata = ) #预测新数据
#实例
library(e1071)
set.seed(1234)
tuned <- tune.svm(class~., data=df.train,
                  gamma=10^(-6:1),  
                  cost=10^(-10:10))
tuned #得出gamma和cost最佳的参数
fit.svm <- svm(class~., data=data.train, gamma=.01, cost=1) #进行调整
svm.pred <- predict(fit.svm, na.omit(data.validate))
svm.perf <- table(na.omit(data.validate)$class, #SVM在预测新样本时不允许有缺失值存在
                  svm.pred, dnn=c("Actual", "Predicted")) #预测
svm.perf #查看

###类别不平衡
catet::upSample(x,y)   #向上取样 #x为解释变量，y为被解释变量
catet::downSample(x,y) #向下取样
##有监督机器学习方法中预测效果最好的解
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}
performance(XX.perf) #
########################聚类分析
#划分聚类，首先指定类的个数，然后观测值形成聚合的类
#层次聚类，每一个观测值自成一类，这些类每次两两合并，直到所有的类聚成一类
#聚类经典步骤：1、选择合适的变量；2、缩放数据，标准化；3、寻找异常点，剔除；4、计算距离
#5、选择聚类的算法；6、获得一种或者多种聚类的算法；7、确定类的数目
#8、获得最终聚类的解决方案；9、结果可视化；10、解读类；11、验证结果
#计算距离
dist(data,method = )
as.matrix()
##层次聚类
nutrient.scale <- scale(nutrient)
d <- dist(nutrient.scale)
a <- hclust(d, method = "average") #single,complete,centroid,ward
plot(a, hang = -1, cex = .8)
install.packages("NbClust") #确定聚类的最佳数目
library(NbClust)
devAskNewPage(ask = TRUE)
nc <- NbClust(nutrient.scale, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "average")
table(nc$Best.n[1,])
clusters <- cutree(a, k =5) #把树状图分为5类,可以查看分类的具体情况
table(clusters) #查看5类的分布
aggregate(nutrient, by = list(cluster = clusters), median) #描述聚类
plot(a, hang = -1, cex = .3)
rect.hclust(a, k = 5) #最后的图
##划分聚类
#K均值聚类 ,对异常值是敏感的
scaled <- sacle(names)
library(NbClust)
set.seed(1234)
devAskNewPage(ask = TRUE)
nc <- NbClust(scaled,min.nc = ,max.nc = ,method = "kmeans")
table(nc$Best.n[1,])
fit.km <- kmeans(scaled, 3, nstart = 25)
#围绕中心点的划分（PAM）
library(cluster)
set.seed(1234)
fit.pam <- pam(datanames, k = 3, stand = TRUE)

