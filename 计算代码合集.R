###############获取帮助文件
help(A) 
args(A) 		#获取函数的参数
example(A) 		#查看A函数的例子
#工作目录
getwd()    		#查看
setwd("")     	#设置
get(" ") 		#接受一个代表对象名字的字符串参数，然后返回该对象的内容
list.files()   	#查看当前工作目录包含的所有文件
update.packages() 			#更新包
unloadNamespace("") 		#解除包
require() 					#加载包并提示是否加载成功
install.packages("pryr")
pryt::call_tree() 			#查看调用函数的递归结构
#读取纯文本文件,用制表符（tab）分隔列
names<-read.table("file", sep = " ",header = TRUE,na.strings = "缺失数据形式",stringsAsFactors = FALSE  ,skip =  ,nrows = )   #1、完整路径；2、文本名称（当前工作目录）
#,skip开始的行数，nrow读取多少行数
# read.table('F:\\R语言\\region.txt', header = F,sep = " ")
getOption("digits") 		#得到n的全局设置
options(n = ) 				#设置全局变量
getOption("warn") 			#查看警告级别
pdf() 						#把创建的图形保存到PDF中
#read家族
library(readr) 				#这个读取数据更智能read_csv() read_table()
asda <- read.csv("F:/R语言/asdasdas.csv")
read.csv() 					#分隔符默认设置为逗号，并假设数据有标题行,skip和nrow指定读取文件中的哪些位置,
#skip从哪一行开始，nrow从哪一行结束，sep="",设置分隔符号,colClass = c("character")
read.csv2() 				#用逗号作为小数位，并用分号作为分隔符
write.table(bb,"region.csv",sep=",") #保存CSV文件
read.delim() 				#使用句号作为小数位
read.delim2() 				#使用逗号作为小数位
readLines() 				#接受一个文件路径和一个可选的最大行数作为参数来读取文件
names<-read.fwf("filenames",widths = c(A,B,C),header = TRUE) #读取固定宽度文件，A为第一个元素的字符数,-1代表一个字符的列被忽略，详见《R语言经典案例》p87 
install.packages("XML") 	#读取XML文件
install.packages("xlsx") 	#读取XLSX文件
read.xlsx(A,1) 				#读取A的第一张表
read.xlsx2()
write.xlsx2()
install.packages("readxl")
read_excel()
install.packages("openxlsx")#可读写编辑xlsx文件
read.xlsx()
write.xlsx()
#其他数据文件
library(foreign)
read.spss()
read.octave()
read.xport() 				#SAS
read.dta() 					#stata
####读取复杂格式数据文件
readLines("names.text", n = 10) #最大读入十行
scan("names.text",what = list(character(0), numeric(0), numeric(0))) 	#详见《R语言经典案例》p98 
#保存
write.csv(data,filenames,"路径", row.names = FALSE) 					#关闭行号
#压缩文件
write.csv(filenames = bzfile("路径"),row.names = FALSE)
read.csv("filenames") #as.is是设置特定列的数据类型的，比如numeric,character.如果不设置，就是默认的把原来的字符转成factor型的
#R文件 
search() 							#查看已载入的R包
data(names, package = "A") 			#使用A包中的names数据
data(package = "A") 				#查看数据集
library() 							#查看已安装的R包
source("XX.R") 						#读取并执行内容
save.image()
names <- readRDS("filenames") 		#读取原生数据，原生数据速度快空间小
saveRDS() #保存原生数据
(load("filenames.RData"))
save(a,b,c,file = "filenames.RData")#保存多个R对象
history(100) #显示最近使用的命令，默认25次
.Last.value #最近一个计算出的表达式
save(list = ls(), file = ".RData") 	#将所有对象保存
#电子表格
install.packages("XLConnect")
library(XLConnect)
#安装JAVA
sheer <- readWorksheetfromFile("file.xlsx",sheet = 1,startRow = 0,startCol = 0,endRow = 100,endCol = 3) #起始行列，结束行列
#另存为电子表格文件
names <- loadWorkbook("files.xlsx",create = T)
createSheet(names,"Sheet 1")
writeWorksheet(wb,data = datanames ,sheet = "Sheet 1")
#一个步骤
writeWorksheetToFile("file.xlsx",data = dataname ,sheet = "Sheet 1",
                     shartRow =1,startCol =1)

###############函数
: 									#产生的是整数
c() 								#产生的是浮点数
demo(plotmath) 						#数学符号演示
formals(a_function)  				#查看函数的参数
args(a_function)
table(b) 							#查看B分类中每种分类的数量
addmargins(table(b)) 				#计算表的边际值
xtabs(~ A + B, data = ) 			#以AB分类看频数 ，适合二维
ftable(A + B ~ C, data = ) 			#横向AB，纵向C，看频数，适合三维
margin.table() 						#边际频数
length(which(b == "dog")) 			#查看dog的数量
formalArgs(a_function)
do.call(a_function,list(a,b)) 		#让a和b按照a_function的方式进行
round() 							#四舍五入
factorial() 						#阶乘
prod(1:5) 							#乘积1*2*3*4*5
mean() 								#均值
sample(1:4,size = 2,replace = T,prob = c())  		#有放回 设置权数
args() 								#查看函数的参数
colSums() 							#列和
rowSums() 							#行和
cbind(x,y) 							#按列输出数据
f <- cut(data, breaks, labels = c())#对数据分级并添加标签
match(value,vec) 					#找到特定值的位置 ,是一个向量在第二个向量中第一次匹配的位置
id <- match(g1, info$g2) 			#g1根据g2匹配数据，并返回g1在g2中的位置
info[id,] 							#查看匹配结果
which.max() 						#最大值的位置
which.min() 						#最小值的位置
replicate(10,f()) 					#将f()运行10次
head(names,5) 						#查看前五行
tail(names) 						#查看后五行
options(width = n) 					#拓宽输出的行数
##############基本统计量 #设定na.rm = TRUE ,告知R语言忽略缺失值
mean(x)
median(x)
sd(x)
var(x) 								#方差
cor(x,y) 							#协方差
cov(x,y) 							#相关系数
sum()
median()
identical(A,B) 						#查看A.B是否相等
B%/%A 								#整数除法
A%%B 								#余数
A%in%B 								#右侧变量中包含左侧变量时，为TRUE
ceiling() 							#向上取整
floor() 							#向下取整
Re(polyroot(c(-4,0,1))) 			#求解 x^2-4=0
n^m 								#n的m次方
exp(n) 								#e的n次方
log(x,base = exp(1)) 
is.finite() 						#判断是否有限
is.infinite() 						#判断是否无限
pmax(A,B) 							#先找A中的最大值，再找B中的最大值
pmin()
#比较整数值是否相等用==
all.equal() 						#检查数字是否相等，提供了一个容忍度
assign("names",data) 				#赋值函数
.Machine 							#显示R的一些数字信息
as.integer() 						#查看因子值的整数
is.* as.* 							#检查、转换
options(digits = n) 				#设置小数点
summary() 							#为不同的数据类型提供汇总信息
str() 								#显示对象的结构
unclass() 							#显示一个整数向量，拥有一个叫levels的属性
View() 								#把变量显示为电子表格
new_names <- eidt(old_names) 		#更改保存
ls() 								#列出所有变量的名称
rm(A,B,C) 							#移除ABC 变量
rm(list = ls()) 					#移除所有变量
#向量
vector() 							#创建一个指定类型和长度的矢量
seq.int() 							#创建一个序列
seq(from = , to = ,by = )
seq(from = , to = ,length.out = n) 	#设定长度为n
length() 							#向量中包含多少元素
nchar() 							#查看每个元素包含的字符串(字符数量)
names(A) <- c("","") 				#命名向量的名称
which() 							#返回逻辑向量中为TRUE的部分,返回位置
rep() 								#重复使用元素来创建向量
append(1:10, 99, after = n) 		#在1：10，n个数后插入99
nrow() 								#返回向量或矩阵的行数或列数
nrow() 								#仅用于矩阵
identical(A, B) 					#判断对象是否相同
union(A, B) 						#求合集
intersect(A, B) 					#求交集
setdiff(A, B) 						#求差集
setequal(A, B) 						#判断是否为相同集合
value %in% x 						#判断value是否在x中
seq_along(x) 						#创建x长度的序列
###############数组
names <- array(
  data,
  dim = c(a,b,c),
  dimnames = list(					#dimnames=list(c("r1","r2"),c("c1","c2")) 
    anames,							#两行两列
    bnames,
    cnames
  )
)
###############矩阵
matrix(
  data,
  nrow = 
    byrow = TRUE, 					#按行填充
  dimnames = list(
    a,
    b
  )
)
a%*%b  								#内乘法
a%o%b 								#外乘
t(A) 								#转置
solve(A) 							#求逆
diag(n, m) 							#一个n阶的m对角（单位）矩阵
dim(x) <- c(a,b) 					#为x重塑维度
cbind() rbind() 					#按行按列绑定2个矩阵
rownames(a) <- c("A","B","C") 		#行名
colnames(a) <- c("A","B","C") 		#列名
A[1,,drop = FALSE] 					#保留维度,禁止降维（由矩阵变为向量）
nrow() 								#行数
ncol() 								#列数

################ 列表
a_list <- list(a = dataA,
               b = dataB,
               c = dataC)
names(a_list) <- c("anames","bnames","cnames")
length(a_list) 						#返回顶层元素的数量
#向量索引,[]产生另一个列表，[[]]返回列表元素中的内容，填下标或者“名称字符串”，也可用$符号
as.list() 							#转化列表
as.numeric() 						#如果列表每个元素都是标量值，可转化为向量
unlist() 							#若列表中包含非标量元素，则用
c(a_list,b_list) 					#组合列表
list_name[1] 						#返回一个列表
list_name[[1]] 						#返回元素
list_name[["names"]] 				#选定名称为name的元素
list_name["names"] 					#返回列表
list_name[c("A", "B", "C")] 		#返回一个参数所决定的元素而构成的列表
list_name["names"] <- NULL 			#移除列表
list_name[sapply(listnames, is.null)] <- NULL 	#移除取值为空值的元素
list_name[listnames = o] <- NULL 				#使用条件来移除列表元素


################# 数据框
a_frame <- data.frame(
  a,
  b, 
  c,
  row.names = c()   #行名
)
subset(data,logical,A) 				#从data中取满足logical的A列，没有A的话取全部
subset(iris,Sepal.Length > 5)$Sepal.Lengt 
as.data.frame() 					#转变为数据框
data.frame(stringsAsFactors = FALSE)#数据框，每个向量之间用逗号隔开
names <- rbind(names, new) 			#添加行至数据框，变量名称要一致
a <- data.frame(colnames = numeric(n),colnames2 = character(n)) #预分配数据框
data[[n]] 							#返回第n列,变为向量
data[n] 							#返回一个第n列的数据框
data[["name"]] 						#返回name列的向量
data$name 							#返回一列
data["name"] 						#返回数据框
subset(data, select=c(A,B,C), subset = (A > 0)) 	#有条件的选择行和列 select= A:C 选择A、B、C列名
colnames(A) <- c("A","B","C") 		#重命名
edit()
fix()
clean <- na.omit(data) 				#从数据框中移除NA值
subset(data,select = -A) 			#排除列 
names <- transform(names, a = a^2) 	#将names中a改为a的平方
cbind(A,B) 							#合并列
rbind(A,B) 							#合并行
transform(data,var1 = , var2 = ) 	#修改VAR1和VAR2
m <- merge(A,B,by = "C")			#合并
merge(a_frame,b_frame,by = "rolnames",all = T)
f <- factor(names, c("A", "B", "C"))#规定因子和因子顺序
levels(name) <- rev(factor(name)) 	#数据、因子都反过来
factor(name, levels = rev(name)) 	#数据不变、因子反过来
by(data, data$factor, FUN) 			#分组应用函数，返回列表
aggregate(data[c("A","B")], list(factor = data$factor), FUN, na.omit = T) #分组应用函数
aggregate(iris[1:4],list(iris$Species),mean)
name <- stack(list(v1 = v1, v2 = v2, v3 = v3)) #或数据框
unstack(data, values ~ ind) 		#逆操作
str(names) 							#查看结构
framenames[1:2,1,drop = FALSE] 		#如果只提取1列，R会返回一个向量，若在括号里添加drop,则返回一个数据框(禁止减少维度)
rownames()							#查看行名
colnames()							#查看列名
View() 								#查看数据视图
rowSums() 
rowMeans()
colSums()
colMeans()
with() 								#允许代码内部直接访问列名
with(data, var[var1 == ""])
with(data, table(var1[var2 == ""])) #统计var1中var2==的频数统计表
within() 							#同上，并可修改数据
attach() 							#可以直接使用a的变量,调用attach()后对变量的修改并不会反映到调用detach（）时的原数据
detach() 							#结束attach
#排序
sort(a,decreasing = T) 				#从小到大排列,decreasing = T从大到小排列
order() 							#返回排序后各数据元素在原数据组中的位置索引
var1 <- var[order(var$a),] 			#根据a排序的var
rank() 								#返回在向量中的排名
###############原子性向量
typeof() 							#查看某个对象是什么类型,返回对象的低级内部结构,返回内部储存模式
is.vector() 						#是否
length() 							#长度
attributes()  						#属性
matrix() 							#组成矩阵
array() 							#组成数组
class() 							#类,返回对象的高级类，返回S3类名称
methods("") 						#方法分派，查看根据函数的类完成方法分派

# = 是赋值符 ==是逻辑运算符
#使用逻辑值修改数据
frame.name$B[frame.name$A == "c"] <- 2 	#若A=c,则把frame中B的值变为2,可将frame.name$A == "c"赋值为一个对象。
frame.name[frame.name$A == "c",] 		#将A中等于c的行全部列出 
###################R的环境系统
install.packages("devtools")
parens(all = TRUE) 					#返回当前会话包含的环境列表
as.environment() 					#指向环境树中的任意一个环境
parent.env() 						#查看某一个环境的父环境
objects() 							#与ls()类似
ls(envir = ) ls.str(envir = ) 		#查看存储在环境中的对象，前者返回对象名称，后者展示每个对象的结构
ls.str(mode = "") 					#查看不同模式的对象
ls.str(pattern = "") 				#正则表达式的形式表述
rm()
rm(list = ls()) 					#删除全部对象
exists("", names) 					#u，可上溯到父环境寻找
ls(all.names = TRUE) 				#显示.开头的变量名
head(globalenv()$A,3) 				#在某个特定环境中，使用$提取某个对象
assign("newobject",value,envir = globalenv()) #将新对象保存到某个特定环境中
environment() 						#查看当前的活环境
environment(fx) 					#查看fx的原环境
trunc() 							#返回整数部分
unique() 							#返回一个向量中的所有非重复值
unname() 							#返回一个对象的副本，但是会将其名称属性移除
print() 							#将结果输入到控制台的窗口中 #只能显示一个对象,能够显示特殊字符
expand.grid(A,B,stringsAsFactors = F) 		#A和B两个向量的所有组合
outer(string1, string2, paste, sep = "") 	#生成字符串所有成对组合
a <- outer(string1, string2, paste, sep = "") ;
a[!lower.tri(a)] 					#生成非重复组合

#####################字符串和因子
paste(c(A,B),c,sep = "-") 			#A-C B-C #paste能赋值对象，cat不能
paste(c(A,B),c,collapse = ",") 		#A C,B C
paste0(c(A,B),c) 					#AC BC
toString(X,width = 40) 				#将向量变成字符串并能限制字符
cat("a") 							#带引号,可以将多个对象连接并以连续的方式显示，包含特殊字符时不显示
cat("A",",","B",sep = "")
message() 							#比cat()正式
as.character() 						#将一个因子强制转化为字符串
noquote() 							#字符串向量不带引号
quote() 							#捕获函数，但不执行
substitute() 						#作用于任意的用户输入表达式
call() 								#创建一个带有相同参数的相同函数的调用
as.call() 							#将一个调用成分的列表转换为调用
eval(fun, list(x =)) 				#执行调用的函数
eval(fun, env) 						#存在fun参数的env
formatC(x,digits = 3,width = 10,format = ,flag = "+") #将A数字向量变为3个数字的字符串向量,中间加空格，科学计数法,前面加+
format()
prettyNum() 						#适合格式化那些非常大或者非常小的数字
cat("a?b")  						#\t 插入制表符 \r \r\n换行符 打印斜杠时需要打\\，打双引号前加\,fill =TRUE 使挂光标在一行结束后移动到下一行
toupper("") 						#大写
tolower("") 						#小写
substr(x, start, stop) 				#截取字符串,强制将数值型变量转换为字符型变量
sprintf() 							#格式化文本（非常重要）《R语言编程指南》p164
trimws("A", which = ) 				#消除A左右变量的空格，可控制left/right
sub(old, new, string)				#替代第一个子串
gsub(old, new, string) 				#替代所有个子串
substring(text, first, last = 1000000L) #注意区别
strsplit(a,",?",fixed = TRUE) 		#fixed = TRUE表示是固定长度的字符串而非正则表达式 忽略，根据分隔符分割字符串
strsplit(a, split = ",") 			#将a文本按,分割成列表
file.path("c:","A","B","C")  		#创建系统文件c:/A/B/C
#字符串处理包stringr 详见极客 P137

##正则表达式，\需要写成\\避免转义
library(stringr)  					#该包简化了正则表达式的使用
str_match(txt,"pattern") 			#注意用括号做标记
readLines("") 						#读取文本
grep(pattern,data) 					#pattern为正则表达式 ，返回行

#因子
levels(a$b) 						#查看因子水平
nlevels(a$b) 						#查看水平级数
a <- factor(b) 						#创建因子水平
factor(a,levels = c("1","2"), labels = c("one","two")) #更改因子水平顺序,因子的名称
levels(a,levels = c("","")) 		#直接改变因子水平值，不是我们想要的。
droplevels() 						#去掉因子水平
ordered(A,b) 						#按照b给因子水平排序
a <- cut(A,seq.int(16,66,10)) 		#将A的连续变量切块变为因子水平
cut(d,c(0,2,5),labels = F)
is.factor() 						#判断给定值是否因子
ordered(names, c("A","B")) 			#创建有序因子
is.ordered() 						#判断是否为有序因子
#生成因子水平
#合并因子
###############################日期和时间
#R中自带3个日期和时间类：POXIXct、POSIXlt、Date。POXIXct最适合与储存和计算计算时间，POSIXlt
#最适用与提取日期中的某个特定部分。Date最适用于不在乎一天中的某个时刻
sys.time() 							#以POXIXct的形式返回当前的日期和日期
sys.Date() 							#当天日期
as.Date("...",format = "") 			#将字符串转换为时间
strptime("char","format") 			#将字符串转换为时间
#解析日期：%H小时（24） %M分钟 %S秒 %m月数 %d当月的第几天 %Y四位数的年份
#格式化日期 将日期变量转换为字符串
strftime(names,"%%%%")
#计算日期
#将数字与POSIX日期想加，会以秒为单位增加时间，与DATE相加会以天数为单位
difftime(A,B,units = )
seq(A,B,by = "1 year")
########################数据清理和转换
library(stringr)
str_detect(names,fixed(",")) 		#找出names数据中，包含，的行
#缺失值
complete.cases() 					#告诉哪些行没有缺失值，给出逻辑值
names[complete.cases()] 			#给出无缺失值的数据
names[!complete.cases()] 			#给出缺失值
na.omit() 							#删除数据框中带有缺失值的行，na.exlude() 一样
na.fail() 							#检查有无缺失值，代替NA
install.packages("zoo")
library(zoo)
na.locf() 							#非缺失值后面紧跟着一个缺失值时，就用该非缺失值填补后面的缺失值。(向后填充)
DMwR::centrallmputation(data) 		#使用中心值替换NA
DMwR::knnImputation(data,k) 		#使用k相邻分类算法求加权平均值
#在宽和长表格之间进行转换
library(reshape2)
newnames <- melt(names , id.vars = "ID") #宽变长
melt(
  data,
  id.vars = c() ,					#识别字符
  measure.vars = c(),				#若省略则id以外所有列均视为测量值列
  measure.name = "", 				#为测量值命名
  na.rm = F
)
eg. melt(iris,id.vars=1:4)
dcast(
  data,
  formula #A+B~ AB为id.var ~右侧
)
dcast(data,A ~ B + C,value.var = "D",length) #n重交叉列表统计，A为纵轴，B*C为横轴，内容为D的个数
newnames <- dcast(names,var1~varN) #长变宽

#######数据表
install.packages("data.table")
library(data.table)
data.table(names) 					#创建数据表
as.data.table() 					#将数据框转换为数据表
tables() 							#列出数据表类的所有对象
fread() 							#读取数据并返回dt
data[i, j, by] 						#i选择行，用by分组，然后计算j
.N 									#表示在当前分组中，对象的数据（即每组的行数）
A[.N] 								#最后一行
TABLE[1,							#第一个参数为行号或者有关行的逻辑值
2, 									#第二个参数为列或者有关列的逻辑值
3] 									#用于设置分组变量 
#eg:TABLE[, A, b] 根据b分组的A列的所有行
setkey(
  table,
  a 								#设置a键
)
setkeyv(
  table,
  "a" 								#设置a键，但只接受字符向量
)
dt["a1"] 							#查看a键中的a1的行
dt1[dt2] 							#如果两个表格有相同的键，可以轻松将它们连接在一起
setkey(table,key1,key2) 			#设置两个键
table[.("key1",key2)] 				#key1为字符型，顺序必须与设置一致
setDT(data,key = "a") 				#将data转换为table，a为键
setDF() 							#将dt转变为df。
setnames(dt, "a","b") 				#将a的名字变为b
setcolorder(dt, c("a","b")) 		#排列列名的顺序
TABLE[J(a),FUN] 					#创建a键后，即可使用J
TABLE1[TABLE2,y] 					#在1中查找2对应的行，并执行运算
dt[,var := var == ""] 				#替换原有的列
dt[,var := ...] 					#创建新列
dt[,c("var1","var2"):=list(1:6,2:7)]#创建两列
dt[,c("var1","var2"):=NULL] 		#取消两列
dt[var1 == "B" & var2 > 3, var3:=100]
dt[, var3:=ifelse(var1 == "B" & var2 > 3, 50 ,var3)]
dt[, sum(var2), by = var1] 			#分组求和
######apply family
group <- split(x,f) 				#x为向量，f为因子，向量分组，返回列表
subset(x, b, select = c("A", "B")) 	#选取x数据框中，满足b条件的AB两列

lapply(list, function) 				#作用于列表的每一个元素，返回另一个列表
rapply(object, f, classes = "ANY", deflt = NULL, how = c("unlist", "replace", "list"), ...)
#object:list数据
#f: 自定义的调用函数
#classes : 匹配类型, ANY为所有类型
#deflt: 非匹配类型的默认值
#how: 3种操作方式，当为replace时，则用调用f后的结果替换原list中原来的元素；当为list时，新建一个list，类型匹配调用f函数，不匹配赋值为deflt；当为unlist时，会执行一次unlist(recursive = TRUE)的操作
#…: 更多参数，可选
unlist() #将列表转换为向量
#使用 lapply后，将列表转换为数据框的方法
#一、同类型数据
#1 使用unlist() 转换为向量
#2 使用matrix() 将向量转换为矩阵
#3 使用as.data.fame() 将矩阵转换为数据框
#4 使用names()从列表获取变量名，赋给数据框各列
#二、不同类型数据
#使用do.call
data.frame(do.call(cbind,lapply()))
vapply(list, function, FUN.VALUE = type, ...,USE.NAMES = TRUE) #应用于列表而返回向量，第三个参数为返回的模板
#参数列表：
#X:数组、矩阵、数据框
#FUN: 自定义的调用函数
#FUN.VALUE: 定义返回值的行名row.names.FUN.VALUE=c('a'=1,'b'=0,'c'=0,'d'=0) 数值随意设置
#…: 更多参数，可选
#USE.NAMES: 如果X为字符串，TRUE设置字符串为数据名，FALSE不设置 
sapply(list, function) #尽可能地把结果简化到一个合适的向量和数组中(返回向量、矩阵、数组)
#sapply(X, FUN, ..., simplify=TRUE, USE.NAMES = TRUE)
#参数列表：
#X:数组、矩阵、数据框
#FUN: 自定义的调用函数
#…: 更多参数，可选
#simplify: 是否数组化，当值array时，输出结果按数组进行分组
#USE.NAMES: 如果X为字符串，TRUE设置字符串为数据名，FALSE不设置
#使用as.data.frame（）将返回的向量x转换为数据框
as.data.frame(t(x)) #注意转置
apply(array, margin, function) 		#将函数应用于每行、每列，1行2列。数据框仅包含数字数据或字符串数据，向数组或矩阵
									#应用函数，然后将结果值以向量、数组或列表形式返回
tapply(data,f,function) 			#f为分组因子，将函数应用于每组数据
tapply(data,
       list(data$var1,data$var2), 	#以VAR1和VAR2为分组变量
       fun,
       na.rm = TRUE)
tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
with(data,tapply(A,B,max)) 			#以B为分组，A为值，求最大
#X: 向量
#INDEX: 用于分组的索引
#FUN: 自定义的调用函数
#…: 接收多个数据
#simplify : 是否数组化，当值array时，输出结果按数组进行分组
###建立INDEX### 详细看R语言与数据分析实践
#其他分组处理数据的代码
install.packages("doBy")
library(doBy)
summaryBy(A + B ~ C, data) 			#data依据c分类查看A和B的均值
orderBy( ~ a + b, data) 			#按照a和b进行排序
sampleBy( ~ A, frac = 0.1, data = ) #基于A分类，收取比例为10%
aggregate(
  y ~ x, 							#y是要计算的变量
  data, 							#数据
  FUN 								#待应用的函数
) 
mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,USE.NAMES = TRUE)
#FUN: 自定义的调用函数
#…: 接收多个数据
#MoreArgs: 参数列表
#SIMPLIFY: 是否数组化，当值array时，输出结果按数组进行分组
#USE.NAMES: 如果X为字符串，TRUE设置字符串为数据名，FALSE不设置
mapply(
  data, 							#用作参数的矩阵或数据框
  fun 								#待调用的函数
) 									#返回值为数据框

#如果向量参数不是一个，我们需要自定义一个函数来封装那个真正想调用的函数，更常见的做法是把函数的定义包括在lapply的调用中。
#####遍历数组
#lapply、vapply、sapply都可以用于矩阵和数组上
library("matlab") 					#matlab会覆盖一些命令，使用detach("package:matlab")解除
magic() 							#创建一个方阵
apply(array, margin, ...) 			#1将函数应用于每一行，2代表将函数应用于每一列
#当把函数应用在数据框上，apply和sapply的行为相同
############拆分-应用-合并
#revalue
a$Species <- revalue(a$Species,c("setosa"="jizhipeng"))
install.packages("plyr")
plyr包的主函数是**ply形式的，其中首字母可以是(d、l、a)，第二个字母可以是(d、l、a、_)，
不同的字母表示不同的数据格式，d表示数据框格式，l表示列表，a表示数组，_则表示没有输出。
第一个字母表示输入的待处理的数据格式，第二个字母表示输出的数据格式。例如ddply函数，
即表示输入一个数据框，输出也是一个数据框。
a*ply(.data, .margins, .fun, ..., .progress = "none") 
d*ply(.data, .variables, .fun, ..., .progress = "none")
l*ply(.data, .fun, ..., .progress = "none")
#.progress决定是否显示以及用哪种方式显示进度条 。"text"显示处理进度条 "win"windos自带进度条
library("plyr")
adply( 
  data, 							#矩阵、数组、数据框
  margin, 							#1 2 或者c(1,2)对每个元素进行数据处理
  FUN
) 									#返回数据框

ddply(
  data,
  .(var), 							#用于数据分组的变量. #EG：   .(a,x > 5) #两种分类 .(var1,var2)
  FUN 								#必须用函数function
)
ddply(
  df,
  .(df$group),
  function(df){
    data.frame(var = fun(df$group))
  }
)
m*ply(
  .data,  							#用做参数的矩阵或者数据框
  .fun = ,							#待调用的参数
  .inform
)
splat() 							#将原函数中多个参数打包为一个list作为参数，然后输出新的函数
splat(fun)(data[1,])
splat(fun)(data) 					#将data中的变量输入fun中
# m*ply(data,FUN)的作用和a*ply(data,1,splat(FUN)) 的作用一样
each(fun) 							#一系列函数作用再输入的数据上，并返回一个已命名的向量（不能给作用的函数制定附加参数）
each(fun1,fun2,fun3)(data) 
colwise() 							#把作用于数据库行向量的函数（mean）转化为数据框列向量
colwise(fun)(data)
ddply(data,.(var),colwise(fun)) 	#对组变量应用
ddply(data,.(var),colwise(fun,.(var1,var2))) #对组变量应用,只输出var1和var2
ddply(data,.(var),colwise(fun,is.character)) #只作用于字符型变量
failwith(default = NULL,FUN,quiet = F) #修正一个函数，使得函数出现错误时返回一个默认值
fun1 <- failwith(default = ,FUN,quiet = T);fun1(data)
arrange(dt,var1,var2) 				#按照列给数据框排序（其他列跟着变）,升序
arrange(dt,desc(var1)) 				#降序
rename(x,replace,warn_missing = T) 	#通过名字修改名字
rename(data,replace = c("a" = "c"))
count(df, vars = "",wt_var = ) 		#数据中观测值个数(vars为分组)
count(df,c("var1","var2")) 			#两个变量组合的数
match_df(x,y,on = NULL) 			#从一个数据框中提取与亮一个数据库中相同的行,on为制定对比的变量，默认为全部变量
count(baseball,"id");longterm = subset(count(baseball,"id"),freq > 25);bb_long <- match_df(baseball,longterm,on = "id")
join(x,y,by = "VAR",type = "left",match = "all") #by是制定要联合的变量
#type = "inner"只显示有匹配；type = "right" type = "left"  “all”基于那个数据匹配 和merge类似
#match = "all" 全部显示，"first"只显示第一次匹配的数据

#ddply经常使用一下函数
#transform
ddply(
  data,
  .(var), 
  transform, 						#在数据中添加newvar一列
  newvar = oldvar - 1
)
#mutate
ddply(
  data,
  .(var), 
  mutate, 							#在数据中添加多列
  newvar1 = oldvar - 1
  newvar1 = oldvar - 2
)
#summarise
ddply(
  data,
  .(var), 
  summarise, 						#只返回var和newvar
  newvar = min(oldvar) 
)
#subset
ddply(
  data,
  .(var), 
  subset, 							#只返回var和newvar
  vara == max(vara) 
)
#使用dplyr管道操作处理数据框
install.packages("dplyr")
select(data, var1, var2, var3) 		#返回选中的新列，并将这些列储存在新创建的表中
filter(data, var == "a", var2 == "b") #返回满足的数据，全部列
mutate(data, var = a/b) 			#创建或替换新列
arrange(data, var, desc(var1)) 		#创建一个新数据框，这个数据框是按一个或者多个列排列的
#dplyr包也提供了丰富的对接函数


