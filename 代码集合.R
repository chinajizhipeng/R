#工作目录
getwd()     #查看
setwd(“地址”)     #设置
list.files()   #查看当前工作目录包含的所有文件
#读取纯文本文件
names<-read.table("file", sep = " ",header = TRUE,na.strings = "缺失数据形式",stringsAsFactors = FALSE  ,skip =  ,nrows = )   #1、完整路径；2、文本名称（当前工作目录）,skip开始的行数，nrow读取多少行数
options() #设置全局变量
#read家族
library(readr)
asda <- read.csv("F:/R语言/asdasdas.csv")
read.csv() #分隔符默认设置为逗号，并假设数据有标题行,skip和nrow指定读取文件中的哪些位置,
#skip从哪一行开始，nrow从哪一行结束，sep="",设置分隔符号
read.csv2() #用逗号作为小数位，并用分号作为分隔符
read.delim() #使用句号作为小数位
read.delim2() #使用逗号作为小数位
readLines() #接受一个文件路径和一个可选的最大行数作为参数来读取文件
names<-read.fwf("filenames",widths = c(A,B,C),header = TRUE) #读取固定宽度文件，A为第一个元素的字符数
install.packages("XML") #读取XML文件
install.packages("xlsx") #读取XLSX文件
read.xlsx(A,1) #读取A的第一张表
read.xlsx2()
write.xlsx2()
#其他数据文件
library(foreign)
read.spss()
read.octave()
read.xport() #SAS
read.dta() #stata

#保存
write.csv(data,filenames,"路径", row.names = FALSE) #关闭行号
#压缩文件
write.csv(filenames = bzfile("路径"),row.names = FALSE)
read.csv("filenames")
#R文件 
names <- readRDS("filenames")
(load("filenames.RData"))
save(a,b,c,file = "filenames.RData")
saveRDS(a,file = "files.RDS")
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
ls() #查看已经命名的对象
dir() #返回在指定目录中的文件名，默认为当前工作目录

