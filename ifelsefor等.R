##############流程控制##################
stopifnot() #如果正确执行，那么就会啥都没发生，如果错误了，就会跳入Debug模式，报错，让函数立刻停下来。
f2 <- edit(f1)
#if语句结构判断语句的取值，只能是一个逻辑值，而不是逻辑向量
if (this){
  that   
}
#else语句
if (this){
  plana 
}else{     #else必须与if语句的大括号紧接在同一行
  planb 
}
#如果面临2个以上的互斥性情况，可以将if和else结合起来使用，也就是在else后紧跟一个if。
if (this){
  plana
} else if(that){
  planb
} else if(these){
  planc
} else{
  other
}

ifelse(test, yes, no) #第一个参数是逻辑条件向量，
                      #第二个参数是为T时返回，
                      #第三个参数为F时返回
with(data = DF,
     ifelse((VAR1 == ""), 1, 
     ifelse((VAR1 == ""), 2, 
     ifelse((VAR1 == ""), 3, 4))))  # 1 ，2 ，3赋值，其余为4
#switch函数。如果包含太多的else语句，能降低代码的可读性，可使用switch函数
switch(EXPR, ...) #第一个参数为一个返回字符串的表达式，其后的参数为与第一个参数相匹配时的
                  #返回值

#################循环#################
#repeat循环 ，反复的执行代码，直到告诉它停止为止。
repeat(a next break) #跳出正在执行的语句块，进入下一个循环

#for循环
  
for (value in that){   #for循环接受一个迭代器变量和一个向量参数 (i in c("a", "b"))
  this
}         
#循环填充
for(i in 1:4){
  char[i] <- words[i]
}

#while循环 #只要某个条件为真，就重复运行某个代码 小心
while (condition) {
  code
} 
###定义函数
formals(fun) #FUN的形式参数列表
body(fun)    #FUN的函数体
function(a,b,c){
  expression
  expression
  return()
}
###定义泛型函数S3
#为现有类定义泛型函数
G_f <- function(var,var)
  UseMethod("G_f") #调用该函数出发分类
G_f.default <- fun #按照不用的class定义不同的fun
G_f.data.frame <- fun #按照不用的class定义不同的fun
#定义新类并创建对象(列表对象)
newclass <- function(v1,v2,v3) {
  structure(list(v1 = v1,
                 v2 = v2,
                 v3 = v3),
            class = "newclass")
} #创建新类
#print为新类创建实现方式
print.newclass <- function(x, ...) {
  cat()
  cat()
  cat()
  invisible() #阻止函数返回对象的重复打印 像qui
}
###S4对象系统
setClass("A",  #A为类定义
         representation(A1 = "character",
                        A2 = "numeric",
                        A3 = "interger"),#A1-A3为字段
         prototype(A1 = "", A2 = "", A3 = ""),#设置默认值
         validity = AAA,#设置返回错误定义的类的警告
         contains = "") #设置继承
getSlots("A") #获得A的字段
a <- new("A",A1 = , A2 = , A3 = ) #创建一个新的S4类对象实例
isS4() #是否为S4类
a@A1 #使用@来访问
slot(a, "A1") #访问字段
#定义S4泛型函数
setGeneric("A", function(object) { #创建S4泛型函数
  standardGeneric("A") #为A()提供一个调用的函数的泛型函数可用于S4方法的分派
}, valueClass = "numeric") #确保返回值为数值型
setMethod()
###RC(引用类)
A <- setRefClass("A", #类定义
                 fields = list(A1 = "character",
                               A2 = "numeric"))
A$new() #创建一个新的A类对象实例
A$A1 #访问RC字段
###R6
install.packages("R6")

###管道操作magrittr《R极客》p132
library(magrittr)
A %>% B          #向右操作符，把左侧的程序的数据集传递给程序的右侧 `+`
A %T>% B %>% C   #向左操作符，A值传递给B，然后A值再传递给C
A %$% B          #解释操作符，右侧可以直接使用列名操作数据 data.frame(x=1:10,y=rnorm(10),z=letters[1:10]) %$% .[which(x>5),]
A %<>% B %>% C   #复合赋值操作符，C最后再传给A