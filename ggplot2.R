#绘图设备相关命令：
#1、显示绘图设备信息
dev.list() #显示出有几个绘图设备，及对应的设备号。
dev.cur() #显示当前绘图设备类型及设备号
win.graph() #打开图形设备窗口
X11() #打开图形设备窗口
dev.new() #打开图形设备窗口
pdf("r-graph.pdf") #输出到pdf文件
png("r-graph.png") #输出到png文件
dev.off(2) #关闭设备号为2的绘图设备，可再利用dev.list()查看设备是否关闭。
graphics.off() #关闭所有绘图窗口和图形设备

############散点图
#geom_point()
plot(A$x,A$y)
qplot(A$x,A$y)
qplot(A$x,A$y,data = , colour = , shape = ,alpha = I(.1)) #alpha设定重合值
#如果2个参数向量包含在同一个数据框内，则可以：
qplot(a,b,data = A)
ggplot(data,aes(x = data$a,y = data$b))+geom_point()
ggplot(data,aes(x = data$a,y = data$b))+geom_point(shape = 21) #设定点的形状
ggplot(data,aes(x = data$a,y = data$b))+geom_point(size = 1.5) #设定点的大小
##使用点的点形和颜色属性，并基于某变量对数据进行分组，分组变量必须是因子型或字符型
ggplot(data,aes(x = data$a,y = data$b,colour = groupvar))+geom_point() #颜色
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar))+geom_point() #点形
#修改默认属性
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar,colour = groupvar))+
  geom_point() +
  scale_shape_manual(values = c(1,2)) + #设置点形
  scale_colour_brewer(palette = "Set1" ) #设置颜色
#两种分组
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar,fill = groupvar2))+
  geom_point(size = 2.5) +
  scale_shape_manual(values = c(1,2)) + #设置点形
  scale_fill_manual(values = c(NA,"black")  + #NA为不填充
                      guide = guide_legend(override.aes = list(shape = 21)))
##将连续型变量映射到点的颜色或大小属性上
ggplot(data,aes(x = data$a,y = data$b,colour = groupvar))+geom_point() #颜色深度
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar))+geom_point() #点形大小
#自定义
ggplot(data,aes(x = data$a,y = data$b,fill = groupvar)) + 
  geom_point(shape = 21,size = 2.5) +
  scale_fill_gradient(low = "black", high = "white",breaks = seq(70,170,by = 20),
                      guide = guide_legend()) #程度20分层
#两组（一组连续型变量，一组分类变量）
ggplot(data,aes(x = data$a,y = data$b,size = lianxv,colour = fenlei)) + 
  geom_point(alpha = .5) +
  scale_size_area() + #使数据点的面积正比于变量值
  scale_colour_brewer(palette = "Set1")
##处理图形重叠
#1、使用半透明的点；2、使用数据分箱；3、使用箱线图
ggplot(data,aes(x = a,y = b)) + geom_point(alpha = .1) #1
ggplot(data,aes(x = a,y = b)) +
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue",high = "red", limits = c(0, 6000)) #2自定义
#一个或两个数据轴是离散时，可添加随机扰动项
ggplot(data,aes(x = a,y = b)) + geom_point() +
  geom_point(position = "jitter")
ggplot(data,aes(x = a,y = b)) + geom_boxplot(aes(group = a)) #当数据集对应一个离散数据轴和一个连续型数据轴时,用箱线图，a为离散型数据
##添加回归模型拟合线
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  stat_smooth(method = lm, level = 0.99) #添加回归线，并添加99%的置信区间
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  stat_smooth(method = lm, se = FALSE, colour = "black") #没有置信区间,设置回归线颜色，linetype线条，size大小
ggplot(data, aes(x = a,y = b)) + geom_point(colour = "grey60") + 
  stat_smooth(method = loess) #局部加权的回归线
#Logistic模型回归线
ggplot(data,aes(x = a,y = b)) + geom_point() + #y是哑变量
  geom_point(position = possition_jitter(width = 0.3,height = 0.06),alpha = 0.4,
             shape = 21, size =1.5) +
  stat_smooth(method = glm, family = binomial) 
#分组添加回归线
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  scale_colour_brewer(palette = "Set1") +
  geom_smooth()
#分组外推  
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  scale_colour_brewer(palette = "Set1") +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
##向散点图添加模型系数（在图里）
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  annotate("text", label = "r^2 = 0.42",x = 16.5, y = 52) #xy为坐标位置
#纯数学表达公式
ggplot(data, aes(x = a,y = b)) +
  geom_point() + 
  annotate("text", label = "r^2 == 0.42", parse = TRUE, x = 16.5, y = 52) #双等号
##向散点图添加边际地毯
ggplot(data, aes(x = a,y = b)) + geom_point() + geom_rug()
##向散点图添加标签
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  annotate("text",x = , y = ,label = "CHINA") +
  annotate("text",x = , y = ,label = "GOOD") #从数据集中看xy坐标
#自动添加标签
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(label = namesvar), size = 4)
#标签位置修改
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(label = namesvar), size = 4, vjust = 0) #vjust=0为标签文本的基线会与数据点对齐，
                                                        #vjust=1为标签文本的顶部会与数据点对齐
#标签的位置沿着y轴移动
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(y=b + .1, label = namesvar), size = 4, vjust = 0)
#标签的位置沿着x轴移动
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(x=a + .1, label = namesvar), size = 4, hjust = 0) #hjust=0左对齐，hjust=1右对齐
#自动添加少量的标签
#1 首先复制一个副本
#2 用%in%找出希望所保留的标签，生成逻辑向量
#3 取其标签向量中其他的值为NA
ggplot(data, aes(x = a,y = b)) + 
  geom_point() + 
  geom_text(aes(x=a + .1, label = NEW_namesvar), size = 4, hjust = 0) + #NEW_namesvar为新的标签变量
  xlim(2000,10000)
##绘制气泡图
#映射半径
ggplot(data, aes(x = a, y = b, size = GDP)) + #将大小变量映射给size属性
  geom_point(shape = 21, colour = "black", fill = "cornsilk")
#映射面积
ggplot(data, aes(x = a, y = b, size = GDP)) + #将大小变量映射给size属性
  geom_point(shape = 21, colour = "black", fill = "cornsilk") + 
  scale_size_area(max_size = 15) 
##绘制散点图矩阵
pairs(names[,2:5]) #2至5列

###########绘制折线图
plot(A$a,A$b,type = "l")
points(A$a,A$b) #加数据点
lines(A$a,b,col("")) #加折线
qplot(A$a,A$b,geom = "line")
qplot(a,b, data = A,geom = "line")
ggplot(datas,aes(x=a,y=b))+geom_line()
qplot(a,b,data = S,geom = c("line","point")) # 添加数据点
ggplot(datas,aes(x=a,y=b,group = 1))+geom_line() #x为因子时，必须设定group
ggplot(datas,aes(x=a,y=b))+geom_line()+ylim(0,max(datas$b)) #设置Y轴的起点
ggplot(datas,aes(x=a,y=b))+geom_line()+expand_limits(y = 0)
##向折线图添加数据标记
ggplot(datas,aes(x=a,y=b))+geom_line()+geom_point() # 添加数据点
##绘制多重折线图和标记
ggplot(datas,aes(x=a,y=b, colour = groupvar))+geom_line() #将分组上色
ggplot(datas,aes(x=a,y=b, linetype = groupvar))+geom_line() #将分组上线条
ggplot(datas,aes(x=factor(a),y=b, colour = groupvar,group = groupvar))+geom_line() #转变为因子型
ggplot(datas,aes(x=a,y=b, shape = groupvar))+geom_line()+geom_point(size=4) #对应不同点形的折线图
ggplot(datas,aes(x=a,y=b, fill = groupvar))+geom_line()+geom_point(size=4,shape=21) #对应不同颜色的折线图
ggplot(datas,aes(x=a,y=b, shape = groupvar))+
  geom_line(position = position_dodge(0.2))+ #将连接点左右移动0.2
  geom_point(position = position_dodge(0.2),size=4) #将点的位置左右移动0.2
##修改线条样式
#通过设置linetype、size、colour参数可以分别修改折线的线形、线宽和颜色
ggplot(datas,aes(x=a,y=b))+geom_line(linetype = "", size=1,colour = "blue") 
ggplot(datas,aes(x=a,y=b,group = groupvar))+
  geom_line( size=1,colour = "blue") #两组单一颜色和宽度的折线
ggplot(datas,aes(x=a,y=b,colour = groupvar))+
  geom_line(linetype = "")+
  geom_point(shape = 22,size = 3,fill = white) #颜色不同，并添加数据标记
##修改数据标记样式
ggplot(datas,aes(x=a,y=b))+
  geom_line()+
  geom_point(size=4,shape = 22,colour="",fill="") #自定义数据标记的大小、形状、颜色和填充色
ggplot(datas,aes(x=a,y=b,fill=groupvar))+
  geom_line(position =position_dodge(0.2) )+
  geom_point(size=4,shape = 22,position_dodge(0.2))+ #自定义数据标记的大小、形状、颜色和填充色
  sacle_fill_maual(values = c("black","white")) #将填充色设置为黑白两色，并微调数据标记
##绘制面积图
#运行geom_area()可以绘制面积图
ggplot(datanames,aes(x = a,y = b))+geom_area()
ggplot(datanames,aes(x = a,y = b))+
  geom_area(colour="black",fill="blue",alpha = 0.2) #alpha=0.2表示透明度为80%
ggplot(datanames,aes(x = a,y = b))+
  geom_area(fill="blue",alpha = 0.2)+
  geom_line() #覆盖顶部的面积图，先绘制不带边框线的面积图，再添加新图层，并用geom_line绘制轨迹线
##绘制堆积面积图
#运行geom_area()，并映射一个因子型变量给填充色
ggplot(datanames,aes(x = a,y = b,fill=factorvar)) + geom_area()
ggplot(datanames,aes(x = a,y = b,fill=factorvar)) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues",breaks=rev(levels(datanames$factorvar))) #反转标度切分
ggplot(datanames,aes(x = a,y = b,fill=factorvar,order=desc(factorvar))) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") #反转堆积顺序的堆积面积图
ggplot(datanames,aes(x = a,y = b,fill=factorvar,order=desc(factorvar))) +
  geom_area(colour=NA, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") +
  geom_line(position = "stack",size = 0.2) #没有左右边框线的堆积面积图。先……后……
##绘制百分比堆积图
ggplot(datanames,aes(x = a,y = percent,fill=factorvar)) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues",breaks=rev(levels(datanames$factorvar)))
##添加置信区间
ggplot(datanames,aes(x = a,y = b))+
  geom_ribbon(aes(ymin = b-α,ymax = b+α),alpha = 0.2) #以阴影区域表示置信域的折线图
ggplot(datanames,aes(x = a,y = b))+
  geom_line(aes(y=b-α),colour = "grey50", linetype = "dotted")+
  geom_line(aes(y=b+α),colour = "grey50", linetype = "dotted")+
  geom_line()  #以虚线表示置信域的折线图

######绘制条形图
barplot(A$x, names.agr = A$y) #第一个向量设定条形的高度，第二个向量设定每个条形对应的标签
barplot(table(S$a)) #标签为频数时
qplot(factor(A$X),A$Y,geom = "bar", stat = "identity")
qplot(X,Y,data = A, geom = "bar", stat = "identity")
ggplot(data, aes(x = X, y = Y)) + geom_bar(stat = "identity")
qplot(factor(x),data = A) #频数条形图
ggplot(data, aes(x = factor(x))) + geom_bar()
######直方图
hist(A$x,breaks = 10) #间距为10
qplot(x,data = A,binwidth = 4)
ggplot(data, aes(x=X)) + geom_histogram(binwidth = 4)
######箱线图
plot(A$x,A$y) #当x为因子型变量时，默认绘制箱线图
boxplot(a~b, data = A) #b为类型，a为y轴
boxplot(a~b + c, data = A) #加入b和c的交互项
qplot(A$a,A$b,geom = "boxplot")
qplot(a,b,data = A) #a为类型
ggplot(names,aes(x=a,y=b))+geom_boxplot()
######绘制函数图像
curve(x^3-5*3,from = -4,to = 4)
qplot(c(0,20),fun=mtfun,stat = "function",geom = "line")

##################描述数据分布
##绘制简单直方图
ggplot(datanames,aes(x = a)) + geom_histogram() #默认情况下分30组
#设定组距
ggplot(datanames,aes(x = a)) + 
  geom_histogram(binwidth = 5, fill = "white",colour = "black")
#切分为15组
binsize <- diff(range(datanames$a)) / 15
ggplot(datanames,aes(x = a)) + 
  geom_histogram(binwidth = binsize, fill = "white",colour = "black")
##基于分组数据绘制分组直方图
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ .) #groupvar为分组变量
#修改分面标签
library(plyr)
datanames$groupvar <- revalue(datanames$groupvar,c("o" = "F","1" = "M"))
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ .)
#单独设定各个分面y轴标度
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ ., scales = "free")
#同一图内的多组直方图
ggplot(datanames,aes(x = a, fill = groupvar)) + 
  geom_histogram(position = "identity", alpha = 0.4)
##绘制密度曲线
ggplot(datanames,aes(x = a)) + geom_density()
#不绘制图形两侧和底部的线段
ggplot(datanames,aes(x = a)) + geom_line(stat = "density") +
  expand_limits(y = 0) 
#有填充色
ggplot(datanames,aes(x = a)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  xlim(35,105)
#填充色无底部实线
ggplot(datanames,aes(x = a)) + 
  geom_density(fill = "blue", colour = NA, alpha = 0.2) +
  geom_line(stat = "density") +
  xlim(35,105) #设置x轴的范围
#直方图和密度函数在同一张图
ggplot(datanames,aes(x = a, y =..density..)) + #使直方图和密度函数的刻度一致
  geom_histogram(fill = "cornsilk", colour = "grey60", alpha = 0.2) +
  geom_density() +
  xlim(35,105) 
##基于分组数据绘制分组密度曲线 分组变量必须是字符串或者因子
ggplot(datanames,aes(x = a,colour = groupvar)) + geom_density() #不同颜色线条
ggplot(datanames,aes(x = a,fill = groupvar)) + geom_density(alpha = 0.3) #不同填充颜色
#分面
ggplot(datanames,aes(x = a)) + geom_density() + facet_grid(groupvar ~ .) #注意修改标签
#直方图和密度函数在同一张图的分组
##频数多边图
ggplot(datanames,aes(x = a)) + geom_freqpoly()
#控制带宽
ggplot(datanames,aes(x = a)) + geom_freqpoly(binwidth = 4)
#控制组数
##绘制基本箱线图
ggplot(datanames,aes(x=factor(a),y = b)) + geom_boxplot()
#绘制单组数据箱线图
ggplot(datanames,aes(x=1,y = b)) + geom_boxplot() + 
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank()) #绘制单组数据的箱线图必须给x参数映射一个特定的取值
#向箱线图添加槽口
#向箱线图添加均值
##绘制小提琴图
##绘制Wilkinson点图
##基于分组数据绘制分组点图
##绘制二维数据的密度图
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d()
#添加颜色
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d(aes(colour = ..level..))
#填充色
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)
#填充色带数据点
####################注解
##添加文字注解
p + annotate("text",x = , y = , label = "GGG") #知道坐标轴
#指定文本属性
p + annotate("text",x = , y = , label = "GGG", family = "serif",
             fontface = "italic", colour = "darkred", size = 3)  #调整字体颜色大小
#如果坐标轴是连续型的，可以使用Inf和-Inf在绘图区域的边缘放置文本注解
##在注解中使用数学表达式
p + annotate("text",x = , y = , parse = TRUE, label = "XXX") 
#在常规文本融入表达式中，只需要双引号内使用单引号标出纯文本的部分
#要显示两个相邻的变量，需要在它们之间放一个*
##添加直线
p + geom_hline(yintercept = 60) #横线
p + geom_vline(xintercept = 60) #竖线
p + geom_abline(intercept = 60, slope = 1.75) #斜线
#添加每组均值线
#x轴为离散数据时，添加第几个因子的线，就写xintercept=几
##添加线段和箭头
p + annotate("segment", x= 开始, xend =结束, y= 开始, yend =结束)
#添加剪头
p + annotate("segment", x= 开始, xend =结束, y= 开始, yend =结束,
             arrow = arrow(ends = "both",angel = 90, length = unit(.2,"cm")))
##添加矩阵阴影
p + annotate("rect", xmin = 起始,xmax = 结束, ymin = 起始, ymax = 结束, alpha = .1,
             fill = "blue")
##高亮某一个元素
##添加误差线
##向独立分面添加注解
##############坐标轴
##交换x和y轴
ggplot(datanames,aes(x = a, y = b)) + geom_point() + coord_flip()
##设置值域
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  ylim(0,max(datanames$b))
#扩展值域
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  expand_limits(y = 0)
##反转一条连续型坐标轴
ggplot(datanames,aes(x = a, y = b)) + geom_point() + scale_y_reverse(limits = c(8,0))
#指定
ggplot(datanames,aes(x = a, y = b)) + geom_point() + ylim(6.5, 3.5)
##修改类别型坐标轴上的项目
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() +
  scale_x_discrete(limits = c("c", "b", "a"))
#反转项目顺序
##设置x轴和y轴的缩放比例
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() #1：1缩放
#指定位置放置刻度线
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() +
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 30))
#指定坐标轴缩放比例
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed(ratio = 1/2) + #x/y = 1/2
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 30))
##设置刻度线位置
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) #显示0-3的刻度
#设置x轴（离线型）
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  scale_x_continuous(limits = c("a", "b"), breaks = "b") #只显示b
##移除刻度线和标签
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.text.y = element_blank()) #移除刻度标签
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) #移除刻度和标签
#移除刻度线、刻度标签、网格线
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  scale_y_continuous(breaks = NULL) #仅对连续坐标轴有效
##修改刻度标签的文本
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  scale_y_continuous(breaks = c(1,2,3), #将y轴的1,2,3改成a,b,c
                     labels = c("a", "b", "c"))
#标签以某种形式储存（高度，时间）
##修改刻度标签的外观
ggplot(datanames,aes(x = a, y = b)) + boxplot() + 
  scale_x_discrete(breaks = c("a", "b", "c"), 
                  labels = c("A", "B", "C")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) #将文本旋转90度
#其他文本属性(大小、样式、字体)
ggplot(datanames,aes(x = a, y = b)) + boxplot() + 
  scale_x_discrete(breaks = c("a", "b", "c"), 
                   labels = c("A", "B", "C")) +
  theme(axis.text.x = element_text(family = "Times", face = "italic",
                                   colour = "darkred", size = rel(0.9))) #rel(0.9)当前大小字体的0.9倍
##修改坐标轴标签的文本
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  xlab("A") + ylab("B\nC") #换行
##移除坐标轴标签
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.title.x = element_blank())
##修改坐标轴标签的外观(y轴同理)
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  theme(axis.title.x = element_text(angle = 90, face = "italic", colour = "darkred", size = 14))
##沿坐标轴显示直线
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  theme(axis.line = element_line(colour = "black"))
##使用对数坐标轴
##在坐标轴上使用日期
ggplot(datanames,aes(x = data, y = b)) + geom_line() 
#使用指定分割点
ggplot(datanames,aes(x = data, y = b)) + geom_line() +
  scale_x_date(seq(as.Date("1993-09-01"), as.Date(2018-01-17), by = "2 month")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) #旋转文本标签
#指定标签格式
ggplot(datanames,aes(x = data, y = b)) + geom_line() +
  scale_x_date(seq(as.Date("1993-09-01"), as.Date(2018-01-17), by = "2 month"),
               labels = date_format("%Y %b")) + #p171看时间格式内容
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
##在坐标轴上使用相对时间
####################控制图形的整体外观
##设置图形标题
p + ggtitle("X\nX")
#在图片内部
p + ggtitle("X\nX") + theme(plot.title  = element_text(vjust = -2.5))
##修改文本外观
#主题项目（标题、坐标轴标签、坐标轴刻度线）
p + ggtitle("X\nX") +
  theme(plot.title = element_text(size = rel(1.5), lineheight = .9), family = "Times",
        fontface = "bold.italic", colour = "red")
#文本集合对象
p + annotate("text", x = 15, y = 53, label = "Some text", size = 7, family = "Times",
             fontface = "bold.italic", colour = "red")
##theme()中控制文本外观的主题项目
##主题元素和文本几何对象的文本属性 p177
##使用主题
p + theme_grey() #灰色主题
p + theme_bw()   #黑色主题
#修改主题的元素，配合相应的element_xx对象添加函数(P182)页
#自定义主题
##隐藏网格线
p + theme(panel.grid.major = element_blank(), #主网格线panel.grid.major.x/y 横纵控制
          panel.grid.minor = element_blank()) #次网格线panel.grid.minor.x/y 横纵控制
##############图例
#移除图例
p + guides(fill = FALSE)
p + scale_fill_discrete(guide = FALSE)
p + theme(legend.position = "none")
#修改图例的位置
p + theme(legend.position = "top") #top\left\right\bottom
p + theme(legend.position = c(1,0)) # 11 10 01 00
#带有不透明背景和外框线的图例
p + theme(legend.position = c(1,0)) +
  theme(legend.background = element_rect(fill = "white", colour = "black"))
#不含背景和外框线的图例
p + theme(legend.position = c(1,0)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())
##修改图例项目的顺序
p + scale_fill_discrete(limits = c("C","B","A"))
##反转图例项目的顺序
p + guides(fill = duide_legend(reverse = TRUE))
##修改图例标题
p + labs(fill = "XXX")
p + scale_fill_discrete(name = "XXX")
#两组图例
p + labs(shape = "AAA", colour = "BBB")
##修改图例的外观
p + theme(legend.title = element_text(face = "", family = "", 
                                      colour = "", size = ))
##移除图例标题
p + guides(fill = guide_legend(title = NULL))
##修改图例标签
p + scale_fill_discrete(labels = c("a", "b", "c"))
#两组图例
p + scale_shape_discrete(labels = c("a", "b", "c")) +
    scale_colour_discrete(labels = c("A", "B", "C"))
##修改图例标签的外观
p + guides(fill = guide_legend(label.theme = 
                                 element_text(face = "", family = "", 
                                              colour = "", size = )))
##使用含多行文本的标签（注意修改间距）
p + scale_fill_discrete(labels = c("a\nbb", "b\nbbb")) +
  theme(legend.text = element_text(lineheight = .8), 
        legend.key.heigh = unit(1, "cm"))

#######################分面
p + facet_grid(groupvar ~ .) #纵向
p + facet_grid(. ~ groupvar) #横向
p + facet_grid(groupxvar ~ groupyvar) #同时
p + facet_wrap( ~ groupvar,nrow = 3) #规定几行
#在不同坐标轴下使用分面
#修改分面的文本标签
#修改分面标签和标题的外观
######################配色
#对离散型变量使用自定义调色板
#使用色盲友好的调色板
#对连续型变量使用自定义调色板
#设定阴影颜色
#####################其他图形
##绘制相关矩阵图
##绘制函数曲线
##函数曲线加阴影
##绘制三维散点图
##绘制饼图
a <- table(data$aa) #统计频数
pie(a) 
pie(c(99, 18, 120), labels = c("A", "B", "C")) #添加标签
####################绘制地图
library(ggplot2)
library(maps)
states_map <-map_data("state")
ggplot(states_map,aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
#世界可选地图
world_map <- map_data("world")
sort(unique(world_map$region))
#世界地图
library(maps)
map("world", fill = TRUE, col = rainbow(200),
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))
title("世界地图")
#抽取中国地图
library(maps)
library(mapdata)
map("china") #不完整
library(sp)
library(maptools)
x= readShapePoly('bou2_4p.shp') 











