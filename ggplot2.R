#��ͼ�豸������
#1����ʾ��ͼ�豸��Ϣ
dev.list() #��ʾ���м�����ͼ�豸������Ӧ���豸�š�
dev.cur() #��ʾ��ǰ��ͼ�豸���ͼ��豸��
win.graph() #��ͼ���豸����
X11() #��ͼ���豸����
dev.new() #��ͼ���豸����
pdf("r-graph.pdf") #�����pdf�ļ�
png("r-graph.png") #�����png�ļ�
dev.off(2) #�ر��豸��Ϊ2�Ļ�ͼ�豸����������dev.list()�鿴�豸�Ƿ�رա�
graphics.off() #�ر����л�ͼ���ں�ͼ���豸

############ɢ��ͼ
#geom_point()
plot(A$x,A$y)
qplot(A$x,A$y)
qplot(A$x,A$y,data = , colour = , shape = ,alpha = I(.1)) #alpha�趨�غ�ֵ
#���2����������������ͬһ�����ݿ��ڣ�����ԣ�
qplot(a,b,data = A)
ggplot(data,aes(x = data$a,y = data$b))+geom_point()
ggplot(data,aes(x = data$a,y = data$b))+geom_point(shape = 21) #�趨�����״
ggplot(data,aes(x = data$a,y = data$b))+geom_point(size = 1.5) #�趨��Ĵ�С
##ʹ�õ�ĵ��κ���ɫ���ԣ�������ĳ���������ݽ��з��飬������������������ͻ��ַ���
ggplot(data,aes(x = data$a,y = data$b,colour = groupvar))+geom_point() #��ɫ
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar))+geom_point() #����
#�޸�Ĭ������
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar,colour = groupvar))+
  geom_point() +
  scale_shape_manual(values = c(1,2)) + #���õ���
  scale_colour_brewer(palette = "Set1" ) #������ɫ
#���ַ���
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar,fill = groupvar2))+
  geom_point(size = 2.5) +
  scale_shape_manual(values = c(1,2)) + #���õ���
  scale_fill_manual(values = c(NA,"black")  + #NAΪ�����
                      guide = guide_legend(override.aes = list(shape = 21)))
##�������ͱ���ӳ�䵽�����ɫ���С������
ggplot(data,aes(x = data$a,y = data$b,colour = groupvar))+geom_point() #��ɫ���
ggplot(data,aes(x = data$a,y = data$b,shape = groupvar))+geom_point() #���δ�С
#�Զ���
ggplot(data,aes(x = data$a,y = data$b,fill = groupvar)) + 
  geom_point(shape = 21,size = 2.5) +
  scale_fill_gradient(low = "black", high = "white",breaks = seq(70,170,by = 20),
                      guide = guide_legend()) #�̶�20�ֲ�
#���飨һ�������ͱ�����һ����������
ggplot(data,aes(x = data$a,y = data$b,size = lianxv,colour = fenlei)) + 
  geom_point(alpha = .5) +
  scale_size_area() + #ʹ���ݵ����������ڱ���ֵ
  scale_colour_brewer(palette = "Set1")
##����ͼ���ص�
#1��ʹ�ð�͸���ĵ㣻2��ʹ�����ݷ��䣻3��ʹ������ͼ
ggplot(data,aes(x = a,y = b)) + geom_point(alpha = .1) #1
ggplot(data,aes(x = a,y = b)) +
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue",high = "red", limits = c(0, 6000)) #2�Զ���
#һ������������������ɢʱ������������Ŷ���
ggplot(data,aes(x = a,y = b)) + geom_point() +
  geom_point(position = "jitter")
ggplot(data,aes(x = a,y = b)) + geom_boxplot(aes(group = a)) #�����ݼ���Ӧһ����ɢ�������һ��������������ʱ,������ͼ��aΪ��ɢ������
##���ӻع�ģ�������
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  stat_smooth(method = lm, level = 0.99) #���ӻع��ߣ�������99%����������
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  stat_smooth(method = lm, se = FALSE, colour = "black") #û����������,���ûع�����ɫ��linetype������size��С
ggplot(data, aes(x = a,y = b)) + geom_point(colour = "grey60") + 
  stat_smooth(method = loess) #�ֲ���Ȩ�Ļع���
#Logisticģ�ͻع���
ggplot(data,aes(x = a,y = b)) + geom_point() + #y���Ʊ���
  geom_point(position = possition_jitter(width = 0.3,height = 0.06),alpha = 0.4,
             shape = 21, size =1.5) +
  stat_smooth(method = glm, family = binomial) 
#�������ӻع���
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  scale_colour_brewer(palette = "Set1") +
  geom_smooth()
#��������  
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  scale_colour_brewer(palette = "Set1") +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
##��ɢ��ͼ����ģ��ϵ������ͼ�
ggplot(data, aes(x = a,y = b, colour = groupvar)) +
  geom_point() + 
  annotate("text", label = "r^2 = 0.42",x = 16.5, y = 52) #xyΪ����λ��
#����ѧ���﹫ʽ
ggplot(data, aes(x = a,y = b)) +
  geom_point() + 
  annotate("text", label = "r^2 == 0.42", parse = TRUE, x = 16.5, y = 52) #˫�Ⱥ�
##��ɢ��ͼ���ӱ߼ʵ�̺
ggplot(data, aes(x = a,y = b)) + geom_point() + geom_rug()
##��ɢ��ͼ���ӱ�ǩ
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  annotate("text",x = , y = ,label = "CHINA") +
  annotate("text",x = , y = ,label = "GOOD") #�����ݼ��п�xy����
#�Զ����ӱ�ǩ
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(label = namesvar), size = 4)
#��ǩλ���޸�
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(label = namesvar), size = 4, vjust = 0) #vjust=0Ϊ��ǩ�ı��Ļ��߻������ݵ���룬
                                                        #vjust=1Ϊ��ǩ�ı��Ķ����������ݵ����
#��ǩ��λ������y���ƶ�
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(y=b + .1, label = namesvar), size = 4, vjust = 0)
#��ǩ��λ������x���ƶ�
ggplot(data, aes(x = a,y = b)) + geom_point() + 
  geom_text(aes(x=a + .1, label = namesvar), size = 4, hjust = 0) #hjust=0����룬hjust=1�Ҷ���
#�Զ����������ı�ǩ
#1 ���ȸ���һ������
#2 ��%in%�ҳ�ϣ���������ı�ǩ�������߼�����
#3 ȡ���ǩ������������ֵΪNA
ggplot(data, aes(x = a,y = b)) + 
  geom_point() + 
  geom_text(aes(x=a + .1, label = NEW_namesvar), size = 4, hjust = 0) + #NEW_namesvarΪ�µı�ǩ����
  xlim(2000,10000)
##��������ͼ
#ӳ��뾶
ggplot(data, aes(x = a, y = b, size = GDP)) + #����С����ӳ���size����
  geom_point(shape = 21, colour = "black", fill = "cornsilk")
#ӳ�����
ggplot(data, aes(x = a, y = b, size = GDP)) + #����С����ӳ���size����
  geom_point(shape = 21, colour = "black", fill = "cornsilk") + 
  scale_size_area(max_size = 15) 
##����ɢ��ͼ����
pairs(names[,2:5]) #2��5��

###########��������ͼ
plot(A$a,A$b,type = "l")
points(A$a,A$b) #�����ݵ�
lines(A$a,b,col("")) #������
qplot(A$a,A$b,geom = "line")
qplot(a,b, data = A,geom = "line")
ggplot(datas,aes(x=a,y=b))+geom_line()
qplot(a,b,data = S,geom = c("line","point")) # �������ݵ�
ggplot(datas,aes(x=a,y=b,group = 1))+geom_line() #xΪ����ʱ�������趨group
ggplot(datas,aes(x=a,y=b))+geom_line()+ylim(0,max(datas$b)) #����Y������
ggplot(datas,aes(x=a,y=b))+geom_line()+expand_limits(y = 0)
##������ͼ�������ݱ��
ggplot(datas,aes(x=a,y=b))+geom_line()+geom_point() # �������ݵ�
##���ƶ�������ͼ�ͱ��
ggplot(datas,aes(x=a,y=b, colour = groupvar))+geom_line() #��������ɫ
ggplot(datas,aes(x=a,y=b, linetype = groupvar))+geom_line() #������������
ggplot(datas,aes(x=factor(a),y=b, colour = groupvar,group = groupvar))+geom_line() #ת��Ϊ������
ggplot(datas,aes(x=a,y=b, shape = groupvar))+geom_line()+geom_point(size=4) #��Ӧ��ͬ���ε�����ͼ
ggplot(datas,aes(x=a,y=b, fill = groupvar))+geom_line()+geom_point(size=4,shape=21) #��Ӧ��ͬ��ɫ������ͼ
ggplot(datas,aes(x=a,y=b, shape = groupvar))+
  geom_line(position = position_dodge(0.2))+ #�����ӵ������ƶ�0.2
  geom_point(position = position_dodge(0.2),size=4) #�����λ�������ƶ�0.2
##�޸�������ʽ
#ͨ������linetype��size��colour�������Էֱ��޸����ߵ����Ρ��߿�����ɫ
ggplot(datas,aes(x=a,y=b))+geom_line(linetype = "", size=1,colour = "blue") 
ggplot(datas,aes(x=a,y=b,group = groupvar))+
  geom_line( size=1,colour = "blue") #���鵥һ��ɫ�Ϳ��ȵ�����
ggplot(datas,aes(x=a,y=b,colour = groupvar))+
  geom_line(linetype = "")+
  geom_point(shape = 22,size = 3,fill = white) #��ɫ��ͬ�����������ݱ��
##�޸����ݱ����ʽ
ggplot(datas,aes(x=a,y=b))+
  geom_line()+
  geom_point(size=4,shape = 22,colour="",fill="") #�Զ������ݱ�ǵĴ�С����״����ɫ�����ɫ
ggplot(datas,aes(x=a,y=b,fill=groupvar))+
  geom_line(position =position_dodge(0.2) )+
  geom_point(size=4,shape = 22,position_dodge(0.2))+ #�Զ������ݱ�ǵĴ�С����״����ɫ�����ɫ
  sacle_fill_maual(values = c("black","white")) #�����ɫ����Ϊ�ڰ���ɫ����΢�����ݱ��
##�������ͼ
#����geom_area()���Ի������ͼ
ggplot(datanames,aes(x = a,y = b))+geom_area()
ggplot(datanames,aes(x = a,y = b))+
  geom_area(colour="black",fill="blue",alpha = 0.2) #alpha=0.2��ʾ͸����Ϊ80%
ggplot(datanames,aes(x = a,y = b))+
  geom_area(fill="blue",alpha = 0.2)+
  geom_line() #���Ƕ��������ͼ���Ȼ��Ʋ����߿��ߵ����ͼ����������ͼ�㣬����geom_line���ƹ켣��
##���ƶѻ����ͼ
#����geom_area()����ӳ��һ�������ͱ��������ɫ
ggplot(datanames,aes(x = a,y = b,fill=factorvar)) + geom_area()
ggplot(datanames,aes(x = a,y = b,fill=factorvar)) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues",breaks=rev(levels(datanames$factorvar))) #��ת����з�
ggplot(datanames,aes(x = a,y = b,fill=factorvar,order=desc(factorvar))) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") #��ת�ѻ�˳��Ķѻ����ͼ
ggplot(datanames,aes(x = a,y = b,fill=factorvar,order=desc(factorvar))) +
  geom_area(colour=NA, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") +
  geom_line(position = "stack",size = 0.2) #û�����ұ߿��ߵĶѻ����ͼ���ȡ����󡭡�
##���ưٷֱȶѻ�ͼ
ggplot(datanames,aes(x = a,y = percent,fill=factorvar)) +
  geom_area(colour="black",size = 0.2, alpha = 0.4) +
  scale_fill_brewer(palette = "Blues",breaks=rev(levels(datanames$factorvar)))
##������������
ggplot(datanames,aes(x = a,y = b))+
  geom_ribbon(aes(ymin = b-��,ymax = b+��),alpha = 0.2) #����Ӱ�����ʾ�����������ͼ
ggplot(datanames,aes(x = a,y = b))+
  geom_line(aes(y=b-��),colour = "grey50", linetype = "dotted")+
  geom_line(aes(y=b+��),colour = "grey50", linetype = "dotted")+
  geom_line()  #�����߱�ʾ�����������ͼ

######��������ͼ
barplot(A$x, names.agr = A$y) #��һ�������趨���εĸ߶ȣ��ڶ��������趨ÿ�����ζ�Ӧ�ı�ǩ
barplot(table(S$a)) #��ǩΪƵ��ʱ
qplot(factor(A$X),A$Y,geom = "bar", stat = "identity")
qplot(X,Y,data = A, geom = "bar", stat = "identity")
ggplot(data, aes(x = X, y = Y)) + geom_bar(stat = "identity")
qplot(factor(x),data = A) #Ƶ������ͼ
ggplot(data, aes(x = factor(x))) + geom_bar()
######ֱ��ͼ
hist(A$x,breaks = 10) #���Ϊ10
qplot(x,data = A,binwidth = 4)
ggplot(data, aes(x=X)) + geom_histogram(binwidth = 4)
######����ͼ
plot(A$x,A$y) #��xΪ�����ͱ���ʱ��Ĭ�ϻ�������ͼ
boxplot(a~b, data = A) #bΪ���ͣ�aΪy��
boxplot(a~b + c, data = A) #����b��c�Ľ�����
qplot(A$a,A$b,geom = "boxplot")
qplot(a,b,data = A) #aΪ����
ggplot(names,aes(x=a,y=b))+geom_boxplot()
######���ƺ���ͼ��
curve(x^3-5*3,from = -4,to = 4)
qplot(c(0,20),fun=mtfun,stat = "function",geom = "line")

##################�������ݷֲ�
##���Ƽ�ֱ��ͼ
ggplot(datanames,aes(x = a)) + geom_histogram() #Ĭ������·�30��
#�趨���
ggplot(datanames,aes(x = a)) + 
  geom_histogram(binwidth = 5, fill = "white",colour = "black")
#�з�Ϊ15��
binsize <- diff(range(datanames$a)) / 15
ggplot(datanames,aes(x = a)) + 
  geom_histogram(binwidth = binsize, fill = "white",colour = "black")
##���ڷ������ݻ��Ʒ���ֱ��ͼ
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ .) #groupvarΪ�������
#�޸ķ����ǩ
library(plyr)
datanames$groupvar <- revalue(datanames$groupvar,c("o" = "F","1" = "M"))
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ .)
#�����趨��������y����
ggplot(datanames,aes(x = a)) + 
  geom_histogram(fill = "white",colour = "black") +
  facet_grid(groupvar ~ ., scales = "free")
#ͬһͼ�ڵĶ���ֱ��ͼ
ggplot(datanames,aes(x = a, fill = groupvar)) + 
  geom_histogram(position = "identity", alpha = 0.4)
##�����ܶ�����
ggplot(datanames,aes(x = a)) + geom_density()
#������ͼ������͵ײ����߶�
ggplot(datanames,aes(x = a)) + geom_line(stat = "density") +
  expand_limits(y = 0) 
#�����ɫ
ggplot(datanames,aes(x = a)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  xlim(35,105)
#���ɫ�޵ײ�ʵ��
ggplot(datanames,aes(x = a)) + 
  geom_density(fill = "blue", colour = NA, alpha = 0.2) +
  geom_line(stat = "density") +
  xlim(35,105) #����x��ķ�Χ
#ֱ��ͼ���ܶȺ�����ͬһ��ͼ
ggplot(datanames,aes(x = a, y =..density..)) + #ʹֱ��ͼ���ܶȺ����Ŀ̶�һ��
  geom_histogram(fill = "cornsilk", colour = "grey60", alpha = 0.2) +
  geom_density() +
  xlim(35,105) 
##���ڷ������ݻ��Ʒ����ܶ����� ��������������ַ�����������
ggplot(datanames,aes(x = a,colour = groupvar)) + geom_density() #��ͬ��ɫ����
ggplot(datanames,aes(x = a,fill = groupvar)) + geom_density(alpha = 0.3) #��ͬ�����ɫ
#����
ggplot(datanames,aes(x = a)) + geom_density() + facet_grid(groupvar ~ .) #ע���޸ı�ǩ
#ֱ��ͼ���ܶȺ�����ͬһ��ͼ�ķ���
##Ƶ�����ͼ
ggplot(datanames,aes(x = a)) + geom_freqpoly()
#���ƴ���
ggplot(datanames,aes(x = a)) + geom_freqpoly(binwidth = 4)
#��������
##���ƻ�������ͼ
ggplot(datanames,aes(x=factor(a),y = b)) + geom_boxplot()
#���Ƶ�����������ͼ
ggplot(datanames,aes(x=1,y = b)) + geom_boxplot() + 
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank()) #���Ƶ������ݵ�����ͼ�����x����ӳ��һ���ض���ȡֵ
#������ͼ���Ӳۿ�
#������ͼ���Ӿ�ֵ
##����С����ͼ
##����Wilkinson��ͼ
##���ڷ������ݻ��Ʒ����ͼ
##���ƶ�ά���ݵ��ܶ�ͼ
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d()
#������ɫ
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d(aes(colour = ..level..))
#���ɫ
ggplot(datanames,aes(x = a, y = b)) + geom_point() +
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)
#���ɫ�����ݵ�
####################ע��
##��������ע��
p + annotate("text",x = , y = , label = "GGG") #֪��������
#ָ���ı�����
p + annotate("text",x = , y = , label = "GGG", family = "serif",
             fontface = "italic", colour = "darkred", size = 3)  #����������ɫ��С
#����������������͵ģ�����ʹ��Inf��-Inf�ڻ�ͼ����ı�Ե�����ı�ע��
##��ע����ʹ����ѧ����ʽ
p + annotate("text",x = , y = , parse = TRUE, label = "XXX") 
#�ڳ����ı��������ʽ�У�ֻ��Ҫ˫������ʹ�õ����ű�����ı��Ĳ���
#Ҫ��ʾ�������ڵı�������Ҫ������֮���һ��*
##����ֱ��
p + geom_hline(yintercept = 60) #����
p + geom_vline(xintercept = 60) #����
p + geom_abline(intercept = 60, slope = 1.75) #б��
#����ÿ���ֵ��
#x��Ϊ��ɢ����ʱ�����ӵڼ������ӵ��ߣ���дxintercept=��
##�����߶κͼ�ͷ
p + annotate("segment", x= ��ʼ, xend =����, y= ��ʼ, yend =����)
#���Ӽ�ͷ
p + annotate("segment", x= ��ʼ, xend =����, y= ��ʼ, yend =����,
             arrow = arrow(ends = "both",angel = 90, length = unit(.2,"cm")))
##���Ӿ�����Ӱ
p + annotate("rect", xmin = ��ʼ,xmax = ����, ymin = ��ʼ, ymax = ����, alpha = .1,
             fill = "blue")
##����ĳһ��Ԫ��
##���������
##�������������ע��
##############������
##����x��y��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + coord_flip()
##����ֵ��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  ylim(0,max(datanames$b))
#��չֵ��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  expand_limits(y = 0)
##��תһ��������������
ggplot(datanames,aes(x = a, y = b)) + geom_point() + scale_y_reverse(limits = c(8,0))
#ָ��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + ylim(6.5, 3.5)
##�޸�������������ϵ���Ŀ
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() +
  scale_x_discrete(limits = c("c", "b", "a"))
#��ת��Ŀ˳��
##����x���y������ű���
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() #1��1����
#ָ��λ�÷��ÿ̶���
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() +
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 30))
#ָ�����������ű���
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed(ratio = 1/2) + #x/y = 1/2
  scale_y_continuous(breaks = seq(0, 420, 30))+
  scale_x_continuous(breaks = seq(0, 420, 30))
##���ÿ̶���λ��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  coord_fixed() +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) #��ʾ0-3�Ŀ̶�
#����x�ᣨ�����ͣ�
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  scale_x_continuous(limits = c("a", "b"), breaks = "b") #ֻ��ʾb
##�Ƴ��̶��ߺͱ�ǩ
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.text.y = element_blank()) #�Ƴ��̶ȱ�ǩ
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) #�Ƴ��̶Ⱥͱ�ǩ
#�Ƴ��̶��ߡ��̶ȱ�ǩ��������
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  scale_y_continuous(breaks = NULL) #����������������Ч
##�޸Ŀ̶ȱ�ǩ���ı�
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  scale_y_continuous(breaks = c(1,2,3), #��y���1,2,3�ĳ�a,b,c
                     labels = c("a", "b", "c"))
#��ǩ��ĳ����ʽ���棨�߶ȣ�ʱ�䣩
##�޸Ŀ̶ȱ�ǩ�����
ggplot(datanames,aes(x = a, y = b)) + boxplot() + 
  scale_x_discrete(breaks = c("a", "b", "c"), 
                  labels = c("A", "B", "C")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) #���ı���ת90��
#�����ı�����(��С����ʽ������)
ggplot(datanames,aes(x = a, y = b)) + boxplot() + 
  scale_x_discrete(breaks = c("a", "b", "c"), 
                   labels = c("A", "B", "C")) +
  theme(axis.text.x = element_text(family = "Times", face = "italic",
                                   colour = "darkred", size = rel(0.9))) #rel(0.9)��ǰ��С�����0.9��
##�޸��������ǩ���ı�
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  xlab("A") + ylab("B\nC") #����
##�Ƴ��������ǩ
ggplot(datanames,aes(x = a, y = b)) + geom_boxplot() + 
  theme(axis.title.x = element_blank())
##�޸��������ǩ�����(y��ͬ��)
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  theme(axis.title.x = element_text(angle = 90, face = "italic", colour = "darkred", size = 14))
##����������ʾֱ��
ggplot(datanames,aes(x = a, y = b)) + geom_point() + 
  theme(axis.line = element_line(colour = "black"))
##ʹ�ö���������
##����������ʹ������
ggplot(datanames,aes(x = data, y = b)) + geom_line() 
#ʹ��ָ���ָ��
ggplot(datanames,aes(x = data, y = b)) + geom_line() +
  scale_x_date(seq(as.Date("1993-09-01"), as.Date(2018-01-17), by = "2 month")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) #��ת�ı���ǩ
#ָ����ǩ��ʽ
ggplot(datanames,aes(x = data, y = b)) + geom_line() +
  scale_x_date(seq(as.Date("1993-09-01"), as.Date(2018-01-17), by = "2 month"),
               labels = date_format("%Y %b")) + #p171��ʱ���ʽ����
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
##����������ʹ�����ʱ��
####################����ͼ�ε��������
##����ͼ�α���
p + ggtitle("X\nX")
#��ͼƬ�ڲ�
p + ggtitle("X\nX") + theme(plot.title  = element_text(vjust = -2.5))
##�޸��ı����
#������Ŀ�����⡢�������ǩ��������̶��ߣ�
p + ggtitle("X\nX") +
  theme(plot.title = element_text(size = rel(1.5), lineheight = .9), family = "Times",
        fontface = "bold.italic", colour = "red")
#�ı����϶���
p + annotate("text", x = 15, y = 53, label = "Some text", size = 7, family = "Times",
             fontface = "bold.italic", colour = "red")
##theme()�п����ı���۵�������Ŀ
##����Ԫ�غ��ı����ζ�����ı����� p177
##ʹ������
p + theme_grey() #��ɫ����
p + theme_bw()   #��ɫ����
#�޸������Ԫ�أ������Ӧ��element_xx�������Ӻ���(P182)ҳ
#�Զ�������
##����������
p + theme(panel.grid.major = element_blank(), #��������panel.grid.major.x/y ���ݿ���
          panel.grid.minor = element_blank()) #��������panel.grid.minor.x/y ���ݿ���
##############ͼ��
#�Ƴ�ͼ��
p + guides(fill = FALSE)
p + scale_fill_discrete(guide = FALSE)
p + theme(legend.position = "none")
#�޸�ͼ����λ��
p + theme(legend.position = "top") #top\left\right\bottom
p + theme(legend.position = c(1,0)) # 11 10 01 00
#���в�͸������������ߵ�ͼ��
p + theme(legend.position = c(1,0)) +
  theme(legend.background = element_rect(fill = "white", colour = "black"))
#��������������ߵ�ͼ��
p + theme(legend.position = c(1,0)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank())
##�޸�ͼ����Ŀ��˳��
p + scale_fill_discrete(limits = c("C","B","A"))
##��תͼ����Ŀ��˳��
p + guides(fill = duide_legend(reverse = TRUE))
##�޸�ͼ������
p + labs(fill = "XXX")
p + scale_fill_discrete(name = "XXX")
#����ͼ��
p + labs(shape = "AAA", colour = "BBB")
##�޸�ͼ�������
p + theme(legend.title = element_text(face = "", family = "", 
                                      colour = "", size = ))
##�Ƴ�ͼ������
p + guides(fill = guide_legend(title = NULL))
##�޸�ͼ����ǩ
p + scale_fill_discrete(labels = c("a", "b", "c"))
#����ͼ��
p + scale_shape_discrete(labels = c("a", "b", "c")) +
    scale_colour_discrete(labels = c("A", "B", "C"))
##�޸�ͼ����ǩ�����
p + guides(fill = guide_legend(label.theme = 
                                 element_text(face = "", family = "", 
                                              colour = "", size = )))
##ʹ�ú������ı��ı�ǩ��ע���޸ļ�ࣩ
p + scale_fill_discrete(labels = c("a\nbb", "b\nbbb")) +
  theme(legend.text = element_text(lineheight = .8), 
        legend.key.heigh = unit(1, "cm"))

#######################����
p + facet_grid(groupvar ~ .) #����
p + facet_grid(. ~ groupvar) #����
p + facet_grid(groupxvar ~ groupyvar) #ͬʱ
p + facet_wrap( ~ groupvar,nrow = 3) #�涨����
#�ڲ�ͬ��������ʹ�÷���
#�޸ķ�����ı���ǩ
#�޸ķ����ǩ�ͱ�������
######################��ɫ
#����ɢ�ͱ���ʹ���Զ����ɫ��
#ʹ��ɫä�Ѻõĵ�ɫ��
#�������ͱ���ʹ���Զ����ɫ��
#�趨��Ӱ��ɫ
#####################����ͼ��
##������ؾ���ͼ
##���ƺ�������
##�������߼���Ӱ
##������άɢ��ͼ
##���Ʊ�ͼ
a <- table(data$aa) #ͳ��Ƶ��
pie(a) 
pie(c(99, 18, 120), labels = c("A", "B", "C")) #���ӱ�ǩ
####################���Ƶ�ͼ
library(ggplot2)
library(maps)
states_map <-map_data("state")
ggplot(states_map,aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
#�����ѡ��ͼ
world_map <- map_data("world")
sort(unique(world_map$region))
#�����ͼ
library(maps)
map("world", fill = TRUE, col = rainbow(200),
    ylim = c(-60, 90), mar = c(0, 0, 0, 0))
title("�����ͼ")
#��ȡ�й���ͼ
library(maps)
library(mapdata)
map("china") #������
library(sp)
library(maptools)
x= readShapePoly('bou2_4p.shp') 










