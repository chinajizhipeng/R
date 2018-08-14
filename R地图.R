###############山东省地图绘制
#1
install.packages("devtools")
install.packages("curl")
library(devtools)  
library(curl)  
install_github('lchiffon/REmap')
library(REmap)
data = data.frame(country = mapNames("山东"),  
                    +value = 5*sample(17)+200)  
out = remapC(data,maptype = "shandong",color = 'skyblue',theme = get_theme("Sky"))  
plot(out) 
#2

##############中国地图绘制
#1
install.packages("devtools")
install.packages("curl")
library(devtools)  
library(curl)  
install_github('lchiffon/REmap')
library(REmap)
data = data.frame(country = mapNames("china"),  
                    +value = 5*sample(34)+200)  
out = remapC(data,maptype = "China",color = 'skyblue',theme = get_theme("Sky"))  
plot(out)  


##############世界地图绘制
#1
install.packages("devtools")
install.packages("curl")
library(devtools)  
library(curl)  
install_github('lchiffon/REmap')
library(REmap)

data = data.frame(country = mapNames("world"),  
                  value = 5*sample(178)+200)  #修改value即可
out = remapC(data,maptype = "world",color = 'skyblue',theme = get_theme("Sky"))  
plot(out)  