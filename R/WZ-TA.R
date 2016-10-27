WZTA <- function(data){
  SNET <- data[,"SNET"]#净值序列
  #======开始画图=========
  jpeg()
  par(family='SimHei')#中文字体
  #画净值曲线
  #plot.xts(SNET,main = "净值曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
  #添加图例
  #legend("topleft",inset=c(0,0),legend = c("Denisovan","Neandertal","Papuan","YRI","CEU","JPT","CHB"),pch=16)
  cmaxx <- SNET/cummax(SNET)-1
  data<-merge.xts(data,cmaxx)
  #plot.xts(cmaxx,main = "回撤曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
  plot.xts(data[,2:3],col=c("red","blue"),lwd = 2,main = "回撤分析")
  #产品月度收益率
  barplot(monthlyReturn(SNET),main="产品月度收益率",col="blue")
  #产品季度收益率
  barplot(quarterlyReturn(SNET),main="产品季度收益率",col="blue")
  dev.off()
}

setwd("/Users/jason-mini/R-Workspaces/WZxts/")
data<- as.xts(read.zoo(file="R/data.csv",sep = ";", format = "%Y/%m/%d"))#从csv导入数据
dimnames(data) <- list(c(1:2),c("NET","SNET"))#设定净值列名
index(data) <- as.POSIXct(index(data))#将时间序列的时间类型从"Date" 转成 "POSIXct" "POSIXt"
data<- apply.weekly(data,last)#转为周净值
#data<- getSymbols("300170.sz",from=Sys.Date() -90,auto.assign = FALSE)#从雅虎导入数据

WZTA(data)


