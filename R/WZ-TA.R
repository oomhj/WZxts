library("zoo")
library("xts")
library("xtsExtra")
library("TTR")
library("quantmod")
library("RMySQL")
Queryfundids <- function(){
  
}
WZTA <- function(from,to,fundid){
  from <- as.Date(from,format='%Y-%m-%d')
  to <- as.Date(to,format='%Y-%m-%d')
  sql<-paste("select statistic_date,nav,added_nav,swanav from fund_nv_data where statistic_date > date('",from,"') and statistic_date <  date('",to,"') and fund_id = '",fundid,"'   order by statistic_date desc",sep = "")
  conn <- dbConnect(MySQL(), dbname = "product",host="121.40.18.150",port=3306, username="jr_wztz", password="511ee310e9540f8fbfd60eaf5a2eaed9576736d2")
  users = dbGetQuery(conn,sql)
  dbDisconnect(conn)

  data <- xts(users$nav,as.Date(users$statistic_date))
  rm(users)#清理内存
  dimnames(data) <- list(c(1),c("SNET"))#设定净值列名
  index(data) <- as.POSIXct(index(data))#将时间序列的时间类型从"Date" 转成 "POSIXct" "POSIXt"
  data<- apply.weekly(data,last)#转为周净值
  SNET <- data[,"SNET"]#净值序列
  #======开始画图=========
  #jpeg(filename =paste(fundid,"%03d.jpg"))
  par(family='STHeiti')#中文字体
  #画净值曲线
  #plot.xts(SNET,main = "净值曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
  #添加图例
  #legend("topleft",inset=c(0,0),legend = c("Denisovan","Neandertal","Papuan","YRI","CEU","JPT","CHB"),pch=16)
  cmaxx <- SNET/cummax(SNET)-1
  data<-merge.xts(data,cmaxx)
  #plot.xts(cmaxx,main = "回撤曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
  plot.xts(data[,1:2],col=c("red","blue"),lwd = 2,main = "回撤分析")
  #产品月度收益率
  barplot(monthlyReturn(SNET),main="产品月度收益率",col="blue")
  #产品季度收益率
  barplot(quarterlyReturn(SNET),main="产品季度收益率",col="blue")
  dev.off()
}
WZTA("2016-01-20","2016-08-01","JR077945")


