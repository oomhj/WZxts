data<- getSymbols("300170.sz",from=Sys.Date()-30,to=Sys.Date()-1,auto.assign = FALSE)#从雅虎导入数据
dimnames(data) <- list(c(1:4),c("Open","High","Low","Close","Volume","Adjusted"))#设定净值列名
index(data) <- as.POSIXct(index(data))#将时间序列的时间类型从"Date" 转成 "POSIXct" "POSIXt"
plot.xts(data[,1:4], type = "l")
