data<- getSymbols("300170.sz",from=Sys.Date()-30,to=Sys.Date()-1,auto.assign = FALSE)#从雅虎导入数据
dimnames(data) <- list(c(1:4),c("Open","High","Low","Close","Volume","Adjusted"))#设定净值列名
index(data) <- as.POSIXct(index(data))#将时间序列的时间类型从"Date" 转成 "POSIXct" "POSIXt"
plot.xts(data[,1:4], type = "l")

cars <- c(1, 3, 6, 4, 9)
plot(cars)
plot(cars, type="l", col="blue")
title(main="Autos", col.main="red", font.main=4) 
box()
text(axTicks(3), -0.8, srt=45, adj=0.5,labels=c("Mon", "Tue", "Wed", "Thu", "Fri"),xpd=T, cex=0.8)
text(x=c(1,2,3,4,5),y=4, srt=90, adj=0.5,labels=c("Mon", "Tue", "Wed", "Thu", "Fri"),xpd=T, cex=0.8)

dim1 <- dimnames(sample_matrix)[[1]]
dim2 <- c("c","B","C","D")
dimnames(sample_matrix)[[2]] <- dim2

dimnames(sample_matrix)

