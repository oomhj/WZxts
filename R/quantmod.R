library("zoo")
library("xts")
library("xtsExtra")
library("TTR")
library("quantmod")
#library("tserise")
library("pryr")
#source("WZ-TA.R")
library("devtools")
data<- as.xts(read.zoo(file="R/data.csv",sep = ";", format = "%Y/%m/%d"))#从csv导入数据
dimnames(data) <- list(c(1:2),c("NET","SNET"))#设定净值列名
index(data) <- as.POSIXct(index(data))#将时间序列的时间类型从"Date" 转成 "POSIXct" "POSIXt"
data<- apply.weekly(data,last)#转为周净值
#data<- getSymbols("300170.sz",from=Sys.Date() -90,auto.assign = FALSE)#从雅虎导入数据

SNET <- data[,"SNET"]#净值序列

#======开始画图=========
bmp()
par(family='SimHei')#中文字体
#画净值曲线
plot.xts(SNET,main = "净值曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
#添加图例
#legend("topleft",inset=c(0,0),legend = c("Denisovan","Neandertal","Papuan","YRI","CEU","JPT","CHB"),pch=16)
#产品月度收益率
barplot(monthlyReturn(SNET),main="产品月度收益率",col="blue")
#产品季度收益率
barplot(quarterlyReturn(SNET),main="产品季度收益率",col="blue")

maxdrawdown(SNET)
cmaxx <- SNET/cummax(SNET)-1
data<-merge.xts(data,cmaxx)
plot.xts(cmaxx,main = "回撤曲线",col="red",lwd = 2, xlab = "x轴标签",  ylab = "y轴标签",type="l")
plot.xts(data[,2:3],col=c("red","blue"),lwd = 2,main = "回撤分析")
dev.off()
#======指标计算=========
measure=vector(mode="numeric",length=8);#存储结果
measure[1]= (as.numeric(last(SNET))/as.numeric(first(SNET)))*(52/nweeks(SNET))-1;#Annualized Yield 年化收益率
measure[2]=(52^0.5)*std(TS(2:end,1)./TS(1:end-1,1)-1);#Annualized STD

measure[3]=min(dra(:,2));#Maximum Drawdown
#measure(4)=(TS(end)/TS(1)-1)/measure(3);#Calmar Ratio
measure(5)=(measure(1)-0.03)/measure(2);#Annualized Sharpe Ratio
measure(6)=Maximum_Recovery_Period(TS);#Maximum Recovery Period
finder=find(TS(2:end)./TS(1:end-1)-1<0);
measure(7)=std(TS(finder+1,1)./TS(finder,1)-1); #下偏矩
measure(8)=(measure(1)-0.03)/(measure(7)*52^0.5); #索提诺比率

plot(TS,'r','linewidth',3)
#xlabel('交易日')
ylabel('净值')
get(gca, 'ylabel')
ylim([min(TS),max(TS)])

aa=max(TS);
bb=min(TS);
gap=(aa-bb)/12;
if measure(1)>0
pos=max(TS)-(1:8)*gap;
else
  pos=min(TS)+(7:-1:0)*gap;
end

text(length(TS)/30,pos(1),['平均收益率=',num2str(100*measure(1)),'%'],'FontSize',10)
text(length(TS)/30,pos(2),['收益率标准差=',num2str(100*measure(2)),'%'],'FontSize',10)
text(length(TS)/30,pos(3),['最大回撤=',num2str(measure(3)*100),'%'],'FontSize',10)
text(length(TS)/30,pos(4),['夏普比率=',num2str(measure(5))],'FontSize',10)
text(length(TS)/30,pos(5),['最大回撤=',num2str(measure(6))],'FontSize',10)
text(length(TS)/30,pos(6),['负向波动率=',num2str(100*measure(7)),'%'],'FontSize',10)
text(length(TS)/30,pos(7),['索比诺比率=',num2str(measure(8))],'FontSize',10)
%text(length(TS)/30,pos(4),['Calmar Ratio=',num2str(measure(4))],'FontSize',8)

% text(length(TS)/30,pos(1),['年化收益率=',num2str(100*measure(1)),'%'],'FontSize',8)
% text(length(TS)/30,pos(2),['年化波动率=',num2str(100*measure(2)),'%'],'FontSize',8)
% text(length(TS)/30,pos(3),['最大回撤=',num2str(measure(3)*100),'%'],'FontSize',8)
% text(length(TS)/30,pos(4),['卡玛比=',num2str(measure(4))],'FontSize',8)
% text(length(TS)/30,pos(5),['夏普比=',num2str(measure(5))],'FontSize',8)
% text(length(TS)/30,pos(6),['最大回撤周期=',num2str(measure(6))],'FontSize',8)
% text(length(TS)/30,pos(7),['负向波动率=',num2str(100*measure(7)),'%'],'FontSize',8)
% text(length(TS)/30,pos(8),['索提诺比率=',num2str(measure(8))],'FontSize',8)


N=floor(size(data,1)/4);
N1=[1,1+N,1+2*N,1+3*N,4*N];
ti=num2str(data(N1,1));
set(gca,'XTick',N1);
set(gca,'XTickLabel',ti);
title('\fontsize{12}综合业绩指标');
grid on