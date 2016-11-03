#首先给定一个模型，根据xvar和yvar预测zvar
#默认为变量x和y的范围，
predictgrid <- function(model, xvar, yvar, zvar, res = 16, type = NULL) {
  # 计算预测变量的范围，下面的代码对lm，glm以及其他建模方法都适用
  # 但针对其他模型方法时，可能需要适当调整.
  xrange <- range(model$model[xvar])
  yrange <- range(model$model[yvar])
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res))
  names(newdata) <- c(xvar, yvar)
  newdata[zvar] <- predict(model, newdata = newdata, type = type)
  newdata
}
# 将长数据框中的x,y,z转化为列表
# 其中x,y为行列值，z为矩阵.
df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
  if (is.null(xvar)) xvar <- names(p)[1]
  if (is.null(yvar)) yvar <- names(p)[2]
  if (is.null(zvar)) zvar <- names(p)[3]
  x <- unique(p[xvar])
  y <- unique(p[yvar])
  z <- matrix(p[zvar], nrow = length(y), ncol = length(x))
  m <- list(x, y, z)
  names(m) <- c(xvar, yvar, zvar)
  m
}
# 交错出现两个向量的元素
interleave <- function(v1, v2) as.vector(rbind(v1,v2))
#利用这些定义好的功能函数，我们可以对数据生成线性模型，然后利用surface3d()函数在原散点图上添加网格式的预测图
library(rgl)
# 复制数据集
m <- mtcars
# 生成线性模型
mod <- lm(mpg ~ wt + disp + wt:disp, data = m)
# 根据wt和disp，得到mpg的预测值
m$pred_mpg <- predict(mod)
# 根据wt和disp的网格，得到mpg的预测值
mpgrid_df <- predictgrid(mod, "wt", "disp", "mpg")
mpgrid_list <- df2mat(mpgrid_df)
# 根据数据点绘图
plot3d(m$wt, m$disp, m$mpg, type="s", size=0.5, lit=FALSE)
#添加预测点（较小）
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type="s", size=0.5, lit=FALSE)
# 添加表示误差的线段
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col="red")
# 添加预测点网格
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front="lines", back="lines")
movie3d(spin3d(axis=c(0,0,1),rpm=3),duration=10,fps=50)