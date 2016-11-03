require(grDevices) 
x <- seq(-4,4,length=60);
y <- seq(-4,4,length=60);
f <- function(x, y) { r <- sin(x)*cos(y)}
z <- outer(x, y, f)
#x <- seq(-10, 10, length= 60)
#y <- x
#f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
#z <- outer(x, y, f)
#z[is.na(z)] <- 1
op <- par(bg = "white")

persp(x, y, z, theta = 30, phi = 30,expand = 0.5, col = rainbow(5000))
