library(leaps)
library(ISLR)
library(glmnet)
set.seed (1)

#1.a
x= rnorm(30)
ep=rnorm(30)

#1.b
y= 3- 2*x + 3*(x^2) + ep

#1.c
#i
grid=10^seq(10,-2,length=100)
xpoly=poly(x,7)
lasso.mod=glmnet(x=xpoly,y=y,alpha=1, lambda=grid)
plot(lasso.mod,xvar="lambda",label=TRUE, xlim=c(-5,5))
legend('topright', c("1","2","3","4","5","6","7") ,  bty='n', lty=1,col=c('black', 'red', 'green',' blue', 'light blue', 'pink','black'), cex=.75, lwd=c(2.5,2.5))


#ii
cv.lassomod <- cv.glmnet(xpoly,y=y,alpha=1)
plot(cv.lassomod)
best_lambda <- cv.lassomod$lambda.min
best_lambda

#iii
lasso.mod1=glmnet(xpoly, y=y, lambda = best_lambda, alpha=1)
coef(lasso.mod1)

#d
x1= rnorm(1000)
ep1=rnorm(1000)

y1= 3- 2*x1 + 3*(x1^2) + ep1
x1poly=poly(x1,7)

lasso.pred1=predict (lasso.mod1,s=best_lambda,newx=x1poly)
mean((lasso.pred1 -y1)^2)

