library(leaps)
library(ISLR)
library(glmnet)
set.seed (1)

#2.a
x= rnorm(30)
ep=rnorm(30)
y= 3- 2*x + 3*(x^2) + ep
xpoly=poly(x,7)

x1= rnorm(1000)
ep1=rnorm(1000)
y1= 3- 2*x1 + 3*(x1^2) + ep1
x1poly=poly(x1,7)

grid =10^ seq (10,-2, length =100)

#i
ridge.mod=glmnet(x=xpoly,y=y,alpha=0, lambda = grid)
plot(ridge.mod,xvar="lambda", label=TRUE)
legend('topright', c("1","2","3","4","5","6","7") ,  bty='n', lty=1,col=c('black', 'red', 'green',' blue', 'light blue', 'pink','black'), cex=.75, lwd=c(2.5,2.5))

#ii
cv.ridgemod <- cv.glmnet(xpoly,y=y,alpha=0)
plot(cv.ridgemod)
best_lambda <- cv.ridgemod$lambda.min
best_lambda

#iii
ridge.pred=predict (ridge.mod ,s=best_lambda,newx=xpoly)
mean(( ridge.pred -y)^2)

rid.out=glmnet (xpoly,y,alpha =0,lambda = best_lambda)
ridge.coef=predict (rid.out ,type ="coefficients",s=best_lambda )
ridge.coef

#b
ridge.pred1=predict(rid.out,s=best_lambda,newx=poly(x1,7))
mean((ridge.pred1 -y1)^2)


#c
set.seed(1)
xl1<-rnorm(30)
el1<-rnorm(30)
yl1<-3-(2*xl1)+(3*(xl1^2))+el1
xlpoly<-poly(xl1,7)
ls.mod<-lm(yl1~xlpoly)

xl2<-rnorm(1000)
el2<-rnorm(1000)
xl2poly<-poly(xl2,7)
yl2<-3-(2*xl2)+(3*(xl2^2))+el2

ls.pred<-predict(ls.mod,newx=xl2poly)
mean((ls.pred-yl2)^2)

