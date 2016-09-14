library(tree)
library(ISLR)
library(MASS)
attach(Auto)
Auto=Auto
set.seed(1)

train = sample (1: nrow(Auto), nrow(Auto)/2)

#All except name
tree.auto =tree(mpg~.-name,Auto,control=tree.control(196, mincut = 2, minsize = 4, mindev = 0.0001),subset =train)
summary (tree.auto)
plot(tree.auto)
text(tree.auto, pretty = 0)

#All except name and year
tree.auto1 =tree(mpg~.-name-year,Auto,control=tree.control(196, mincut = 2, minsize = 4, mindev = 0.0001),subset =train)
summary (tree.auto1)
plot(tree.auto1)
text(tree.auto1, pretty = 0)

#Cross validation test
cv.auto =cv.tree(tree.auto)
plot(cv.auto$size ,cv.auto$dev ,type='b')
tree.min = which.min(cv.auto$dev)
points(cv.auto$size[tree.min], cv.auto$dev[tree.min], col = "red", cex = 2, pch = 20)


#Prune tree
prune.auto=prune.tree(tree.auto, best=9)
summary(prune.auto)
plot(prune.auto)
text(prune.auto, pretty = 0)

#Mean squared error
yhat=predict (tree.auto ,newdata =Auto [-train ,])
auto.test=Auto [-train ,"mpg"]
plot(yhat ,auto.test)
abline (0,1)
mean((yhat -auto.test)^2)
