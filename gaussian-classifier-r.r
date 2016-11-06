data(iris);
f=4;

virg=iris[iris[,5]=="virginica",1:f]
mv=as.matrix(colMeans(virg));
Sv=as.matrix(cov(virg))
Cv=solve(Sv)
dv=det(Sv)

vers=iris[iris[,5]=="versicolor",1:f]
mr=as.matrix(colMeans(vers));
Sr=as.matrix(cov(vers))
Cr=solve(Sv)
dr=det(Sv)

seto=iris[iris[,5]=="setosa",1:f]
ms=as.matrix(colMeans(seto));
Ss=as.matrix(cov(seto))
Cs=solve(Sv)
ds=det(Sv)

n=dim(iris)[1];
plot(iris[,1],iris[,2],pch='');#blankplottosetaxes
points(virg[,1],virg[,2],pch='v')
points(seto[,1],seto[,2],pch='s')
points(vers[,1],vers[,2],pch='r')
l=rep(0,3);
names=c("setosa","versicolor","virginica")
class=rep(0,n)
for(i in 1:n){
  x=t(as.matrix(iris[i,1:f]));
  l[1]=.5*log(ds) - .5*t(x-ms) %*% Cs %*% (x-ms)
  l[2]=.5*log(dr) - .5*t(x-mr) %*% Cr %*% (x-mr)
  l[3]=.5*log(dv) - .5*t(x-mv) %*% Cv %*% (x-mv)
  #print(which.max(l))
  class[i]=names[which.max(l)]
}
plot(iris[,1],iris[,2],pch='');#blankplottosetaxes
points(seto[,1],seto[,2],pch='s',col=ifelse(class[1:50]==
                                              iris[1:50,5],'green','red'))
points(vers[,1],vers[,2],pch='r',col=ifelse(class
                                            [51:100]==iris[51:100,5],'green','red'))
points(virg[,1],virg[,2],pch='v',col=ifelse(class
                                            [101:150]==iris[101:150,5],'green','red'))

#Splitintotrainingandtestsets
training.rows=c(1:25,51:75,101:125)
training=iris[training.rows,]
test=iris[-training.rows,1:4]
test.classes.true=iris[-training.rows,5]

virg=training[training[,5]=="virginica",1:f]
mv=as.matrix(colMeans(virg));
Sv=as.matrix(cov(virg))
Cv=solve(Sv)
dv=det(Sv)

vers=training[training[,5]=="versicolor",1:f]
mr=as.matrix(colMeans(vers));
Sr=as.matrix(cov(vers))
Cr=solve(Sv)
dr=det(Sv)

seto=training[training[,5]=="setosa",1:f]
ms=as.matrix(colMeans(seto));
Ss=as.matrix(cov(seto))
Cs=solve(Sv)
ds=det(Sv)
l=rep(0,3);
n=dim(training)[1]
class=rep(0,n)

for(i in 1:n){
  x=t(as.matrix(test[i,1:f]));
  l[1]=.5*log(ds) - .5*t(x-ms) %*% Cs %*% (x-ms)
  l[2]=.5*log(dr) - .5*t(x-mr) %*% Cr %*% (x-mr)
  l[3]=.5*log(dv) - .5*t(x-mv) %*% Cv %*% (x-mv)
  class[i]=names[which.max(l)]
}
plot(iris[,1],iris[,2],pch='');#blankplottosetaxes
points(seto[,1],seto[,2],pch='s',col=ifelse(class[1:25]==
                                              test.classes.true[1:25],'green','red'))
points(vers[,1],vers[,2],pch='r',col=ifelse(class[26:50]==
                                              test.classes.true[26:50],'green','red'))
points(virg[,1],virg[,2],pch='v',col=ifelse(class[51:75]==
                                              test.classes.true[51:75],'green','red'))