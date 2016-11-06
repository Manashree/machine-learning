# The famous iris data from Fisher are classified with an logistic regression model.
# We look at the problem as a two-class problem trying to distinguish the setosa iris
# from the others. Simple gradient descent with a small step size is used.

data(iris);
class=as.integer(iris[,5]=="setosa");
w=rep(0,4);
n=dim(iris)[1];

for(iter in 1:20){
  ll=0;
  for(i in 1:n){
    x=as.vector(iris[i,1:4]);
    if(class[i]==1){
      ll=ll+log(1/(1+exp(-sum(w*x))));
    }else{
      ll=ll+log(1-(1/(1+exp(-sum(w*x)))));
    }
  }
  print(ll);
  grad=rep(0,4);
  for(i in 1:n){
    x=as.vector(iris[i,1:4]);
    grad=grad+(class[i]*(1/(1+exp(-sum(w*x)))))*x
  }
  w=w+.001*grad;#stepsize=1
}

classhat=rep(0,n);
for(i in 1:n){
  x=as.vector(iris[i,1:4]);
  classhat[i]=(1/(1+exp(-sum(w*x)))>.5)
}
classhat=as.integer(classhat)
classhat