data_2 <- read.table("pred2.dat", header=FALSE)
label_2 <- read.table("resp2.dat", header=FALSE)

sigma2= .075;
row2= 500
col2 = 500

readData <-function(){
  X2 <- as.matrix(data_2)
  Y2 <- as.matrix(label_2)
  X2_fHalf <<- X2[1:500,]
  X2_sHalf <<- X2[501:1000,]
  Y2_fHalf <<- Y2[1:500,]
  Y2_sHalf <<- Y2[501:1000,]
}

trainLRforRidge <- function(X,Y,l,im){
  w_hat = solve((t(X) %*% X) + (l * im) ) %*% t(X) %*% Y
  return(w_hat)
}
testLR <- function(X,w){
  y_hat = X %*% w
  return(y_hat)
}

calculateSSE <- function(Y,y_hat){
  SSE = sum((Y - y_hat)^2)  
  return(SSE)
}

RidgeRegression <- function(){
  readData()
  print(paste("For dataset",2))
  
  lamda <<- c(1,2,3,4,5,6,7,8,9,10,20,50)
  #lamda <<- c(20)
  identityMatrix <<-diag(col2)
  sseValues <<-c(0)
  for (val in lamda){
    W_hat = trainLRforRidge(X2_fHalf,Y2_fHalf,val,identityMatrix)
    Y_hat = testLR(X2_sHalf,W_hat)
    SSE = calculateSSE(Y2_sHalf,Y_hat)
    print(paste("Lambda:",val, "SSE:",SSE))
    #print(SSE)
  }
}
RidgeRegression()
# We get the best performance with the ridge regression parameter with lambda 7 
