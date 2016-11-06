data_1 <- read.table("pred1.dat", header=FALSE)
label_1 <- read.table("resp1.dat", header=FALSE)
sigma2= .075;
row1 = 500
col1 = 50
row2= 500
col2 = 500
errors <- c()

readData <-function(){
  X1 <- as.matrix(data_1)
  Y1 <- as.matrix(label_1)
  X1_fHalf <<- X1[1:500,]
  X1_sHalf <<- X1[501:1000,]
  Y1_fHalf <<- Y1[1:500,]
  Y1_sHalf <<- Y1[501:1000,]
}

trainLR <- function(X,Y){
  w_hat = solve(t(X) %*% X) %*% t(X) %*% Y
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

calculateVariance <- function(Y,y_hat){
  errors <<- c(errors,Y - y_hat)
  VAR = var(errors)
  return(VAR)
}

calculateSigmaHat <- function(SSE,n_rows,n_cols){
  sigma_hat = SSE/(n_rows-n_cols)
  return(sigma_hat)
}

LinearModel <-function(){
  readData()
  W_hat = trainLR(X1_fHalf,Y1_fHalf)
  Y_hat = testLR(X1_sHalf,W_hat)
  SSE = calculateSSE(Y1_sHalf,Y_hat)
  VAR = calculateVariance(Y1_sHalf,Y_hat)
  sigmaHat1 = calculateSigmaHat(SSE,row1,col1)
  print(paste("SSE:",SSE,"Variance:",VAR," Sigma_Hat:",sigmaHat1))
}

LinearModel()


#Stepwise Regression
#Q4 For dataset 1 Variable Selection using Greedy Method

variableSelection <- function(){
  sse_vector <- c()
  arr <- c(0,1,2)
  indices <- seq(1,col1)
  setOfFeatureIndx <- c()
  
  for(v in arr) {
    minSSE <- Inf
    minSSEIndx <- -1
    
    for(i in indices){
      if(minSSEIndx == -1)
        setOfFeatureIndx <- c(i)
      else {
        setOfFeatureIndx <- c(sse_vector, i)
      }
      
      W_hat_1 = trainLR(as.matrix(X1_fHalf[,setOfFeatureIndx]),Y1_fHalf)
      Y_hat_1 = testLR(as.matrix(X1_fHalf[,setOfFeatureIndx]),W_hat_1)
      SSE_1 = calculateSSE(Y1_fHalf,Y_hat_1)
      if(minSSE > SSE_1) {
        minSSE <- SSE_1
        minSSEIndx <- i
      }
    }
    sse_vector <- c(sse_vector, minSSEIndx)
    print(paste("Selected predictor variable: ",minSSEIndx))
    indices <- indices[indices != minSSEIndx]
    print(paste("For feature index:",minSSEIndx," error is:",SSE_1))
  }
  return(sse_vector)
}

LinearModelUsingVariableSelection <-function(){
  setofFeatures = variableSelection()
  W_hat1 = trainLR(as.matrix(X1_fHalf[,setofFeatures]),Y1_fHalf)
  Y_hat1 = testLR(as.matrix(X1_sHalf[,setofFeatures]),W_hat1)
  SSE_VR = calculateSSE(Y1_sHalf,Y_hat1)
  print(paste("Stepwise Regression method; error is: ",SSE_VR))
}
LinearModelUsingVariableSelection()
