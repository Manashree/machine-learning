# Fisher’s famous iris data set measures sepal width, petal width, sepal length, and petal length for three different
# types of iris: Virginica, Versicolor, and Setosa. This problem requires you to build a Gaussian classifier for the data.

BayesModel <- function(input,ifTest=FALSE){
  if(ifTest == FALSE){
    V <<- input[input[,5] == "versicolor",1:4]
    mv_v <<- as.matrix(colMeans(V));
    sv_v <<- as.matrix(cov(V))
    
    S <<- input[input[,5] == "setosa",1:4]
    mv_s <<- as.matrix(colMeans(S));
    sv_s <<- as.matrix(cov(S))
    
    R <<- input[input[,5] == "virginica",1:4]
    mv_r <<- as.matrix(colMeans(R));
    sv_r <<- as.matrix(cov(R))
    
    denom_s <<- (((2*pi)^2)*sqrt(det(sv_s)))^-1
    denom_v <<- (((2*pi)^2)*sqrt(det(sv_v)))^-1
    denom_r <<- (((2*pi)^2)*sqrt(det(sv_r)))^-1
    
    inv_s <<- solve(sv_s) #inverse of Cov of setosa
    inv_v <<- solve(sv_v) #inverse of Cov of versicolor
    inv_r <<- solve(sv_r) #inverse of Cov of virginica
    categoricalLabels <<- input$Species
    input$Species <- NULL
  }else{
    input$predicted = NA
    for(i in 1:length(input$Sepal.Length)){
      ##for setosa
      tr_s = as.matrix(input[i,1:4]-t(mv_s))
      temp_s = tr_s%*%inv_s%*%t(tr_s)
      p_cs = denom_s*exp(-0.5*temp_s) ##P(X|class=setosa)
      p_cs
      ##for versicolor
      tr_v = as.matrix(input[i,1:4]-t(mv_v))
      temp_v = tr_v%*%inv_v%*%t(tr_v)
      p_cv = denom_v*exp(-0.5*temp_v) ##P(X|class=versicolor)
      p_cv
      ##for virginica
      tr_r = as.matrix(input[i,1:4]-t(mv_r))
      temp_r = tr_r%*%inv_r%*%t(tr_r)
      p_cr = denom_r*exp(-0.5*temp_r) ##P(X|class=virginica)
      p_cr
      if(p_cs > p_cv && p_cs > p_cr)
        input[i,5] = "setosa"
      else if(p_cv > p_cs && p_cv > p_cr)
        input[i,5] = "versicolor"
      else if(p_cr > p_cs && p_cr > p_cv)
        input[i,5] = "virginica"
    }
  }
  return(input)
}
plotError <- function(plotData){
  plotData$errorRate <- 0
  plotData$errorRate[plotData$actual_labels !=plotData$predicted] <- 1
  plot(plotData$Sepal.Length, plotData$Sepal.Width,xlab="atr1",ylab="atr2", col =
         ifelse(plotData$errorRate == 1,'red','green'), pch = 15, main="Test Data" )
}
trainTestSplit <- function(ip){
  train_data <-c(25,5)
  test_data <-c(25,5)
  S <<- ip[ip[,5] == "setosa",1:5]
  train_data <- S[1:25,]
  test_data <- S[26:50,]
  V <<- ip[ip[,5] == "versicolor",1:5]
  train_data <- rbind(train_data,V[1:25,])
  test_data <- rbind(test_data,V[26:50,])
  R <<- ip[ip[,5] == "virginica",1:5]
  train_data <<- rbind(train_data,R[1:25,])
  test_data <<- rbind(test_data,R[26:50,])
}
data(iris)
trainTestSplit(iris)
testing_labels <- as.matrix(test_data[,5])
test_data$Species <- NULL
BayesModel(train_data)
result <- BayesModel(test_data,ifTest = TRUE)
result$actual_labels <- testing_labels
plotError(result)