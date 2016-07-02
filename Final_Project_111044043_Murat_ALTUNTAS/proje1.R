
bankData <- read.csv("C:/Users/Murat/Desktop/ss/Veri Madenciliği/Veri madenciliği Proje/bank/bank.csv",sep=";", header = TRUE)

bankData[[2]] <- as.numeric(factor(bankData[[2]])) # discrit attributlarÄ± continuous yapar. 2,3,4,5,7,8,9,11,15
bankData[[3]] <- as.numeric(factor(bankData[[3]]))
bankData[[4]] <- as.numeric(factor(bankData[[4]]))
bankData[[5]] <- as.numeric(factor(bankData[[5]]))
bankData[[7]] <- as.numeric(factor(bankData[[7]]))
bankData[[8]] <- as.numeric(factor(bankData[[8]]))
bankData[[9]] <- as.numeric(factor(bankData[[9]]))
bankData[[11]] <- as.numeric(factor(bankData[[11]]))
bankData[[15]] <- as.numeric(factor(bankData[[15]]))

library(entropy)
library(gmodels)
library(data.tree)
library(party)
library(rpart)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

createConfusionMatrix <- function(x, y, size) {
  cf <- matrix(0 ,nrow=size, ncol=size)
  
  x <- as.integer(x)
  y <- as.integer(y)
  
  for(i in 1:length(x)){
    cf[x[i],y[i]] <- cf[x[i],y[i]] + 1
  }
  
  return (cf);
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

getInfoGain <- function(labelLeft, labelRight) {
  trainLabel <- c(labelLeft, labelRight)
  s1 <- entropy.empirical(table(trainLabel)/length(trainLabel), unit="log2")
  w2left  <- length(labelLeft)/length(trainLabel) 
  s2left  <- w2left  * entropy.empirical(table(labelLeft)/length(labelLeft), unit="log2")
  w2right <- length(labelRight)/length(trainLabel)
  s2right <- w2right * entropy.empirical(table(labelRight)/length(labelRight), unit="log2")
  ig <- (s1 - (s2left+s2right))
  if(is.na(ig))
    return (0)
  else
    return (ig)
}

createDecisionTree <- function(train, cname, prePrunEntropy) {
  #Get class number 
  cnum <- match(cname, colnames(train))
  
  #if my entrophy is 0, return
  ent <- entropy.empirical(table(train[cnum])/length(train[cnum]), unit="log2") 
  if(ent <= prePrunEntropy){
    return (Node$new(paste(Mode(train[[cnum]]),nrow(train),sep=" ")))
  }
    
  #find best label and treshold
  bestInfoGains <- c()
  bestInfoGainThresholds <- c()
  for(i in 1:ncol(train)){
    if(i != cnum) {
      infoGains <- c()
      stepSize  <- (max(train[i]) - min(train[i])) / (length(train[[i]])*2)
      threshold <- min(train[i])
      while(threshold < max(train[i])){
        #calculate information gain
        threshold <- threshold + stepSize
        labelLeft <-train[train[i] < threshold,cnum]
        labelRight <-train[train[i] >= threshold,cnum]
        infoGains<- c(infoGains, getInfoGain(labelLeft, labelRight))
      }
      if(stepSize != 0){
        bestInfoGains <- c(bestInfoGains, max(infoGains))
        bestInfoGainThresholds <- c(bestInfoGainThresholds, match(max(infoGains),infoGains) * stepSize + min(train[i]))
      }
      else {
        bestInfoGains <- c(bestInfoGains, 0)
        bestInfoGainThresholds <- c(bestInfoGainThresholds, 0)
      }
    }
    else{
      bestInfoGains <- c(bestInfoGains, 0)
      bestInfoGainThresholds <- c(bestInfoGainThresholds, 0)
    }
  }

  rootLabel <- colnames(train[match(max(bestInfoGains),bestInfoGains)])
  rootLabeltreshhold <- bestInfoGainThresholds[match(max(bestInfoGains),bestInfoGains)]
  treeRoot <- Node$new(paste(rootLabel,rootLabeltreshhold, sep=" " ))
  treeRoot$AddChildNode(createDecisionTree(train[train[rootLabel] <= rootLabeltreshhold,],
                                           cname, prePrunEntropy))
  treeRoot$AddChildNode(createDecisionTree(train[train[rootLabel] > rootLabeltreshhold,],
                                           cname, prePrunEntropy))
  return (treeRoot)
}

predictUsingDecisionTree <- function(tree, testData){
  
  resultLabels <- c()
  for(i in 1:nrow(testData)){
    tempTree <- tree
    while(!tempTree$isLeaf){
      nodeData <- strsplit(tempTree$name, split=" ")
      nodeLabel <- nodeData[[1]][1]
      nodeCompNum <- as.numeric(nodeData[[1]][2])
      if(testData[[nodeLabel]][i] <= nodeCompNum){
        tempTree <- tempTree$children[[1]]
      }
      else{
        tempTree <- tempTree$children[[2]]
      }
    }
    className <- strsplit(tempTree$name, split=" ")
    resultLabels <- c(resultLabels, className[[1]][1])
  }
  return (factor(resultLabels))
  
}

createDecisionTreeBagging <- function(train, cname, prePrunEntropy, nbagg){
  #%63.2 original
  origNum <- floor(nrow(train) * 0.632)
  bagOfTree <- c()
  for(i in 1:nbagg){
    train_orig <- train[sample(origNum),]
    train_orig <- rbind(train_orig, train_orig[sample(origNum, nrow(train) - origNum), ])
    bagOfTree <- c(bagOfTree, createDecisionTree(train_orig, cname, prePrunEntropy))
  }
  return (bagOfTree)
}

predictUsingDecisionTreeBagging <- function(bagOfTree, testData){
  
  predictions <- c()
  for(i in 1:length(bagOfTree)) {
    predictions <- c(predictions, predictUsingDecisionTree(bagOfTree[[1]], testData))
  }
  
  freqPredictions <- c()
  for(i in 1:nrow(testData)) {
    freqPredictions <- c(freqPredictions, Mode(predictions[seq(i,
                                    i + nrow(testData)* (length(bagOfTree)-1), nrow(testData))]))
  }
  
  return (freqPredictions)
}

classLabelIndex <- 16      # class label stunu

#randomize bankData
bankDatar <- bankData[sample(nrow(bankData)),]

#plot(bankData, pch=21, bg=c("red","green3","blue")[unclass(bankData$Species)])
#Create 10 equally size folds
folds <- cut(seq(1,nrow(bankDatar)),breaks=10,labels=FALSE)

#Set number of classes
classnum <- max(as.integer(factor(bankData[,classLabelIndex])))

#Create confusion matrices
dtmatrix <- matrix(0 ,nrow=classnum, ncol=classnum)

chooseMethod <- 2

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)

  bankDatar_train <- bankDatar[-testIndexes, ]
  bankDatar_test <- bankDatar[testIndexes, ]

  bankDatar_test_labels <- bankDatar[testIndexes, classLabelIndex]

  if(chooseMethod == 1){
    myTree <- createDecisionTree(bankDatar_train, "y", 0)
    bankDatar_test_pred <- predictUsingDecisionTree(myTree, bankDatar_test)
  }
  else if(chooseMethod == 2){
    myTree <- createDecisionTree(bankDatar_train, "y", 0.5)
    bankDatar_test_pred <- predictUsingDecisionTree(myTree, bankDatar_test)
  }
  else if(chooseMethod == 3){
    myTree <- createDecisionTreeBagging(bankDatar_train, "y", 0.3, 10)
    bankDatar_test_pred <- predictUsingDecisionTreeBagging(myTree, bankDatar_test)
  }
  else if(chooseMethod == 4){
    # ctree
    myTree <- ctree(Species ~ . , data=bankDatar_train)
    #Prune not working...
    #myTree <- prune(myTree, 
    #             cp=myTree$cptable[which.min(myTree$cptable[,"xerror"]),"CP"])
    bankDatar_test_pred <- predict(myTree,
                   newdata = bankDatar_test,
                   type = "response")
  }
  else if(chooseMethod == 5){
    # rpart
    myTree <- rpart(Species ~ . , method="class",
                  data=bankDatar_train, parms = list(split = "information"))
    myTree<- prune(myTree, 
                 cp=myTree$cptable[which.min(myTree$cptable[,"xerror"]),"CP"])
    bankDatar_test_pred <- predict(myTree,
                   newdata = bankDatar_test,
                   type = "class")
  }
  dtmatrix <- dtmatrix + createConfusionMatrix(bankDatar_test_pred,
                                             bankDatar_test_labels,classnum)
}
print(myTree)
CrossTable(dtmatrix)
print(sum(diag(dtmatrix)) / sum(dtmatrix))

