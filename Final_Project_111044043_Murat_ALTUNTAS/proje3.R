bankData <- read.csv("C:/Users/Murat/Desktop/ss/Veri Madenciliği/Veri madenciliği Proje/bank/bank2.csv",sep=";", header = TRUE)
bankData[[2]] <- as.numeric(factor(bankData[[2]])) # discrit attributlari continuous yapar. 2,3,4,5,7,8,9,11,15
bankData[[3]] <- as.numeric(factor(bankData[[3]]))
bankData[[4]] <- as.numeric(factor(bankData[[4]]))
bankData[[5]] <- as.numeric(factor(bankData[[5]]))
bankData[[7]] <- as.numeric(factor(bankData[[7]]))
bankData[[8]] <- as.numeric(factor(bankData[[8]]))
bankData[[9]] <- as.numeric(factor(bankData[[9]]))
bankData[[11]] <- as.numeric(factor(bankData[[11]]))
bankData[[15]] <- as.numeric(factor(bankData[[15]]))

library(kernlab)
library(pROC)
library(ROCR)
library(class)
library(gmodels)
library(stats)
#####################################################################
############################# Functions #############################
#####################################################################
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
euc_dist <- function(x1, x2) { return (sqrt(sum((x1 - x2) ^ 2))) }
manh_dist <- function(p,q){ sum(abs(p-q)) }
Mode <- function(x) {
  ls <- unique(x)
  ls[which.max(tabulate(match(x, ls)))]
}

CreateTable <- function(x1,x2) {
	total <- matrix(0,2,2)

	for(i in 1:34) {
		total[x1[i],x2[i]] <- total[x1[i],x2[i]] + 1
	}
	return(total)
}

myKnnEuc <- function(train,test,cl,k) {
	train_row <- nrow(train) # train dataset row sayısı
	test_row <- nrow(test)   # test dataset row sayısı
	eucArr <- 1:train_row    # eucledian hesaplarının row sayısı
	labelArr <- 1:k          # labeller
	result <- 1:test_row     # sonuc arrayi
	for(i in 1:test_row) {
		for(j in 1:train_row) {
			eucArr[j] <- euc_dist(train[j,],test[i,])  
		}
		for(l in 1:k) { 
			labelArr[l] <- cl[match(l,rank(eucArr))]  
		}
		result[i] <- Mode(labelArr)
	}
	return(result)
}

myKnnManh <- function(train,test,cl,k) {
	train_row <- nrow(train) # train dataset row sayısı
	test_row <- nrow(test)   # test dataset row sayısı
	eucArr <- 1:train_row    # eucledian hesaplarının row sayısı
	labelArr <- 1:k          # labeller
	result <- 1:test_row     # sonuc arrayi
	for(i in 1:test_row) {
		for(j in 1:train_row) {
			eucArr[j] <- manh_dist(train[j,],test[i,])  
		}
		for(l in 1:k) { 
			labelArr[l] <- cl[match(l,rank(eucArr))]  
		}
		result[i] <- Mode(labelArr)
	}
	return(result)
}

#####################################################################
######################### bankData (Eucledian) ######################
#####################################################################

#Randomly shuffle the data
bankData<-bankData[sample(nrow(bankData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bankData)),breaks=10,labels=FALSE)
gp <- runif(nrow(bankData))   # random siralama
bankData <- bankData[order(gp),]
totalMAtEuc <- matrix(0,2,2)
#Perform 10 fold cross validation
for(i in 1:10){
	bankData_n <- as.data.frame(lapply(bankData[,c(1:15)],normalize))
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    bankData_test <- bankData_n[testIndexes, ]
    bankData_train <- bankData_n[-testIndexes, ]
    #Use the test and train data partitions however you desire...

	#-- train ve test olarak ayırma --#
	bankData_test_target <- bankData[testIndexes, 16]   # 5 => kolon numarası
	bankData_train_target <- bankData[-testIndexes, 16]
	test_Euc <- myKnnEuc(bankData_train, bankData_test, bankData_train_target,16)   # part 1 
	#-- Cros Table --#
	totalMAtEuc <- as.matrix(CreateTable(bankData_test_target, test_Euc)) + totalMAtEuc

}

print(totalMAtEuc)
cat("%", (sum(diag(totalMAtEuc)) / sum(totalMAtEuc) * 100))

#####################################################################
########################### bankData (Manhattan) ####################
#####################################################################

#Randomly shuffle the data
bankData<-bankData[sample(nrow(bankData)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bankData)),breaks=10,labels=FALSE)
gp <- runif(nrow(bankData))   # random siralama
bankData <- bankData[order(gp),]
totalMAtManh <- matrix(0,2,2)
#Perform 10 fold cross validation
for(i in 1:10){
	bankData_n <- as.data.frame(lapply(bankData[,c(1:15)],normalize))
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    bankData_test <- bankData_n[testIndexes, ]
    bankData_train <- bankData_n[-testIndexes, ]
    #Use the test and train data partitions however you desire...

	#-- train ve test olarak ayırma --#
	bankData_test_target <- bankData[testIndexes, 16]   # 5 => kolon numarası
	bankData_train_target <- bankData[-testIndexes, 16]
	test_Manh <- myKnnManh(bankData_train, bankData_test, bankData_train_target,16) # part 2
	#-- Cros Table --#
	totalMAtManh <- as.matrix(CreateTable(bankData_test_target, test_Manh)) + totalMAtManh

}

print(totalMAtManh)
cat("%", (sum(diag(totalMAtManh)) / sum(totalMAtManh) * 100))



#####################################################################
########################### bankData ################################
#####################################################################

gp <- runif(nrow(bankData))   # random siralama
bankData <- bankData[order(gp),]
bankData_n <- as.data.frame(lapply(bankData[,c(1:15)],normalize))
#-- train ve test olarak ayırma --#
bankData_train <- bankData_n[(1:(nrow(bankData)*85/100)),]
bankData_test <- bankData_n[-(1:(nrow(bankData)*85/100)),]
bankData_train_target <- bankData[(1:(nrow(bankData)*85/100)), 16]   # 5 => kolon numarası
bankData_test_target <- bankData[-(1:(nrow(bankData)*85/100)), 16]
test_Euc <- myKnnEuc(bankData_train, bankData_test, bankData_train_target,16)   # part 1 
test_Manh <- myKnnManh(bankData_train, bankData_test, bankData_train_target,16) # part 2
#-- Cros Table --#
CrossTable(x = bankData_test_target, y = test_Euc, prop.chisq = FALSE)
CrossTable(x = bankData_test_target, y = test_Manh, prop.chisq = FALSE)


#####################################################################
######################## bankData(Linear svm) #######################
#####################################################################

#Randomly shuffle the data
bankData<-bankData[sample(nrow(bankData)),]

#set class as factor
bankData[[16]]=factor(bankData[[16]])

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bankData)),breaks=10,labels=FALSE)
gp <- runif(nrow(bankData))   # random siralama
bankData <- bankData[order(gp),]
totalMatLinear <- matrix(0,2,2)
all_predict_L <- c()
all_bankData_test_target <- c()

for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	bankData_test <- bankData[testIndexes, ]
	bankData_train <- bankData[-testIndexes, ]
	bankData_train_target <- bankData[-testIndexes, 16]   # 1 => kolon numarası
	bankData_test_target <- bankData[testIndexes, 16]

	all_bankData_test_target <- c(all_bankData_test_target, bankData_test_target)

	filter_Linear <- ksvm(y~.,data=bankData_train,kernel="vanilladot", prob.model = TRUE)	

	bankData_type_L <- predict(filter_Linear,bankData_test, type = "prob")

	all_predict_L <- rbind(all_predict_L, bankData_type_L)

	bankData_type_L <- predict(filter_Linear,bankData_test, type = "response")
	
	table(bankData_test_target, bankData_type_L)
	totalMatLinear <- as.matrix(table(bankData_test_target, bankData_type_L)) + totalMatLinear
}

pred <- prediction( all_predict_L[,1], all_bankData_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(bankData[[16]])))){
  pred <- prediction( all_predict_L[,i], all_bankData_test_target == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(bankData[[1]]))))
perf@y.values <- list(yValues / length(levels(factor(bankData[[1]]))))
perf@alpha.values <- list(aValues / length(levels(factor(bankData[[1]]))))
plot( perf, col = "red")

cat("Linear SVM: %", (sum(diag(totalMatLinear)) / sum(totalMatLinear) * 100))


#####################################################################
###################### bankData(Polynomial svm) #####################
#####################################################################

#Randomly shuffle the data
bankData<-bankData[sample(nrow(bankData)),]

#set class as factor
bankData[[16]]=factor(bankData[[16]])

#Create 10 equally size folds
folds <- cut(seq(1,nrow(bankData)),breaks=10,labels=FALSE)
gp <- runif(nrow(bankData))   # random siralama
bankData <- bankData[order(gp),]
totalMatLinear <- matrix(0,2,2)
all_predict_L <- c()
all_bankData_test_target <- c()

for(i in 1:10){
	#-- train ve test olarak ayırma --#
	#Segement your data by fold using the which() function 
	testIndexes <- which(folds==i,arr.ind=TRUE)
	bankData_test <- bankData[testIndexes, ]
	bankData_train <- bankData[-testIndexes, ]
	bankData_train_target <- bankData[-testIndexes, 16]   # 1 => kolon numarası
	bankData_test_target <- bankData[testIndexes, 16]

	all_bankData_test_target <- c(all_bankData_test_target, bankData_test_target)

	filter_Linear <- ksvm(y~.,data=bankData_train,kernel="polydot", prob.model = TRUE)	

	bankData_type_L <- predict(filter_Linear,bankData_test, type = "prob")

	all_predict_L <- rbind(all_predict_L, bankData_type_L)

	bankData_type_L <- predict(filter_Linear,bankData_test, type = "response")
	
	table(bankData_test_target, bankData_type_L)
	totalMatLinear <- as.matrix(table(bankData_test_target, bankData_type_L)) + totalMatLinear
}

pred <- prediction( all_predict_L[,1], all_bankData_test_target == 1)
perf <- performance( pred, "tpr", "fpr" )
xValues <- unlist(perf@x.values)
yValues <- unlist(perf@y.values)
aValues <- unlist(perf@alpha.values)
for(i in 2:length(levels(factor(bankData[[16]])))){
  pred <- prediction( all_predict_L[,i], all_bankData_test_target == i)
  perf <- performance( pred, "tpr", "fpr" )
  xValues <- xValues + unlist(perf@x.values)
  yValues <- yValues + unlist(perf@y.values)
  aValues <- aValues + unlist(perf@alpha.values)
}
perf@x.values <- list(xValues / length(levels(factor(bankData[[1]]))))
perf@y.values <- list(yValues / length(levels(factor(bankData[[1]]))))
perf@alpha.values <- list(aValues / length(levels(factor(bankData[[1]]))))
plot( perf, col = "blue")

cat("Polynomial SVM: %", (sum(diag(totalMatLinear)) / sum(totalMatLinear) * 100))





