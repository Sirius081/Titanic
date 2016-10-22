extractFeature<-function(data){
  features<-c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
  fea<-data[,features]
  for (i in 1:length(fea$Age)){
    if(is.na(fea$Age[i])){
      name=data$Name[i]
      if(length(grep(".*Miss.*",name))==1){
        fea$Age[i]<-avgAge$miss[1]
      }else if(length(grep(".*Mrs*",name))==1){
        fea$Age[i]<-avgAge$mrs[1]
      }else if(length(grep(".*Mr.*",name))==1){
        fea$Age[i]<-avgAge$mr[1]
      }else{
        fea$Age[i]<-avgAge$other[1]
      }
    }
  }
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Age<-round(fea$Age,2)
  fea$Sex<-as.factor(fea$Sex)
  fea$Pclass<-as.factor(fea$Pclass)
  fea$Embarked<-as.factor(fea$Embarked)
  
  fea
}
train<-read.csv("train.csv")
test<-read.csv("test.csv")
trainFea<-extractFeature(train)
testFea<-extractFeature(test)
rf<-randomForest(trainFea,as.factor(train$Survived),importance = TRUE)
submission$PassengerId<-test$PassengerId
submission$Survived<-predict(rf,testFea)