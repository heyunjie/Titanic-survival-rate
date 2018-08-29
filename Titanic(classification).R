##Analysis about the survival rate of Titanic event, using classification
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

summary(train)

##Divide the name into different parts
title1 <- vector(length=length(train$Name))
for(i in 1:length(train$Name)){ title1[i] <- strsplit(train$Name,split = '[,.]')[[i]][2] }
train$Title <- title1
table(train$Sex, train$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
train$Surname <- sapply(train$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

##Select useful variables by hand: survived ; pclass ; sex ; age; sibsp ; parch ; embarked
train <- train[,c(2,3,5,6,7,8)]
test <- test[,c(2,4,5,6,7)]

##There're missing values in Age
train <- na.omit(train)
test <- na.omit(test)

train$Sex <- as.factor(train$Sex)
test$Sex <- as.factor(test$Sex)
for(i in 1:length(train$Survived)){
 ifelse(train$Survived[i]==1, train$Survived[i]<-"YES",train$Survived[i]<-"NO")
}
train$Survived <- as.factor(train$Survived)
##CV
test_num <- sample(1:length(train$Survived),500)
model_test <- train[-test_num,]
model_train <- train[test_num,]

attach(model_train)
tree_train <- tree(formula = Survived~Pclass + Sex + Age + SibSp + Parch)
plot(tree_train)
text(tree_train)
treePred <- predict(tree_train,model_test)
for(i in 1:length(treePred)){
    ifelse(treePred[i]>=0.5,treePred[i]<-1 , treePred[i] <- 0)
}
table(treePred,model_test$Survived)
## Accuracy rate is 0.7990654

cv.tree(tree_train)
pruneTree_train <- prune.misclass(tree_train,best=5) 
pruneTreePred <- predict(pruneTree_train,model_test)
prune_pred <- vector(length=length(pruneTreePred[,1]))
for(i in 1:length(pruneTreePred[,1])){
    ifelse(pruneTreePred[i,1]>0.5,prune_pred[i]<-"NO",prune_pred[i]<-"YES")
}
prune_pred <- as.factor(prune_pred)
table(prune_pred,model_test$Survived)


