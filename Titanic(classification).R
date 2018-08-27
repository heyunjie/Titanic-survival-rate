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

##There're missing values in Age
train_NoAge <- train[!is.na(train$Age),]
train_NoAge$survival <- ifelse(train_NoAge$Survived==1,"Yes","No")
as.factor(train_NoAge$survival) 

##Analyze with using age
#tree_train_Age <- tree(formula = survival ~ Sex+Age+Pclass+SibSp+Parch+Cabin+Embarked+Title,data=train_NoAge)
tree_train_Age <- tree(formula = survival ~ Sex,data=train_NoAge)
summary(tree_train_Age)
plot(tree_train_Age)
text(tree_train_Age,cex=0.5,pretty=0)

##Analyze without using age
table(train$Title)
