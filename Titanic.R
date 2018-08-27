library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data
# check data
str(full)
title1 <- vector(length=length(full$Name))
for(i in 1:length(full$Name)){ title1[i] <- strsplit(full$Name,split = '[,.]')[[i]][2] }
full$Title <- title1
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Surname <- sapply(full$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') +
    scale_x_continuous(breaks=c(1:11)) +
    labs(x = 'Family Size') +
    theme_few()

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
strsplit(full$Cabin[2], NULL)[[1]]
embark_fare <- full %>%
    filter(PassengerId != 62 & PassengerId != 830)
# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2)
+scale_y_continuous(labels=dollar_format()) +
    theme_few()
# Let’s visualize Fares among all others sharing their class and embarkment (n = 494).
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], aes(x = Fare)) + geom_density(fill = '#99d6ff', alpha=0.4)
+ geom_vline(aes(xintercept=median(Fare, na.rm=T)), colour='red', linetype='dashed', lwd=1) + scale_x_continuous(labels=dollar_format())
+ theme_few()
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
# Save the complete output 
mice_output <- complete(mice_mod)
#########################
#mice package
set.seed(2016)
data <- airquality
data[sample(nrow(data),7),3] <- NA
data[sample(nrow(data),7),4] <- NA
data <- data[-c(5,6)]
library(mice)
init = mice(data, maxit=0)
meth = init$method
predM = init$predictorMatrix

########################
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
#======================================================================
library(dpylr)
attach(train)
survived <- train[Survived==1,]
Death <- train[Survived==0,]
detach(train)
#=================survival=============
attach(survived)
##class
##
as.factor(Pclass)
hist(Pclass,main = "which class is more likely to survive",probability = TRUE)
table(Pclass)
cat("the probability of survival for class 3 is ",0.3479532,"\n")
cat("the probability of survival for class 2 is ",0.254386,"\n")
cat("the probability of survival for class 1 is ",0.3976608,"\n")
##sex
##
as.factor(Sex)
barplot(table(Sex),main="Male vs Female 's survival rate")
cat("male is more likely to survive")
##age,delete missing values
##
survived <- survived[!is.na(Age),]
summary(Age)
boxplot(Age,main="The age range of survivals")
plot(Age)
hist(Age)
cat("0~20")  # very likely
cat("20~40") # most likely
cat("40~60") # less likely
cat("60~80") # very unlikely
##sibling
##
as.factor(SibSp)
hist(SibSp,main = "Sib affects survival rate",probability = TRUE)
table(SibSp)
cat("people with less Sib is more likely to survive")
##parch
as.factor(Parch)
hist(Parch,main = "Parch affects survival rate",probability = TRUE)
table(Parch)
cat("people with less Parch is more likely to survive")

##embark
table(Embarked)
#   C   Q   S 
#   93  30  217 
detach(survived)

#================Death============
attach(Death)
##class
##
as.factor(Pclass)
hist(Pclass,main = "which class is more likely to die",probability = TRUE)
table(Pclass)

##sex
##
as.factor(Sex)
barplot(table(Sex),main="Male vs Female 's death rate")
cat("male is more likely to survive")
##age,delete missing values
##
survived <- survived[!is.na(Age),]
summary(Age)
boxplot(Age,main="The age range of survivals")
plot(Age)
hist(Age)
cat("0~20")  # very likely
cat("20~40") # most likely
cat("40~60") # less likely
cat("60~80") # very unlikely
##sibling
##
as.factor(SibSp)
hist(SibSp,main = "Sib affects survival rate",probability = TRUE)
table(SibSp)
cat("people with less Sib is more likely to survive")
##parch
as.factor(Parch)
hist(Parch,main = "Parch affects survival rate",probability = TRUE)
table(Parch)
cat("people with less Parch is more likely to survive")

##embark
table(Embarked)
detach(Death)

#===========Using classification methods========================
View(train)
library(tree)

title1 <- vector(length=length(train$Name))
for(i in 1:length(train$Name)){ title1[i] <- strsplit(train$Name,split = '[,.]')[[i]][2] }
train$Title <- title1
table(train$Sex, train$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
train$Surname <- sapply(train$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

train_NoAge <- train[!is.na(train$Age),]
train_NoAge$survival <- ifelse(train_NoAge$Survived==1,"Yes","No")
as.factor(train$Survived)
##With age
tree_train_Age <- tree(formula = Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title,data=train_NoAge)
summary(tree_train_Age)
plot(tree_train_Age)
text(tree_train_Age,cex=0.5,pretty=0)




