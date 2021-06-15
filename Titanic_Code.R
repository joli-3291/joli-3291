if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(titanic)) install.packages("titanic", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")


library(titanic)    # loads titanic_train data frame
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(randomForest)
library(kernlab)
library(gbm)
library(nnet)



# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# Variables type
str(titanic_train)

# Survived count

titanic %>%
  ggplot(aes(x = Survived)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat="count", aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

#Survived Count by Sex

titanic %>%
  ggplot(aes(x = Survived, fill=Sex)) +
  geom_bar(position = position_dodge())+
  geom_text(stat="count",  
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic() 

#Survived Count by Pclass

titanic %>%
  ggplot(aes(x = Survived, fill=Pclass)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat="count",   
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5) +
  theme_classic()

# Survival Rate by Fare

titanic%>%ggplot(aes(x = Fare, y = as.numeric(Survived)))+
  geom_point(alpha = 0.1)+
  geom_smooth(color="red", fill="#69b3a2", se=TRUE)+
  scale_y_continuous(breaks = seq(1, 2.5, 0.5), labels = c (0, 0.5, 1, 1.5))+
  labs(title = "Survival by Fare",
       y = "Survived")+
  theme_classic()


# Age Density

ggplot(titanic, aes(x = Age)) +
  geom_density(fill="coral") 

# Survival by Age

# Discretize age to plot survival

titanic$DiscretizedAge = cut(titanic$Age, c(0,10,20,30,40,50,60,70,80,100))

# Plot discretized age

titanic %>% 
  filter(!is.na(titanic$DiscretizedAge))%>%
  ggplot(aes(x = DiscretizedAge, fill=Survived)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat="count", aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

# Age Distributions by Sex

titanic %>%  ggplot(aes(Age,Sex)) + 
  geom_violin(fill="grey") + 
  coord_flip() +
  geom_jitter(col=ifelse(titanic$Survived=="1","blue","red"),alpha=.5) + 
  ggtitle("Age Distributions") + 
  ylab("Gender") +
  xlab("Age")

#Age Distributions by Sex and Pclass

ggplot(titanic, aes(y=Age,x=Sex, fill=Survived))+
  geom_boxplot()+ 
  ggtitle("Distribution of ages by sex and class") +
  xlab("Sex")+
  ylab("Age")+
  scale_fill_discrete("Survived or no",labels=c("perished","survived"))+ 
  facet_grid( ~ Pclass)

# Create a family size variable including the passenger themselves

titanic$Fsize <- titanic$SibSp + titanic$Parch + 1

# Create a family variable 
# Discretize family size

titanic$FsizeD[titanic$Fsize == 1] <- "singleton"
titanic$FsizeD[titanic$Fsize < 5 & titanic$Fsize > 1] <- "small"
titanic$FsizeD[titanic$Fsize > 4] <- "large"

# Show family size by survival using a mosaic plot

mosaicplot(table(titanic$FsizeD, titanic$Survived), main="Family Size by Survival", shade=TRUE) 

#Train and Test Sets

set.seed(42, sample.kind = "Rounding")    # simulate R 3.5
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)    # create a 20% test set
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]

nrow(train_set)
nrow(test_set)

#The survival proportion

mean(train_set$Survived == 1)


#Survival by Fare+Age+Sex - LDA

#set.seed(1) # R 3.5
set.seed(1, sample.kind = "Rounding") # R 3.6
train_lda <- train(Survived ~ Fare+Age+Sex, 
                   method = "lda", 
                   data = train_set)
lda_preds <- predict(train_lda, test_set)
confusionMatrix(lda_preds, test_set$Survived)



#Survival by Fare+Age+Sex - QDA

#set.seed(1) # R 3.5
set.seed(1, sample.kind = "Rounding") # R 3.6
train_qda <- train(Survived ~ Fare+Age+Sex, 
                   method = "qda", 
                   data = train_set)
qda_preds <- predict(train_qda, test_set)
confusionMatrix(qda_preds, test_set$Survived)



#kNN model on the training set 

#set.seed(6)
set.seed(6, sample.kind = "Rounding")    # simulate R 3.5
control = trainControl( method="LOOCV");
train_knn = train( Survived~Pclass+Sex+Age+Fare+Embarked+SibSp+Parch+FamilySize,
                   data=train_set,
                   method="knn", 
                   trControl=control, 
                   preProcess=c("center","scale"), 
                   tuneLength=20 )
train_knn

#plot the kNN model

plot( train_knn)


#The accuracy of the kNN model on the test set is:

knn_preds <- predict(train_knn, test_set)
confusionMatrix(knn_preds, test_set$Survived)


#Cross-validation
#We use 10-fold cross-validation where each partition 
#consists of 10% of the total and tuning with k = seq(3, 51, 2). 
#The optimal value of k using cross-validation is:

#set.seed(8)
set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune

#The accuracy on the test set using the cross-validated kNN model is:

knn_cv_preds <- predict(train_knn_cv, test_set)
confusionMatrix(knn_cv_preds, test_set$Survived)


#Classification tree model
#we use caret to train a decision tree with the rpart method 
#tuning  the complexity parameter with cp = seq(0, 0.05, 0.002).
#The optimal value of the complexity parameter (cp) is:

#set.seed(10)
set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

#The accuracy of the decision tree model on the test set is:

rpart_preds <- predict(train_rpart, test_set)
confusionMatrix(rpart_preds, test_set$Survived)


#Using the rpart library we make a decision tree that takes into account 
#the Class, Sex, Age, Siblings & Spouses, the Parch, Fare and Embarked
#and plot it using the following code:

decision_tree <- rpart(Survived ~ as.numeric(Pclass) + Sex + Age + SibSp + Parch + Fare + Embarked,data=train_set,method="class")
fancyRpartPlot(decision_tree)

options(repr.plot.width=25, repr.plot.height=25)

# Random Forest model

#set.seed(1234)
set.seed(1234, sample.kind = "Rounding")    


# Set control parameters

control <- trainControl(method="repeatedcv",
                        number=10,
                        repeats=5,
                        search = "random")

# Determine baseline mtry

mtry <- sqrt(ncol(train_set))
tune_grid = expand.grid(.mtry=mtry)

#Train RF model
#Random generate 15 mtry values with tuneLength = 15

train_rf <- train(Survived ~ .,
                  data=train_set,
                  method="rf",
                  tuneLength=15, 
                  trControl=control,
                  importance=TRUE,
                  localImp=TRUE,
                  tuneGrid=tune_grid)

# Explain final RF model

fit_rf <- train_rf$finalModel
fit_rf

# The Accuracy using the ConfusionMatrix

rf_preds <- predict(train_rf, test_set) %>%confusionMatrix(test_set$Survived)
rf_preds

#Tuning is the process of maximizing a model's performance without overfitting or creating too high #of a variance. In machine learning, this is accomplished by selecting appropriate hyperparameters."
#Mtry is the number of variables available for splitting at each tree node.

#set.seed(1234)
set.seed(1234, sample.kind = "Rounding")  

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tune_grid <- expand.grid(.mtry=c(1:5))
rf_gridsearch <- train(Survived~., 
                       data=train_set, 
                       method="rf", 
                       tuneGrid=tune_grid, 
                       trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


# It seems that best value for mtry is 3. 
#Finally, lets use this mtry to check best ntree (number of trees to grow) parameter:

#set.seed(1234)
set.seed(1234, sample.kind = "Rounding")  
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tune_grid <- expand.grid(.mtry=c(3))
modellist <- list()
for (ntree in c(100, 250, 500)) {
  rf_manual <- train(Survived~., 
                     data=train_set, 
                     method="rf", 
                     tuneGrid=tune_grid, 
                     trControl=control, 
                     ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- rf_manual
}
results <- resamples(modellist)
summary(results)

# build a model by using randomForest on the train set for almost all variables.

#set.seed(14)
set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilySize, data = train_set)

# Show model error

plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

#Let's look at relative variable importance by plotting 
#the mean decrease in Gini calculated across all trees.
# Get importance

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, color = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_bw()

# Gradient Boosting Model ('gbm') 

#set.seed(616)
set.seed(616, sample.kind = "Rounding")    
control <- trainControl(method="repeatedcv",number=5, repeats=5)
gbmGrid <- expand.grid(interaction.depth = seq(1,7,by=2),
                       n.trees = seq(50, 250, by = 25),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 5)

train_gbm <-train(Survived ~ ., 
                  data = train_set,
                  method = "gbm",
                  verbose = FALSE,
                  trControl = control,
                  tuneGrid = gbmGrid)
train_gbm

train_gbm$bestTune


gbm_preds <- predict(train_gbm, test_set, type = "raw") %>%confusionMatrix(test_set$Survived)
gbm_preds

ggplot(train_gbm) + 
  theme(legend.position = "top") + 
  labs(title = "Stochastic Gradient Boosting 5-fold CV tuning parameter(s): tree depth, iterations, learning rate")

# Boosted Logistic Regression

# set.seed(616)
set.seed(616, sample.kind = "Rounding") 
control = trainControl(method="repeatedcv",number=5, repeats=5)
train_blr = train(Survived~., 
                  data= train_set, 
                  method="LogitBoost", 
                  verbose=FALSE, 
                  trControl=control)
train_blr

preds_blr = predict(train_blr, test_set, type = "raw") %>%confusionMatrix(test_set$Survived)
preds_blr

ggplot(train_blr) + 
  theme(legend.position = "top")+  
  labs(title = "Boosted Logistic Regression 5-fold CV tuning parameter(s): iterations")


# Support Vector Machine Model ('svm')

#set.seed(616)
set.seed(616, sample.kind = "Rounding")
control = trainControl(method="repeatedcv",number=5, repeats=5) 
svmGrid = expand.grid(C = sapply(-3:3, exp), sigma = sapply(-3:1, exp))

train_svm = train(Survived ~., 
                  data=train_set, 
                  method="svmRadial", 
                  trControl=control, 
                  tuneGrid=svmGrid)
train_svm

svm_preds = predict(train_svm, test_set) %>%confusionMatrix(test_set$Survived)
svm_preds

ggplot(train_svm) + 
  theme(legend.position = "top") + 
  labs(title = "Support Vector Machine Classifier 5-fold CV tuning parameter(s): cost, sigma")



# Neural Network Model ('nnet')

# We select parameters for neural network by using 10-fold cross-validation with caret
# This make take several minutes


tune_grid <-expand.grid(size=1:10,decay=c(0,0.0001,0.05,0.1))

# set.seed(1)

set.seed(1, sample.kind = "Rounding")

control <- trainControl(method="repeatedcv", number=10, repeats=10) 

train_nnet <-train(Survived~., 
                   data=train_set, 
                   method="nnet",
                   trControl=control, 
                   preProcess="range",
                   tuneLength=10, 
                   tuneGrid=tune_grid,
                   trace=FALSE, 
                   verbose=FALSE, 
                   maxit=500)
nnet_preds <- predict(train_nnet,test_set)%>%confusionMatrix(test_set$Survived)
nnet_preds

plot(train_nnet)

best_size <- train_nnet$bestTune[1,"size"]
best_size

best_decay <- train_nnet$bestTune[1,"decay"]
best_decay

best_nn <- nnet( Survived ~ .,
                 method="nnet",  
                 data = train_set, 
                 size=best_size, 
                 decay=best_decay, 
                 maxit=500, 
                 trace=FALSE)

best_nn_pred <- predict( best_nn, newdata=test_set, type="class" )  # yields "0", "1"
confusionMatrix(factor(best_nn_pred), test_set$Survived)


