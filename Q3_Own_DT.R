head(trainX)
summary(trainX)
train = cbind(trainX,trainY)
head(train)

colnames(train)<-c("mean_Radius", "mean_Texture", "mean_Perimeter", "mean_Area", "mean_Smoothness", "mean_Compactness", "mean_Concavity", "mean_contour", "mean_Symmetry", "mean_Fractal dimension", "std_Radius", "std_Texture", "std_Perimeter", "std_Area", "std_Smoothness", "std_Compactness", "std_Concavity", "std_contour", "std_Symmetry", "std_Fractal dimension", "max_Radius", "max_Texture", "max_Perimeter", "max_Area", "max_Smoothness", "max_Compactness", "max_Concavity", "max_contour", "max_Symmetry", "max_Fractal dimension","Cancer")
train
str(train)

sum(is.na(train)) #to check any null values in the data

train$Cancer<-as.factor(train$Cancer) #Converting int to num
library(rpart)

#classification 
tree_class<-rpart(Cancer~.,data = train,method = "class",control = rpart.control(minsplit = 0,minbucket = 0, cp=-1))
library(rpart.plot)
rpart.plot(tree_class)
print(tree_class)
printcp(tree_class)

#From the plot  and from the print information we see there are 17 asterisks which tell us we have 17 leaves for the classification tree

#Regression
tree_reg<-rpart(Cancer~.,data=train,method ="anova",control = rpart.control(cp=-1))
print(tree_reg)

rpart.plot(tree_reg)

#From the plot and the print data in the console we can see we have 10 asterisks and thus we have 10 leaves for this regression tree

## Q2. 
#Mean contour(root node), Max texture(36%) & Max Perimeter(64%) are the major predictors of diagnosis 

##Q3. Give two strong rules that describe who is likely to have cancer. Please justify your choices.

## If Mean Contour < 0.068, Max Perimeter < 107, Standard Deviation of Radius < 0.91, Max Texture >39 -> Then likely
#to have cancer.
## If Mean Contour > 0.068, Max Texture <21 and Mean Contour > 0.091 -> Then likely to have cancer.


##Q4. Accuracy of this model on train and test data

colnames(testX)<-c("mean_Radius", "mean_Texture", "mean_Perimeter", "mean_Area", "mean_Smoothness", "mean_Compactness", "mean_Concavity", "mean_contour", "mean_Symmetry", "mean_Fractal dimension", "std_Radius", "std_Texture", "std_Perimeter", "std_Area", "std_Smoothness", "std_Compactness", "std_Concavity", "std_contour", "std_Symmetry", "std_Fractal dimension", "max_Radius", "max_Texture", "max_Perimeter", "max_Area", "max_Smoothness", "max_Compactness", "max_Concavity", "max_contour", "max_Symmetry", "max_Fractal dimension")
colnames(testY)= "Cancer"
tree_pred_prob <- predict(tree_class, testX, type = "prob")
tree_pred_class <- predict(tree_class, testX, type = "class")
mean(testY$Cancer == tree_pred_class) #Accuracy on Test Data

colnames(trainX)<-c("mean_Radius", "mean_Texture", "mean_Perimeter", "mean_Area", "mean_Smoothness", "mean_Compactness", "mean_Concavity", "mean_contour", "mean_Symmetry", "mean_Fractal dimension", "std_Radius", "std_Texture", "std_Perimeter", "std_Area", "std_Smoothness", "std_Compactness", "std_Concavity", "std_contour", "std_Symmetry", "std_Fractal dimension", "max_Radius", "max_Texture", "max_Perimeter", "max_Area", "max_Smoothness", "max_Compactness", "max_Concavity", "max_contour", "max_Symmetry", "max_Fractal dimension")
colnames(trainY)= "Cancer"
tree_pred_class_train <- predict(tree_class, trainX, type = "class")
mean(trainY$Cancer == tree_pred_class_train) #Accuracy on Train Data

##Q5. Is it possible to improve the performance of your model?
#Yes, by pruning techniques i.e. by Changing Cp values and Minsplit numbers


#Construct the best possible decision tree to predict the Y labels. Explain how you construct such tree.
#By choosing an optimum value of cp(i.e CP where the xerror is (LOW) and pruning it, we can increase the accuracy of the
#model from 84% to 87%. The model now has 9 leaves.  


model_pruned <- prune(tree_class, cp = 0.0059172 )
colnames(testX)<-c("mean_Radius", "mean_Texture", "mean_Perimeter", "mean_Area", "mean_Smoothness", "mean_Compactness", "mean_Concavity", "mean_contour", "mean_Symmetry", "mean_Fractal dimension", "std_Radius", "std_Texture", "std_Perimeter", "std_Area", "std_Smoothness", "std_Compactness", "std_Concavity", "std_contour", "std_Symmetry", "std_Fractal dimension", "max_Radius", "max_Texture", "max_Perimeter", "max_Area", "max_Smoothness", "max_Compactness", "max_Concavity", "max_contour", "max_Symmetry", "max_Fractal dimension")
colnames(testY)= "Cancer"
tree_pred_prob <- predict(model_pruned, testX, type = "prob")
tree_pred_class <- predict(model_pruned, testX, type = "class")
mean(testY$Cancer == tree_pred_class) #Accuracy on Test Data
rpart.plot(model_pruned)

#####################
 