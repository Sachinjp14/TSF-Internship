#Loading all the required datasets
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(caret)

#Loading the dataset
iris

#Setting the seed
set.seed(4060)

#Splitting the data set into train and test
n=nrow(iris)
itrain = sample(1:n,round(0.8*n))

#Training data set
iris.train = iris[itrain,]

#Test data set
iris.test = iris[-itrain,]

#Train the model to predict species
iris_model <- rpart(Species ~.,
                    data = iris.train,
                    method = "class",
                    control = rpart.control(cp = 0),
                    parms = list(split = "information"))

#Plot 1
prp(iris_model, extra = 1, faclen=0,  nn = T,
    box.col=c("green", "red"))

#Plot2
plot(iris_model)
text(iris_model)

#Plot3
rpart.plot(iris_model, type=4 , extra=101)

#Predicting the tree
iris_pred <- predict(object = iris_model,
                     newdata = iris.test,
                     type = "class")

#Confusion Matrix 1
table(iris.test[,5],iris_pred)

#Confusion Matrix 2
confusionMatrix(data = iris_pred,
                reference = iris.test$Species)

