library(caret)
library(ggplot2)
library(readr)
library(rpart)
library(ROSE)
library(DMwR)

# Reading the file

mushrooms <- read.csv("data_train1.csv")

# Data summary

summary(mushrooms)

# Inspecting the data

plot(mushrooms$class) #imbalanced classes
round(prop.table(table(mushrooms$class)), 2)


# Data structure

str(mushrooms)
View(mushrooms)

# Removing col 1,6,21,22 no visible in a picture
# col 7 missing values
# col 17 only 1 value
# col 12 many ? values

mushrooms <- mushrooms[c(3,2,4,5,8,9,10,11,17,18,20,23)]
View(mushrooms)

# Creating training and testing data

set.seed(123)

inTrain <- createDataPartition(y= mushrooms$class,p=.70, list = FALSE, times = 3) 

# Partitioning the data

training <- mushrooms[ inTrain,]
testing <- mushrooms[-inTrain,]

# Fixing data imbalance with mixed sampling, apply both under sampling and over sampling

training<- ovun.sample(class ~ ., data = training, method="both", p=0.5, seed=222, N=227846)$data
print(table(training$class))

# Penialize false negatives

loss_matr <- matrix(c(0, 20, 1, 0), nrow = 2)

# Rpart Decision tree

set.seed(123)
mushrooms_tree <- rpart(class ~ .,
                 data = training,
                 method = "class",
                 parms = list(loss = loss_matr)
                 
)

#Variable importance

mushrooms_tree$variable.importance #variable importance

# confusion matrix train

confusionMatrix(data=predict(mushrooms_tree, type = "class"), 
                       reference = training$class, 
                       positive="e")

pred.mushroom.tree <- predict(mushrooms_tree, newdata = testing) #prediction



# check the accuracy of this prediction

accuracy.meas(testing$class, pred.mushroom.tree[,2])

# confusion matrix test

confusionMatrix(data = predict(mushrooms_tree, newdata = testing, type = "class"), 
                       reference = testing$class, 
                       positive = "e")

# Plotting the tree
fancyRpartPlot(mushrooms_tree)


