Fundraising <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-spring/STAT 4440/Group Project/Fundraising.csv")


#Step1 1: partitioning
dim(Fundraising) # find the dimension of data frame

set.seed(12345)

## partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(Fundraising), dim(Fundraising)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- Fundraising[train.rows, ]
train.origin <- Fundraising[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(Fundraising), train.rows) 
valid.data <- Fundraising[valid.rows, ]
valid.origin<-  Fundraising[valid.rows, ]
#test data
new.data <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-spring/STAT 4440/Group Project/FutureFundraising.csv")
new.origin <- new.data

#Step 2: model building
#1.select classification tool and parameters.
train.data <- train.data[,-24] #no TARGET_D in the analysis
valid.data <- valid.data[,-24]
new.data <- new.data[,-24]

train.data <- train.data[,-c(1,2)]#IDs would not have relationships with donations.
valid.data <- valid.data[,-c(1,2)]
new.data <- new.data[,-c(1,2,23)]

#Classification and regression trees

library(rpart)
library(rpart.plot)

gini.tree <- rpart(train.data$TARGET_B ~ ., data = train.data, method = "class",
                     control = rpart.control(maxdepth = 5, minsplit = 1, minbucket = 1,cp = 0, xval = 10),
                     parms = list(split = "gini"))
prp(gini.tree, type = 1, extra = 101, split.font = 2, varlen = -10, 
      box.palette="Blues", fallen.leaves = F, leaf.round=F)

#Logistic Regression

#Naive Bayes

#k-nearest neighbours
# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.data
valid.norm.df <- valid.data

# use preProcess() from the caret package to normalize Income and Lot_Size.
library(caret)

norm.values <- preProcess(train.data[, 1:20], method=c("center", "scale"))
train.norm.df[, 1:20] <- predict(norm.values, train.data[, 1:20])
valid.norm.df[, 1:20] <- predict(norm.values, valid.data[, 1:20])
valid.morm.df.test_in_k <- valid.norm.df[,-21]
#cause we need to use validation to test firstly, so we delete the last column now,
#otherwise, we cannot do the caculation. It conflicts with the 21th column in train.
new.norm.df <- predict(norm.values, new.data)

# use knn() to compute knn for new data record 
library(FNN)
nnn <- knn(train = train.norm.df[, 1:20], test = valid.morm.df.test_in_k, cl = train.norm.df[,21], prob = "TRUE", k = 3)
nnn

# probability of winning class
attr(nnn, "prob")

# nearest k neighbors to new record
row.names(train.origin)[attr(nnn, "nn.index")]

# Euclidean (not square) distances
attr(nnn, "nn.dist")

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 24, 1), accuracy = rep(0, 24))

# compute knn for different k on validation.
for(i in 1:24) {
  knn.pred <- knn(train.norm.df[, 1:20], valid.norm.df[, 1:20], cl = train.norm.df[, 21], k = i, prob = "TRUE")
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 21], positive='1')$overall[1] 
}
accuracy.df
# Best k values
which(accuracy.df[,2] == max(accuracy.df[,2]))
