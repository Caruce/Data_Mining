Fundraising <- read.csv("Fundraising.csv")

#Step1 1: partitioning
set.seed(12345)

## partitioning into training (60%) and validation (40%)
train.data <- read.csv("Train.csv")
valid.data <- read.csv("Valid.csv")
train.origin <- train.data
valid.origin <- valid.data
#test data
new.data <- read.csv("FutureFundraising.csv")
new.orgin <- new.data


#Step 2: model building
#1.select classification tool and parameters.
train.data <- train.data[,-c(1,2,24)]
valid.data <- valid.data[,-c(1,2,24)]
new.data <- new.data[,-c(1,2,23,24)]

#1st Classification and regression trees
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

gini.tree <- rpart(train.data$TARGET_B ~ ., data = train.data, method = "class",
                     control = rpart.control(maxdepth = 5, minsplit = 1, minbucket = 1,cp = 0, xval = 10),
                     parms = list(split = "gini"))
prp(gini.tree, type = 1, extra = 101, split.font = 2, varlen = -10, 
      box.palette="Blues", fallen.leaves = F, leaf.round=F)

#2nd Logistic Regression

#3rd Naive Bayes

#4th k-nearest neighbours
# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.data
valid.norm.df <- valid.data
# use preProcess() from the caret package to normalize Income and Lot_Size.
install.packages("ggplot2")
install.packages("lattice")
install.packages("dplyr")
install.packages("caret")
library(ggplot2)
library(lattice)
library(dplyr)
library(caret)

norm.values <- preProcess(train.data[, 1:20], method=c("center", "scale"))
train.norm.df[, 1:20] <- predict(norm.values, train.data[, 1:20])
valid.norm.df[, 1:20] <- predict(norm.values, valid.data[, 1:20])
valid.norm.df.test_in_k <- valid.norm.df[,-21]
#cause we need to use validation to test firstly, so we delete the last column now,
#otherwise, we cannot do the caculation. It conflicts with the 21th column in train.
new.norm.df <- predict(norm.values, new.data)
 
# use knn() to compute knn for new data record 
install.packages("FNN")
library(FNN)
nnn <- knn(train.norm.df[, 1:20], valid.norm.df[, 1:20], cl = train.norm.df[, 21], k = 3, prob = "TRUE")
nnn

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 1872, 1), accuracy = rep(0, 1872))
# compute knn for different k on validation.
for(i in 1:1872) {
  knn.pred <- knn(train.norm.df[, 1:20], valid.norm.df[, 1:20], cl = train.norm.df[, 21], k = i, prob = "TRUE")
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 21],positive='1')$overall[1] 
}
accuracy.df


# Best k values
which(accuracy.df[,2] == max(accuracy.df[,2]))

# use knn() to compute knn for validation data records using a best k
knn.pred <- knn(train.norm.df[, 1:20], valid.norm.df[, 1:20], cl = train.norm.df[, 3], k = "the best k", prob = "TRUE")
confusionMatrix(knn.pred, valid.norm.df[, 21], positive='1', mode = "everything")

## delete non-training indicator and append results of knn
valid.norm.df$Winner <- knn.pred
valid.norm.df$WinProb <- attr(knn.pred, "prob")
p <- valid.norm.df$WinProb
valid.norm.df$DonorProb <- ifelse(valid.norm.df$Winner == "1", p, 1-p)
valid.norm.df

## gain chart
library(gains)
gain <- gains(valid.norm.df$TARGET_B, rbind(valid.norm.df$DonorProb), groups=dim(valid.norm.df)[1])
plot(c(0, gain$cume.pct.of.total*sum(valid.norm.df$TARGET_B)) ~ c(0, gain$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(valid.norm.df$TARGET_B))~c(0,dim(valid.norm.df)[1]), col="gray", lty=2)

## lift chart
barplot(gain$mean.resp / mean(valid.norm.df$TARGET_B), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")


