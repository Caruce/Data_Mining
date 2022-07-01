setwd("~/My Courses/Data Mining/Datasets/DMBA-3eR-datasets")
universities.df<-read.csv("Universities.csv")
dim(universities.df)

#Remove missing data and first two (text) columns 
universities.complete.df<-na.omit(universities.df[,-c(1,2)])
dim(universities.complete.df)
#AND last column (grad rate) as response variable for regression
universities.x.df<-na.omit(universities.complete.df[,-c(18)])
dim(universities.x.df)
head(universities.x.df)

#Principal Component Analysis on correlation matrix (normalized)                        
pcs.cor=prcomp(universities.x.df, scale=TRUE)
summary(pcs.cor)
#Principal Componenent rotations (weights) - first 5
pcs.cor$rot[,c(1:5)]

#Z scores (to replace original X data)
pcs.scores.cor <- pcs.cor$x
head(pcs.scores.cor)

#scree plot of summary variances which sum to #predictors, p
plot(pcs.cor, xlab = "Principal Component")

#Multiple Linear Regression using first 2 principal components (via Anil)
#Creating new data frame which includes the Y variable "Graduation Rate" & first 3 principal components
test <- data.frame(Graduation.rate = universities.complete.df$Graduation.rate, pcs.scores.cor[,1:2])
head(test)

# Create a linear model with Graduation rate as our dependent variable & putting rest all as independent variables
model_pcs <- lm(formula = Graduation.rate~.,data = test)

# Using first 2 principal components we were able to achieve 44.25% R^2 
summary(model_pcs)

# Search for best linear model (without PCA)
install.packages("leaps")
library(leaps)
search <- regsubsets(Graduation.rate ~ ., data = universities.complete.df, nbest = 1, nvmax = 10, method = "exhaustive")
sum <- summary(search) 
# show models 
sum$which 
# show metrics 
sum$rsq 
sum$adjr2
