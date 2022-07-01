UB_R <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-spring/STAT 4440/HW 6/UB_R.csv")


install.packages("e1071")
library(e1071)
options(scipen=999)

UB_R <- UB_R[,-c(1,9)]
UB_R <- UB_R[,-c(1,2,3,4,5,6,7,9,10)]

ub.nb <- naiveBayes(UB_R$Personal.Loan~UB_R$Online+UB_R$CreditCard, data = UB_R, laplace=0.000)
ub.nb

Newloans.df <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-spring/STAT 4440/HW 6/UB_Classify.csv")

pred.prob <- predict(ub.nb, newdata = Newloans.df, type = "raw")
pred.prob
