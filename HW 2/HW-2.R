Universities <- read.csv("D:/UNDERGRADUATE_TJPU & ELS & BGSU/Senior-spring/STAT 4440/HW2/Universities.csv")
#a
#Remove all categorical variables
U <- Universities[,-c(2,3)]
#Remove all records with missing numerical measurements
U2 <- na.omit(U)

#b
pcs.cor <- prcomp(U2[,-c(1)])
pcs.cor <- prcomp(U2[,-c(1)], scale. = T)
summary(pcs.cor)
pcs.cor$rot
pcs.scores.cor <- pcs.cor$x
diag(cov(pcs.scores.cor))

#scree plot
plot(pcs.cor)
