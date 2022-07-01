#### Table 4.10

setwd("~/My Courses/Data Mining/Datasets/DMBA-3eR-datasets")
cereals.df <- read.csv("Cereals.csv") 
head(cereals.df)
unique(cereals.df$shelf)
heatmap(cor(na.omit(cereals.df[,-c(1:3)])))
plot(na.omit(cereals.df[,c(4:12,16)]))

# compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
# head(pcs)
summary(pcs) 
pcs$rotation
norm(as.matrix(pcs$rotation[,1]), type="F")
scores <- pcs$x
head(scores, 5)
biplot(pcs)


#### Table 4.11

pcs <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)
round(cor(scores),2)


#### Table 4.12

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)
pcs.cor$rot
#scree plot
plot(pcs.cor)





