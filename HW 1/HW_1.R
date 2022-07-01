TC <- read.csv("C:/Users/ruigao/Downloads/ToyotaCorolla.csv")
#Problem 2.11
#a



#(b).(i)
#Delete "Model"
TC <- TC[,-2]

#Converting to dummies
install.packages("dummies")
library(dummies)

TC <- dummy.data.frame(TC, sep = ".")
names(TC)
#(ii)



#Problem 4.3.

#e.
TC2 <- read.csv("C:/Users/ruigao/Downloads/ToyotaCorolla.csv")
TC2 <- TC2[,-2]
TC2<- TC2[,-10]
TC2<- TC2[,-7]
TC2.cor <- cor(TC2)

#heatmap 
##simple heatmap
heatmap(cor(TC2[, c(2,3,4,5,6,7,10,14,15)]))

## heatmap with values
install.packages("gplots")
library(gplots)
heatmap.2(cor(TC2), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(TC2),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# alternative plot with ggplot
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)
cor.mat <- round(cor(TC2),2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


#matrix scatterplot
## simple plot
plot(TC2[, c(2,3,4,5,6,7,10,14,15)])
##alternative, nicer plot (displayed)
install.packages("gplots")
library(GGally)
ggpairs(TC2[, c(2,3,4,5,6,7,10,14,15)])
