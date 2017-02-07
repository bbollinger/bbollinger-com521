
##PC1. Load the data into R. Now get to work on reshaping the dataset. I think a good format would be a data frame with two columns: group, time of death (i.e., lifespan). 
TOD <- c(owan03$X1, owan03$X2, owan03$X3, owan03$X4)

Group1 <- df[c(1:11),]
Group2 <- df[c(12:22),]
Group3 <- df[c(23:33),]
Group4 <- df[c(34:44),]

All.Groups <- cbind(Group1, Group2, Group3, Group4)
