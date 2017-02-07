COS.Statistics.Top5000.Pages<- read.csv("~/Desktop/Winter 2017/COM 521/bbollinger-com521/COS-Statistics-Mobile_Sessions.csv")
COS.Statistics.Mobile_Sessions <- read.csv("~/Desktop/Winter 2017/COM 521/bbollinger-com521/COS-Statistics-Top5000-Pages.csv")

#PC2
set1 <- COS.Statistics.Mobile_Sessions
set2 <- COS.Statistics.Top5000.Pages

dim(set1) #looking at the dataset
dim(set2)
head(set1) #looking at the dataset
head(set2)
missing1 <- is.na(set1) #finding and looking at the missing data
as.numeric(missing1)
sum(length(which(is.na(set1))))
missing2 <- is.na(set2)
as.numeric(missing2)
sum(length(which(is.na(set2))))

set1[sample(nrow(set1), 10), ] #do this multiple times, with a different number in the 10 place, to get a new sample and sample size from this data
set2[sample(nrow(set2), 10), ]

#PC3
newdfset2 <- set2[, c(2,8),]
tapply(newdfset2$Pageviews, newdfset2$Month, sum)
sum.views <- tapply(newdfset2$Pageviews, newdfset2$Month, sum)
sum.set2 <- data.frame(Month1=names(sum.views), Total.Views=sum.views)

#PC4
newdfset1 <- set1[, c(1,2,8),]
tapply(newdfset1$Sessions, newdfset1$Month, sum)
sum.views1 <- tapply(newdfset1$Sessions, newdfset1$Month, sum)
sum.names1 <- tapply(newdfset1$Sessions, newdfset1$Operating_System, sum)
sum.set1 <- data.frame(Month=names(sum.views1), Total.Views=sum.views1)

#PC5
merge(sum.set1, sum.set2, by.x="Month", by.y="Month")
merge(sum.set1, sum.set2, by.x="Month", by.y="Month1", all.x=TRUE, all.y=TRUE)

#PC6
m.merge <- merge(sum.set1, sum.set2, by.x="Month", by.y="Month", all.x=TRUE, all.y=TRUE)
m.merge$percentage <- (m.merge$Total.Views.y/m.merge$Total.Views.x)*100

#PC7
library(ggplot2)
ggplot(data = m.merge) + geom_point() + aes(x=Month, y=percentage)