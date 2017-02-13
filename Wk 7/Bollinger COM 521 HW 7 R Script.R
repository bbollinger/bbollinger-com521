##PC1. Download this dataset in Stata DTA format which contains an anonymized and reduced version of the data visualized in the Buechley and Hill paper on Lilypad. Once you have it:
lilypad_anonymized <- read.table("~/Downloads/lilypad_anonymized.dta", header=TRUE, quote="\"")
View(lilypad_anonymized)
library(foreign)
data=read.dta("~/Downloads/lilypad_anonymized.dta")
LPdata <- data

#(a) Reproduce both Table 1 and Table 2 (just US users) using the dataset (as closely as possible).
table(LPdata$gender, LPdata$order_type)
gender.table <- table(LPdata$gender, LPdata$order_type)
gender.matrix <- matrix(c(1332, 7687, 890, 41, 250, 79, 91, 598, 367), nrow=3)
colnames(gender.matrix) <- c("Arduino Only", "Both", "Lilypad Only")
rownames(gender.matrix) <- c("Unknown", "Male", "Female")
gender.matrix

country.table <- table(LPdata$gender, LPdata$order_type, LPdata$country=="United States")
country.matrix <- matrix(c(890, 6724, 810, 17, 178, 61, 52, 382, 279), nrow=3)
colnames(country.matrix) <- c("Arduino Only", "Both", "Lilypad Only")
rownames(country.matrix) <- c("Unknown", "Male", "Female")
country.matrix

#(b) Run a χ 2 {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}-test on both tables. Compare to the paper. Did you reproduce it?
chisq.test(gender.matrix)
chisq.test(country.matrix)
#Yes, I reproduced it. A little bit off, but mostly small decimal wise.

#(c) Install the package "gmodels" and try to display the table using the function CrossTable(). This will give you output very similar to SPSS.
install.packages("gmodels")
library(gmodels)
gmodels::CrossTable(gender.matrix)
gmodels::CrossTable(country.matrix)

#(c) It's important to be able to import tables directly into your word processor without cutting and pasting individual cells. Can you export the output of your table? There are a bunch of functions you can use to do this. I used the "xtable" package but I think that write.table() and Excel would do the job just as well.
as.table(country.matrix)
write.csv(CrossTable(country.matrix), "Country Matrix.csv")

##PC2. At the Community Data Science Workshops we had two parallel afternoon sessions on Day 1. In my session, there were 42 participants. In Tommy Guy's session, there were only 19. The next week (Day 2), we asked folks to raise their hands if they had been in Tommy's session (14 did ) and how many had been in mine (31 did). There was clearly attrition in both groups! Was there more attrition in one group than another? Try answering this both with a test of proportions (prop.test()) and with a χ 2 {\displaystyle \chi ^{2}} {\displaystyle \chi ^{2}}. Compare your answers. Is there convincing evidence that there is a dependence between instructor and attrition?
session.matrix <- matrix(c(42, 19, 31, 14), nrow=2)
colnames(session.matrix) <- c("Day 1", "Day 2")
rownames(session.matrix) <- c("Hill Class", "Guy Class")
session.matrix

prop.test(session.matrix)
chisq.test(session.matrix)
#The difference, is that the chi square test had an x-squared of 3.0045e-31, while the prop.test had an x-squared of 1.4092e-30. They kept the same degrees of freedom and p-value, however. 
#Yes, there is convincing evidence, because the p-value is over 1 for both, it shows that they are not dependent on eachother (e.g., independence tests you always want a high p-value)

##PC3. Download this dataset that was just published on "The Effect of Images of Michelle Obama’s Face on Trick-or-Treaters’ Dietary Choices: A Randomized Control Trial." The paper doesn't seem to have even been published yet so I think the abstract is all we have. We'll come back to it again next week.
#(a) Download and import the data into R. I needed to install the "readstata13" package to do so.
install.packages("readstata13")
ObamaData=readstata13::read.dta13("~/Downloads/Halloween2012-2014-2015_PLOS.dta")

#(b) Take a look at the codebook if necessary. Recode the data on being presented with Michelle Obama's face and the data on whether or not kids picked up fruit. we'll leave it at that for now.
table(ObamaData$obama, ObamaData$fruit)
fruit.matrix <- matrix(c(593, 322, 185, 122), nrow=2)
colnames(fruit.matrix) <- c("No Fruit", "Took Fruit")
rownames(fruit.matrix) <- c("No Face", "Face Present")
fruit.matrix

#(c) Do a simple test on whether or not the two groups are dependent. Be ready to report those tests. The results in the paper will use linear regression. Do you have a sense, from your reading, why your results using the coding material we've learned might be different?
chisq.test(fruit.matrix)
#They are independent from eachohter, because the p-value was over .05 (i.e., they are only dependent if the p-value is below .05)