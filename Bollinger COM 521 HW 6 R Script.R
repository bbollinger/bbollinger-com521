##PC0. Download the dataset from from this webpage. You'll find that the it's not in an ideal setup. It's an Excel files (XLS) with a series of columns labeled X1.. X4. The format is not exactly tabular. Take a look.
owan03 <- read.csv("~/Downloads/owan03.csv", header=TRUE) 
View(owan03) #I first downloaded the XLS file and then saved it as a CSV on my computer. Then I imported the CSV file into R to make it easier. I needed to make the header=TRUE in order to get rid of the extra line.

##PC1. Load the data into R. Now get to work on reshaping the dataset. I think a good format would be a data frame with two columns: group, time of death (i.e., lifespan). 
TOD <- c(70, 77, 83, 87, 92, 93, 100, 102, 102, 103, 96, 49, 60, 63, 67, 70, 74, 77, 80, 89, 30, 37, 56, 65, 76, 83, 87, 90, 94, 97, 34, 36, 48, 48, 65, 91, 98, 102)
Group <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4 )
df <- data.frame(Group, TOD)

#PC2. Create summary statistics and visualizations for each group. Write code that allows you to generate a useful way to both (a) get a visual sense both for the shape of the data and its relationships and (b) the degree to which the assumptions for t-tests and ANOVA hold. What is the global mean of your dependent variable?
group1 <- df$TOD[which(df$Group=="1")]
group2 <- df$TOD[which(df$Group=="2")]
group3 <- df$TOD[which(df$Group=="3")]
group4 <- df$TOD[which(df$Group=="4")]

summary(group1)
summary(group2)
summary(group3)
summary(group4)

hist(TOD)
hist(group1)
hist(group2)
hist(group3)
hist(group4)

mean(df$TOD)
#PC3. Do a t-test between mice with any RD40 and mice with at least a small amount. Run a t-test between the group with a high dosage and control group. How would you go about doing itusing formula notation? Be ready to report, interpret, and discuss the results in substantive terms.

all <- c(group2, group3, group4)
low <- c(group2)
control <- c(group1)
high <- c(group4)

t.test(low, all)
t.test(high, control)
t.test(high, control, data=df)

#PC4. Run an anova using aov() to see if there is a difference between the groups. Be ready to report, interpret, and discuss the results in substantive terms.

TOD.model <- aov(formula = TOD ~ Group, data = df)
summary(TOD.model)