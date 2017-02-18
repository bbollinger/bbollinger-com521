#PC0. Load up your dataset as you did in Week 3 PC2.
week3_dataset.beth <- read.csv("~/Desktop/Winter 2017/COM 521/bbollinger-com521/Wk 3/week3_dataset-beth.csv")
View(week3_dataset.beth)

#PC1. If you recall from Week PC6, x and y seemed like they linearly related. We now have the tools and terminology to describe this relationship and to estimate just how related they are. Run a t.test between x and y in the dataset and be ready to interpret the results for the class.
xv1 <- week3_dataset.beth$x
yv1 <- week3_dataset.beth$y
t.test(xv1, yv1)
#It looks as if this is statistically signficant at the .05 level, showing that there is a difference between the two groups

#PC2. Estimate how correlated x and y are with each other.
cor(xv1, yv1, use="everything", method = c("pearson"))
#It looks like they are highly correlated with each other, showing a Pearson's correlation of 0.815

#PC3. Recode your data in the way that I laid out in Week 3 PC7.
bethdata3i <- week3_dataset.beth$i
bethdata3i.f <- factor(bethdata3i, labels = c("FALSE", "TRUE"))
is.factor(bethdata3i.f)
levels(bethdata3i.f)

bethdata3j <- week3_dataset.beth$j
bethdata3j.f <- factor(bethdata3j, labels = c("FALSE", "TRUE"))
is.factor(bethdata3j.f)
levels(bethdata3j.f)

bethdata3k <- week3_dataset.beth$k
bethdata3k.f <- factor(bethdata3k, labels = c("None", "Some", "Lots", "All"))
is.factor(bethdata3k.f)
levels(bethdata3k.f)

#PC4. Generate a set of three linear models and be ready to intrepret the coefficients, standard errors, t-statistics, p-values, and R 2 for each:
#(a) y ^ = β 0 + β 1 x + ε 
ylm <- lm(yv1~xv1, data = week3_dataset.beth)
summary(ylm)
summary(ylm)$r.squared #calculating rsquared for the above model
#(b) y ^ = β 0 + β 1 x + β 2 i + β 3 j + ε 
ylm.ij <- lm(yv1~xv1+bethdata3i.f+bethdata3j.f, data = week3_dataset.beth)
summary(ylm.ij)
summary(ylm.ij)$r.squared #calculating rsquared for the above model
#(c) y ^ = β 0 + β 1 x + β 2 i + β 3 j + β k + ε 
finalylm <- lm(yv1~xv1+bethdata3i.f+bethdata3j.f+bethdata3k.f, data = week3_dataset.beth)
summary(finalylm)
summary(finalylm)$r.squared #calculating rsquared for the above model

#PC5. Generate a set of residual plots for the final model (c) and be ready to interpret your model in terms of each of these:
#(a) A histogram of the residuals.
plot(hist(resid(finalylm)))
#(b) Plot the residuals by your values of x, i, j, and k (four different plots).
xmodel <- lm(yv1~xv1, data = week3_dataset.beth)
plot(resid(xmodel))
imodel <- lm(yv1~bethdata3i.f, data = week3_dataset.beth)
plot(resid(imodel))
jmodel <- lm(yv1~bethdata3j.f, data = week3_dataset.beth)
plot(resid(jmodel))
kmodel <- lm(yv1~bethdata3k.f, data = week3_dataset.beth)
plot(resid(kmodel))
#(c) A QQ plot to evaluate the normality of residuals assumption.
qqnorm(resid(finalylm))

#QQ plot
finalylm.graph <- data.frame(x = yv1, residuals = residuals(finalylm))
finalylm.graph
library(ggplot2)
ggplot(data=finalylm.graph) + aes(x=xv1, y=residuals) +geom_point()

#PC6. Generate a nice looking publication-ready table with a series of fitted models and put them in a Word document.
library("ggplot2")
f1 <- plot(fitted(finalylm))
x1 <- plot(fitted(xmodel))
i1 <- plot(fitted(imodel))
j1 <- plot(fitted(jmodel))
k1 <- plot(fitted(kmodel))
install.packages("stargazer")
library("stargazer")
stargazer(finalylm, type = "text")

#PC7. Load up the dataset once again and fit the following linear models and be ready to interpret them similar to the way you did above in PC4:
install.packages("readstata13")
ObamaData=readstata13::read.dta13("~/Desktop/Winter 2017/COM 521/Homework/Week 7/Halloween2012-2014-2015_PLOS.dta")
View(ObamaData)
#(a) f r u i t ^ = β 0 + β 1 o b a m a + ε 
fruit <- ObamaData$fruit
obama <- ObamaData$obama
fruit.obama <- lm(fruit~obama, data = ObamaData)
summary(fruit.obama)
#(b) Add a control for age and a categorical version of a control for year to the model in (a).
age <- ObamaData$age
year <- as.factor(ObamaData$year)
control.fruit.obama <- lm(fruit~obama+age+year, data = ObamaData)
summary(control.fruit.obama)

#PC8. Take a look at the residuals for your model in (a) and try to interpret these as you would in PC4 above. What do you notice?
hist(resid(fruit.obama))
plot(resid(fruit.obama))
qqnorm(resid(fruit.obama))
#these plots look like it is violating a lot of rules -- abnormal looking!

#PC9. Run the simple model in (a) three times on three subsets of the dataset: just 2012, 2014, and 2015. Be ready to talk through the results.
d.2012 <- subset(ObamaData, year == 2012)
d.2014 <- subset(ObamaData, year == 2014)
d.2015 <- subset(ObamaData, year == 2015)

fruit.2012 <- d.2012$fruit
obama.2012 <- d.2012$obama
fruit.2014 <- d.2014$fruit
obama.2014 <- d.2014$obama
fruit.2015 <- d.2015$fruit
obama.2015 <- d.2015$obama

fruit.obama.2012 <- lm(fruit.2012 ~ obama.2012, data = d.2012)
summary(fruit.obama.2012)

fruit.obama.2014 <- lm(fruit.2014 ~ obama.2014, data = d.2014)
summary(fruit.obama.2014)

fruit.obama.2015 <- lm(fruit.2015 ~ obama.2015, data = d.2015)
summary(fruit.obama.2015)
#none of these show significance in their p-values




