#PC0
com521_population <- read.delim("~/Desktop/Winter 2017/COM 521/uwcom521-assignments_beth/week_05/com521_population.tsv")
mean(com521_population$x) #true population mean (41.87806)

week3_dataset.beth <- read.csv("~/Desktop/Winter 2017/COM 521/bbollinger-com521/week3_dataset-beth.csv") #download Week 3 dataset

#PC1
43.09519 #the mean from Week 2 data, sample mean
mean(com521_population$x) #the mean from true population (41.87806)
#a
sd(com521_population$x) #use to get numbers to plug in for the SE formula (42.0246)
length(com521_population$x) #use to get numbers to plug in for the SE formula (10000)
sqrt(10000) #you need to get the square root of N for SE forumula (100)
(42.0246/100) #SE for true population (0.420246)
SEdata2 <- (42.0246/100)
1.96*(SEdata2) #sd time SE for true population (0.8236822)
(41.87806 + 0.8236822) #high range of CI for true population (42.70174)
(41.87806 - 0.8236822) #low range of CI for true population (41.05438)
#true mean CI range: 41.05438 - 42.70174

sd(week3_dataset.beth$x) #use to get numbers to plug in for the SE formula (41.66937)
length(week3_dataset.beth$x) #use to get numbers to plug in for the SE formula (100)
sqrt(100) #you need to get the square root of N for SE forumula (10)
(41.66937/10) #SE for sample population (4.166937)
SEdata1 <- (41.66937/10)
1.96*(SEdata1) #sd times SE for sample population (8.167197)
(43.09519 + 8.167197) #high range of CI for true population (51.26239)
(43.09519 - 8.167197) #low range of CI for true population (34.92799)
#sample mean CI range: 34.92799 - 51.26239

#b
t.test(com521_population$x) #true population CI with R (41.05430 - 42.70183)
t.test(week3_dataset.beth$x) #sample population CI with R (34.82708 - 51.36330)
#At first guess, I'm thinking it is because R is not assuming a normal distribution for t, versus our hand calculation do, which is why they are slightly different. 

#c
43.09519 #the mean from sample population
41.87806 #the mean from true population 
34.92799 - 51.26239 #sample mean CI range
41.05438 - 42.70174 #true mean CI range
#This shows that my sample mean was actually above my true popoulation mean, fitting the true mean within the sample population mean estimate. Additionally, the sample CI range was a bit too low and high to match my true CI range. However, for my true population CI, it did fit within my sample CI range. 
#The true mean was within both my true population CI and my sample population CI. 

#PC2
hist(com521_population$x)
hist(week3_dataset.beth$x)
#According to my histograms, my true population has a more normal and even distribution, however, they are both similar to eachother.
range(com521_population$x)
range(week3_dataset.beth$x)
median(com521_population$x)
median(week3_dataset.beth$x)
sd(com521_population$x)
sd(week3_dataset.beth$x)
#It is interesting in these descriptive statistics that the range of data represented is significantly smaller (sample population only ranging from -40 to 173, while true population ranges from -131 to 232), however, their medians are almost exactly the same. Additionally, for such a different range from one another, they still capture fairly similar SD's, showing that they are not too far from one another, even with significantly less data to work from in the sample population versus the true population.

#PC3
mean(com521_population$y) #mean of y from true population (241.5074)
mean(week3_dataset.beth$y) #mean of y from sample poopulation (259.2561)

t.test(com521_population$y) #CI of y from true population (237.3742 - 245.6047)
t.test(week3_dataset.beth$y) #CI of y from sample population (220.2196 - 298.2925)
#The CI from my sample does contain the CI from my true population. 

#PC4
#a
x.population <- runif(n=10000, min=0, max=9) #I created a vector that used rnorm, inputing n as 10,000 variablees that fell within the range of 0 and 9.
#b
mean(x.population) #I then took the mean of the vector (4.466529)
hist(x.population) #histogram of the vector
#c
find.sample <- function(i) {
  my.sample <- mean(sample(x.population, 2))
} #I created a function to take 2 mean items from my x.population 
sapply(rep(1, 100), find.sample) #I then took sapply to take 100 samples from x.population 

#d
find.sample <- function(i) {
  my.sample <- mean(sample(x.population, 10))
} #I created a function to take 10 mean items from my x.population 
sapply(rep(1, 100), find.sample) #I then took sapply to take 100 samples in the function
samp10 <-sapply(rep(1, 100), find.sample) #created a new vector for this function
hist(samp10) #histogram of this vector

find.sample <- function(i) {
  my.sample <- mean(sample(x.population, 100))
} #I created a function to take 100 mean items from my x.population 
sapply(rep(1, 100), find.sample) #I then took sapply to take 100 samples in the function
samp100 <-sapply(rep(1, 100), find.sample) #created a new vector for this function
hist(samp100) #histogram of this vector
#You can see that the histogram has higher numbers available in the larger sample size, as well as has a more grouped together range of numbers. 

#PC5
x.population2 <- rnorm(n=10000, mean=4.466529, sd=2.587295) #I used rnorm, and then used the mean and SD from x.population to keep these two samples the same, just using different rnorm/runif functions
mean(x.population2) #I then took the mean of the vector (4.45315)
hist(x.population2) #histogram of the vector

find.sample2 <- function(i) {
  my.sample2 <- mean(sample(x.population2, 2))
} #I created a function to take 2 mean items from my x.population2 
sapply(rep(1, 100), find.sample2) #I then took sapply to take 100 samples from x.population 

find.sample2 <- function(i) {
  my.sample2 <- mean(sample(x.population2, 10))
} #I created a function to take 10 mean items from my x.population2 
sapply(rep(1, 100), find.sample2) #I then took sapply to take 100 samples in the function
2samp10 <-sapply(rep(1, 100), find.sample2) #created a new vector for this function
hist(samp10) #histogram of this vector

find.sample2 <- function(i) {
  my.sample2 <- mean(sample(x.population2, 100))
} #I created a function to take 100 mean items from my x.population2 
sapply(rep(1, 100), find.sample2) #I then took sapply to take 100 samples in the function
2samp100 <-sapply(rep(1, 100), find.sample2) #created a new vector for this function
hist(samp100) #histogram of this vector
#My results are not that different, which makes sense, because the means were almost the same when I started. I am not sure if I did this question right, but it makes sense to me that they came out the same, but slightly different means, because of the difference in functions applied, but I told R to use the same data, therefore, giving similar histogram distributions. 