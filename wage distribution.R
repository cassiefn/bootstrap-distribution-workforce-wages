# read in data
teaching <- read.csv("teacherWages.csv", stringsAsFactors = FALSE)

# inspect data
hist(teaching$Hourly.Wage,
     xlab = "Hourly Wage",
     main = "Histogram of Early Childhood Teacher \n Hourly Wage")
summary(teaching$Hourly.Wage)

# nonparametric bootstrap of mean
set.seed(10)
m = 999
xbar_non = mean(sample(teaching$Hourly.Wage, replace = T))

for(i in 1:m){
  xsamp_mean = sample(teaching$Hourly.Wage, replace = T)
  xbar_non = c(xbar_non, mean(xsamp_mean))
}

hist(xbar_non, freq = F, breaks = 20, 
     main = "Nonparametric Bootstrap of Mean")
summary(xbar_non)

# nonparametric bootstrap of median
set.seed(10)
m = 999
med_non = median(sample(teaching$Hourly.Wage, replace = T))

for(i in 1:m){
  xsamp_med = sample(teaching$Hourly.Wage, replace = T)
  med_non = c(med_non, median(xsamp_med))
}

hist(med_non,freq = F, breaks = 10, 
     main = "Nonparametric Bootstrap of Median")
summary(med_non)

length(xbar_non[12.07 <= xbar_non]) / length(xbar_non)
length(med_non[12.07 <= med_non]) / length(med_non)

# confidence intervals
CI.boot.mean = quantile(xbar_non,probs = c(0.025, 0.975))
CI.boot.mean

CI.boot.med = quantile(med_non,probs = c(0.025, 0.975))
CI.boot.med

# standard deviations
sd(xbar_non)
sd(med_non)

# variances
var(xbar_non)
var(med_non)

# mean bias
12.30-12.30

# median bias
11.19-11.15
