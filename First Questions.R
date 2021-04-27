## Question 1
x1 <- seq(5,50,5)
n <- length(x1)
m1 <- matrix(0,n,1)
m2 <- matrix(0,n,1)
m3 <- matrix(0,n,1)

for(i in x1){
    tmp <- c(log(i), exp(i), i%/%13)
    print(tmp)
    m1[i/5,] <- tmp[1]
    m2[i/5,] <- tmp[2]
    m3[i/5,] <- tmp[3]
    
}

## Question 2
library(data.table)
salary <- fread("salary.csv")
sal_male <- subset(salary, Gender=="M")
sal_female <- subset(salary, Gender=="F")
sal_male_hist <- hist(sal_male$salary)
sal_female_hist <- hist(sal_female$salary)
exp_male <- hist(sal_male$Expr)
exp_female <- hist(sal_female$Expr)

## Question 3
library(PerformanceAnalytics)
statistics <- c('Mean', 'Median', 'Mode', 'Variance', 'Standard Deviation',
                'Range', '1st Quartile', '3rd Quartile', 'IQR',
                '10th percentile', '90th percentile', 'Skewness',
                'Excess Kurtosis')

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

stats_male <- c(mean(sal_male$salary), median(sal_male$salary), getmode(sal_male$salary),
           var(sal_male$salary), sd(sal_male$salary), max(sal_male$salary)-min(sal_male$salary),
           quantile(sal_male$salary, 0.25), quantile(sal_male$salary, 0.75),
           quantile(sal_male$salary, 0.75)-quantile(sal_male$salary, 0.25),
           quantile(sal_male$salary, 0.10), quantile(sal_male$salary, 0.90),
           skewness(sal_male$salary), kurtosis(sal_male$salary, method="excess"))

stats_female <- c(mean(sal_female$salary), median(sal_female$salary), getmode(sal_female$salary),
                var(sal_female$salary), sd(sal_female$salary), max(sal_female$salary)-min(sal_female$salary),
                quantile(sal_female$salary, 0.25), quantile(sal_female$salary, 0.75),
                quantile(sal_female$salary, 0.75)-quantile(sal_female$salary, 0.25),
                quantile(sal_female$salary, 0.10), quantile(sal_female$salary, 0.90),
                skewness(sal_female$salary), kurtosis(sal_female$salary, method="excess"))

final <- data.frame(statistics, stats_male, stats_female)

summary_stats <- function(data){
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    stats <- c(mean(data), median(data), getmode(data),
               var(data), sd(data), max(data)-min(data),
               quantile(data, probs=0.25)[[1]], quantile(data, probs=0.75)[[1]],
               quantile(data, probs=0.75)[[1]]-quantile(data, probs=0.25)[[1]],
               quantile(data, probs=0.10)[[1]], quantile(data, probs=0.90)[[1]],
               skewness(data), kurtosis(data, method="excess"))
    return(stats)
}
male_vec <- summary_stats(sal_male$salary)
male_vec
