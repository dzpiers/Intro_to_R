## Question 11
bottle_mean <- 303
bottle_sd <- 2
bottle_under <- 0.99*300

set.seed(123456)
bottle_sim <- rnorm(100, bottle_mean, bottle_sd)
sum(bottle_sim<bottle_under)

pnorm(bottle_under, bottle_mean, bottle_sd, lower.tail = TRUE)

## Question 12
bottle_over <- 310
bottle_sim <- rnorm(1000, bottle_mean, bottle_sd)
sum(bottle_sim>bottle_over)
1-pnorm(bottle_over, bottle_mean, bottle_sd, lower.tail = TRUE)

## Question 13
bottle_mean <- 303
target <- pnorm(bottle_under, bottle_mean, bottle_sd, lower.tail = TRUE)
while(target>0.0001){
    target <- pnorm(bottle_under, bottle_mean, bottle_sd, lower.tail = TRUE)
    target_mean <- bottle_mean
    bottle_mean = bottle_mean + 0.0001
}
print(target_mean)

normcdf_mean <- function(mu){
    ufill <- 0.99*300
    distance <- (pnorm(ufill,mean=mu,sd=2)-0.0001)^2
    return(distance)
}

solve <- optimize(normcdf_mean, c(290,310))
solve

## Question 1.2
i=1
draw=0
while(abs(draw)<3){
    draw=rnorm(1,0,1)
    if(i==1){
        record=draw
    } else if(abs(draw)<3){
        record=rbind(record,draw)
    }
    i=i+1
}

## Question 2.1
library(data.table)
stonks <- read.csv("stocks.csv")
stonks_stats <- matrix(0, 6, 3)
colnames(stonks_stats) <- c("AORDS", "BHP", "CBA")
rownames(stonks_stats) <- c("Mean", "Variance", "Standard Deviation", 
                            "Lower Bound", "Upper Bound", "p-value")
for(i in 2:ncol(stonks)){
    stonks_stats[1,i-1] <- mean(stonks[,i])
    stonks_stats[2,i-1] <- var(stonks[,i])
    stonks_stats[3,i-1] <- sd(stonks[,i])
    stonks_stats[4,i-1] <- mean(stonks[,i])-qnorm(0.975)*sd(stonks[,i])/sqrt(nrow(stonks))
    stonks_stats[5,i-1] <- mean(stonks[,i])+qnorm(0.975)*sd(stonks[,i])/sqrt(nrow(stonks))
    stonks_stats[6,i-1] <- (1-pnorm(mean(stonks[,i])/(sd(stonks[,i])/sqrt(nrow(stonks)))))
}
stonks_stats

## Question 3.1
salary <- read.csv("salary.csv")

fit <- lm(salary~Expr, data=salary)
summary(fit)

coef_CI <- function(level, fit){
    lbound=coef(fit)+qnorm((1-level)/2)*sqrt(diag(vcov(fit)))
    ubound=coef(fit)+qnorm(level+(1-level)/2)*sqrt(diag(vcov(fit)))
    return(c(lbound, ubound))
}

## Question 3.2
fit1 <- lm(salary~Expr+age, data=salary)
summary(fit1)

fit2 <- lm(salary~Expr+age+education, data=salary)
summary(fit2)
