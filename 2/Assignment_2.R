# MSCI 718, Assignment 2, Akash Kadiri(20925765)

# Q1. Simulate data x from rnorm(n = 50; mean = 0; sd = 2) and y from rnorm(n = 50; mean = 2 + 1:5*x; sd = 10) using set.seed(4).
set.seed(4)
x=rnorm(n=50,mean=0,sd=2)
y=rnorm(n=50,mean=1.5*x,sd=10)
model=lm(y~x)
model

# 1.a. Plot the data and add the fitted line.
plot(x,y) # plotting the value
abline(lm(y~x)) #adding the fitted line

# 1.b. Extract the p-value attached to the estimator beta 1 from summary().
summary(model) 
# p-value associated with the beta 1 is 0.0056 which is less than the level of significance 0.05 so we are rejecting the null hypothesis.

# 1.c. Repeat this exercise by generating another set of data. Keep the same structure of the data generating process but use set.seed(200).
set.seed(200)
x=rnorm(n=50,mean=0,sd=2)
y=rnorm(n=50,mean=1.5*x,sd=10)
model=lm(y~x)
model

plot(x,y) # plotting the value
abline(lm(y~x)) #adding the fitted line
summary(model)
# p-value of beta 1 is 0.285 which is greater than the level of significance 0.05 so we are accepting the null hypothesis 


#Q2. Use a binomial logistic regression to predict vote intention (y) using the indicator for being married (x1) and the state indicator (x2) as predictors.
data <- read.csv("voting_data.csv", header=TRUE, stringsAsFactors=FALSE)
print(head(data))

y <- data[, c('dem_vote')]
x_1 <- data[, c('marital_id')]
x_2 <- data[,c('state_id')]
df <- data.frame(y = y, x_1 = x_1, x_2 = x_2)
head(df)

bn_fit <- glm(y ~ x_1 + x_2, data = df, family = "binomial")
bn_fit

# Q3.a. Write a code to to calculate the number of ways the observed sequence
probability <-function(N,Kw,Kb,R,T){
  bagB <- c(rep("B",Kb), rep("W",Kw))
  sequence <- T
  countB <- length(which(sequence == "B"))
  countW <- length(which(sequence == "W"))
  hypothesis <- c()
  for(i in 0:N)
  {
    bagA <- c()
    totalWays <- c()
    combination <- c()
    sum_TotalWays <- as.integer(0)
    bagA <- c(rep("B", (N-i)), rep("W", i))
    combination <- unique(combn(bagA, R), MARGIN = 2)
    
    for(j in 1:ncol(combination))
    {
      n_bagB <- c()
      n_bagBb <- c()
      n_bagBw <- c()
      n_bagB <- append(n_bagB, bagB)
      n_bagB <- append(n_bagB, combination[,j])
      
      for(k in 1:length(n_bagB))
      {
        if(n_bagB[k] == "B")
          n_bagBb <- append(n_bagBb, n_bagB[k])
        else
          n_bagBw <- append(n_bagBw, n_bagB[k])
      }
      if(length(n_bagBw) == 1)
        totalWays <- append(totalWays, ncol(combn(n_bagBb, countB)) * 1)
      else if(length(n_bagBb) == 1)
        totalWays <- append(totalWays, 1 * ncol(combn(n_bagBw, countW)))
      else if(length(n_bagBb) == countB)
        totalWays <- append(totalWays, 1 * ncol(combn(n_bagBw, countW)))
      else if(length(n_bagBw) == countW)
        totalWays <- append(totalWays, ncol(combn(n_bagBb, countB)) * 1)
      else
        totalWays <- append(totalWays, ncol(combn(n_bagBb, countB)) * ncol(combn(n_bagBw, countW)))
    }
    sum_TotalWays <- sum(totalWays)
    hypothesis <- append(hypothesis, sum_TotalWays)
  }
  return(hypothesis)
}

#Q3.b. Which hypothesis is most likely when N = 20;Kw = 10;Kb = 10;R = 5 and the observed sequence is [W;B;B;W;B;W]
probability_result<-probability(N=20,Kw=10,Kb=10,R=5,T=c("W", "B", "B", "W", "B", "W"))
probability_result