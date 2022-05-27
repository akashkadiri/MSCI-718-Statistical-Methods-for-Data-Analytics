# Q2) There is an urn with 2 black balls (B), 3 white balls (W),
      # and 3 red balls (R).
simple_urn <- c(rep(1, 2), rep(2, 3), rep(3, 3))
n_trial <- 10^4
w_replacement <- matrix(NA, nrow = n_trial, ncol = length(simple_urn))
wo_replacement <- matrix(NA, nrow = n_trial, ncol = length(simple_urn))

for (i in 1:n_trial) {
  w_replacement[i,] <- sample(simple_urn, length(simple_urn), replace = TRUE)
  wo_replacement[i,] <- sample(simple_urn, length(simple_urn), replace = FALSE)
}

  # we want sequence were Black is first and White is last
success_fun <- function(x){
  target_seq <- 1:3
  success <- c()
  # First and last balls taken out are observed and if the first is black
  # and last is White the success is assigned as 1 or else its 0
  if(x[1] == target_seq[1] && x[8] == target_seq[2]){
    total_success <- 1
  }
  else{
    total_success <- 0
  }
  # total number of times this happens is
  return(total_success)
}

w_replacement_result <- apply(w_replacement, 1, function(x) success_fun(x))
wo_replacement_result <- apply(wo_replacement, 1, function(x) success_fun(x))
# (2a) What is the probability of observing B first and W last when we draw without
      #replacement
(length(w_replacement_result) - sum(w_replacement_result == 0))/
  length(w_replacement_result)
# (2b) What is the probability of observing B first and W last when we draw without
      #replacement
(length(wo_replacement_result) - sum(wo_replacement_result == 0))/
  length(wo_replacement_result)




# Q3 The initial composition of colours in the urn is 20
#black, 10 white, 7 red, and 7 green. When a ball of a certain colour is picked, we remove
#another ball of the same colour from the urn.

polya_run <- function(int_n, lambda, n_trial){
  res <- matrix(NA, nrow = n_trial, ncol = length(int_n))
  res[1,] <- int_n
  
  for(i in 2:n_trial){
    interval <- cumsum(res[i-1,])/sum(res[i-1,])
    u <- runif(1)
    bin_assign <- c(interval > u)*1
    bin_select <- c() 
    
    for(j in 1:length(bin_assign)){
      if(bin_assign[j] == 1){
        bin_select <- j
        break
      }
    }
    
    res[i,] <- res[i-1,]
    if( res[i, bin_select]==1){
      lamba = 1
    }
    else if ( res[i, bin_select]==0){
      lamba = 0
    }
    else{
      res[i, bin_select] <- res[i, bin_select] - lambda
    }
  }
  return(res)
}
# (3a)Simulate the composition of colors in each draw for 20 trials
set.seed(100) 
pr1 <- polya_run(c(20,10,7,7), 2, 20)
# (3b)Repeat this experiment 3 more times (with different set.seed)
set.seed(200)
pr2 <- polya_run(int_n = c(20,10,7,7), lambda = 2, n_trial = 20)
set.seed(300)
pr3 <- polya_run(int_n = c(20,10,7,7), lambda = 2, n_trial = 20)
set.seed(400)
pr4 <- polya_run(int_n = c(20,10,7,7), lambda = 2, n_trial = 20)

#Plot
list_polya <- list(pr1, pr2, pr3, pr4)
colorslist <- c('#030ffc','#0af50a','#fcb603','#e0091f')

par(mfrow = c(2,2))
par(mar = c(1, 1, 1, 1))
plot(Time,Occurance)
for(i in 1:length(list_polya)){
  y_min <- min(list_polya[[i]])
  y_max <- max(list_polya[[i]])
  plot(1, type="n", ylim= c(y_min, y_max), xlim=c(1,20), xlab='Time', ylab='Number of occurences')
  for(j in 1:4){
    lines(1:20, list_polya[[i]][,j], col=colorslist[j], lwd=1.5)
  }
}


# Q4.MLE for normal distribution
# 4(a)Generate 50 random numbers with set.seed(200) from the normal distribution
set.seed(200)
sample = rnorm(50,0,2)
prod(dnorm(sample))

# 4(b)Given this synthetic data in (a), estimate using the MLE. Use optim() or nlm() function. Check whether the estimated parameters are close to the actual
# ones
likelihoodNLFunc = function(pars, data) {
  # parameters
  mu = pars[1] #- mean
  sigma = pars[2] #- standard deviation
  # x - set of observations. Should be initialized before MLE
  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}

#estimated
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = likelihoodNLFunc, data = sample,
            control = list(parscale = c(mu = 0.2, sigma = 1.5)))
mle$par

#actual
c(mu = mean(sample), sigma = sd(sample))


# 4(c) Repeat this exercise with 5000 generated numbers instead of 50 numbers with
# set.seed(200). What difference can you tell?
set.seed(200)
sample2 = rnorm(5000,0,2)
prod(dnorm(sample))

#estimated
mle2 = optim(par = c(mu = 0.2, sigma = 1.5), fn = likelihoodNLFunc, data = sample2,
             control = list(parscale = c(mu = 0.2, sigma = 1.5)))
mle2$par

#actual
c(mu = mean(sample2), sigma = sd(sample2))