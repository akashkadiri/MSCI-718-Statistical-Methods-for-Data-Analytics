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