set.seed(7)
num.sim <- 1
time.max <- 420
time.last <- 0
doc.free.time <- c(0, 0, 0)
num.patients <- 0
num.patients.wait <- 0
wait.time.list <- c()
while (time.last < time.max) {
  arrival.time <- time.last + rexp(1, rate = 1/10)
  num.patients <- num.patients + 1
  time.last <- arrival.time
  min.doc.free.time <- which.min(doc.free.time)
  wait.time <- max(c(0, doc.free.time[min.doc.free.time] - arrival.time))
  if (wait.time > 0) {
    num.patients.wait <- num.patients.wait + 1
    wait.time.list <- append(wait.time.list, wait.time)
  }
  doc.free.time[min.doc.free.time] <- arrival.time + wait.time + runif(1, 5, 20)
}
if (length(wait.time.list) == 0) {
  avg.wait.time <- 0
} else {
  avg.wait.time <- mean(wait.time.list)
}
office.close.time <- max(doc.free.time)

arrival.time
avg.wait.time
doc.free.time
min.doc.free.time
num.patients
num.patients.wait
num.sim
office.close.time
time.last
time.max
wait.time
wait.time.list



set.seed(7)
num.sim <- 1000
time.max <- 420
num.patients.list <- c()
num.patients.wait.list <- c()
avg.wait.time.list <- c()
office.close.time.list <- c()
for (i in 1:num.sim) {
  time.last <- 0
  doc.free.time <- c(0, 0, 0)
  num.patients <- 0
  num.patients.wait <- 0
  wait.time.list <- c()
  while (time.last < time.max) {
    arrival.time <- time.last + rexp(1, rate = 1/10)
    num.patients <- num.patients + 1
    time.last <- arrival.time
    min.doc.free.time <- which.min(doc.free.time)
    wait.time <- max(c(0, doc.free.time[min.doc.free.time] - arrival.time))
    if (wait.time > 0) {
      num.patients.wait <- num.patients.wait + 1
      wait.time.list <- append(wait.time.list, wait.time)
    }
    doc.free.time[min.doc.free.time] <- arrival.time + wait.time + runif(1, 5, 20)
  }
  if (length(wait.time.list) == 0) {
    avg.wait.time <- 0
  } else {
    avg.wait.time <- mean(wait.time.list)
  }
  office.close.time <- max(doc.free.time)
  num.patients.list <- append(num.patients.list, num.patients)
  num.patients.wait.list <- append(num.patients.wait.list, num.patients.wait)
  avg.wait.time.list <- append(avg.wait.time.list, avg.wait.time)
  office.close.time.list <- append(office.close.time.list, office.close.time)
}
median.num.patients <- median(num.patients.list)
median.num.patients.wait <- median(num.patients.wait.list)
median.avg.wait.time <- median(avg.wait.time.list)
median.office.close.time <- median(office.close.time.list)

median.avg.wait.time
median.num.patients
median.num.patients.wait
median.office.close.time