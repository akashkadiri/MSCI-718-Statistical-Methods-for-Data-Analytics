Nb <- seq(60, 400)
Ng <- seq(40, 400)
numcomb <- length(Nb)*length(Ng)
lhood.list <- c()
multiple <- choose(100,60)*choose(100,20)
nb.ng.list <- vector(mode = "list", length = numcomb)
i <- 1
for (nb in Nb) {
  for (ng in Ng) {
    pb <- nb/(nb+ng)
    pg <- 1 - pb
    pt <- 100/(nb+ng)
    lhood <- multiple * pb^60 * pg^40 * pt^20 * (1-pt)^80
    lhood.list <- append(lhood.list, lhood)
    nb.ng.list[[i]] <- c(nb, ng)
    i <- i+1
  }
}
plot(lhood.list)
optimal.nb.ng <- nb.ng.list[[which.max(lhood.list)]]
optimal.nb.ng




nb <- 280
ng <- 220
multiple <- choose(100,60)*choose(100,20)
pb <- nb/(nb+ng)
pg <- 1 - pb
pt <- 100/(nb+ng)
lhood1 <- multiple * pb^60 * pg^40 * pt^20 * (1-pt)^80
lhood1
nb <- 290
ng <- 210
multiple <- choose(100,60)*choose(100,20)
pb <- nb/(nb+ng)
pg <- 1 - pb
pt <- 100/(nb+ng)
lhood2 <- multiple * pb^60 * pg^40 * pt^20 * (1-pt)^80
lhood2