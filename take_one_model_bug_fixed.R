# Mason flowering and switchover time 
# pheno is a function of genotype  and (if you want) random noise
# fitnes is a function of your phenoype, and the phenotypes of others'


# Take 1. No mutations. One season. Values from runif
n      <- 100
season <- 10

gens<-5


# Bug has been fixed
Sim1 <- function(n, season, gens){
  inds     <- 1:n
  geno     <- matrix(runif(200) ,ncol = 2)#diploid
  mean.var <- matrix(0, gens ,ncol = 2); 
  colnames(mean.var) <- c("mean","var")
  for(g in 1:gens){
    switch.time   <- season * rowMeans(geno)
    season.mates <- do.call(rbind,lapply(1:season, function(day){
      males         <- which(switch.time > day)
      females       <- which(switch.time < day)
      if(length(females) * length(males) == 0){return(data.frame(males = 0, females = 0) )}
      mated.males   <-  -1*sample(-males, length(females), replace = TRUE)
      mating.pairs  <- data.frame(males = mated.males, females = females)
    }))
    
    season.mates <- season.mates[rowMeans(season.mates) != 0,]
    mated <- season.mates[sample(1:nrow(season.mates), size = n),]
    dads  <- geno[mated[,"males"],]
    mums  <- geno[mated[,"females"],]
    genos <- cbind(apply(X = mums, MARGIN = 1, sample, 1),  apply(X = dads, MARGIN = 1, sample, 1)) 
    # there might be efficincies to be gained above
    
    mean.var[g,] <- c( mean(c(genos)), var(c(genos)))
    
    length(table((genos)))
    
  }
  return(mean.var)
  
}

#Sim1(n=100, season=10, gens = 50)
