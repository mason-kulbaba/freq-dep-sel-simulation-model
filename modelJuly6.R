
# pheno is a function of genotype  and (if you want) random noise
# fitnes is a function of your phenoype, and the phenotypes of others'
n <- 100
season <- 10

inds    <- 1:n
geno    <- matrix(runif(200) ,ncol = 2)
switch.time   <- season * rowMeans(geno)

season.mates <- do.call(rbind,lapply(1:season, function(day){ #problem here in generation of season.mates?
  my.females       <- which(switch.time < day)
  my.males         <- which(switch.time > day)
  if(length(my.females) * length(my.males) == 0){return(data.frame(day = day, males = 0, females = 0) )}
  mated.males   <- -1*sample(-my.males, length(my.females), replace = TRUE) 
  # this -1 * -thing is a hack to prevent r from doing evil things
  mating.pairs  <- data.frame(day = day, males = mated.males, females = my.females) 
  return( mating.pairs )
}))
season.mates <- season.mates[rowMeans(season.mates[,-1]) != 0,]
 #  plot(season.mates$day,switch.time[season.mates$females]) ;  points(1:10,1:10, col = "red")
 #  plot(season.mates$day,switch.time[season.mates$males]) ; points(1:10,1:10, col = "red")
mated <- season.mates[sort(sample(1:nrow(season.mates), size = 100 )),]
 #  plot(mated$day,switch.time[mated$females]) ; abline(h = 1:10) ; points(1:10,1:10, col = "red")
 #  plot(mated$day,switch.time[mated$males]) ; abline(h = 1:10); points(1:10,1:10, col = "red")



