# pheno is a function of genotype  and (if you want) random noise
# fitnes is a function of your phenoype, and the phenotypes of others'
n <- 100
season <- 10

inds    <- 1:n
geno    <- matrix(runif(200) ,ncol = 2)
switch.time   <- season * rowMeans(geno)

season.mates <- do.call(rbind,lapply(1:season, function(day){ #problem here in generation of season.mates?
  males         <- which(switch.time > day)
  females       <- which(switch.time < day)
  if(length(females) * length(males) == 0){return(data.frame(males = 0, females = 0) )}
  mated.males   <- sample(males, length(females), replace = TRUE) #or could replace be the issue here?
  mating.pairs  <- data.frame(males = mated.males, females = females) 
}))

season.mates <- season.mates[rowMeans(season.mates) != 0,]
mated <- season.mates[sample(1:nrow(season.mates), size = 100 ),]



plot(as.numeric(table(factor(season.mates[,"males"], levels = 1:100))), as.numeric(table(factor(season.mates[,"females"], levels = 1:100))))



sort(table(unlist(season.mates )))

# shows that individual 1 mates as a male 24 time, then as a female once, then as male 3 more tiems
# so, switches from male to female, and back to male. Can't do that.
subset(season.mates, males==2|females==2)

