rm(list = ls())

wd <- "C:/Users/gbal/Desktop/bal.2018.ecolmodel.density.dependence"
setwd(wd)

# install package from local folder, THIS IS NOT AN OFFICIAL R PACKAGE !
#install.packages('AHSDD_1.0.tar.gz', repos = NULL)

# load package
require(AHSDD)

# load data used in bal et al 2017
chinook.data <- read.table('chinook.data.bal.et.al.2017.txt' , h = T)

# fit models for one population ===============================================================================

# choose pop (1 to 55)
pop.chosen <- 15

# chose n.draws kept by SIR algorithms
n.draws <- as.integer(10000) # has to be integer

# create list to sotre fits of AHS model and nested submodels
fits <- list()

# run AHS model and nested version and store
fits$ahssir <- AhsSir(n.draws = n.draws ,
                      parents = chinook.data$parents[chinook.data$index == pop.chosen],
                      offsprings = chinook.data$offsprings[chinook.data$index == pop.chosen])
fits$hssir <- HsSir(n.draws = n.draws ,
                    parents = chinook.data$parents[chinook.data$index == pop.chosen],
                    offsprings = chinook.data$offsprings[chinook.data$index == pop.chosen])
fits$alsir <- AlSir(n.draws = n.draws,
                    parents = chinook.data$parents[chinook.data$index == pop.chosen],
                    offsprings = chinook.data$offsprings[chinook.data$index == pop.chosen])
fits$lsir <- LSir(n.draws = n.draws,
                  parents = chinook.data$parents[chinook.data$index == pop.chosen],
                  offsprings = chinook.data$offsprings[chinook.data$index == pop.chosen])

# compute relative credibilities of models
models.cred <- rep(NA, 4)
names(models.cred) <- names(fits)
for(m in 1:3){
  models.cred[m] <- (fits[[m]][n.draws, 'sum.L.n.models.tried'] / fits[[m]][n.draws, 'n.models.tried']) /
                      sum(sapply(1:4, function(i) fits[[i]][n.draws, 'sum.L.n.models.tried'] / fits[[i]][n.draws, 'n.models.tried']))
}
models.cred[4] <- 1 - sum(models.cred[1:3])
sum(models.cred) == 1 # should be true

barplot(models.cred)
