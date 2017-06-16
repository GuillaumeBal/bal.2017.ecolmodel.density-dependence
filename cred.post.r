models.cred.2 <- rep(NA, 4)
names(models.cred.2) <- names(fits)
for(m in 1:3){
  models.cred.2[m] <- mean(exp(fits[[m]][ , 'll'])) /
    sum(sapply(1:4, function(i) mean(exp(fits[[i]][ , 'll']))))
}
models.cred.2[4] <- 1 - sum(models.cred.2[1:3])
sum(models.cred.2) == 1 # should be true

barplot(models.cred.2, ylim = c(0, 1))

head(fits$ahssir)
hist(fits$ahssir[ , 'k'])
hist(fits$hssir[ , 'k'])

quantile(fits$ahssir[ , 'k'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
quantile(fits$hssir[ , 'k'], probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

hist(fits$ahssir[ , 'beta1'])
