first.layer <- vector()

# Extract the first (top) layer no.
for (i in 1:length(pruned.results.level)){
  splitted<- strsplit(pruned.results.level[i],',')
  first.layer[i] <- splitted[[1]][1]
}

kid.total.rate <- (1:nrow(sample.kid))/nrow(new.data)

plot(kid.total.rate, first.layer, type = "l", xlab = "kid-total rate", 
     ylab = "depth of kid", ylim = c(5,1), main = "Layer # of kid as kid-total rate increases")
