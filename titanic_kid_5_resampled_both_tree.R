# Resample both kids and adults
set.seed(2221)

sample.n <- 1:20
full.results.level <- vector()
pruned.results.level <- vector()
for(i in sample.n){
  sample.kid <- titanic.kid[sample(nrow(titanic.kid), (i*nrow(titanic.kid)), replace = TRUE), ]
  sample.adult <- titanic.adult[sample(nrow(titanic.adult), (i*nrow(titanic.adult)), replace = TRUE), ]
  new.data <- rbind(sample.kid, sample.adult)
  
  # (Almost) full tree
  fit <- rpart(survived ~ pclass + sex + parch + sibsp 
               + embarked + fare + kid, method = "class", data = new.data, control = rpart.control(cp = 0.0001, xval = 10))
  # prp(fit, faclen = 0, cex = 0.8, extra = 1)
  
  # Find best cp
  bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
  
  # Prune using best cp
  tree.pruned <- prune(fit, cp = bestcp)
  # prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)
  
  levels <- fit$frame[1:2]
  full.results.level[i] <- FindLevel("kid")
  
  levels <- tree.pruned$frame[1:2]
  pruned.results.level[i] <- FindLevel("kid")
}

full.results.level
pruned.results.level

full.results.level == pruned.results.level