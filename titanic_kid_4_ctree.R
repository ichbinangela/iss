library(party)

sample.n <- 1:40

for(i in sample.n){
  sample.kid <- titanic.kid[sample(nrow(titanic.kid), i, replace = TRUE), ]
  new.data <- rbind(sample.kid, titanic.adult)

  titanic.ctree <- ctree(survived ~ pclass + sex + parch + sibsp 
                         + embarked + fare + kid, data = new.data)
}
# Plot the tree.
plot(titanic.ctree)
