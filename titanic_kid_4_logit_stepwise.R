library(MASS)

selected <- vector()

set.seed(2221)

sample.n <- 1:40

for(i in sample.n){
  sample.kid <- titanic.kid[sample(nrow(titanic.kid), i, replace = FALSE), ]
  new.data <- rbind(sample.kid, titanic.adult)
  new.data <- na.omit(new.data)
  
  titanic.glm <- glm(survived ~ pclass + sex + parch + sibsp 
                   + embarked + fare + kid, data = new.data, family = "binomial")

  glm.stepAIC <- step(titanic.glm, direction=c("both"))

  # Check whether "kid" is selected in the model
  selected[i] <- grepl("kid", glm.stepAIC$call[2])
}

selected
rm(selected)

