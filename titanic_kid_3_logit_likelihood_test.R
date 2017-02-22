set.seed(2221)

sample.n <- 1:40

lrt.p <- vector()
wald.p <- vector()
wald.est <- vector()
wald.std <- vector()

for(i in sample.n){
  sample.kid <- titanic.kid[sample(nrow(titanic.kid), i, replace = TRUE), ]
  new.data <- rbind(sample.kid, titanic.adult)
  
  titanic.glm <- glm(survived ~ pclass + sex + parch + sibsp 
                 + embarked + fare + kid, data = new.data, family = "binomial",  na.action = "na.fail")
  
  summary(titanic.glm)
  
  wald.p[i] <- round(coef(summary(titanic.glm))[,4],5)
  
  titanic.glm.no.kid <- glm(survived ~ pclass + sex + parch + sibsp 
                     + embarked + fare, data = new.data, family = "binomial")
  
  summary(titanic.glm.no.kid)
  
  # likelihood ratio test
  anova(titanic.glm, titanic.glm.no.kid, test="LRT")
  
  # results
  wald.est[i] <- round(coef(summary(titanic.glm))["kid1",1],5)
  wald.std[i] <- round(coef(summary(titanic.glm))["kid1",2],5)
  wald.p[i] <- round(coef(summary(titanic.glm))["kid1",4],5)
  lrt.p[i] <- round(anova(titanic.glm, titanic.glm.no.kid, test="LRT")[2,"Pr(>Chi)"], 5)
}

wald.est
wald.std
wald.p
lrt.p

# Line Chart of the result
kid.total.rate <- (1:nrow(sample.kid))/nrow(new.data)
plot(kid.total.rate, wald.p, type = "l",main = "P-value of Wald Test", xlab = "kid-total rate", ylab = "p-value")

plot(kid.total.rate, lrt.p, type = "l", main = "P-value of LRT", xlab = "kid-total rate", ylab = "p-value")
abline(h = 0.05, col = "red", lty = 3)
text(0,0, "0.05", col = "red", adj = c(-14, -2.2))
