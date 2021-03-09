# EBIO 338/538 Analysis and Visualization of Biological Data
# Class 11 Linear modeling continued - plotting multivariate results with confidence intervals

m1 <- lm(milk$kcal.per.g ~ milk$neocortex.perc)
summary(m1)

plot(milk$kcal.per.g ~ milk$neocortex.perc,
     col = "white", las = 1)
preds <- predict(m1, interval = 'confidence', level = 0.95)
CIs <- cbind(milk$neocortex.perc, preds)

preds.CIs <- CIs[order(CIs[ ,1]),]
lines(preds.CIs[,1], preds.CIs[ ,2],
      lwd = 3, col = "gray60")
lines(preds.CIs [,1], preds.CIs [ ,4], lwd =2, lty = 'dashed', col = 'gray')
lines(preds.CIs [,1], preds.CIs [ ,3], lwd =2, lty = 'dashed', col = 'gray')
points(milk$kcal.per.g ~ milk$neocortex.perc,
        pch = 16, cex = 1)


m2 <- lm(milk$kcal.per.g ~log(milk$mass))
summary(m2)

plot(milk$kcal.per.g ~log(milk$mass),
     col = "white", las = 1)
preds <- predict(m2, interval = 'confidence', level = 0.95)
CIs <- cbind(log(milk$mass), preds)
preds.CIs <- CIs[order(CIs[ ,1]),]
lines(preds.CIs[,1], preds.CIs[ ,2],
      lwd = 3, col = "gray60")
lines(preds.CIs [,1], preds.CIs [ ,4], lwd =2, lty = 'dashed', col = 'gray')
lines(preds.CIs [,1], preds.CIs [ ,3], lwd =2, lty = 'dashed', col = 'gray')
points(milk$kcal.per.g ~log(milk$mass),
       pch = 16, cex = 1)


kcal.per.g     <- milk$kcal.per.g
neocortex.perc <- milk$neocortex.perc
log.mass       <- log(milk$mass)

m3 <- lm(kcal.per.g ~ neocortex.perc + log.mass)
summary(m3)

#assess effect of neocortex.per

m4 <- lm(kcal.per.g ~ log.mass)
y <- resid(m4)   # relative milk quality

m5 <- lm(neocortex.perc ~ log.mass)
x <- resid(m5)   # relative neocortex per

plot(x, y)
m6 <- lm(y ~ x)

plot(y ~ x,
     col = "white", las = 1,
     xlab = "neocortex.perc",
     ylab = "kcal.per.g")
preds <- predict(m6, interval = 'confidence', level = 0.95)
CIs <- cbind(x, preds)
preds.CIs <- CIs[order(CIs[ ,1]),]
lines(preds.CIs[,1], preds.CIs[ ,2],
      lwd = 3, col = "gray60")
lines(preds.CIs [,1], preds.CIs [ ,4], lwd =2, lty = 'dashed', col = 'gray')
lines(preds.CIs [,1], preds.CIs [ ,3], lwd =2, lty = 'dashed', col = 'gray')



#assess effect of log(mass)

m4 <- lm(kcal.per.g ~ neocortex.perc)
y <- resid(m4) 

m5 <- lm(log.mass ~ neocortex.perc)
x <- resid(m5)  
plot(x, y)
m6 <- lm(y ~ x)

plot(y ~ x,
     col = "white", las = 1,
     xlab = "log(mass)",
     ylab = "kcal.per.g")
preds <- predict(m6, interval = 'confidence', level = 0.95)
CIs <- cbind(x, preds)
preds.CIs <- CIs[order(CIs[ ,1]),]
lines(preds.CIs[,1], preds.CIs[ ,2],
      lwd = 3, col = "gray60")
lines(preds.CIs [,1], preds.CIs [ ,4], lwd =2, lty = 'dashed', col = 'gray')
lines(preds.CIs [,1], preds.CIs [ ,3], lwd =2, lty = 'dashed', col = 'gray')

