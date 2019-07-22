
polis.glm <- glm(PA~RATIO, family=binomial, data=polis)

# inspect the results
summary(polis.glm)

# Compare the fit and full models via Goodness-of-fit (G-test)
anova(polis.glm, test="Chisq") # Chi Square

# Calculate predicted values based on fitted model
xs <- seq(0,70,l=1000)
polis.predict <- predict(polis.glm, type="response", se=T, newdata=data.frame(RATIO=xs))

# Produce base plot
plot(PA~RATIO, data=polis, xlab="", ylab="", axes=F, pch=16)

# Plot fitted model and 95% CI bands
points(polis.predict$fit~xs, type="l", col="gray")
lines(polis.predict$fit+polis.predict$se.fit ~ xs, col="gray", type="l", lty=2)
lines(polis.predict$fit-polis.predict$se.fit ~ xs, col="gray", type="l", lty=2)

