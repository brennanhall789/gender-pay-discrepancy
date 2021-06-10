## Preliminary models
# want to model compensation to find influence from gender
# Do not include components of totalComp, jobLevel is completely determined by other variables so it is removed
mNorm <- glm(log(totalComp) ~ .-jobLevel-baseRate-regHours-bonus-overtimeHours, data = data)
anova(mNorm)
summary(mNorm)
plot(mNorm, which=1);plot(mNorm, which=2);plot(mNorm, which=3)
car::vif(mNorm)

mNorm21 <- update(mNorm, .~. -department)
AIC(mNorm21)
mNorm22 <- update(mNorm, .~. -eeo1)
AIC(mNorm22)   ## aic of model without department is lower so prefer that
mNorm2 <- mNorm21
car::vif(mNorm2)   ## model has no multicollinearity

## Diagnostics for model mNorm2
#boot::glm.diag.plots(mNorm2)    ## model satisfies linearity and normality assumptions
mNorm.res.dev <- residuals(mNorm2, type = "response")
shapiro.test(mNorm.res.dev)     ## normality assumption not satisfied
plot(mNorm.res.dev, type='l')   ## model satisfies independence of the residuals
car::avPlots(mNorm2)
mNorm2.eta.hat <- predict(mNorm2, type = "link")
plot(mNorm.res.dev ~ mNorm2.eta.hat, main="Residuals vs Fitted values",
     xlab="Fitted values", ylab="Deviance Residuals")
lines(lowess(mNorm.res.dev ~ mNorm2.eta.hat), col="red", lty=2)

## no significant influence from gender term
anova(mNorm2)    


## Since regression is not a causal modeling technique
# I model probability of being Male as function of other variables.
# Then test if influence from compensation variables (baseRate, bonus, etc.) is non-zero
# Can use logistic (binomial) regression with the canonical logit link

# Remove totalComp since it is linear function of other variables 
m <- glm(gender ~ .-totalComp-jobLevel, data=data, family=binomial)
summary(m)
car::vif(m)
m21 <- update(m, .~. -department)
AIC(m21)
m22 <- update(m, .~. -eeo1)
AIC(m22)   ## aic of model without department is lower so prefer that
m2 <- m21
car::vif(m2)

## Diagnostics on model m2
# Dispersion parameter
Male <- ifelse(data$gender=='Male',1,0)
df <- summary(m2)$df.residual
mu.hat <- predict(m2, type = "response")
eta.hat <- predict(m2, type = "link")
V.fun <- function(p) {p*(1-p)}
V <- V.fun(mu.hat)
r <- (Male - mu.hat)/sqrt(V)
X <- sum(r^2)
(phi <- X/(df))    ## satisfies Binomial glm assumption of dispersion parameter==1

# check deviance residuals diagnostics
res.dev <- residuals(m2, type = "deviance")
plot(res.dev ~ eta.hat, main="Residuals vs Fitted values",
     xlab="Fitted values", ylab="Deviance Residuals")
lines(lowess(res.dev ~ eta.hat), col="red", lty=2)    ## linearity assumption satisfied
qqnorm(res.dev); qqline(res.dev, lty=2)
boot::glm.diag.plots(m2)
shapiro.test(res.dev)      ## asymptotic normality not reached (this is expected)
plot(res.dev, type='l')    ## error independence assumption satisfied

## no significant influence from gender
anova(m2)
summary(m2)

## Print tables of model coefficients
xtable::xtable(m2)
# also look at TBI Paper for multi model summary table