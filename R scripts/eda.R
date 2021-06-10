## Preliminary EDA for variable association with 
# variables relating to pay gap, eg. totalComp, promotion, bonus

dataMale <- filter(data, gender=='Male')
dataMale <- dataMale[,-3]
dataFemale <- filter(data, gender=='Female')
dataFemale <- dataFemale[,-3]

summary(dataMale)
summary(dataFemale)

gComp <- ggplot(data, aes(log(totalComp),factor(gender)))
gComp + geom_violin() + 
  labs(title="Violin plot of Total Compensation per Gender",
       x="Total Compensation", y="Gender")


## Test for significant difference between male/female variables
# assume CLT is applicable due to large sample size

# proportion of promotion
prop.test(x=c(sum(dataMale$promotion), sum(dataFemale$promotion)),n=c(nrow(dataMale),nrow(dataFemale)))
# indicates no significant difference in rate of promotion between genders

# bonus percentage
t.test(dataMale$bonus, dataFemale$bonus)
# indicates no significant difference b/w bonuses for male vs female

# kruskal-wallis test for testing equal distribution
# used to equivalently test difference in median of totalComp
kruskal.test(list(dataMale$totalComp, dataFemale$totalComp))
# indicates no significant difference b/w totalComp for male vs female

# baseRate
t.test(dataMale$baseRate, dataFemale$baseRate)
# indicates no significant difference b/w baseRate for male vs female

# regHours
t.test(dataMale$regHours, dataFemale$regHours)
# indicates no significant difference b/w regHours for male vs female

# overtimeHours
t.test(dataMale$overtimeHours, dataFemale$overtimeHours)
# indicates no significant difference b/w totalComp for male vs female


