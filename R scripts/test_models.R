## Conduct difference of means on predicted test data

## create train/test data sets for separate male and female data
set.seed(123)
idx <- sample(nrow(data),.9*nrow(data))
train <- data[idx,]
test <- data[-idx,]

## Train model
model <- glm(gender ~ .-jobLevel-totalComp-department, data=train, family=binomial)

summary(model)

## Conduct proportion test on accuracy of model.
# If the model predicted probabilities are statistically not different
# than the naive proportion of Male in the dataset (p=0.675) this indicates
# no influence on gender from other variables (particularly compensation)
nrow((filter(test,gender=="Male")))/nrow(test)
modelFitt <- predict(model, newdata = test, type = "response")
test <- test %>% mutate(model_pred = 1*(modelFitt > 0.675) + 0, 
                        gender_binary = 1*(gender == "Male") + 0)

test <- test %>% mutate(accurate = 1*(model_pred == gender_binary))

prop.test(sum(test$accurate), nrow(test),p=0.637)
## result shows 0.63 within 95% confidence interval
# indicating model probability is similar to naive probability
