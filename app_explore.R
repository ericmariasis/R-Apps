# Dataset: 
#  https://www.kaggle.com/marlonferrari/elearning-student-reactions
library(ggplot2)
library(dplyr)
library(corrplot)
library(pROC)
library(ROCR)
library(Metrics)
library(popbio)

df = read.csv("online_classroom_data.csv")

# Clean
df$sk1_classroom =
  as.numeric(sub(",", ".", df$sk1_classroom, fixed = TRUE))
df$sk2_classroom =
  as.numeric(sub(",", ".", df$sk2_classroom, fixed = TRUE))
df$sk3_classroom =
  as.numeric(sub(",", ".", df$sk3_classroom, fixed = TRUE))
df$sk4_classroom =
  as.numeric(sub(",", ".", df$sk4_classroom, fixed = TRUE))
df$sk5_classroom =
  as.numeric(sub(",", ".", df$sk5_classroom, fixed = TRUE))
df$Approved = factor(df$Approved, levels = c(0,1))
str(df)
## Explore

# boxplot broken down by approval
ggplot(df, aes(x = Approved, y = total_posts)) +
  geom_boxplot() + 
  scale_y_continuous(trans="log10")

# median and IQR of approvals
df %>%
  group_by(Approved) %>%
  summarize(median(total_posts), IQR(total_posts))

ggplot(df, aes(x = Approved, fill = Approved)) +
  geom_bar() +
  ggtitle("Count of students by approval type")

# params (10,15,20,25,30 for bins)
ggplot(df, aes(x = nice_code_post, fill = Approved)) +
  geom_histogram(bins = 25) +
  facet_wrap(~ Approved) +
  scale_x_continuous(trans="log10")
  ggtitle("Count broken down by approval")

ggplot(df, aes(x = nice_code_post, fill = Approved)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(trans="log10")
  ggtitle("Density broken down by approval")

correlations = cor(df[,11:15])
corrplot(correlations, method = "circle")

# train and test set
set.seed(124)
sample_size = floor(0.85 * nrow(df))
train_ind = sample(seq_len(nrow(df)), size = sample_size)

train = df[train_ind, ]
test = df[-train_ind, ]

glmModel = glm(Approved ~ sk3_classroom, 
              data = train, 
              family = "binomial",
              control = list(maxit=200))

summary(glmModel)

# confidence interval summary
confint(glmModel)

anova(glmModel, test = 'Chisq')

test$modPredict = predict(glmModel, 
                     newdata = test, 
                     type = "response")

head(test)

test$origModPredict = test$modPredict
test$modPredict = ifelse(test$modPredict > 0.5, 1, 0)

# Correctness of model
mean(test$modPredict == test$Approved)

# Visualize
plot(train$sk1_classroom, train$Approved, xlab="sk1_classroom", ylab="P(Approved)")
trainLR = glm(Approved ~ sk1_classroom, data=train, family=binomial(link="logit"), control = list(maxit=200))
curve(predict(trainLR,data.frame(sk1_classroom=x),type="resp"),add=TRUE)
points(train$Approved,fitted(trainLR),pch=20)

# plot ROC
pred = predict(glmModel, test[,2:15], type = "response")
pObject = ROCR::prediction(pred, test$Approved )

rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
aucObj = ROCR::performance(pObject, measure="auc")  
plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4))) 