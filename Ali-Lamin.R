# http://datascienceplus.com/perform-logistic-regression-in-r/
data <- read.csv('~/Downloads/UCL.csv',header=T)
data$Group[data$Group == 2] <- 0

data = data[, !(colnames(data) %in% c("Hosp.No","DOB","Basline.visit.date","Age","gender","Eye","X"))]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

             "Y2.volume"
library(caret)
## set the seed to make your partition reproductible
set.seed(11)
train.index <- createDataPartition(data$Group, p = .7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]


#train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#train <- data[train_ind, ]
#test <- data[-train_ind, ]

model <- glm(Group ~ Y2.count * Y2.area * Y2.volume,family=binomial(link='logit'),data=train)

#B.count * B.area * B.volume * Y1.count * Y1.area * Y1.volume * Y2.count * Y2.area
anova(model, test="Chisq")

summary(model)


fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Group)
print(paste('Accuracy',1-misClasificError))


library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Group)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

group1 = data[data$Group == 1,]
group0 = data[data$Group == 0,]

wilcox.test(group1$B.count ~ group0$B.count, data=mtcars)
