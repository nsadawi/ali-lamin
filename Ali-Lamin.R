# http://datascienceplus.com/perform-logistic-regression-in-r/
data <- read.csv('~/Downloads/UCL.csv',header=T)
data$Group[data$Group == 1] <- 0
data$Group[data$Group == 2] <- 1


s = as.character(data$gender) 
s[s == "M"] <- 1
s[s == "F"] <- 0
data$gender = as.numeric(s)

data = data[, !(colnames(data) %in% c("Hosp.No","DOB","Basline.visit.date","Eye","X"))]

data$BY1.count.diff <- data$Y1.count - data$B.count
data$Y1Y2.count.diff <- data$Y2.count - data$Y1.count
group1 = data[data$Group == 1,]
group0 = data[data$Group == 0,]
#wilcox.test(group0$BY1.count.diff , group0$Y1Y2.count.diff,paired=TRUE)
#wilcox.test(group1$BY1.count.diff , group0$BY1.count.diff,paired=FALSE)
wilcox.test(group0$Y1Y2.count.diff , group1$Y1Y2.count.diff,paired=FALSE)


data$BY1.area.diff <- data$Y1.area - data$B.area
data$Y1Y2.area.diff <- data$Y2.area - data$Y1.area
group1 = data[data$Group == 1,]
group0 = data[data$Group == 0,]
#wilcox.test(group0$BY1.area.diff , group0$Y1Y2.area.diff,paired=TRUE)
#wilcox.test(group1$BY1.area.diff , group0$BY1.area.diff,paired=FALSE)
wilcox.test(group0$Y1Y2.area.diff , group1$Y1Y2.area.diff,paired=FALSE)

data$BY1.vol.diff <- data$Y1.volume - data$B.volume
data$Y1Y2.vol.diff <- data$Y2.volume - data$Y1.volume
group1 = data[data$Group == 1,]
group0 = data[data$Group == 0,]
#wilcox.test(group0$BY1.vol.diff , group0$Y1Y2.vol.diff,paired=TRUE)
#wilcox.test(group1$BY1.vol.diff , group0$BY1.vol.diff,paired=FALSE)
wilcox.test(group1$Y1Y2.vol.diff , group0$Y1Y2.vol.diff,paired=FALSE)



## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

             


group1 = data[data$Group == 1,]
group0 = data[data$Group == 0,]

wilcox.test(group1$B.count , group1$Y1.count,paired=TRUE)


#train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#train <- data[train_ind, ]
#test <- data[-train_ind, ]

#model <- glm(Group ~ Age + Y2.count + Y2.area + Y2.volume, family=binomial(link='logit'),data=data)

model <- glm(Group ~ Age + Y2.count + Y2.volume, family=binomial(link='logit'),data=data)

#B.count * B.area * B.volume * Y1.count * Y1.area * Y1.volume * Y2.count * Y2.area
anova(model, test="Chisq")

summary(model)







library(caret)
## set the seed to make your partition reproductible
set.seed(11)
train.index <- createDataPartition(data$Group, p = .7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]

model <- glm(Group ~ Age + Y2.count + Y2.volume, family=binomial(link='logit'),data=train)
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



