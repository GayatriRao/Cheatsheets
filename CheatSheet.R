install.packages(package, repos="http://cran.us.r-project.org")

ANOVA:   

Within Ss:
aov_sum = summary(aov(DV~(IV)+Error(ErrTerm/(IV)))) 
Between Ss:
aov(DV~IV) 

Mixed:
fit <- aov(y~(W1*W2*B1*B2)+Error(Subject/(W1*W2))+(B1*B2), data=mydataframe)

nested: 
summary(aov(Centroid~Role*Speaker*Task*Sex+Error(token/Role/Speaker/Task/Sex)))

Post-Hoc:
pairwise.t.test(f_dat$centroid, f_dat$speaker, p.adj = "bonf")

interaction.plot(m_dat$task, m_dat$speaker, m_dat$cent_diff, type="b", col=c(1:6), 
			ylab="Centroid Difference", xlab="Task", main="Speaker X Task")

REGRESSION:
lm()
lmrob()

training.mod = lmer(Accuracy ~  Condition * cTrial + (cTrial|Subject) + (1|Subject), family = 'binomial')
training.mod = lmer(Accuracy ~  Condition * cTrial * Speaker + (cTrial|Subject) + (1|Subject) + (1|Speaker), family = 'binomial')

Family flags:
binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")

Error checking:
qqnorm(salary)
qqline(salary) 
plot errors

#---------------
write max function
Get the max element of array

arr = c(2,3,7,1,4)

tmp_max = arr[1]

for (i in 1:length(arr)){

  y = arr[i+1]

  if (tmp_max < y) {
    tmp_max = y 
  }
}

#---------------

listener_id, day, n_songs
534, 2015-03-01, 17
534, 2015-03-02, 0
my_listener_id <- 99

data = read.csv("path/data.csv", na.strings = c(NA, ""))

x = data$listener_id
subset = data[which(data$listener_id == 99),]

ids = unique(data$listener_id)

for (i in 1:length(ids)) {
  id = ids[i] 
  subset = data[which(data$listener_id == id),]
  
}
or tapply

#---------------
# final project
train.dat = na.omit(read.csv("pml-training.csv", header = TRUE))
test.dat = read.csv("pml-testing2.csv", header = TRUE)  #NA excluded

##first only include the variables that are available in test:
testvars <- names(train.dat) %in% names(test.dat)
testvars[[length(testvars)]] = TRUE
training <- train.dat[testvars]

var = nearZeroVar(training, saveMetrics=TRUE)
# remove all variables with nzv
myvars <- names(training) %in% c("new_window")

#exploring variables
featurePlot(training[-length(training)],y, "box", data = training)
pairs(head(training, 100), col="gray20")

train <- training[!myvars]
test <- test.dat[!myvars]

train_var = train[-147]

control <- trainControl(method="cv")
modelFitrf = train(train$classe ~ ., method = "rf", data = train_var,trControl=control)
modelFitrf
prf = predict(modelFitrf, newdata=test)
prf

varImp(prf, scale=FALSE)
#OR
library(rpart)
model.rpart <- rpart(train$classe ~ ., train_var)
library(rattle)
fancyRpartPlot(model.rpart )


ctrl = trainControl("cv", number=10, savePredictions=T,  
                    classProbs=TRUE, summaryFunction=twoClassSummary )  
tr_glm = train(pizza_rec ~ .,
               data      = training,
               method    = "glm",
               family    = binomial,
               trControl = ctrl, 
               metric="ROC")
fitpred = tr_glm$finalModel$fitted.values
fitpredt = function(t) ifelse(fitpred > t , 1,0)
confusionMatrix(fitpredt(0.5),training$pizza_rec)

