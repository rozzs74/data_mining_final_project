library(mlbench)
library(caret)
library(doMC)
library(tidyverse)

registerDoMC(cores=8)

data(GermanCredit)

generate_seed <- function(n) {
    set.seed(n)
    return(TRUE)
}


get_train_control <- function(type, no_fold, no_repeats) {
    control <- trainControl(method=type, number=no_fold, repeats=no_repeats) 
    return(control)
}

get_train_control_params <- function() {
	return(list(metric="Accuracy", number=10, method="repeatedcv", repeats=3))
}

train_model <- function(data, method, train_control, metric) {
    model <- train(Class~., data=data, method=method, trControl=train_control, metric=metric)
    return(model)
}

create_partition <- function(data, p, list) {
	rs <- createDataPartition(data, p=p, list=list)
	return(rs)
}
save_model <- function (model, path) {
	saveRDS(finalModel, path)
	return(TRUE)
}

load_model <- function(path) {
	superModel <- readRDS(path)
	return(TRUE)
}

plot_normal <- function(data) {
    plot(data)
    return(TRUE)
}

plot_dot <- function (data) {
    dotplot(data)
    return(TRUE)
}

plot_density <- function(data, options) {
    densityplot(data, scales=options, pch="|")
    return(TRUE)
}

plot_dot <- function(data, options) {
    dotplot(data, scales=options)
	return(TRUE)
}

plot_parallel <- function(data) {
    parallelplot(data)
    return(TRUE)
}

plot_scatter_matrix <- function(data) {
    splom(data)
    return(TRUE)
}


plot_pairwise_xy <- function(data, models) {
    xyplot(data, models=models)
    return(TRUE)
}

get_statistical_significance <- function(data) {
    diffs <- diff(data)
    summary(diffs)
    return(TRUE)
}


DS <- GermanCredit
# dim(DS)
# names(DS)
# str(DS)
# sapply(DS, class)

#setwd("C:/Users/oanonuevo/Desktop/TIP/Submissions")
#DS <- read.csv("jm1.csv", na.strings = c("", " "), stringsAsFactors = FALSE)
# remove 5 rows with "??? value
#DS <- DS %>% filter(uniq_Op != "?")

#change to numeric value
#DS[, 17:21] <- lapply(DS[,17:21], as.numeric)
#DS$defects <- as.factor(DS$defects)

#check nulls
#colSums(is.na(DS))


PARAMS <- get_train_control_params()
METRIC <- PARAMS$metric
train_control <- get_train_control(PARAMS$method, PARAMS$number,PARAMS$repeats)

generate_seed(7)	
index <- create_partition(DS$Class, 0.80, FALSE)
trainData <- DS[index,]
testData <- DS[-index,]

# Logistic Regression
run_lgr_model <- function (trainData, testData, metric, train_control) {
	generate_seed(7)
	model.lgr <- train_model(trainData, "glm", train_control, metric)
	model.lgr
    summary(model.lgr)
	model.lgr.final <- model.lgr$finalModel
	probabilities <- predict(model.lgr.final, data=testData, type="response")
	predictions <- ifelse(probabilities > 0.5,'Good','Bad')
	head(predictions)
}
start.time <- Sys.time()
run_lgr_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Random Forest 
run_rf_model <- function (trainData, testData, metric, train_control) {
	generate_seed(7)	
	model.rf <- train_model(trainData, "rf", train_control, metric)
	model.rf
	summary(model.rf)
	model.rf.final <- model.rf$finalModel
	predictions <- predict(model.rf.final, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
}

start.time <- Sys.time()
run_rf_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Naive Bayes
run_nb_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.nb <- train(Class~Duration+Amount+ResidenceDuration+Age, data=trainData, method="nb", trControl=train_control)
	model.nb
	summary(model.nb)
	model.nb.final <- model.nb$finalModel
	predictions <- predict(model.nb.final, testData)
	head(predictions)
}

start.time <- Sys.time()
run_nb_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

