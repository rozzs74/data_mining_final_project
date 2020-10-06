library(mlbench)
library(caret)
library(doMC)
library(tidyverse)
library(plotrix)

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

make_predictions <- function(model, data) {
	p <- predict(model, data)
	return(p)
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

plot_bw <- function(data, options) {
    bwplot(data, scales=options)
}

plot_dot <- function (data, options) {
    dotplot(data, scales=options)
}

plot_density <- function(data, options) {
    densityplot(data, scales=options, pch="|")
}


plot_parallel <- function(data) {
    parallelplot(data)
}

plot_scatter_matrix <- function(data) {
    splom(data)
}

plot_pairwise_xy <- function(data, models) {
    xyplot(data, models=models)
}

plot_pie <- function(x, y, title) {
	colors = c('#4286f4','#bb3af2','#ed2f52','#efc023','#ea7441', '#000000')
	piepercent<- round(x, 2)
	pie3D(x, labels=piepercent, main=title, explode=0.1, height=0.05, col=colors)
	legend("topright",y, cex = 0.8,fill = colors)
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

x <- c()
labels <- c()


# # Logistic Regression
run_lgr_model <- function (trainData, testData, metric, train_control) {
	generate_seed(7)
	model.lgr <- train_model(trainData, "glm", train_control, metric)
	model.lgr
    summary(model.lgr)
	model.lgr.final <- model.lgr$finalModel
	probabilities <- predict(model.lgr.final, testData, type="response")
	predictions <- ifelse(probabilities > 0.5,'Good','Bad')
	head(predictions)
	return(model.lgr)
}
labels <- c(labels, "Logistic Regression")
start.time <- Sys.time()
logistic_regression_model <- run_lgr_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)

# Random Forest 
run_rf_model <- function (trainData, testData, metric, train_control) {
	generate_seed(7)	
	model.rf <- train_model(trainData, "rf", train_control, metric)
	model.rf
	summary(model.rf)
	model.rf.final <- model.rf$finalModel
	predictions <- make_predictions(model.rf, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.rf)
}


labels <- c(labels, "Random Forest")
start.time <- Sys.time()
random_forest_model <- run_rf_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)


# Naive Bayes
run_nb_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.nb <- train(Class~Amount+Age, data=trainData, method="nb", trControl=train_control)
	model.nb
	summary(model.nb)
	model.nb.final <- model.nb$finalModel
	predictions <- make_predictions(model.nb, testData)
	head(predictions)
	return(model.nb)
}

labels <- c(labels, "Naive Bayes")
start.time <- Sys.time()
naive_bayes_model <- run_nb_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)


#SVM
run_svm_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.svm <- train_model(trainData, "svmRadial", train_control, metric)
	model.svm
	summary(model.svm)
	model.svm.final <- model.svm$finalModel
	model.svm.final
	predictions <- make_predictions(model.svm, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.svm)
}

labels <- c(labels, "Super Vector Machine")
start.time <- Sys.time()
super_vector_machine_model <- run_svm_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)


#CART
run_cart_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.cart <- train_model(trainData, "rpart", train_control, metric)
	model.cart
	summary(model.cart)
	model.cart.final <- model.cart$finalModel
	predictions <- make_predictions(model.cart, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.cart)
}
labels <- c(labels, "Classification and Regression Trees")
start.time <- Sys.time()
cart_model <- run_cart_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)


#KNN
run_knn_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.knn <- train_model(trainData, "knn", train_control, metric)
	model.knn
	summary(model.knn)
	model.knn.final <- model.knn$finalModel
	predictions <- make_predictions(model.knn, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.knn)
}

labels <- c(labels, "K-Nearest Neighbor")
start.time <- Sys.time()
knn_model <- run_knn_model(trainData, testData, METRIC, train_control)
end.time <- Sys.time()
time.taken <- round(end.time - start.time, 2)
x <- c(x, time.taken)

# results <- resamples(list(LGR=logistic_regression_model,RF=random_forest_model, NB=naive_bayes_model, SVM=super_vector_machine_model, CART=cart_model,KNN=knn_model))
# summary(results)


# scales <- list(x=list(relation="free"), y=list(relation="free"))
# # plot_bw(results, scales)
# # plot_density(results, scales)
# # plot_dot(results, scales)

# # plot_parallel(results)

# # plot_scatter_matrix(results)

# plot_pairwise_xy(results, c=("LGR", "RF"))
plot_pie(x, labels, "Time execution in seconds")