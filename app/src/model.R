library(mlbench)
library(caret)
library(doMC)

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
	xyplot(data, models=models))
}

get_statistical_significance <- function(data) {
    diffs <- diff(data)
    summary(diffs)
}


DS <- GermanCredit
# dim(DS)
# names(DS)
# str(DS)
# sapply(DS, class)

PARAMS <- get_train_control_params()
METRIC <- PARAMS$metric
train_control <- get_train_control(PARAMS$method, PARAMS$number,PARAMS$repeats)

generate_seed(7)	
index <- create_partition(DS$Class, 0.80, FALSE)
trainData <- DS[index,]
testData <- DS[-index,]

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
# # start.time <- Sys.time()
logistic_regression_model <- run_lgr_model(trainData, testData, METRIC, train_control)
# # end.time <- Sys.time()
# # time.taken <- end.time - start.time
# # time.taken

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

# # start.time <- Sys.time()
random_forest_model <- run_rf_model(trainData, testData, METRIC, train_control)
# # end.time <- Sys.time()
# # time.taken <- end.time - start.time
# # time.taken


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

# start.time <- Sys.time()
naive_bayes_model <- run_nb_model(trainData, testData, METRIC, train_control)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

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

# start.time <- Sys.time()
super_vector_machine_model <- run_svm_model(trainData, testData, METRIC, train_control)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken


#CART
run_cart_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.cart <- train_model(trainData, "rpart", train_control, metric)
	# model.cart
	# summary(model.cart)
	model.cart.final <- model.cart$finalModel
	predictions <- make_predictions(model.cart, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.cart)
}
# start.time <- Sys.time()
cart_model <- run_cart_model(trainData, testData, METRIC, train_control)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

run_knn_model <- function(trainData, testData, metric, train_control) {
	generate_seed(7)
	model.knn <- train_model(trainData, "knn", train_control, metric)
	# model.knn
	# summary(model.knn)
	model.knn.final <- model.knn$finalModel
	predictions <- make_predictions(model.knn, testData)
	head(predictions)
	confusionMatrix(predictions, testData$Class)
	return(model.knn)
}

# start.time <- Sys.time()
knn_model <- run_knn_model(trainData, testData, METRIC, train_control)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

results <- resamples(list(LGR=logistic_regression_model,RF=random_forest_model, NB=naive_bayes_model, SVM=super_vector_machine_model, CART=cart_model,KNN=knn_model))
summary(results)


scales <- list(x=list(relation="free"), y=list(relation="free"))
plot_bw(results, scales)
plot_density(results, scales)
plot_dot(results, scales)

plot_parallel(results)

plot_scatter_matrix(results)

plot_pairwise_xy(results, c=("LGR", "RF"))

get_statistical_significance(results)