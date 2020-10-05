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

PARAMS <- get_train_control_params()
METRIC <- PARAMS$metric
train_control <- get_train_control(PARAMS$method, PARAMS$number,PARAMS$repeats)

index <- createDataPartition(DS$Class, p=0.80, list=FALSE)
trainData <- GermanCredit[index,]
testData <- GermanCredit[-index,]

# Logistic Regression
generate_seed(7)
model.lgr <- train_model(trainData, "glm", train_control, METRIC)
model.lgr
model.lgr.final <- model.lgr$finalModel
model.lgr.final
probabilities <- predict(model.lgr.final, data=testData, type="response")
predictions <- ifelse(probabilities > 0.5,'Good','Bad')
head(predictions)