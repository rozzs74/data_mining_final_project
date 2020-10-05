library(mlbench)
library(caret)


generate_seed <- function(n) {
    set.seed(n)
    return(TRUE)
}

get_train_control <- function(type, no_fold, no_repeats, search_method) {
    control <- trainControl(method=type, number=no_fold, repeats=no_repeats, search=search_method) 
    return(control)
}

train_model <- function(type, control, data_sets, metric, tuneGrid) {
    # model <- train(Class~., data=data_sets, method=type, trControl=control, metric=metric, tuneGrid=tuneGrid)
	model <- NULL
    return(model)
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

