library(caret)

read_iris_data = function(data_path) {
    dataset <- read.csv(data_path, header=FALSE)
    colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
    return(dataset)
}

split_train_test = function(dataset, train_ratio) {
    split_index <- createDataPartition(dataset$Species, p=train_ratio, list=FALSE)
    train_dataset <- dataset[split_index,]
    test_dataset <- dataset[-split_index,]
    dataset <- list()
    dataset$train <- train_dataset
    dataset$test <- test_dataset
    return(dataset)
}

explore_dataset = function(dataset) {
    cat("Dimensions:", dim(dataset), "\n")
    cat("Types:", sapply(dataset, class), "\n")
    cat("Species:", levels(dataset$Species), "\n")
    species_percentage <- prop.table(table(dataset$Species)) * 100
    cat("Percentage:", species_percentage, "\n")
    print(summary(dataset))
}

visualize_dataset = function(dataset, figure_path) {
    x <- dataset[, 1:4]
    y <- dataset[, 5]

    pdf(file=paste(figure_path, "feature_distribution.pdf"))
    par(mfrow=c(1,4))
    for(i in 1:4) {
        boxplot(x[,i], main=names(iris)[i])
    }

    pdf(file=paste(figure_path, "feature_scatter_plot.pdf"))
    featurePlot(x=x, y=y, plot="ellipse")

    pdf(file=paste(figure_path, "feature_box_plot.pdf"))
    featurePlot(x=x, y=y, plot="box")

    pdf(file=paste(figure_path, "feature_density_plot.pdf"))
    scales <- list(x=list(relation="free"), y=list(relation="free"))
    featurePlot(x=x, y=y, plot="density", scales=scales)
}

build_models = function(dataset) {
    control <- trainControl(method="cv", number=10)
    metric <- "Accuracy"

    fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
    fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
    fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
    fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
    fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

    models = list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf)
    results <- resamples(models)
    print(summary(results))

    pdf(file=paste(figure_path, "model_accuracy.pdf"))
    dotplot(results)
    return(models)
}

data_path <- "data/iris.data"
figure_path <- "output/figures/"
train_ratio <- 0.8

raw_dataset <- read_iris_data(data_path)
dataset <- split_train_test(raw_dataset, train_ratio)

explore_dataset(dataset$train)
visualize_dataset(dataset$train, figure_path)
models <- build_models(dataset$train)

predictions <- predict(models$lda, dataset$test)
confusionMatrix(predictions, dataset$test$Species)