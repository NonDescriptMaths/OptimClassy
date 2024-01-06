# Install and load MASS if user doesnt have them installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)


Logistic_Regression_glm <- function(data){
    set.seed(123) 
    train_indices <- sample(1:n_samples, 0.7 * n_samples)
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    # Fit logistic regression model
    logistic_model <- glm(data$labels ~ x1 + x2, data = train_data, family = "binomial")


    probabilities <- predict(logistic_model, newdata = test_data, type = "response")
    predicted_labels <- ifelse(probabilities > 0.5, 1, 0)


    confusion_matrix <- table(predicted_labels, test_data$labels)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    cat("Accuracy:", accuracy, "\n")

    return(logistic_model)
}
