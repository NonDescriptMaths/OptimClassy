# Install and load MASS if user doesnt have them installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# Set seed for reproducibility
set.seed(1)

#Generate MVN-----------------------------------------------|
Binary_MVN<- function(mean1, mean2, cov_matrix, n_samples) {
  class1 <- mvrnorm(n = n_samples, mu = mean1, Sigma = cov_matrix)
  class2 <- mvrnorm(n = n_samples, mu = mean2, Sigma = cov_matrix)
  seperable_data <- list(class1 = class1, class2 = class2)
  #Generate two mvn normals with n_samples each and produce a list
  seperable_data <- data.frame(class1 = seperable_data$class1,class2 = seperable_data$class2)
  Class1 <- seperable_data[,c("class1.1","class1.2")]
  Class2 <- seperable_data[,c("class2.1","class2.2")]
  Class1$label <- 0
  Class2$label <- 1
  Class1$labelpl <- '0'
  Class2$labelpl <- '1'
  names(Class1) = c("x","y","label","labelpl")
  names(Class2) = c("x","y","label","labelpl")
  sep_data <- rbind(Class1,Class2)
  #returns a dataframe with all data, but labelled to differeniate between them
  return(sep_data)
}
#Generate points in a ring----------------------------------|
generate_ring_points <- function(center, n_points, radius, width) {
  theta <- runif(n_points, 0, 2 * pi)
  r <- radius + rnorm(n_points, 0, width)
  x_1 <- center[1] + r * cos(theta)
  x_2 <- center[2] + r * sin(theta)
  df = data.frame(x=x_1,y=x_2)
  return(df)}

#Generate MVN normal and a ring-----------------------------|
Binary_RING <- function(n){
  normal_data <- as.data.frame(mvrnorm(n = n, mu = c(0, 0), 
                Sigma = matrix(c(1, 0, 0, 1), nrow = 2)))
  names(normal_data) <- c("x","y")
  #Generate MVN normal distribution
  ring_data <- generate_ring_points(center = c(0, 0), 
              n_points = n, radius = 5, width = 0.5)
  #Generate random distribution in a ring around the MVN normal

  #|Combine Data --------------|
  combined_data <- rbind(normal_data, ring_data)
  #Bind data into one dataframe
  combined_data$labelpl <- rep(c('0', '1'), each = 500)
  #Generate plot-labels (these are only for plotting)
  combined_data$label <- rep(c(0, 1), each = 500)
  #Generate labels (for classifying)
  return(combined_data)
}
#Transform 2D dataframe to 3d-------------------------------|
Quadratic_Transform <- function(Data){
  Data$xy <- (Data$x)^2 + (Data$y)^2
  return(Data)
}
