gradient_descent <- function(objective_function,gradient, initial_point, learning_rate, max_iterations) {
  x <- initial_point
  for (i in 1:max_iterations){
    if (sum(abs(x))  > 300){
      x <- c(5,5)
      return(x)}
    x = x - learning_rate * gradient(x)
  }
  return(x)
}


Negative_Log_Likelihood_LINEAR <- function(w){
  w_0 <- w[1]
  w_g <- w[-1]
  x_vals <- sep_data[,1:2]
  y_vals <- sep_data$label
  dot_prod <- w_g*t(x_vals)
  linear_comb <- apply(dot_prod,2,sum) + w_0
  for (i in 1:length(y_vals)){
    if (y_vals[i]==0){
      linear_comb[i] = -linear_comb[i]
    }
  }
  log_sigmoid_vals <- log(1/(1+exp(linear_comb)))
  neg_likelihood <- -sum(log_sigmoid_vals)
  return(neg_likelihood)
}


Negative_Log_Likelihood_QUAD <- function(w){
  w_0 <- w[1]
  w_g <- w[-1]
  x_vals <- Cone_Data[,c("x","y","xy")]
  y_vals <- Cone$label
  dot_prod <- w_g*t(x_vals)
  linear_comb <- apply(dot_prod,2,sum) + w_0
  for (i in 1:length(y_vals)){
    if (y_vals[i]==0){
      linear_comb[i] = -linear_comb[i]
    }
  }
  log_sigmoid_vals <- log(1/(1+exp(linear_comb)))
  neg_likelihood <- -sum(log_sigmoid_vals)
  return(neg_likelihood)
}