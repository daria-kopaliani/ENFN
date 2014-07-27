normalized_series <- function(directory, n_samples = NA) {
  range01 <- function(x){(x - min(x))/(max(x) - min(x))}
  series <- read.csv(directory)
  if (!is.na(n_samples)) {
    if (n_samples > nrow(series)) {
      message("n_samples is larger than total number of rows")
    } else {
      series <- series[1:n_samples, ,drop=FALSE]  
    }
  }
  series.names <- c("x")
  series <- range01(series)
  invisible(series)
}

### training set contains n (n = `train_sample_ratio` * series length) samples, testing set contains series length - n samples.
### Each sample in the training set is a vector of `history_length` values, the network is tasked to predict history_length + 1 value 
train_and_run <- function(series, train_sample_ratio = 0.66, membership_functions = 3,
                          history_length = 10, inference_order = 0, learning_rate = 0.9)
{
  norm_vector <- function(x) sqrt(sum(x^2))
  
  membership_function_value <- function(x, l, c) {
    if ((l > 1) && (x >= c[l-1]) && (x <= c[l])) {
      return ((x - c[l-1])/(c[l] - c[l-1]))
    } else {
      if (l + 1 <= length(c)) {
        if ((l <= (length(c) + 1)) && (x >= c[l]) && (x <= c[l+1])) {    
          return ((c[l + 1] - x)/(c[l + 1] - c[l]))
        } else {
          return (0)
        }
      } else {
        return (0)
      }
    }
  }
  
  inputs <- array(0, dim=c((length(series$x)-history_length), history_length))
  obs <- series$x[(history_length+1) : length(series$x)]
  for (i in 1 : (length(series$x) - history_length)) {
    inputs[i, ] <- series$x[i : (i + history_length - 1)]
  }
  first_test_sample = floor(train_sample_ratio * length(obs))
  
  ## mu - membership function values matrix
  mu <- array(0, history_length * membership_functions * (inference_order + 1))
  ## w - weight matrix
  w <- array(0, history_length * membership_functions * (inference_order + 1))
  sim <- rep(0, length(obs))
  e <- rep(0, length(obs))
  r <- 0
  
  for (k in seq(along = obs)) { 
    input <- inputs[k,]
    for (i in seq(along = input)) {
      for (l in 1 : membership_functions) {
        c <- seq(0, 1, length.out = membership_functions)
        muValue <- membership_function_value(input[i], l, c)
        for (j in 1 : (inference_order + 1)) {
          mu[(i - 1) * membership_functions + (l - 1) * (inference_order + 1) + j] <- muValue * (input[i]^(j - 1))
        }        
      }
    }
    sim[k] <- w %*% mu
    e[k] <- obs[k] - sim[k]
    ## Training
    if (k < first_test_sample) {
      r <- r * learning_rate + (norm_vector(mu))^2
      w <- w + e[k] * mu / r    
    }
  }
  
  data <- list(e = e, obs = obs, sim = sim, train_sample_ratio = train_sample_ratio,
                 membership_functions = membership_functions, history_length = history_length, 
                 inference_order = inference_order, learning_rate = learning_rate)
  invisible(data)
}

visualize <- function(data, first_plot_sample = 1, last_plot_sample = NA,
                      plot_title = "", sub = NA, printErrors = TRUE) {
  if (is.na(last_plot_sample)) {
    last_plot_sample <- length(data$obs)
  }
  if (is.na(sub)) {
    sub <- paste(data$membership_functions, " membersip functions, ",
                 data$inference_order, " order fuzzy inference", sep="");
  }
  x <- first_plot_sample : last_plot_sample
  plot(x, data$obs[first_plot_sample : last_plot_sample],
       type ="l", col = "green", main = plot_title, sub = sub, ylab = "", xlab = "", ylim = 0:1)
  lines(x, data$sim[first_plot_sample : last_plot_sample], pch = 22, lty = 2,col = "blue")
  lines(x, data$e[first_plot_sample : last_plot_sample], type = "l", col = "red")
  if (!is.na(data$train_sample_ratio)) {
    first_test_sample = floor(data$train_sample_ratio * length(x))
    abline(v = first_test_sample, col = "orange", lty = 1) 
  }
  if (printErrors) {
    test_sample_only <- data$train_sample_ratio < 1    
    if (data$train_sample_ratio < 1) {
      print(paste("RMSE on test sample -", rmse(data, test_sample_only = TRUE)))
      print(paste("MSE on test sample -", mse(data, test_sample_only = TRUE)))
      print(paste("SMAPE on test sample -", smape(data, test_sample_only = TRUE)))
    } else {
      print(paste("RMSE -", rmse(data, test_sample_only = FALSE)))
      print(paste("MSE -", mse(data, test_sample_only = FALSE)))
      print(paste("SMAPE -", smape(data, test_sample_only = FALSE)))
    }
  }
}

rmse <- function(data, test_sample_only = TRUE) {
  rmse <- sqrt(mse(data, test_sample_only = test_sample_only))
  rmse
}

mse <- function(data, test_sample_only = TRUE) {
  range <- 1:length(data$obs)
  if (test_sample_only) {
    range <- floor(data$train_sample_ratio * length(data$obs)) : length(data$obs)
  }
  mse <- mean((data$sim[range] - data$obs[range])^2, na.rm = TRUE)
  mse
}

smape <- function(data, test_sample_only = TRUE) {
  range <- 1:length(data$obs)
  if (test_sample_only) {
    range <- floor(data$train_sample_ratio * length(data$obs)) : length(data$obs)
  }
  smape <- (1 / length(range)) * sum (abs(data$sim - data$obs) / (0.5 * (data$sim + data$obs)))
  smape
}

#directory <- file.path(getwd(), "DataSets", "MackeyGlass.txt")