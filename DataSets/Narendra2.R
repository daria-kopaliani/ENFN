f <- function(x1, x2, x3, x4, x5) {
  return ((x1 * x2 * x3 * x5 *(x3 - 1) + x4)/(1 + x3^2 + x2 ^ 2))
}

u <- function(k) {
  if (k < 250) {
    return (sin(pi*k/25))
  } else if (k < 500) {
    return (1)
  } else if (k < 750) {
    return (-1)
  } else {
    return (0.4 * sin(pi*k/25) + 0.1 * sin(pi*k/32) + 0.6 * sin(pi*k/10)) 
  }
}

data <- rep(0, 1500)
data[k] <- 0.2
for (k in 1 : 1500) {
  data[k+3] <- f(data[k+2], data[k+1], data[k], u(k+3), u(k+2))
}

plot(1:length(data), data, type="l", col="green")

write(data, file = "Narendra2.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data));
#for(k in 2 : length(data)) {
 # fd[k] <- data[k] - data[k-1]
#}
#plot(fd, data, type="l", col="blue")