f <- function(k) {
  if (k < 500) {
    return ((sin(pi*k/250)))^3
  } else {
    return (0.8*(sin(pi*k/250) + 0.2*sin(pi*k/25))^3)
  }
}

data <- rep(0, 2000)
for (k in 1 : 2000) {
  data[k+1] <- (data[k] / (1 + data[k]^2)) + f(k)
}

plot(1:length(data), data, type="l", col="green")

write(data, file = "Narendra1.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data));
#for(k in 2 : length(data)) {
#  fd[k] <- data[k-1]
#}
#plot(fd, data, type="l", col="blue")