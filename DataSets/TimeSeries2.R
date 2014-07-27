f <- function(k) {
  return (sin(k+sin(2*k)))
}

data <- rep(0, 500)
for (k in 1 : 500) {
  data[k+1] <- f(k)
}

plot(1:length(data), data, type="l", col="green")

write(data, file = "TimeSeries2.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data));
#for(k in 2 : length(data)) {
#  fd[k] <- data[k-1]
#}
#plot(fd, data, type="l", col="blue")