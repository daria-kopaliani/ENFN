
data <- rep(0, 500)
for (k in 1 : 500) {
  fu <- sin(2*pi*k / 25) + sin(2*pi*k / 10)
  data[k+1] <- (data[k] / (1 + data[k]^2)) + fu^3
}

plot(1:length(data), data, type="l", col="green")

#write(data, file = "Narendra4.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data));
#for(k in 2 : length(data)) {
#  fd[k] <- data[k] - data[k-1]
#}
#plot(fd, data, type="l", col="blue")