data <- rep(0, 1200)
data[1] <- 0.2
for (k in 1 : 1500) {
  data[k+1] <- 4 * data[k] * (1 - data[k])
}

plot(1:length(data[1420:1500]), data[1420:1500], type="l", col="green")

write(data, file = "Mandelbrot.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data));
#for(k in 2 : length(data)) {
#  fd[k] <- data[k-1]
#}
#plot(fd, data, type="l", col="blue")