
f <- function(k) {
  if (k < 2001) {
    return (cos(2*pi*k/25) + cos(2*pi*k/2))^3
  } else {
    return(sin(2*pi*k/250) + sin(2*pi*k/10))^3
  }
}

data <- rep(0, 3000)
for (k in 2 : (length(data) - 1)) {
  data[k+1] <- data[k]/(1 + (data[k])^2) + f(k)
}

plot(1:length(data[1800:2700]), data[1800:2700], type="l", col="green")

#write(data, file = "Narendra3.txt", ncolumns = 1, append = FALSE, sep = " ")

#fd <- rep(0, length(data))
#for(k in 2 : length(data)) {
#  fd[k] <- data[k-1]
#}
#plot(fd, data, type="l", col="blue")