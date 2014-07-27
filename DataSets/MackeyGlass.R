mackeyglass_eq <- function(x_t, x_t_minus_tau, a, b) {
  return (-b*x_t + a*x_t_minus_tau/(1 + x_t_minus_tau^10.0));
}

mackeyglass_rk4 <- function(x_t, x_t_minus_tau, deltat, a, b) {
  k1 = deltat*mackeyglass_eq(x_t,          x_t_minus_tau, a, b);
  k2 = deltat*mackeyglass_eq(x_t+0.5*k1,   x_t_minus_tau, a, b);
  k3 = deltat*mackeyglass_eq(x_t+0.5*k2,   x_t_minus_tau, a, b);
  k4 = deltat*mackeyglass_eq(x_t+k3,       x_t_minus_tau, a, b);  
  
  return (x_t + k1/6 + k2/3 + k3/3 + k4/6);
}

a        = 0.2;   # value for a in eq (1)
b        = 0.1;   # value for b in eq (1)
tau      = 17;  	# delay constant in eq (1)
x0       = 1.2;		# initial condition: x(t=0)=x0
deltat   = 0.1;	  # time step size (which coincides with the integration step)
sample_n = 12000;	# total no. of samples, excluding the given initial condition
interval = 1;	    # output is printed at every 'interval' time steps

time <- 0;
index <- 1;
history_length <- floor(tau/deltat)
x_history <- rep(0, history_length)
x_t <- x0;

X <- rep(0, sample_n); # vector of all generated x samples
T <- rep(0, sample_n); # vector of time samples

for (i in 1 : sample_n) {
  X[i] <- x_t;  
  if (tau == 0) {
    x_t_minus_tau <- 0
  } else {
    x_t_minus_tau <- x_history[index];
  }
  x_t_plus_deltat = mackeyglass_rk4(x_t, x_t_minus_tau, deltat, a, b);
  if (tau != 0) {
    x_history[index] <- x_t_plus_deltat;
    index = (index %% history_length) + 1;
  }
  
  time <- time + deltat;
  T[i] <- time;
  x_t <- x_t_plus_deltat; 
}

plot(T, X, type="l", col="green")
write(X, file = "MackeyGlass.txt", ncolumns = 1, append = FALSE, sep = " ")