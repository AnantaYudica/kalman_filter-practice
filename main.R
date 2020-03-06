
source("Utils.R")

fx <- function(i, x)
{
  return (sin(i / 8))
}

fu <- function(i, u)
{
  return (sin(i / 8))
}

fw <- function(i, w)
{
  return (0.1)
}

fv <- function(i, v)
{
  return (-0.05)
}

fnoise <- function(v)
{
  return(v - (runif(1, 0, 2) - 1))
}

n <- 50
x0 <- 0
u0 <- 0

x0_est <- 0
p0_est <- 1

x_est <- x0_est
p_est <- p0_est

A <- matrix(.55, 1, 1)
AT <- t(A)

B <- matrix(.5, 1, 1)

H <- matrix(1, 1, 1)
HT <- t(H)

I <- matrix(0, 1, 1)
diag(I) <- 1

Q <- 0.1

#initial data

#create vector x with noise fx
vec_x <- c(fnoise(fx(1, fx(0, x0))))
lapply(2:n, function(i){vec_x[i] <<- fnoise(fx(i, vec_x[i-1]))})

#create vector x0 with noise fx
vec_x0 <- c(fx(0, x0))
lapply(2:n, function(i){vec_x0[i] <<- vec_x[i-1]})

#create vector x real with fx
vec_x_real <- c(fx(1, fx(0, x0)))
lapply(2:n, function(i){vec_x_real[i] <<- fx(i, vec_x_real[i-1])})

#create vector xn real with fx
vec_xn_real <- c(fx(2, fx(1, fx(0, x0))))
lapply(2:n, function(i){vec_xn_real[i] <<- fx(i + 1, vec_xn_real[i-1])})

#create vector u with fu
vec_u <- c(fu(1, u0))
lapply(2:n, function(i){vec_u[i] <<- fu(i, vec_u[i-1])})

#create vector mean x
vec_mean_x <- c()
lapply(1:n, function(i){vec_mean_x[i] <<- mean(c(x0, vec_x[1:i]))})

#create vector standard deviation x
vec_stddev_x <- c()
lapply(1:n, function(i){vec_stddev_x[i] <<- sd(c(x0, vec_x[1:i]))})

#create vector gaussian.noise x
vec_g_noise_x <- c()
lapply(1:n, function(i){
  vec_g_noise_x[i] <<- gaussian.noise(vec_x[i], vec_mean_x[i], vec_stddev_x[i])})

#create vector y with y = H*x + v
vec_y <- c()
lapply(1:n, function(i){vec_y[i] <<- (H * vec_x[i] + fv(i, vec_g_noise_x[i]))})

vec_x_curr <- c()
vec_p_curr <- c()
vec_k <- c()
vec_y_est <- c()
vec_x_est <- c()
vec_p_est <- c()
vec_x_err <- c()

for (i in 1:n)
{
  x_prev = x_est
  p_prev = p_est
  y = vec_y[i]
  
  #calc x = A* x_prev + B*u + w
  x = A * x_prev + B * vec_u[i] + fw(i, vec_g_noise_x[i])
  vec_x_curr[i] <- x
  
  #calc p = (A * p_prev * AT) + Q
  p = (A * p_prev * AT) + Q
  vec_p_curr[i] <- p
  
  #calc K = (P * HT)/ ((H * P * HT) + Q)
  K = (p * HT) / ((H * p * HT) + Q)
  vec_k[i] <- K
  
  #calc y_est = (y - (H * x))
  y_est = (y - (H * x))
  vec_y_est[i] <- y_est
  
  #x_est = x + K * yn
  x_est <<- (x + K * y_est)
  vec_x_est[i] <- x_est
  
  #p_est = (I - (K * H)) * P 
  p_est <<- ((I - (K * H)) * p)
  vec_p_est[i] <- p_est
  
  #x_err = abs(xn.real - x_est)
  x_err = abs(vec_xn_real[i] - x_est)
  vec_x_err[i] <- x_err
}

#view data frame
df <- data.frame(
  x0 = vec_x0,
  x = vec_x,
  xn.real = vec_xn_real,
  u = vec_u,
  x.mean = vec_mean_x,
  x.stddev = vec_stddev_x,
  x.gaussian.noise = vec_g_noise_x,
  x.curr = vec_x_curr,
  y = vec_y,
  p.curr = vec_p_curr,
  K = vec_k,
  y.est = vec_y_est,
  x.est = vec_x_est,
  p.est = vec_p_est,
  x.err = vec_x_err
)

print(df)

plot1_y_min = min(vec_x, vec_u, vec_x_est) * 1.5
plot1_y_max = max(vec_x, vec_u, vec_x_est) * 1.5

plot(seq(0, n, length = 20), 
     seq(plot1_y_min, plot1_y_max, length = 20), 
     type = "n", xlab = "t", ylab = "x")
points(x <- 0:n, y <- c(x0, vec_x), cex = .5, col = "dark blue", p = 2)
points(x <- 0:n, y <- c(u0 ,vec_u), cex = .5, col = "red", p = 6)
lines(x <- 0:n, y <- c(x0_est ,vec_x_est), "l", col="green", lwd = 2)

legend("topright", 
       legend = c("x", "u", expression(widehat("x"))), 
       lty = c(0, 0, 1),pch = c(2, 6, NA),
       col = c("dark blue", "red", "green"),
       cex = c(0.75, 0.75, 0.75),
       lwd = c(1, 1, 2))

hist_min = min(vec_x, vec_u, vec_x_est) * 2
hist_max = max(vec_x, vec_u, vec_x_est) * 2
hist_seq = seq(hist_min, hist_max, length = n)
vec_x_dnorm = dnorm(hist_seq, mean(vec_x), sd(vec_x))
vec_u_dnorm = dnorm(hist_seq, mean(vec_u), sd(vec_u))
vec_x_est_dnorm = dnorm(hist_seq, mean(vec_x_est), sd(vec_x_est))
hist_top = max (vec_x_dnorm, vec_u_dnorm, vec_x_est_dnorm) * 1.125

plot(seq(hist_min, hist_max, length = 20), 
     seq(0, hist_top, length = 20), 
     type = "n", xlab = "x", ylab = "y")

lines(hist_seq, vec_x_dnorm, "l", col="dark blue", lwd = 2)
lines(hist_seq, vec_u_dnorm, "l", col = "red", lwd = 2)
lines(hist_seq, vec_x_est_dnorm, "l", col="green", lwd = 2)
lines(hist_seq, dnorm(hist_seq, mean(vec_y), sd(vec_y)), "l", col="yellow", lwd = 2)

legend("topright", 
       legend = c("x", "u", expression(widehat("x"), y)), 
       lty = c(1, 1, 1, 1),pch = c(NA, NA, NA),
       col = c("dark blue", "red", "green", "yellow"),
       cex = c(0.75, 0.75, 0.75, 0.75),
       lwd = c(2, 2, 2, 2))
