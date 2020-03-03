
source("Utils.R")

fx <- function(x)
{
  return(x + 1 / (x + 1))
}

fu <- function(u)
{
  return (0)
}

n <- 10
x <- 0
u <- 0

p <- 1

A <- matrix(1, 1, 1)
AT <- t(A)

B <- matrix(1, 1, 1)

C <- matrix(1, 1, 1)
CT <- t(C)

I <- matrix(0, 1, 1)
diag(I) <- 1

Q <- 0

#create vector x with fx
vec_x <- c(x)
lapply(2:n, function(i){vec_x[i] <<- fx(vec_x[i-1])})

#create vector xn_ with fx
vec_xn_ <- c(fx(x))
lapply(2:n, function(i){vec_xn_[i] <<- fx(vec_xn_[i-1])})

#create vector u with fu
vec_u <- c(u)
lapply(2:n, function(i){vec_u[i] <<- fu(vec_u[i-1])})

#create vector mean x
vec_mean_x <- c()
lapply(1:n, function(i){vec_mean_x[i] <<- mean(vec_x[1:i])})

#create vector standard deviation x
vec_stddev_x <- c()
lapply(1:n, function(i){vec_stddev_x[i] <<- sd(vec_x[1:i])})

#create vector gaussian.noise x
vec_g_noise_x <- c()
lapply(1:n, function(i){
  vec_g_noise_x[i] <<- gaussian.noise(vec_x[i], vec_mean_x[i], vec_stddev_x[i])})

#create vector xn with xn = A*x + B*u + w
vec_xn <- c()
lapply(1:n, function(i){vec_xn[i] <<- (A * vec_x[i] + B * vec_u[i] + vec_g_noise_x[i])})

#create vector y with y = C*x + v
vec_y <- c()
lapply(1:n, function(i){vec_y[i] <<- (C * vec_x[i] + vec_g_noise_x[i])})

#create vector pn = A * P * AT + Q
vec_p <- c(p)
vec_pn <- c()
lapply(2:n, function(i){vec_p[i] <<- ((A * vec_p[i-1] * AT) + Q)})
lapply(1:n, function(i){vec_pn[i] <<- ((A * vec_p[i] * AT) + Q)})

#create vector K = (P * CT)/ (C * P * CT + Q)
vec_k <- c()
lapply(1:n, function(i){vec_k[i] <<- ((vec_p[i] * CT)/((C * vec_p[i] * CT) - Q))})

#create vector yn = (y - C * x)
vec_yn <- c()
lapply(1:n, function(i){vec_yn[i] <<- (vec_y[i] - (C * vec_x[i]))})

#create vector x_est = x + K * yn
vec_x_est <- c()
lapply(1:n, function(i){vec_x_est[i] <<- (vec_x[i] + (vec_k[i] * vec_yn[i]))})

#create vector x_err = abs(xn.real - x_est)
vec_x_err <- c()
lapply(1:n, function(i){vec_x_err[i] <<- abs(vec_xn_[i] - vec_x_est[i])})

#create vector p_est = (I - (K * C)) * P 
vec_p_est <- c()
lapply(1:n, function(i){vec_p_est[i] <<- ((I - (vec_k[i] * C)) * vec_p[i])})

#view data frame
df <- data.frame(
  x = vec_x,
  xn.real = vec_xn_,
  u = vec_u,
  x.mean = vec_mean_x,
  x.stddev = vec_stddev_x,
  x.gaussian.noise = vec_g_noise_x,
  xn = vec_xn,
  y = vec_y,
  p = vec_p,
  pn = vec_pn,
  K = vec_k,
  yn = vec_yn,
  x.est = vec_x_est,
  x.err = vec_x_err,
  p.est = vec_p_est
)

print(df)