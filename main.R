
source("Utils.R")

fx <- function(i, x)
{
  return (sin(i / 8))
}

fu <- function(i, u)
{
  return (0)
}

fw <- function(i, w)
{
  return (0)
}

fv <- function(i, v)
{
  return (0)
}

fnoise <- function(v)
{
  return(v - (runif(1, 0, 0.2) - 0.1))
}

n <- 50
x0 <- 0
u0 <- 0

x0_est <- 0
p0_est <- 1

x_est <- x0_est
p_est <- p0_est

A <- matrix(1, 1, 1)
AT <- t(A)

B <- matrix(1, 1, 1)

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
vec_u <- c(fu(0, u0))
lapply(2:n, function(i){vec_u[i] <<- fu(i - 1, vec_u[i-1])})

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

plot(x <- 0:n, y <- c(x0, vec_x), "l", col="blue", xlab = "t", ylab = "x")
points(x, y, cex = .5, col = "dark blue", p = 2)
lines(x <- 0:n, y <- c(x0_est ,vec_x_est), "l", col="green")
points(x, y, cex = .5, col = "dark green")
