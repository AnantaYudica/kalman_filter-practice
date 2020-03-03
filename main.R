
source("Utils.R")

fx <- function(x)
{
  return(x + 1 / (x + 1))
}

fu <- function(u)
{
  return (0)
}

fw <- function(w)
{
  return (0)
}

fv <- function(v)
{
  return (0)
}

n <- 10
x0 <- 1
u0 <- 0

x_est <- 0
p_est <- 1

A <- matrix(1, 1, 1)
AT <- t(A)

B <- matrix(1, 1, 1)

H <- matrix(1, 1, 1)
HT <- t(H)

I <- matrix(0, 1, 1)
diag(I) <- 1

Q <- 0.1

#initial data

#create vector x with fx
vec_x <- c(x0)
lapply(2:n, function(i){vec_x[i] <<- fx(vec_x[i-1])})

#create vector xn real with fx
vec_xn_real <- c(fx(x0))
lapply(2:n, function(i){vec_xn_real[i] <<- fx(vec_xn_real[i-1])})

#create vector u with fu
vec_u <- c(u0)
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

vec_xn <- c()
vec_y <- c()
vec_p <- c()
vec_pn <- c()
vec_k <- c()
vec_yn <- c()
vec_x_est <- c()
vec_p_est <- c()
vec_x_err <- c()

for (i in 1:n)
{
  x_prev = x_est
  x = vec_x[i]
  p = p_est
  
  #calc xn = A*x + B*u + w
  xn = A * x + B * vec_u[i] + fw(vec_g_noise_x[i])
  vec_xn[i] <- xn
  
  #calc y = H*x + v
  y = H * x + fv(vec_g_noise_x[i])
  vec_y[i] <- y
  
  #calc pn = (A * P * AT) + Q
  pn = (A * p * AT) + Q
  vec_p[i] <- p
  vec_pn[i] <- pn
  
  #calc K = (P * HT)/ ((H * P * HT) + Q)
  K = (p * HT) / ((H * p * HT) + Q)
  vec_k[i] <- K
  
  #calc yn = (y - (H * x))
  yn = (y - (H * x))
  vec_yn[i] <- yn
  
  #x_est = x + K * yn
  x_est <<- (x + K * yn)
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
  x = vec_x,
  xn.real = vec_xn_real,
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
  p.est = vec_p_est,
  x.err = vec_x_err
)

print(df)