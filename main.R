
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

n <- 50
x0 <- 0
u0 <- 0

x0_est <- 0

x_est <- x0_est
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

#create vector x0 with fx
vec_x0 <- c(fx(0, x0))
lapply(2:n, function(i){vec_x0[i] <<- fx(i - 1, vec_x0[i-1])})

#create vector x with fx
vec_x <- c(fx(1, fx(0, x0)))
lapply(2:n, function(i){vec_x[i] <<- fx(i, vec_x[i-1])})

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
  xn = A * x + B * vec_u[i] + fw(i, vec_g_noise_x[i])
  vec_xn[i] <- xn
  
  #calc y = H*x + v
  y = H * x + fv(i, vec_g_noise_x[i])
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
  x0 = vec_x0,
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

plot(x <- 0:n, y <- c(x0, vec_x), "l", col="blue", xlab = "t", ylab = "x")

points(x, y, cex = .5, col = "dark blue")

lines(x <- 0:n, y <- c(x0_est ,vec_x_est), "l", col="green")
points(x, y, cex = .5, col = "dark green")
