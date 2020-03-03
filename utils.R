
gaussian.noise <- function(v, mean_v, stddev_v)
{
  return((1 / ((stddev_v * sqrt(2 * pi)))) *
                exp(-((v - mean_v)^2/(2 * (stddev_v ^ 2)))))
}
