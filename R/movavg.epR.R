
### this is taken from the pracma package with added na.rm
movavg.ep <- function (x, n, type = c("s", "t", "w", "m", "e", "r"))
{
  stopifnot(is.numeric(x), is.numeric(n), is.character(type))
if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1)
  stop("Window length 'n' must be a single integer greater 1.")
nx <- length(x)
if (n >= nx)
  stop("Window length 'n' cannot be greater then length of time series.")
y <- numeric(nx)
if (type == "s") {
  for (k in 1:(n - 1)) y[k] <- mean(x[1:k], na.rm = TRUE)
  for (k in n:nx) y[k] <- mean(x[(k - n + 1):k], na.rm = TRUE)
}
else if (type == "t") {
  n <- ceiling((n + 1)/2)
  s <- movavg(x, n, "s")
  y <- movavg(s, n, "s")
}
else if (type == "w") {
  for (k in 1:(n - 1)) y[k] <- 2 * sum((k:1) * x[k:1],na.rm = TRUE)/(k *
                                                          (k + 1))
  for (k in n:nx) y[k] <- 2 * sum((n:1) * x[k:(k - n +
                                                 1)], na.rm= TRUE)/(n * (n + 1))
}
else if (type == "m") {
  y[1] <- x[1]
  for (k in 2:nx) y[k] <- y[k - 1] + (x[k] - y[k - 1])/n
}
else if (type == "e") {
  a <- 2/(n + 1)
  y[1] <- x[1]
  for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
}
else if (type == "r") {
  a <- 1/n
  y[1] <- x[1]
  for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
}
else stop("The type must be one of 's', 't', 'w', 'm', 'e', or 'r'.")
return(y)
}


sum()
