x <- runif(250, 0, 10)
y <- rgamma(250, x ** 3, x)

rnp <- function(x, y, h, K = dnorm) {
  return(function(x0) {
    sumK = K(t(t(x%*%t(array(1, length(x0)))) - x0)/h)
    return(y%*%sumK/colSums(sumK))
  })
}

mt <- rnp(x, y, 0.5)
plot(x, y)
curve(mt, add = TRUE, col = "red")