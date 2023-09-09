n <- 1000000
desvio <- abs(rnorm(1, 0, 1))
x <- rnorm(n, mean = 0, sd = desvio)
print(desvio)
print(sqrt(1/n * sum(x^2)))