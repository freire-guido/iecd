n <- 10000

desvio <- abs(rnorm(1, 0, 1))
x <- rnorm(n, mean = 0, sd = desvio)
# print(desvio)
# print(sqrt(1/n * sum(x^2)))

# ej 11
rFinv = function(Finv, n) {
    return(Finv(runif(n, 0, 1)))
}

t <- 1000
esd <- 0
for (i in 1:10000) {
    x <- rFinv(function(x) return((log(1-x)/-t)^(1/3)), n) #inversa generalizada de U
    tmv <- n/sum(x^3)
    esd <- (tmv - t)^2
}
print(esd/n)