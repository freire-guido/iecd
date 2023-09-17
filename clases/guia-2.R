n <- 0

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
    esd <- esd + (tmv - t)^2
}
print(esd/n)

# ej 17
n <- 3000
t <- 1
sd1 <- 0
sd2 <- 0
for (i in 1:n) {
    x <- rFinv(function(x) return((log(1-x)/-t)), n)
    t1 <- exp(-1/mean(x))
    t2 <- sum(x >= 1)/n
    sd1 <- sd1 + (t1 - exp(-t))^2
    sd2 <- sd2 + (t2 - exp(-t))^2
    print(c(t1, t2))
}
print(c("P(x>1) = ", exp(-t)))
print(c("t1", "t2"))
print(c(sd1/n, sd2/n))
print(c(exp(-2*t)/t^4, exp(-t)-exp(-2*t)))