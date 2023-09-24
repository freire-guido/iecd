ic = function(x, nivel, de = NA) {
    a = 1 - nivel
    if (is.na(de)) {
        de = sd(x)
        quant = qt(1 - a/2, length(x) - 1)
    } else {
        quant = qnorm(1 - a/2)
    }
    u = mean(x)
    return(c(u - quant*de/sqrt(length(x)), u + quant*de/sqrt(length(x))))
}
x = c(5.1, 5.2, 5.6, 5.1, 5.5, 5.8, 5.9, 4.9, 5.2, 5.6)
print(ic(x, 0.95, 0.25))