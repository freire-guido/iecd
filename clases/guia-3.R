icNorm = function(x, nivel, de = NA) {
    a = 1 - nivel
    if (is.na(de)) {
        de = sd(x)
        quant = qt(1 - a/2, length(x) - 1)
    } else {
        quant = qnorm(1 - a/2)
    }
    return(c(u - quant*de/sqrt(length(x)), u + quant*de/sqrt(length(x))))
}
x = c(5.1, 5.2, 5.6, 5.1, 5.5, 5.8, 5.9, 4.9, 5.2, 5.6)
# print(icNorm(x, 0.95, 0.25))
# print(icNorm(x, 0.95))

icExp = function(x, nivel) {
    a = 1 - nivel
    suma = sum(x)
    return(c(qchisq(a/2, 2*length(x))/(2*suma), qchisq(1-a/2, 2*length(x))/(2*suma)))
}

x = c(25, 45 ,50, 61, 39, 40, 45 ,47, 38, 39, 54, 60, 39, 46, 39, 50, 42, 50, 62, 50)
print(icExp(x, 0.99))
print(length(x)/sum(x)) # Comparo el i.c. de nivel 0.99 con el MLE de muestra exponencial

#ej. 6
p_h = 0.3
desv = p_h*(1-p_h)
cuant = qnorm(0.05/2)
print(c(p_h - sqrt(desv/100), p_h + sqrt(desv/100)))