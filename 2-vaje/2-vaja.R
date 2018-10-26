library(actuar)
# uvozim podatke

podatki <- scan('vzorec3.txt')
#histogram
graf <- hist(podatki)

# b)
parametri <- mde(podatki, dexp, start = list(rate = 1), measure = 'CvM')
rate <- parametri$estimate
hist(podatki, probability = TRUE)
curve(dexp(x, rate), add=TRUE)

# d)
# S = sum(1,N)Yi
# Waldovi identiteti:
# E(s) = E(E(S|N))=E(N)E(Y)
# var(s) = var(y)E(N) + E(Y^2)var(N)

# podatki binomske porazdelitve
n = 20
p = 1/2
#bin = rbinom(n, size=20,p)

# upanje
upanje_y <- 1 / rate
upanje_s = 20 * upanje_y * 1/2

#varianca

var_y <- 1/(rate ^ 2)
drugi_moment_y <- var_y + upanje_y ^2
var_s <- var_y * n *p + drugi_moment_y * n/4
# poglej doma ni čist prov

# 2
h = 0.25
n = 100
diskretna <- discretize(pexp(x,rate),0,n*h,step=h, method = 'upper')
plot(diffinv(diskretna))
plot(stepfun(seq(0, (n-1)*h, by = h), diffinv(diskretna)))
curve(pexp(x, rate), add = TRUE, col = 'blue')

# c)



