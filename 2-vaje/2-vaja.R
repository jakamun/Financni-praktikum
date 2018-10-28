library(actuar)

# 1. naloga

# a) uvoz podatkov in histogram
podatki <- scan('2-vaje\\vzorec3.txt')
histogram <- hist(podatki, main = 'Histogram odškodnin', xlab = 'Višina odškodnine', ylab = 'Frekvenca')

# b) ocenjevanje lambde za eksponentno porazdelitev
parametri <- mde(podatki, dexp, start = list(rate = 1 / 200), measure = 'CvM')
lambda <- parametri$estimate

# c) graf gostote eksponentne porazdelitve
hist(podatki, main = 'Histogram odškodnin', xlab = 'Višina odškodnine', probability = TRUE)
curve(dexp(x, lambda), col = 'red', add=TRUE)

# d) Računanje kumulativna škode S
# S = sum(1,N)Yi;  N je binomska porazdelitev v našem primeru
# Waldovi identiteti:
#   E(s) = E(E(S|N))=E(E(NY|N))=E(N)E(Y)
#   var(s) = var(y)E(N) + E(Y^2)var(N)

# podatki binomske porazdelitve
n = 20
p = 1/2

# upanje
upanje_y <- 1 / lambda
upanje_s0 <- n * p * upanje_y 

#varianca
var_y <- 1/(lambda ^ 2)
drugi_moment_y <- var_y + upanje_y ^ 2
var_s0 <- var_y * n * p + drugi_moment_y * n/4

# 2. naloga Določanje porazdelitve kumulativne škode s Panjerjevim algoritmom
# a) Z zaokroževanjem diskretizirajte porazdelitev spremenljivke Y 
h = 0.25
n = 100
diskretna <- discretize(pexp(x,lambda),0 ,n * h,step = h)

# b) graf
plot(diffinv(diskretna)) # diffinv smo dali zato da smo inverzno dobil ker nam disretize vrne ravno obratno
plot(stepfun(seq(0, (n-1) * h, by = h), diffinv(diskretna)), 
     main = "Eksponentna porazdelitev", xlab = "x", ylab = "Porazdelitvena funkcija")
curve(pexp(x, lambda), add = TRUE, col = 'red')
legend("bottomright", legend=c('diskretizacija eksponentne porazdelitve', 'eksponentna porazelitev'),
       col=c('black','red'), lty=1:1, cex=0.8)

# c) S Panjerjevim algoritmom izračunajte porazdelitveno funkcijo kumulativne škode S.
# S = sum(1,N)Yi; N binomska

panjer <- aggregateDist(method = 'recursive', model.freq = 'binom', model.sev = diskretna,
              size = n, prob = p, x.scale = h, maxit = 10000, tol = 0.025)

# d) upanje in varianca

upanje_s1 <- mean(panjer)
var_s1 <- sum(diff(panjer) * knots(panjer) ^ 2) - upanje_s1 ^ 2

# 3. naloga Monte-Carlo metoda
# a) Simulacija 10000 vrednosti slučajne spremenljivke S

simulacija_N <- rbinom(10000, n, p)
simulacija_S <- c()

for (i in simulacija_N){
  simulacija_S <- c(simulacija_S, sum(rexp(i, lambda)))
}
simulacija_S

# b) upanje in varianca

upanje_s2 <- mean(simulacija_S)
var_s2 <- var(simulacija_S)

# c) graf

plot(ecdf(simulacija_S),
     col = 'red',
     add = TRUE)

# Vidimo da se grafa nekoliko razlikujeta