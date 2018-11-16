library(Rlab)
library(combinat)

# 1. naloga

# a)
# začetni podatki
S_0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03
W = c(1:6)

# vse možnosti razvoja trga

#matrika <- matrix(, nrow = T, ncol = (T+1))
#vektor <- c(S_0, S_0, S_0, S_0, S_0)
#matrika[, 1] <- vektor

#for(column in 2:(T+1)){
 # vektor[1] <- vektor[1] * u
#  vektor[2] <- vektor[2] * u
 # vektor[3] <- vektor[3] * d
#  vektor[4] <- vektor[4] * d
 # vektor[5] <- vektor[5] * u
#  matrika[, column] <- vektor
#}

pot1 = c(50, 52.50, 49.88, 47.38, 45.01, 47.26)
pot2 = c(50, 52.50, 52.12, 57.88, 60.78, 63.81)
pot3 = c(50, 47.50, 49.88, 47.38, 45.01, 42.76)
pot4 = c(50, 47.50, 45.12, 47.38, 45.01, 47.26)
pot5 = c(50, 52.50, 49.88, 52.37, 54.99, 52.24)

izplacilo <- function(vrsta, W, type){
  K = 0
  L = 0
  for(i in 1:length(W)) {
    K <- K + vrsta[i]*W[i]
    L <- L + W[i]
  }
  razlika <- vrsta[length(vrsta)] - K/L
  if (type == 'call'){
    izplacilo <- max(razlika, 0)
  }
  else {
    razlika <- K/L - vrsta[length(vrsta)]
    izplacilo <- max(razlika, 0)
  }
  izplacilo
}

prodajna1 <- izplacilo(pot1, W, "put")
prodajna2 <- izplacilo(pot2, W, "put")
prodajna3 <- izplacilo(pot3, W, "put")
prodajna4 <- izplacilo(pot4, W, "put")
prodajna5 <- izplacilo(pot5, W, "put")

nakupna1 <- izplacilo(pot1, W, "call")
nakupna2 <- izplacilo(pot2, W, "call")
nakupna3 <- izplacilo(pot3, W, "call")
nakupna4 <- izplacilo(pot4, W, "call")
nakupna5 <- izplacilo(pot5, W, "call")

# 2. naloga

binomski <- function(S0,u,d,R,T,W,type) {
  q <- (1+R-d)/(u-d)
  h <- hcube(rep(2,T)) -1
  razvoji <- rowSums(h)
  Q <- q ^ (T-razvoji) * (1-q) ^ razvoji
  poti <- u ^ (1-h) * d ^ h
  drevo <- cbind(rep(S_0, 2 ^ T), poti)
  vrednosti <- t(apply(drevo, 1, cumprod))
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x, W, type))
  E <- sum(izplacila * Q)
  cena <- E / ((1+R)^T)
  return(cena)
}


# monte carlo

monte <- function(S0, u, d, R, T, W, type, N) {
  q <- (1+R-d)/(u-d)
  stanja <- matrix(rbinom(T*N, 1, q), N, T)
  stanja <- u ^ (stanja) * d ^ (1-stanja)
  
  razvoji <- rowSums(stanja)
  Q <- q ^ (T-razvoji) * (1-q) ^ razvoji
  
  vrednosti <- t(apply(drevo, 1, cumprod))
  drevo <- cbind(S_0, vrednosti)
  
  izplacila <- apply(vrednosti, 1, function(x) izplacilo(x, W, type))
  E <- sum(izplacila)/N
  cena <- E / ((1+R)^T)
  return(cena)
}



