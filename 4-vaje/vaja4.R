library(readr)

# 1. naloga
# a)

srebro <- read_csv("4-vaje\\srebro.csv", locale=locale(encoding = "cp1250"))
srebro <- srebro[c(126:1),c(5)]
srebro$Close <- as.numeric(gsub("\\$", "", srebro$Close))

# b)
vrsta <- ts(srebro)
graf <- ts.plot(vrsta, ylab="Dollar", main="Vrednost srebra")

# 2. naloga

G <- function(vrsta, k) {
  dolzina <- length(vrsta)
  glajenje <- c()
  for (i in 1:(dolzina-k)) {
    glajenje[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  return(ts(glajenje))
}

zglajena5 <- G(vrsta, 5)

# napoved 
dolzina <- length(vrsta)
napoved5 <- sum(vrsta[(dolzina-5+1):dolzina])/5 

graf2 <- ts.plot(vrsta, zglajena5, xlab='Leto', ylab ='Vrednost srebra', main = 'Drseče povprečje reda 5', col=c('black', 'red'),lwd = 2:1)
legend('topright', 
       legend = c('vrsta', 'zglajena5'),
       col = c('black', 'red'),
       lwd = 1:2,
       bty = 'n')

# srednja kvadratna napaka
MSE <- function(vrsta, zglajena_vrsta, k) {
  T <- length(vrsta)
  delna_vsota <- 0
  for (i in (k+1): T) {
    delna_vsota <- delna_vsota + (vrsta[i] - zglajena_vrsta[i-k])^2
  }
  napaka <- (1/(T-k))*delna_vsota
  return(napaka)
}

napaka5 <- MSE(vrsta, zglajena5, 5)

# e)

zglajena15 <- G(vrsta, 15)
napoved15 <- sum(vrsta[(dolzina-15+1):dolzina])/15
napaka15 <- MSE(vrsta, zglajena15, 15)

zglajena30 <- G(vrsta, 30)
napoved30 <- sum(vrsta[(dolzina-30+1):dolzina])/30
napaka30 <- MSE(vrsta, zglajena30, 30)

# graf

par(mfrow = c(2,2))
graf2 <- ts.plot(vrsta, zglajena5, ylab = "Vrednost srebra", main = "Drseče povprečje reda 5", col = c("black","red") , lwd = 3)
legend('topright', c('časovna vrsta', 'zglajena vrsta'),col = c("black","red"), lwd = 2:2, bty = 'n')

graf3 <- ts.plot(vrsta, zglajena15, ylab = "Vrednost srebra", main = "Drseče povprečje reda 15", col = c("black","red") , lwd = 3)
legend('topright', c('časovna vrsta', 'zglajena vrsta'),col = c("black","red"), lwd = 2:2, bty = 'n')

graf4 <- ts.plot(vrsta, zglajena30, ylab = "Vrednost srebra", main = "Drseče povprečje reda 30", col = c("black","red") , lwd = 3)
legend('topright', c('časovna vrsta', 'zglajena vrsta'),col = c("black","red"), lwd = 2:2, bty = 'n')
par(mfrow = c(1,1))


# 3. naloga
# a)

EG <- function(vrsta, alpha) {
  glajena <- c(vrsta[1])
  dolzina <- length(vrsta)
  for (i in 2:dolzina) {
    glajena[i] <- alpha*vrsta[i] + (1-alpha)*glajena[i-1]
  }
  return(ts(glajena))
}

#b.primer
#alpha = 0.3

zglajena_vrsta <- EG(vrsta, 0.3)
#napoved_cetrtina <- zglajena_alpha_cetrtina[60]

graf_6 <- ts.plot(vrsta, zglajena_vrsta, ylab = "Vrednost srebra",
                  main = "Eksponentno glajenje", col =c("black", "red")  , lwd = 3)
legend('topright', c('časovna vrsta', 'zglajena vrsta'),col = c("black","red"), lwd = 2:2 )

#c

MSE_e <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina-1))
}

optimalni_alpha <- optimize(MSE_e, c(0,1), vrsta = vrsta)

#d.primer 
zglajena_optimalna <- EG(vrsta, optimalni_alpha$minimum)

graf_7 <- ts.plot(vrsta,zglajena_optimalna, ylab = "Vrednost srebra", 
                  main = "Eksponentno glajenje, optimalni alpha", col =c("black", "red")  , lwd = 3)
legend('topright', c('časovna vrsta', 'zglajena vrsta'),col = c("black","red"), lwd = 2:2, bty = 'n' )
