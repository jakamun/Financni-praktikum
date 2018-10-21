library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(tidyr)

# 1. naloga

# priprava tabel
prvo_leto = read_csv('hist_EURIBOR_2013.CSV', locale=locale(encoding="cp1250"))
prvo_leto <- prvo_leto[c(1,2,24,44,64,85,107,127,150,172,193,216,237)]
prvo_leto <- prvo_leto[-c(3,7,8,10,11,13,14),]
#View(prvo_leto)

drugo_leto <- read_csv('hist_EURIBOR_2014.csv', locale = locale(encoding ="cp1250"))
drugo_leto <- drugo_leto[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]
#View(drugo_leto)

tretje_leto <- read_csv('hist_EURIBOR_2015.csv', locale = locale(encoding = "cp1250"))
tretje_leto <- tretje_leto[c(1,2,23,43,65,85,105,127,150,171,193,215,236)]
#View(tretje_leto)

zdruzena <- cbind(prvo_leto, drugo_leto, tretje_leto)
zdruzena <- zdruzena[-c(1,14,27)]
zdruzena <- as.data.frame(t(zdruzena))
colnames(zdruzena) <- c('1w', '2w', '1m', '2m', '3m', '6m', '9m', '12m')
View(zdruzena)

#risanje grafov
polletje <- ts(zdruzena[6], start = c(2013, 1), frequency = 12)
konec_leta <- ts(zdruzena[8], start = c(2013, 1), frequency = 12)
ts.plot(polletje, konec_leta, col = c("blue", "red"), main = "Euribor", xlab = "Leto", ylab = "Obrestna mera(%)")
legend(x=2013.5, y=0.2, legend = c("6m", "12m"),col = c("blue", "red"), lty = 1, bty = "n")

# 2. naloga
# Izbrani datumi: '03/06/2013', '2/05/2014', '02/01/2015'

tabela <- zdruzena[c(6,17,25),] # izbrani trije datumi

prva_tabela <- as.data.frame(t(tabela[1,]))
prva_tabela$cas <- c(0.25,0.5,1,2,3,6,9,12)
prva_tabela$datum <-  character(8)
prva_tabela$datum <- '03/06/2013'
prva_tabela <- prva_tabela %>% select(datum, cas, procenti = '03/06/2013')

druga_tabela <- as.data.frame(t(tabela[2,]))
druga_tabela$cas <- c(0.25,0.5,1,2,3,6,9,12)
druga_tabela$datum <- character(8)
druga_tabela$datum <- '2/05/2014'
druga_tabela <- druga_tabela %>% select(datum, cas, procenti = '2/05/2014')

tretja_tabela <- as.data.frame(t(tabela[3,]))
tretja_tabela$cas <- c(0.25,0.5,1,2,3,6,9,12)
tretja_tabela$datum <- character(8)
tretja_tabela$datum <- '02/01/2015'
tretja_tabela <- tretja_tabela %>% select(datum, cas, procenti = '02/01/2015')

skupaj <- rbind(prva_tabela, druga_tabela, tretja_tabela)

ggplot(data = skupaj, aes(x=cas, y=procenti, color=datum)) + labs(title= 'EURIBOR za izbrane datume', x= 'Čas', y='Obrestna mera(%)') + geom_line()
# Komentar:
# Logično je da so krivulje naraščajoče, večje kot je dospetje večja bo obrestna mera

# 3. naloga
# a)
term_obr_m <- c(0) # L(0,6,12)
tab <- zdruzena[,c(6,8)]
tab <- cbind(tab,term_obr_m)

U <- 6
T <- 12
for (i in 1:36) {
  stevec <- 1 + T * tab[i,2]
  imenovalec <- 1 + U * tab[i, 1]
  tab[i, 3] <- ((stevec / imenovalec) - 1)/(T - U)}

# b)
Euribor6m <- zdruzena[,6]
tab <- cbind(tab,Euribor6m)
primerjava <- tab[,c(0,4,3)]
napoved <- c(c(NA,NA,NA,NA,NA,NA),primerjava[-c(31:36),2])
primerjava[,2] <- napoved


