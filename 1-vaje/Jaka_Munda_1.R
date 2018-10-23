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

prvo_leto = read_csv('1-vaje\\hist_EURIBOR_2013.CSV', locale=locale(encoding="cp1250"))
prvo_leto <- prvo_leto[c(1,2,24,44,64,85,107,127,150,172,193,216,237)]
prvo_leto <- prvo_leto[-c(3,7,8,10,11,13,14),]
#View(prvo_leto)

drugo_leto <- read_csv('1-vaje\\hist_EURIBOR_2014.csv', locale = locale(encoding ="cp1250"))
drugo_leto <- drugo_leto[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]
#View(drugo_leto)

tretje_leto <- read_csv('1-vaje\\hist_EURIBOR_2015.csv', locale = locale(encoding = "cp1250"))
tretje_leto <- tretje_leto[c(1,2,23,43,65,85,105,127,150,171,193,215,236)]
#View(tretje_leto)

zdruzena <- cbind(prvo_leto, drugo_leto, tretje_leto)
zdruzena <- zdruzena[-c(1,14,27)]
zdruzena <- as.data.frame(t(zdruzena))
colnames(zdruzena) <- c('1w', '2w', '1m', '2m', '3m', '6m', '9m', '12m')
#View(zdruzena)

# graf
polletje <- ts(zdruzena[6], start = c(2013, 1), frequency = 12)
konec_leta <- ts(zdruzena[8], start = c(2013, 1), frequency = 12)
ts.plot(polletje, konec_leta, col = c("blue", "red"), main = "Obnasanje obrestnih mer", xlab = "Leto", ylab = "Obrestna mera(%)")
legend(x=2013.5, y=0.2, legend = c("6 m", "12 m"),col = c("blue", "red"), lty = 1, bty = "n")

# 2. naloga
# Izbrani datumi: '03/06/2013', '2/05/2014', '02/01/2015'

datumi <- zdruzena[c(6,17,25),] # izbrani trije datumi

prvi_datum <- as.data.frame(t(datumi[1,]))
prvi_datum$Cas <- c(0.25,0.5,1,2,3,6,9,12)
prvi_datum$Datum <-  character(8)
prvi_datum$Datum <- '03/06/2013'
prvi_datum <- prvi_datum %>% select(Datum, Cas, Procenti = '03/06/2013')

drugi_datum <- as.data.frame(t(datumi[2,]))
drugi_datum$Cas <- c(0.25,0.5,1,2,3,6,9,12)
drugi_datum$Datum <- character(8)
drugi_datum$Datum <- '2/05/2014'
drugi_datum <- drugi_datum %>% select(Datum, Cas, Procenti = '2/05/2014')

tretji_datum <- as.data.frame(t(datumi[3,]))
tretji_datum$Cas <- c(0.25,0.5,1,2,3,6,9,12)
tretji_datum$Datum <- character(8)
tretji_datum$Datum <- '02/01/2015'
tretji_datum <- tretji_datum %>% select(Datum, Cas, Procenti = '02/01/2015')

izbrani_datumi <- rbind(prvi_datum, drugi_datum, tretji_datum)

graf <- ggplot(data = izbrani_datumi, aes(x=Cas, y=Procenti, color=Datum)) +
  labs(title= 'EURIBOR za izbrane datume', x= 'Čas', y='Obrestna mera(%)') + 
  geom_line() + theme(plot.title = element_text(hjust = 0.5))

# Komentar:
# Logično je da so krivulje naraščajoče, kajti kasneje kot bo dospetje večja bo obrestna mera.

# 3. naloga
# a)

terminska <- zdruzena[,c(6,8)]
colnames(terminska) <- c('Euribor6m', 'Euribor12m')
terminska$ter_obr_m <- ((1 + 12 * terminska$Euribor12m)/(1 + 6 * terminska$Euribor6m) - 1)/6

# b)

primerjava <- terminska[c(1,2)]
primerjava$Napoved6m <- c(c(NA, NA, NA, NA, NA, NA), terminska$ter_obr_m[-c(31:36)])

# c)

primerjava1 <- primerjava
primerjava1$Leto <- as.vector(cbind(seq(2013, 2013, length.out = 12),
                               seq(2014, 2014, length.out = 12), seq(2015, 2015, length.out = 12))) 
primerjava1 <- primerjava1[-c(1:6),]

lm_skupaj <- ggplot(primerjava1, aes(x = Napoved6m, y = Euribor6m)) + 
  geom_point() + 
  geom_abline() +
  geom_smooth(method=lm) +
  coord_cartesian(xlim=c(-0.05,0.45),ylim=c(-0.05,0.45)) + 
  labs(title='Linearna regresija', x='Napovedana obrestna mera', y='Dejanska obrestna mera') +
  theme(plot.title = element_text(hjust = 0.5))

lm_2013 <- ggplot(primerjava1 %>% filter(Leto == 2013), aes(x = Napoved6m, y = Euribor6m))+
  geom_point() + geom_abline() + geom_smooth(method = lm) + coord_cartesian(xlim=c(0.2,0.35),ylim=c(0.2,0.35)) +
  labs(title='Regresija za leto 2013', x='Napovedana obrestna mera', y='Dejanska obrestna mera') +
  theme(plot.title = element_text(hjust = 0.5))

lm_2014 <- ggplot(primerjava1 %>% filter(Leto == 2014), aes(x = Napoved6m, y = Euribor6m))+
  geom_point() + geom_abline() + geom_smooth(method = lm) + coord_cartesian(xlim=c(0.1,0.45),ylim=c(0.1,0.45)) +
  labs(title='Regresija za leto 2014', x='Napovedana obrestna mera', y='Dejanska obrestna mera') +
  theme(plot.title = element_text(hjust = 0.5))

lm_2015 <- ggplot(primerjava1 %>% filter(Leto == 2015), aes(x = Napoved6m, y = Euribor6m))+
  geom_point() + geom_abline() + geom_smooth(method = lm) + coord_cartesian(xlim=c(-0.05,0.25),ylim=c(-0.05,0.25)) +
  labs(title='Regresija za leto 2015', x='Napovedana obrestna mera', y='Dejanska obrestna mera') +
  theme(plot.title = element_text(hjust = 0.5))

#Da bi hipoteza pričakovanj trga veljala bi se morali napovedana in dejanska obrestna mera ujemati.
#Torej bi morale točke ležati na simetrali lihih kvadrantov. 
#V mojem primeru empirični podatki ne potrjujejo hipoteze.