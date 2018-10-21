library(readr)
library(dplyr)
library(rvest)


prvo_leto = read_csv('hist_EURIBOR_2013.CSV', locale=locale(encoding="cp1250"))
prvo_leto <- prvo_leto[c(1,2,24,44,64,85,107,127,150,172,193,216,237)]
#sesti <- prvo_leto[9,]
#dvanajsti <- prvo_leto[15,]
#prva <- data.frame(row.names = colnames(prvo_leto))
prvo_leto <- as.data.frame(t(prvo_leto))
colnames(prvo_leto) <- prvo_leto[1,]
prvo_leto <- prvo_leto[-1,]
View(prvo_leto)

drugo_leto <- read_csv('hist_EURIBOR_2014.csv', locale = locale(encoding ="cp1250"))
drugo_leto <- drugo_leto[c(1,2,24,44,65,85,106,127,150,171,193,216,236)]
drugo_leto <- as.data.frame(t(drugo_leto))
colnames(drugo_leto) <- drugo_leto[1,]
drugo_leto <- drugo_leto[-1,]

tretje_leto <- read_csv('hist_EURIBOR_2015.csv', locale = locale(encoding = "cp1250"))
tretje_leto <- tretje_leto[c(1,2,23,43,65,85,105,127,150,171,193,215,236)]
tretje_leto <- as.data.frame(t(tretje_leto))
colnames(tretje_leto) <- tretje_leto[1,]
tretje_leto <- tretje_leto[-1,]

zdruzena <- cbind(prvo_leto, drugo_leto, tretje_leto)
View(zdruzena)


