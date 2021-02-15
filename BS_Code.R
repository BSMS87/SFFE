
library(haven)
library(mosaic)

## Einlesen ## 
D <- read_sav("ZA6702_v1-0-0.sav")

## Auf Geschaeftfuehrer einschraenken ##
D <- D[D$Q29 == 1, ]
colnames(D) <- tolower(colnames(D))

## Was ist in der Variablen q49 drin? ##
tally(D$q55)
## bereinigung der Extremwerte, die verzerren das Ergebnis
D <- D[D$q55 != 999, ]

D$Alter <- D$q55

## grober Ueberblick ueber die Zahlen min max ##
sort(D$Alter)
## oder so ##
unique(D$Alter)

D$Altersklasse <- D$q55
D$Altersklasse <- ifelse(D$Alter <= 34,  "1", D$Altersklasse)
D$Altersklasse <- ifelse(D$Alter > 34 & D$Alter <= 44,  "2", D$Altersklasse)
D$Altersklasse <- ifelse(D$Alter > 44 & D$Alter <= 54,  "3", D$Altersklasse)
D$Altersklasse <- ifelse(D$Alter > 54 & D$Alter <= 64,  "4", D$Altersklasse)
D$Altersklasse <- ifelse(D$Alter > 64,  "5", D$Altersklasse)

tally(D$Altersklasse)

## In a data frame the columns contain different types of data, ##
## but in a matrix all the elements are the same type of data ##
## nur zur Ueberpruefung der Altersklassenbildung
a <- data.frame(Alter=D$Alter, Altersklasse=D$Altersklasse)

D$Arbeitsrecht <- D$q8_1

b <- data.frame(D$q8_1, D$Arbeitsrecht)

D$Arbeitsrecht[D$q8_1 == 8] <- 4
D$Arbeitsrecht[D$q8_1 == 9] <- 5

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 

par(mfrow=c(1,2)) 
boxplot(Arbeitsrecht ~ Altersklasse, data = D, xlab = "Altersklasse", ylab="Arbeitsrecht", col=rainbow(5))
barplot(table(D$Altersklasse, D$Arbeitsrecht),  beside = TRUE, xlab= "Mehr Recht auf Arbeit", ylab = "Haeufigkeit", legend.text = c("Altersklasse [<34]", "Altersklasse [35-44]", "Altersklasse [45-54]", "Altersklasse [55-64]", "Altersklasse [>65]"), ylim = c(0,60), col=coul, args.legend=list(cex=0.60,x="topright"))

#Untersuche, ob der Mittelwerts-Unterschied zwischen den 5 Gruppen statistisch signifikant ist. 
#Da es sich hierbei um mehr als 2 Gruppen handelt, ist die Varianzanalyse das geeignete Verfahren.

#Links von der Tilde steht die untersuchte Variable (Arbeitsrecht) und rechts von der Tilde die Gruppierungsvariable (Altersklasse).
Mod1 <- aov(Arbeitsrecht~Altersklasse, data=D)
summary(Mod1)

#Bei einem Post-Hoc-Test wird jede Gruppe mit jeder verglichen. 
#Es ist daher möglich zu beurteilen, zwischen welchen Gruppen sich jeweils signifikante Unterschiede ergeben. 
TukeyHSD(aov(D$Arbeitsrecht~D$Altersklasse))

#b <- data.frame(UnabhängigkeitM = D$q3a_1, UnabhaengigkeitW = D$q3b_1, EhrgeizM = D$q3a_14, EhrgeizW = D$q3b_14)

#dim(D)
## 0-Vektor Spalte ##
#D$Traditionell <- numeric(268)
#D$Traditionell <- ifelse(D$q3a_1 == 1 & D$q3b_1 == 0 | D$q3a_14 == 1 & D$q3b_14 == 0, "1", "2")
## D$Traditionell[D$q3a_1 == 1 & D$q3b_1 == 0 | D$q3a_14 == 1 & D$q3b_14 == 0] <- 1 ##

#c <- data.frame(D$q26, D$q8_1)
#D$Religion <- D$q26
#D$Religion[D$q26 < 4] <- 1
#D$Religion[D$q26 > 3] <- 2
#D$Religion[D$q26 > 7] <- 3
#D$Religion[D$q26 == 88] <- 4


#d <- data.frame(D$Arbeit, D$Religion)
#gf_boxplot(Arbeitsrecht ~ Religion, data = D)



