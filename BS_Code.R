library(mosaic)

## Einlesen ## 
ds <- read_sav("ZA6702_v1-0-0.sav")

## Auf Geschaeftfuehrer einschraenken ##
ds <- ds[ds$Q29 == 1, ]
colnames(ds) <- tolower(colnames(ds))

## Was ist in der Variablen q49 drin? ##
tally(ds$q55)
## bereinigung der Extremwerte, die verzerren das Ergebnis
ds <- ds[ds$q55 != 999, ]

ds$Alter <- ds$q55

## grober Ueberblick ueber die Zahlen min max ##
unique(D$Alter)

ds$Altersklasse <- ds$q55
ds$Altersklasse <- ifelse(ds$Alter <= 34,  "1", ds$Altersklasse)
ds$Altersklasse <- ifelse(ds$Alter > 34 & ds$Alter <= 44,  "2", ds$Altersklasse)
ds$Altersklasse <- ifelse(ds$Alter > 44 & ds$Alter <= 54,  "3", ds$Altersklasse)
ds$Altersklasse <- ifelse(ds$Alter > 54 & ds$Alter <= 64,  "4", ds$Altersklasse)
ds$Altersklasse <- ifelse(ds$Alter > 64,  "5", ds$Altersklasse)

## Ueberbruefung der Klasseneinteilung ##
sort(ds$Alter)
tally(ds$Altersklasse)

## In a data frame the columns contain different types of data, ##
## but in a matrix all the elements are the same type of data ##
## nur zur Ueberpruefung der Altersklassenbildung
a <- data.frame(Alter=D$Alter, Altersklasse=D$Altersklasse)

## Ueberpruefung ##
library(plyr)
arrange(a,Altersklasse,Alter)

ds$Arbeitsrecht <- ds$q8_1

## Ueberpruefung ##
b <- data.frame(ds$q8_1, ds$Arbeitsrecht)
arrange(b, ds.Arbeitsrecht)

## weiß ich nicht & keine Angabe ##
ds$Arbeitsrecht[ds$q8_1 == 8] <- 4
ds$Arbeitsrecht[ds$q8_1 == 9] <- 5

## Ueberpruefung von 'weiß ich nicht' (wn) & 'keine Angabe' (ka) ##
wnKa <- data.frame(Alt = ds$q8_1, Neu = ds$Arbeitsrecht)
arrange(wnKa, desc(Neu))

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 

par(mfrow=c(1,2)) 
boxplot(Arbeitsrecht ~ Altersklasse, data = ds, xlab = "Altersklasse", ylab="Arbeitsrecht", col=rainbow(5))
barplot(table(ds$Altersklasse, ds$Arbeitsrecht),  beside = TRUE, xlab= "Mehr Recht auf Arbeit", ylab = "Haeufigkeit", legend.text = c("Altersklasse [<34]", "Altersklasse [35-44]", "Altersklasse [45-54]", "Altersklasse [55-64]", "Altersklasse [>65]"), ylim = c(0,60), col=coul, args.legend=list(cex=0.60,x="topright"))


## kleine Ueberpruefung der Mediane## 
x <- ds$Arbeitsrecht[ds$Altersklasse == 1]
median(x)
y <- ds$Arbeitsrecht[ds$Altersklasse == 2]
median(y)
z <- ds$Arbeitsrecht[ds$Altersklasse == 3]
median(z)

#Untersuche, ob der Mittelwerts-Unterschied zwischen den 5 Gruppen statistisch signifikant ist. 
#Da es sich hierbei um mehr als 2 Gruppen handelt, ist die Varianzanalyse das geeignete Verfahren.

#Links von der Tilde steht die untersuchte Variable (Arbeitsrecht) und rechts von der Tilde die Gruppierungsvariable (Altersklasse).
Mod1 <- aov(Arbeitsrecht ~ Altersklasse, data=ds)
summary(Mod1)

#Bei einem Post-Hoc-Test wird jede Gruppe mit jeder verglichen. 
#Es ist daher möglich zu beurteilen, zwischen welchen Gruppen sich jeweils signifikante Unterschiede ergeben. 
TukeyHSD(aov(Arbeitsrecht ~ Altersklasse, data = ds))


