#######
##Fragestellungen
#Ist die Zufriedenheit mit der finanziellen Situation an den Bildungsstand gekoppelt?
#In Abhängigkeit dazu: Welcher Gesellschaftsschicht ordnen sich die befragten Personen zu?
#######    

library(haven)
library(mosaic)
library(ggplot2)
library(ggpubr)
library(e1071)

##Data Source (ds) - Datei einlesen
ds <- read_sav("ZA6702_v1-0-0.sav", encoding=NULL, user_na=FALSE, col_select=NULL, skip=0, n_max=Inf,.name_repair="unique")#, use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)

#Setze Spaltennamen auf Kleinbuchstaben
colnames(ds) <- tolower(colnames(ds))

#Struktur - wenn bestimmte Spalten betrachtet werden sollen
#str(ds) 

#Übersicht Personen, die an der Befragung teilgenommen haben (unbenannt)
ds$q29

tally(ds$q29)

#Anzahl Personen, die diese Frage beantwortet haben
sum(ds$q29)
#371

#1.Kriterium - Definition und Konsolidierung - Beschäftigtenstatus (BStatus)
ds$BStatus[ds$q29==1] <- "Geschäftsführer"
ds$BStatus[ds$q29==2] <- "bezahlte Familienangehörige"
ds$BStatus[ds$q29==3] <- "unbezahlte Familienangehörige"
ds$BStatus[ds$q29==4] <- "bezahlter Angestellter"
ds$BStatus[ds$q29==5] <- "unbezahlter Angestellter"
ds$BStatus[ds$q29==6] <- "Keine Antwort"


# Zuweisung Variable - Beschäftigtenstatus
BStatus <- factor(ds$BStatus, levels = c("Geschäftsführer","bezahlte Familienangehörige", "unbezahlte Familienangehörige","bezahlter Angestellter","unbezahlter Angestellter","Keine Antwort"))

tally(BStatus)

#Darstellung aller beteiligten der Befragung (Säulendiagramm)
gf_bar(~ BStatus, ylab = "Anzahl Personen", title = "Beschäftigtenstatus")

#Filter nur Geschäftsführer -> Self-employed/the owner of the business = 1
ds <- ds[ds$q29==1, ]

#Ergebnis Filter nur Geschäftsführer
gf_bar(~ BStatus, data=ds)

#2.Kriterium - Bildung
ds$q56

#Abschluss - Definition und Konsolidierung (letzter Abschluss zählt )
ds$Abschluss[ds$q56==1] <- "Kein Abschluss"
ds$Abschluss[ds$q56==2] <- "Kein Abschluss"
ds$Abschluss[ds$q56==3] <- "Grundschule"
ds$Abschluss[ds$q56==4] <- "Grundschule"
ds$Abschluss[ds$q56==6] <- "Grundschule"
ds$Abschluss[ds$q56==5] <- "Mittelschule"
ds$Abschluss[ds$q56==7] <- "Mittelschule"
ds$Abschluss[ds$q56==8] <- "Mittelschule"
ds$Abschluss[ds$q56==9] <- "Universität"
#ds$Abschluss[ds$q56==98] <- "N/A" - wird nicht benötigt, da nicht relevant
#ds$Abschluss[ds$q56==99] <- "N/A" - wird nicht benötigt, da nicht relevant

#Abschluss - Werte sortieren + NA ausfiltern
Abschluss <- factor(ds$Abschluss, levels = c ("Kein Abschluss","Grundschule","Mittelschule","Universität"))

tally(Abschluss)

#Abschluss - Balkendiagramm
gf_bar(~ Abschluss, ylab = "Anzahl Personen", title = "Höchster Abschluss")

#3.Kriterium - Zufriedenheit finanzieller Status (ZFStatus)
ds$q61_1

#Zufriedenheit finanzieller Status - Zuordnung und Konsolidierung
ds$ZFStatus [ds$q61_1==1] <- "sehr unzufrieden"
ds$ZFStatus [ds$q61_1==2] <- "sehr unzufrieden"
ds$ZFStatus [ds$q61_1==3] <- "unzufrieden"
ds$ZFStatus [ds$q61_1==4] <- "unzufrieden"
ds$ZFStatus [ds$q61_1==5] <- "neutral"
ds$ZFStatus [ds$q61_1==6] <- "neutral"
ds$ZFStatus [ds$q61_1==7] <- "zufrieden"
ds$ZFStatus [ds$q61_1==8] <- "zufrieden"
ds$ZFStatus [ds$q61_1==9] <- "sehr zufrieden"
ds$ZFStatus [ds$q61_1==10] <- "sehr zufrieden"
#ds$ZFStatus [ds$q61_1==98] <- "N/A"
#ds$ZFStatus [ds$q61_1==99] <- "N/A"

#Zufriedenheit finanzieller Status - Werte sortieren
ZFStatus <- factor(ds$ZFStatus, levels = c ("sehr unzufrieden","unzufrieden","neutral","zufrieden","sehr zufrieden"))

tally(ZFStatus)

#Kreuztabelle als Vorbereitung für mosaic
tab_education_satisfaction <- tally(ZFStatus ~ Abschluss, useNA = "no")
print(tab_education_satisfaction)

#Diagramm - Mosaicplot
mosaicplot(tab_education_satisfaction)
print(tab_education_satisfaction)

#Korrelation finanzielle Zufriedenheit und Bildungsstand
xchisq.test(ZFStatus ~ Abschluss)
#p-Wert: 90% - Variablen stehe in keinem Zusammenhang


#4.Kriterium - Einschätzung sozialer Status Q69
ds$q69

#Zuordnung und Konsolidierung - Soziale Schichtzugehörigkeit (SSchicht)
ds$SSchicht[ds$q69==1] <- "Oberschicht"
ds$SSchicht[ds$q69==2] <- "Obere Mittelschicht"
ds$SSchicht[ds$q69==3] <- "Untere Mittelschicht"
ds$SSchicht[ds$q69==4] <- "Arbeiterklasse"
ds$SSchicht[ds$q69==5] <- "Unterschicht"
#ds$sschicht[ds$q69==8] <- "N/A" - die letzten beiden Antwortmöglichkeiten beziehen sich auf
#ds$sschicht[ds$q69==9] <- "N/A"

#Sortierung sschicht
SSchicht <- factor(ds$SSchicht, levels = c ("Unterschicht", "Arbeiterklasse", "Untere Mittelschicht","Obere Mittelschicht","Oberschicht"))

tally(SSchicht)


#Darstellung Naive Bayes
#Naive Bayes basiert auf der Idee, dass die Variablen unabhängig voneinander sind - daher nicht miteinander korrelieren
Model <- naiveBayes( ZFStatus ~., data=as.data.frame(Abschluss))
Model

gf_jitter(ZFStatus ~ Abschluss)

#Anteil Abschluss Universität sehr hoch - Neue Abgrenzungen zur vereinfachten Darstellung
#Neue Abgrenzung Abschluss -> Grenze Mittelschule/Universität
ds$Abschluss2[ds$q56==1] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==2] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==3] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==4] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==6] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==5] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==7] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==8] <- "Niedrigere Bildung"
ds$Abschluss2[ds$q56==9] <- "Höhere Bildung"

#Zuweisung neue Aufteilung Bildungsgrad
Abschluss2 <- factor(ds$Abschluss2, levels = c ("Niedrigere Bildung", "Höhere Bildung"))
tally (Abschluss2)

#Prozentuale Verteilung Bildungsgrad - ohne NA (zur Vereinfachung)
tally (na.omit(Abschluss2), format = "percent")
#prop.table(Abschluss2_Prozent)

#Neue Abgrenzung Zufriedenheit finanzieller Status -> Grenze bei 5/6
ds$ZFStatus2 [ds$q61_1==1] <- "unzufrieden"
ds$ZFStatus2 [ds$q61_1==2] <- "unzufrieden"
ds$ZFStatus2 [ds$q61_1==3] <- "unzufrieden"
ds$ZFStatus2 [ds$q61_1==4] <- "unzufrieden"
ds$ZFStatus2 [ds$q61_1==5] <- "unzufrieden"
ds$ZFStatus2 [ds$q61_1==6] <- "zufrieden"
ds$ZFStatus2 [ds$q61_1==7] <- "zufrieden"
ds$ZFStatus2 [ds$q61_1==8] <- "zufrieden"
ds$ZFStatus2 [ds$q61_1==9] <- "zufrieden"
ds$ZFStatus2 [ds$q61_1==10] <-"zufrieden"

#ZUweisung neue Aufteilung ZFStatus - ohne NA (zur Vereinfachung)
ZFStatus2 <- (ds$ZFStatus2)
tally(ZFStatus2)

#Naive Bayes - Kosolidierte Werte (ZFStatus2/Abschluss2)
Model2 <- naiveBayes( ZFStatus2 ~., data=as.data.frame(Abschluss2))
Model2

gf_jitter(ZFStatus2 ~ Abschluss2)


#Selbsteinschätzung Sozial Schicht
#Naive Bayes - Kosolidierte Werte (Abschluss/SSchicht)
Model4<- naiveBayes( SSchicht ~., data=as.data.frame(Abschluss2))
Model4

#Naive Bayes - Kosolidierte Werte (ZFStatus2/SSchicht) - Vollständigkeitshalber
Model3 <- naiveBayes(SSchicht ~., data=as.data.frame(ZFStatus2))
Model3
