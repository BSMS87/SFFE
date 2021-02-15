library (haven)
library(mosaic)

##ToDo: N/A vor der grafischen Darstellung ausfiltern D <- D[D$q55 != 999, ]

q <- read_sav("ZA6702_v1-0-0.sav")#, use.value.labels=FALSE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(q) <- tolower(colnames(q))

#str(q) #Struktur

#Ausgabe aller Werte zum Label - Geschäftsführer
q$q29

#Häufigkeit - Anzahl Personen (Owner)
tally(q$q29)
#->268

#Filter nur Geschäftsführer -> Self-employed/the owner of the business = 1
q <- q[q$q29==1, ] 

#Bildung
q$q56

#Abschluss - Definition und Konsolidierung
q$Abschluss[q$q56==1] <- "Kein Abschluss"
q$Abschluss[q$q56==2] <- "Kein Abschluss"
q$Abschluss[q$q56==3] <- "Grundschule"
q$Abschluss[q$q56==4] <- "Grundschule"
q$Abschluss[q$q56==6] <- "Grundschule"
q$Abschluss[q$q56==5] <- "Mittelschule"
q$Abschluss[q$q56==7] <- "Mittelschule"
q$Abschluss[q$q56==8] <- "Mittelschule"
q$Abschluss[q$q56==9] <- "Universität"
q$Abschluss[q$q56==98] <- "N/A"
q$Abschluss[q$q56==99] <- "N/A"

#Abschluss - Werte sortieren
q$Abschluss <- factor(q$Abschluss, levels = c ("Kein Abschluss","Grundschule","Mittelschule","Universität","N/A"))

tally(q$Abschluss)
#Abschluss - Balkendiagramm
gf_bar(~ Abschluss, data=q)

#Zufriedenheit finanzieller Status
zfstatus <- q$q61_1
tally(q$q61_1)

#Zufriedenheit finanzieller Status - Zuordnung und Konsolidierung
q$zfstatus [q$q61_1==1] <- "sehr unzufrieden"
q$zfstatus [q$q61_1==2] <- "sehr unzufrieden"
q$zfstatus [q$q61_1==3] <- "unzufrieden"
q$zfstatus [q$q61_1==4] <- "unzufrieden"
q$zfstatus [q$q61_1==5] <- "neutral"
q$zfstatus [q$q61_1==6] <- "neutral"
q$zfstatus [q$q61_1==7] <- "zufrieden"
q$zfstatus [q$q61_1==8] <- "zufrieden"
q$zfstatus [q$q61_1==9] <- "sehr zufrieden"
q$zfstatus [q$q61_1==10] <- "sehr zufrieden"
q$zfstatus [q$q61_1==98] <- "N/A"
q$zfstatus [q$q61_1==99] <- "N/A"

tally(q$zfstatus)

#fstatus - Werte sortieren
q$zfstatus <- factor(q$zfstatus, levels = c ("sehr unzufrieden","unzufrieden","neutral","zufrieden","sehr zufrieden","N/A"))

tally(q$zfstatus)

#Kreuztabelle als Vorbereitung für mosaic
tab_education_satisfaction <- tally(Abschluss ~ zfstatus, data=q)
print(tab_education_satisfaction)

#Diagramm erstellen
mosaicplot(tab_education_satisfaction)
print(tab_education_satisfaction)

#Bei gleich hohen Werten wird der erste ausgegeben (method = first).
#Abschlüsse "Kein Abschluss" und "N/A" sind Randfälle, die nicht ins Gewicht fallen
#Daher werden sie ausgeschlossen.
tab2 <- colnames(tab_edu_sat_alt)[max.col(tab_edu_sat_alt, ties.method = "first")]
print(tab2)

#Erwartete Werte werden sehr klein sein, daher werden die Annäherungen nicht unbedingt korrekt sein (-> Warnmeldung)
xchisq.test(tab_education_satisfaction)

#Prozentuale Verteilung
tally(Abschluss ~ zfstatus, data=q, format = "percent")

#Einschätzung sozialer Status Q69
tally(q$q69)

#Zuordnung und Konsolidierung - Zugehörigkeit soziale Schichtzugehörigkeit (sschicht)
q$sschicht[q$q69==1] <- "Oberschicht"
q$sschicht[q$q69==2] <- "Obere Mittelschicht"
q$sschicht[q$q69==3] <- "Untere Mittelschicht"
q$sschicht[q$q69==4] <- "Arbeiterklasse"
q$sschicht[q$q69==5] <- "Unterschicht"
q$sschicht[q$q69==8] <- "N/A"
q$sschicht[q$q69==9] <- "N/A"

#Sortierung sschicht
q$sschicht <- factor(q$sschicht, levels = c ("Oberschicht", "Obere Mittelschicht","Untere Mittelschicht","Arbeiterklasse","Unterschicht","N/A"))

tally(q$sschicht)