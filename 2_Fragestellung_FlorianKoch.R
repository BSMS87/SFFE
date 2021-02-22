####Code-Projektarbeit Wissenschaftliche Methodik

#Libraries setzen
library(haven)
library(foreign)
library(mosaic)

#Data Source (ds) - Datei einlesen
ds <- read_sav("ZA6702_v1-0-0.sav")#, use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)

#Setze Spaltennamen auf Kleinbuchstaben
colnames(ds) <- tolower(colnames(ds))

#Auf Geschäftsführer einschränken --> Self-employed/ the owner of the business
ds <- ds[ds$q29==1]

##Frage 41 - Welche der folgenden Möglichkeiten würden Sie nutzen, wenn Sie einen Mitarbeiter suchen? JA = 1 /NEIN = 0

#alle Daten hierzu ausgeben lassen (in verschiedenen Varianten)
tally(ds$q41_1) #Freunde
tally(ds$q41_2) #Geschäftsinhaber
tally(ds$q41_3) #Mitarbeiter
tally(ds$q41_7) #Bekanntenkreis
tally(ds$q41_8) #Familienmitglieder

#Kreisdigramm darstellen
slices <- tally(ds$q41_1) + tally(ds$q41_2) + tally(ds$q41_3) + tally(ds$q41_7) + tally(ds$q41_8)
pct <- round(slices/sum(slices)*100)
lbls <- c("n/a","Nein", "Ja")
lbls <- paste(lbls, pct) #Prozente hinzufügen
lbls <- paste(lbls,"%",sep="") #% hinzufügen
pie(slices, labels = lbls, main="Kreisdiagramm")

##Frage 46 - Bevor Sie dieses Unternehmen gegründet haben/ hier angefangen haben zu arbeiten: Was war Ihre vorherige Tätigkeit und berufliche Position?

#alle Daten hierzu ausgeben lassen (in verschiedenen Varianten)
tally(ds$q46)
sort(table(ds$q46)) #Werte aufsteigend
sort(table(ds$q46), decreasing = TRUE) #Werte absteigend

#Umbauen der Kategorien nach White / Blue Colour / Berufsausbildung
ds$Berufsausbildung [ds$q46==1] <- "Hoher Berufsausbildungsgrad"
ds$Berufsausbildung [ds$q46==2] <- "Mittlerer Berufsausbildungsgrad" 
ds$Berufsausbildung [ds$q46==3] <- "Geringer Berufsausbildungsgrad"
ds$Berufsausbildung [ds$q46==4] <- "Mittlerer Berufsausbildungsgrad"
ds$Berufsausbildung [ds$q46==5] <- "Hoher Berufsausbildungsgrad" 
ds$Berufsausbildung [ds$q46==6] <- "Mittlerer Berufsausbildungsgrad" 
ds$Berufsausbildung [ds$q46==7] <- "Geringer Berufsausbildungsgrad" 
tally(ds$Berufsausbildung)

#Sortierung der Kategorien
ds$Berufsausbildung <-factor(ds$Berufsausbildung, levels = c("Geringer Berufsausbildungsgrad" ,"Mittlerer Berufsausbildungsgrad", "Hoher Berufsausbildungsgrad"))

#Säulendiagramm darstellen
tabBerufsausbildung <- table(ds$Berufsausbildung)
barplot(tabBerufsausbildung,xlab= 'Kategorie', cex.names = 0.6, ylab='Häufigkeit')

##Frage 29 - Welche Art von Anstellung ist gegeben?

#alle Daten hierzu ausgeben lassen (in verschiedenen Varianten)
tally(ds$q29)
sort(table(ds$q29)) #Werte aufsteigend
sort(table(ds$q29), decreasing = TRUE) #Werte absteigend

#Kategorien bestimmen 
ds$Beschäftigungsstatus [ds$q29==1] <- "Selbstständiger"
ds$Beschäftigungsstatus [ds$q29==2] <- "Familienang."
ds$Beschäftigungsstatus [ds$q29==3] <- "Familienang."
ds$Beschäftigungsstatus [ds$q29==4] <- "Angestellter"
tally(ds$Beschäftigungsstatus)

#Säulendiagramm darstellen 
tabBeschäftigungsstatus <- table(ds$Beschäftigungsstatus)
barplot(tabBeschäftigungsstatus,xlab= 'Kategorie', cex.names = 0.6, ylab='Häufigkeit')

##Boxplot/Mosaicplot/XChi/Prozentabelle/...ausgeben

#Prozentausgabe auf Frage 29/Frage 46
tally(Berufsausbildung~Beschäftigungsstatus, data=ds, format="percent", useNA="no")

#Mosaicplot auf Frage 29/Frage 46 
mosaicplot(Berufsausbildung~Beschäftigungsstatus, cex.axis=0.7,data=ds, main = "Mosaicplot")

#Chitest auf Frage 29/Frage 46 
xchisq.test(Berufsausbildung~Beschäftigungsstatus, data=ds)
