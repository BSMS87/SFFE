####Code-Projektarbeit Wissenschaftliche Methodik

#Libraries setzen
library(haven)
library(foreign)
library(mosaic)

#Data Source (ds) - Datei einlesen
ds <- read_sav("ZA6702_v1-0-0.sav")#, use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(ds) <- tolower(colnames(ds))

#Auf Geschäftsführer einschränken --> Self-employed/ the owner of the business
ds <- ds[ds$q29==1]

##Frage 46 - alle Daten hierzu ausgeben lassen
tally(ds$q46)
str(ds$q46)
sort(table(ds$q46)) #Werte sortiert ausgeben

##Frage 46 - Werte als Tabelle ausgeben
tabq$q46 <- table(ds$q46)
View(tabq$q46)

#Kategorien bestimmen - Frage 46
ds$Berufsausbildung [ds$q46==-1] <- "N/A"
ds$Berufsausbildung[ds$q46==1] <- "Prof. Lever"
ds$Berufsausbildung  [ds$q46==2] <- "Middle Level"
ds$Berufsausbildung  [ds$q46==3] <- "Junior Level"
ds$Berufsausbildung  [ds$q46==4] <- "forewoman"
ds$Berufsausbildung  [ds$q46==5] <- "skilled worker"
ds$Berufsausbildung  [ds$q46==6] <- "semi-skilled worker"
ds$Berufsausbildung  [ds$q46==7] <- "unskilled worker"
ds$Berufsausbildung  [ds$q46==8] <- "Agricultural worker"
ds$Berufsausbildung  [ds$q46==9] <- "Categories N/A"
tally(ds$Berufsausbildung)

#Umbauen der Kategorien nach White / Blue Colour / Berufsausbildung - Frage 46
ds$Berufsausbildung [ds$q46==1] <- "Professional Level"
ds$Berufsausbildung [ds$q46==2] <- "Middle Level" 
ds$Berufsausbildung [ds$q46==3] <- "Low/Junior/Trainee Level"
ds$Berufsausbildung [ds$q46==4] <- "Middle Level"
ds$Berufsausbildung [ds$q46==5] <- "Professional Level" 
ds$Berufsausbildung [ds$q46==6] <- "Middle Level" 
ds$Berufsausbildung [ds$q46==7] <- "Low/Junior/Trainee Level" 
tally(ds$Berufsausbildung)

#Spalten der Reihenfolge entsprechend aufbereiten - Frage 46
ds$Berufsausbildung <- factor(ds$Berufsausbildung, levels = c("Low/Junior/Trainee Level","Middle Level","Professional Level","N/A"))
tally(ds$Berufsausbildung)

#Säulendiagramm darstellen - Frage 46
tabBerufsausbildung <- table(ds$Berufsausbildung)
barplot(tabBerufsausbildung,xlab= 'Categories', cex.names = 0.6, ylab='Amount Frequency')

##GF_Boxplot erstellen

#Umbauen der Kategorien nach White / Blue Colour / Berufsausbildung mit entsprechenden Zahlen - Frage 46
ds$Berufsausbildung [ds$q46==1] <- 3 #"Professional Level"
ds$Berufsausbildung [ds$q46==2] <- 2 #"Middle Level" 
ds$Berufsausbildung [ds$q46==3] <- 1 #"Low/Junior/Trainee Level"
ds$Berufsausbildung [ds$q46==4] <- 2 #"Middle Level"
ds$Berufsausbildung [ds$q46==5] <- 3 #"Professional Level" 
ds$Berufsausbildung [ds$q46==6] <- 2 #"Middle Level" 
ds$Berufsausbildung [ds$q46==7] <- 1 #"Low/Junior/Trainee Level" 
tally(ds$Berufsausbildung)

#Als Tabelle aufbereiten - Frage 46
tabBerufsausbildung <- table(ds$Berufsausbildung)
View(tabquestion46_with_Category)

#Kategorien bestimmen - Frage 29
ds$Beschäftigungsstatus [ds$q29==1] <- "self-employed"
ds$Beschäftigungsstatus  [ds$q29==2] <- "paid/unpaid family Member"
ds$Beschäftigungsstatus  [ds$q29==3] <- "paid/unpaid family Member"
ds$Beschäftigungsstatus  [ds$q29==4] <- "employed"
tally(ds$Beschäftigungsstatus)

#Spalten der Reihenfolge entsprechend aufbereiten - Frage 29
#ds$Beschäftigungsstatus<- factor(ds$Beschäftigungsstatus, levels = c(""))
#tally(ds$Beschäftigungsstatus)

#Als Tabelle aufbereiten - Frage 29
tabBeschäftigungsstatus <- table(ds$Beschäftigungsstatus)
View(tabBeschäftigungsstatus)

#Säulendiagramm darstellen - Frage 29
barplot(tabBeschäftigungsstatus,xlab= 'Categories', cex.names = 0.6, ylab='Amount Frequency')

#Zufriedenheit finanzieller Status - Frage 61
ZufriedenheitStatus <- ds$q61_1
tally(ds$q61_1)

#Zufriedenheit finanzieller Status - Zuordnung und Konsolidierung - Frage 61
ds$ZufriedenheitStatus [ds$q61_1==1] <- "sehr unzufrieden"
ds$ZufriedenheitStatus [ds$q61_1==2] <- "sehr unzufrieden"
ds$ZufriedenheitStatus [ds$q61_1==3] <- "unzufrieden"
ds$ZufriedenheitStatus [ds$q61_1==4] <- "unzufrieden"
ds$ZufriedenheitStatus [ds$q61_1==5] <- "neutral"
ds$ZufriedenheitStatus [ds$q61_1==6] <- "neutral"
ds$ZufriedenheitStatus [ds$q61_1==7] <- "zufrieden"
ds$ZufriedenheitStatus [ds$q61_1==8] <- "zufrieden"
ds$ZufriedenheitStatus [ds$q61_1==9] <- "sehr zufrieden"
ds$ZufriedenheitStatus [ds$q61_1==10] <- "sehr zufrieden"
ds$ZufriedenheitStatus [ds$q61_1==98] <- "N/A"
ds$ZufriedenheitStatus [ds$q61_1==99] <- "N/A"

##Boxplot/Mosaicplot/XChi/Prozentabelle/...ausgeben

#Boxplot auf Frage 29/Frage 46
gf_boxplot(Berufsausbildung~Beschäftigungsstatus, data=ds)

#Boxplot auf Frage 29/Frage 46/Frage 61 -> Geht das überhaupt?
#gf_boxplot(Berufsausbildung~Beschäftigungsstatus~ZufriedenheitStatus, data=ds)

#Mosaic auf Frage 29/Frage 46 
mosaicplot(Berufsausbildung~Beschäftigungsstatus, data=ds)
mosaicplot(Beschäftigungsstatus~Berufsausbildung, data=ds)

#Chitest auf Frage 29/Frage 46 
xchisq.test(Berufsausbildung~Beschäftigungsstatus, data=ds)

#Tabellenausgabe auf Frage 29/Frage 46 
tally(Berufsausbildung~Beschäftigungsstatus, data=ds)

#Prozentausgabe auf Frage 29/Frage 46
tally(Berufsausbildung~Beschäftigungsstatus, data=ds, format="percent", useNA="no")

#gf_jiterausgabe auf Frage 29/Frage 46 
gf_jitter(Berufsausbildung~Beschäftigungsstatus, data=ds)

#Korrelationausgabe auf Frage 29/Frage 46
ds$Beschäftigungsstatus <- as.numeric(ds$Beschäftigungsstatus)
cor.test(Berufsausbildung~Beschäftigungsstatus, data=ds)
