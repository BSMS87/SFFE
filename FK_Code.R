#Libraries setzen
library(haven)
library(foreign)
library(mosaic)

#Daten einlesen
q <- read_spss("ZA6702_v1-0-0.sav")
#q <- read_saw("ZA6702_v1-0-0.sav", use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(q) <- tolower(colnames(q))

#Auf Geschäftsführer einschränken --> Self-employed/ the owner of the business
q <- q[q$q29==1]

##Frage 46 - alle Daten hierzu ausgeben lassen
tally(q$q46)
str(q$q46)
sort(table(q$q46)) #Werte sortiert ausgeben

##Frage 46 - Werte als Tabelle ausgeben
tabq$q46 <- table(q$q46)
View(tabq$q46)

#Kategorien bestimmen - Frage 46
q$Berufsausbildung [q$q46==-1] <- "N/A"
q$Berufsausbildung[q$q46==1] <- "Prof. Lever"
q$Berufsausbildung  [q$q46==2] <- "Middle Level"
q$Berufsausbildung  [q$q46==3] <- "Junior Level"
q$Berufsausbildung  [q$q46==4] <- "forewoman"
q$Berufsausbildung  [q$q46==5] <- "skilled worker"
q$Berufsausbildung  [q$q46==6] <- "semi-skilled worker"
q$Berufsausbildung  [q$q46==7] <- "unskilled worker"
q$Berufsausbildung  [q$q46==8] <- "Agricultural worker"
q$Berufsausbildung  [q$q46==9] <- "Categories N/A"
tally(q$Berufsausbildung)

#Umbauen der Kategorien nach White / Blue Colour / Berufsausbildung - Frage 46
q$Berufsausbildung [q$q46==1] <- "Professional Level"
q$Berufsausbildung [q$q46==2] <- "Middle Level" 
q$Berufsausbildung [q$q46==3] <- "Low/Junior/Trainee Level"
q$Berufsausbildung [q$q46==4] <- "Middle Level"
q$Berufsausbildung [q$q46==5] <- "Professional Level" 
q$Berufsausbildung [q$q46==6] <- "Middle Level" 
q$Berufsausbildung [q$q46==7] <- "Low/Junior/Trainee Level" 
tally(q$Berufsausbildung)

#Spalten der Reihenfolge entsprechend aufbereiten - Frage 46
q$Berufsausbildung <- factor(q$Berufsausbildung, levels = c("Low/Junior/Trainee Level","Middle Level","Professional Level","N/A"))
tally(q$Berufsausbildung)

#Säulendiagramm darstellen - Frage 46
tabBerufsausbildung <- table(q$Berufsausbildung)
barplot(tabBerufsausbildung,xlab= 'Categories', cex.names = 0.6, ylab='Amount Frequency')

##GF_Boxplot erstellen

#Umbauen der Kategorien nach White / Blue Colour / Berufsausbildung mit entsprechenden Zahlen - Frage 46
q$Berufsausbildung [q$q46==1] <- 3 #"Professional Level"
q$Berufsausbildung [q$q46==2] <- 2 #"Middle Level" 
q$Berufsausbildung [q$q46==3] <- 1 #"Low/Junior/Trainee Level"
q$Berufsausbildung [q$q46==4] <- 2 #"Middle Level"
q$Berufsausbildung [q$q46==5] <- 3 #"Professional Level" 
q$Berufsausbildung [q$q46==6] <- 2 #"Middle Level" 
q$Berufsausbildung [q$q46==7] <- 1 #"Low/Junior/Trainee Level" 
tally(q$Berufsausbildung)

#Als Tabelle aufbereiten - Frage 46
tabBerufsausbildung <- table(q$Berufsausbildung)
View(tabquestion46_with_Category)

#Kategorien bestimmen - Frage 29
q$Beschäftigungsstatus [q$q29==1] <- "self-employed"
q$Beschäftigungsstatus  [q$q29==2] <- "paid/unpaid family Member"
q$Beschäftigungsstatus  [q$q29==3] <- "paid/unpaid family Member"
q$Beschäftigungsstatus  [q$q29==4] <- "employed"
tally(q$Beschäftigungsstatus)

#Spalten der Reihenfolge entsprechend aufbereiten - Frage 29
#q$Beschäftigungsstatus<- factor(q$Beschäftigungsstatus, levels = c(""))
#tally(q$Beschäftigungsstatus)

#Als Tabelle aufbereiten - Frage 29
tabBeschäftigungsstatus <- table(q$Beschäftigungsstatus)
View(tabBeschäftigungsstatus)

#Säulendiagramm darstellen - Frage 29
barplot(tabBeschäftigungsstatus,xlab= 'Categories', cex.names = 0.6, ylab='Amount Frequency')

#Zufriedenheit finanzieller Status - Frage 61
ZufriedenheitStatus <- q$q61_1
tally(q$q61_1)

#Zufriedenheit finanzieller Status - Zuordnung und Konsolidierung - Frage 61
q$ZufriedenheitStatus [q$q61_1==1] <- "sehr unzufrieden"
q$ZufriedenheitStatus [q$q61_1==2] <- "sehr unzufrieden"
q$ZufriedenheitStatus [q$q61_1==3] <- "unzufrieden"
q$ZufriedenheitStatus [q$q61_1==4] <- "unzufrieden"
q$ZufriedenheitStatus [q$q61_1==5] <- "neutral"
q$ZufriedenheitStatus [q$q61_1==6] <- "neutral"
q$ZufriedenheitStatus [q$q61_1==7] <- "zufrieden"
q$ZufriedenheitStatus [q$q61_1==8] <- "zufrieden"
q$ZufriedenheitStatus [q$q61_1==9] <- "sehr zufrieden"
q$ZufriedenheitStatus [q$q61_1==10] <- "sehr zufrieden"
q$ZufriedenheitStatus [q$q61_1==98] <- "N/A"
q$ZufriedenheitStatus [q$q61_1==99] <- "N/A"

##Boxplot/Mosaicplot/XChi/Prozentabelle/...ausgeben

#Boxplot auf Frage 29/Frage 46
gf_boxplot(Berufsausbildung~Beschäftigungsstatus, data=q)

#Boxplot auf Frage 29/Frage 46/Frage 61 -> Geht das überhaupt?
#gf_boxplot(Berufsausbildung~Beschäftigungsstatus~ZufriedenheitStatus, data=q)

#Mosaic auf Frage 29/Frage 46 
mosaicplot(Berufsausbildung~Beschäftigungsstatus, data=q)
mosaicplot(Beschäftigungsstatus~Berufsausbildung, data=q)

#Chitest auf Frage 29/Frage 46 
xchisq.test(Berufsausbildung~Beschäftigungsstatus, data=q)

#Tabellenausgabe auf Frage 29/Frage 46 
tally(Berufsausbildung~Beschäftigungsstatus, data=q)

#Prozentausgabe auf Frage 29/Frage 46
tally(Berufsausbildung~Beschäftigungsstatus, data=q, format="percent", useNA="no")

#gf_jiterausgabe auf Frage 29/Frage 46 
gf_jitter(Berufsausbildung~Beschäftigungsstatus, data=q)

#Korrelationausgabe auf Frage 29/Frage 46
q$Beschäftigungsstatus <- as.numeric(q$Beschäftigungsstatus)
cor.test(Berufsausbildung~Beschäftigungsstatus, data=q)