library(mosaic)
library(haven)
library(tidyverse)

ds <- read_sav("ZA6702_v1-0-0.sav")
colnames(ds) <- tolower(colnames(ds))

## Dataframe bewerten
length(ds)
dim(ds)
head(ds)
str(ds)
print(ds)
print(summary(ds))

### Erste Fragestellung: Spielt die Familie oder die Freunden eine Rolle für den Geschäftserfolg?

# Wichtigkeit der Familie - Frage q1_1
ds$q1_1
tally(ds$q1_1)

ds$WichtigkeitFamilie[ds$q1_1==1] <- "Sehr Wichtig"
ds$WichtigkeitFamilie[ds$q1_1==2] <- "Ziemlich Wichtig"
ds$WichtigkeitFamilie[ds$q1_1==3] <- "Nicht Wichtig"
ds$WichtigkeitFamilie[ds$q1_1==4] <- "Nicht Wichtig"
ds$WichtigkeitFamilie[ds$q1_1==8] <- "Neutral"
ds$WichtigkeitFamilie[ds$q1_1==9] <- "N/A"
ds$WichtigkeitFamilie

# Sortieren der Werte
ds$WichtigkeitFamilie <- factor(ds$WichtigkeitFamilie, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$WichtigkeitFamilie)
typeof(ds$WichtigkeitFamilie)


# Graph - Wichtigkeit der Familie
gf_counts(~ WichtigkeitFamilie, data = ds, fill = ~ WichtigkeitFamilie, position = position_dodge())

# Zufriedenheit der Finanziellen Status - Frage q61_1
ds$q61_1
tally(ds$q61_1)
ds$ZufriedenheitStatus[ds$q61_1==1] <- "sehr unzufrieden"
ds$ZufriedenheitStatus[ds$q61_1==2] <- "sehr unzufrieden"
ds$ZufriedenheitStatus[ds$q61_1==3] <- "unzufrieden"
ds$ZufriedenheitStatus[ds$q61_1==4] <- "unzufrieden"
ds$ZufriedenheitStatus[ds$q61_1==5] <- "neutral"
ds$ZufriedenheitStatus[ds$q61_1==6] <- "neutral"
ds$ZufriedenheitStatus[ds$q61_1==7] <- "zufrieden"
ds$ZufriedenheitStatus[ds$q61_1==8] <- "zufrieden"
ds$ZufriedenheitStatus[ds$q61_1==9] <- "sehr zufrieden"
ds$ZufriedenheitStatus[ds$q61_1==10] <- "sehr zufrieden"
ds$ZufriedenheitStatus[ds$q61_1==98] <- "N/A"
ds$ZufriedenheitStatus[ds$q61_1==99] <- "N/A"

# Sortieren der Werte
ds$ZufriedenheitStatus <- factor(ds$ZufriedenheitStatus, levels = c ("sehr unzufrieden","unzufrieden","neutral","zufrieden","sehr zufrieden","N/A"))
tally(ds$ZufriedenheitStatus)
typeof(ds$ZufriedenheitStatus)

# Graph - Wichtigkeit der Familie mit abhängigkeit der zufriedenheit
gf_jitter(ZufriedenheitStatus~WichtigkeitFamilie, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFamilie, position = position_dodge())

# Übersicht auf die Prozentuale Verteilung der Wichtigkeit von Familie mit abhängigkeit der zufriedenheit
tally(WichtigkeitFamilie ~ ZufriedenheitStatus, data=ds, format = "percent")


# Wichtigkeit der Freunde - Frage q1_2
ds$q1_2
tally(ds$q1_2)
ds$WichtigkeitFreunde[ds$q1_2==1] <- "Sehr Wichtig"
ds$WichtigkeitFreunde[ds$q1_2==2] <- "Ziemlich Wichtig"
ds$WichtigkeitFreunde[ds$q1_2==3] <- "Nicht Wichtig"
ds$WichtigkeitFreunde[ds$q1_2==4] <- "Nicht Wichtig"
ds$WichtigkeitFreunde[ds$q1_2==8] <- "Neutral"
ds$WichtigkeitFreunde[ds$q1_2==9] <- "N/A"
ds$WichtigkeitFreunde

# Sortieren der Werte
ds$WichtigkeitFreunde <- factor(ds$WichtigkeitFreunde, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$WichtigkeitFreunde)
typeof(ds$ZufriedenheitStatus)

# Graph erstellen - Wichtigkeit der Freunde 
gf_counts(~ WichtigkeitFreunde, data = ds, fill = ~ WichtigkeitFreunde, position = position_dodge())

# Graph - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
gf_jitter(ZufriedenheitStatus~WichtigkeitFreunde, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFreunde, position = position_dodge())

# Übersicht auf die Prozentuale Verteilung der Wichtigkeit von Freunde mit abhängigkeit der zufriedenheit
tally(WichtigkeitFreunde ~ ZufriedenheitStatus, data=ds, format = "percent")

### Test - Wichtigkeit der Familie mit abhängigkeit der zufriedenheit
cor.test(q61_1~q1_1, data = ds)
xchisq.test(q61_1~q1_1, data = ds)

### Test - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
cor.test(q61_1~q1_1, data = ds)
xchisq.test(q61_1~q1_1, data = ds)


### Zweite Fragestellung: Wirkt sich mehr Arbeit oder mehr Freizeit zu haben?

# Wichtigkeit der Freizeit - Frage q1_3
ds$q1_3
tally(ds$q1_3)
ds$WichtigkeitFreizeit[ds$q1_3==1] <- "Sehr Wichtig"
ds$WichtigkeitFreizeit[ds$q1_3==2] <- "Ziemlich Wichtig"
ds$WichtigkeitFreizeit[ds$q1_3==3] <- "Nicht Wichtig"
ds$WichtigkeitFreizeit[ds$q1_3==4] <- "Nicht Wichtig"
ds$WichtigkeitFreizeit[ds$q1_3==8] <- "Neutral"
ds$WichtigkeitFreizeit[ds$q1_3==9] <- "N/A"
ds$WichtigkeitFreizeit

# Sortieren der Werte
ds$WichtigkeitFreizeit <- factor(ds$WichtigkeitFreizeit, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$WichtigkeitFreizeit)
typeof(ds$WichtigkeitFreizeit)

# Graph - Wichtigkeit der Freizeit 
gf_jitter(ZufriedenheitStatus~WichtigkeitFreizeit, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFreizeit, position = position_dodge())

# Test - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
cor.test(q61_1~q1_3, data = ds)
xchisq.test(q61_1~q1_3, data = ds)

# Lohnt sich mehr oder Weniger Arbeit - Frage q11_5
tally(ds$q11_5)
ds$MehrWenigerArbeit[ds$q11_5==1] <- "Hard Work sehr Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==2] <- "Hard Work sehr Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==3] <- "Hard Work Ziemlich Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==4] <- "Hard Work Ziemlich Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==5] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==6] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==7] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==8] <- "Hard Work Nicht Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==9] <- "Hard Work Nicht Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==10] <- "Hard Work Nicht Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==98] <- "Neutral"
ds$MehrWenigerArbeit[ds$q11_5==99] <- "N/A"

# Sortieren der Werte
ds$WichtigkeitFreizeit <- factor(ds$WichtigkeitFreizeit, levels = c ("Hard Work sehr Wichtig","Hard Work Ziemlich Wichtig","Hard Work Wichtig","Hard Work Nicht Wichtig","Neutral", "N/A"))

# überprüfen
tally(ds$MehrWenigerArbeit)
typeof(ds$MehrWenigerArbeit)

# Graph - Mehr oder wienger Arbeit: 
gf_counts(~ MehrWenigerArbeit, data = ds, fill = ~MehrWenigerArbeit, position = position_dodge() )
gf_jitter(ZufriedenheitStatus~MehrWenigerArbeit, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ MehrWenigerArbeit, position = position_dodge())
mosaicplot(ZufriedenheitStatus~MehrWenigerArbeit, data = ds)

tally(MehrWenigerArbeit~ZufriedenheitStatus , data=ds, format = "percent")

# Dataframe bereitstellen 
DF_MehrWiengerArbeit_ZufriedenheitStatus <- tally(MehrWenigerArbeit ~ ZufriedenheitStatus, data=ds)
print(DF_MehrWiengerArbeit_ZufriedenheitStatus)

# Graph erstellen
mosaicplot(DF_MehrWiengerArbeit_ZufriedenheitStatus)


# Test - 
cor.test(q61_1~q11_5, data = ds)



