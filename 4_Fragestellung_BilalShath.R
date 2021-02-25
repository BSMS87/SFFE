library(mosaic)
library(haven)

ds <- read_sav("ZA6702_v1-0-0.sav")

# Spaltennamen auf Kleinbuchstaben umsetzen
colnames(ds) <- tolower(colnames(ds))

## Dataframe bewerten
length(ds)
dim(ds)
head(ds)
str(ds)
print(ds)
print(summary(ds))

### Erste Fragestellung: Spielen Familie oder Freunde eine Rolle für den Geschäftserfolg?​?

# Wichtigkeit der Familie - Frage q1_1
ds$q1_1
tally(ds$q1_1)

ds$wichtigkeit_familie[ds$q1_1==1] <- "Sehr Wichtig"
ds$wichtigkeit_familie[ds$q1_1==2] <- "Ziemlich Wichtig"
ds$wichtigkeit_familie[ds$q1_1==3] <- "Nicht Wichtig"
ds$wichtigkeit_familie[ds$q1_1==4] <- "Nicht Wichtig"
ds$wichtigkeit_familie[ds$q1_1==8] <- "Neutral"
ds$wichtigkeit_familie[ds$q1_1==9] <- "N/A"
ds$wichtigkeit_familie

# Sortieren der Werte
ds$wichtigkeit_familie <- factor(ds$wichtigkeit_familie, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$wichtigkeit_familie)
typeof(ds$wichtigkeit_familie)


# Graph - Wichtigkeit der Familie
gf_counts(~ wichtigkeit_familie, data = ds, fill = ~ wichtigkeit_familie, position = position_dodge())

# Zufriedenheit der Finanziellen Status - Frage q61_1
ds$q61_1
tally(ds$q61_1)
ds$zufriedenheit_status[ds$q61_1==1] <- "sehr unzufrieden"
ds$zufriedenheit_status[ds$q61_1==2] <- "sehr unzufrieden"
ds$zufriedenheit_status[ds$q61_1==3] <- "unzufrieden"
ds$zufriedenheit_status[ds$q61_1==4] <- "unzufrieden"
ds$zufriedenheit_status[ds$q61_1==5] <- "neutral"
ds$zufriedenheit_status[ds$q61_1==6] <- "neutral"
ds$zufriedenheit_status[ds$q61_1==7] <- "zufrieden"
ds$zufriedenheit_status[ds$q61_1==8] <- "zufrieden"
ds$zufriedenheit_status[ds$q61_1==9] <- "sehr zufrieden"
ds$zufriedenheit_status[ds$q61_1==10] <- "sehr zufrieden"
ds$zufriedenheit_status[ds$q61_1==98] <- "N/A"
ds$zufriedenheit_status[ds$q61_1==99] <- "N/A"

# Sortieren der Werte
ds$zufriedenheit_status <- factor(ds$zufriedenheit_status, levels = c ("sehr unzufrieden","unzufrieden","neutral","zufrieden","sehr zufrieden","N/A"))
tally(ds$zufriedenheit_status)
typeof(ds$zufriedenheit_status)

# Graph - Wichtigkeit der Familie mit abhängigkeit der zufriedenheit
gf_jitter(zufriedenheit_status~wichtigkeit_familie, data = ds)
gf_counts(~ wichtigkeit_familie, data = ds, fill = ~ zufriedenheit_status , position = position_dodge())

Mod1 <- aov(q61_1 ~ q1_1, data = ds)
summary(Mod1)


# Wichtigkeit der Freunde - Frage q1_2
ds$q1_2
tally(ds$q1_2)
ds$wichtigkeit_freunde[ds$q1_2==1] <- "Sehr Wichtig"
ds$wichtigkeit_freunde[ds$q1_2==2] <- "Ziemlich Wichtig"
ds$wichtigkeit_freunde[ds$q1_2==3] <- "Nicht Wichtig"
ds$wichtigkeit_freunde[ds$q1_2==4] <- "Nicht Wichtig"
ds$wichtigkeit_freunde[ds$q1_2==8] <- "Neutral"
ds$wichtigkeit_freunde[ds$q1_2==9] <- "N/A"
ds$wichtigkeit_freunde

# Sortieren der Werte
ds$wichtigkeit_freunde <- factor(ds$wichtigkeit_freunde, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$wichtigkeit_freunde)
typeof(ds$zufriedenheit_status)

# Graph erstellen - Wichtigkeit der Freunde 
gf_counts(~ wichtigkeit_freunde, data = ds, fill = ~ wichtigkeit_freunde, position = position_dodge())

# Graph - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
gf_jitter(zufriedenheit_status~wichtigkeit_freunde, data = ds)
gf_counts(~ wichtigkeit_freunde, data = ds, fill = ~ zufriedenheit_status, position = position_dodge())

Mod2 <- aov(q61_1 ~ q1_2, data = ds)
summary(Mod2)


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
ds$wichtigkeit_freizeit[ds$q1_3==1] <- "Sehr Wichtig"
ds$wichtigkeit_freizeit[ds$q1_3==2] <- "Ziemlich Wichtig"
ds$wichtigkeit_freizeit[ds$q1_3==3] <- "Nicht Wichtig"
ds$wichtigkeit_freizeit[ds$q1_3==4] <- "Nicht Wichtig"
ds$wichtigkeit_freizeit[ds$q1_3==8] <- "Neutral"
ds$wichtigkeit_freizeit[ds$q1_3==9] <- "N/A"
ds$wichtigkeit_freizeit

# Sortieren der Werte
ds$wichtigkeit_freizeit <- factor(ds$wichtigkeit_freizeit, levels = c ("Sehr Wichtig","Ziemlich Wichtig","Nicht Wichtig","Neutral","N/A"))

# überprüfen 
tally(ds$wichtigkeit_freizeit)
typeof(ds$wichtigkeit_freizeit)

# Graph - Wichtigkeit der Freizeit 
gf_jitter(zufriedenheit_status~wichtigkeit_freizeit, data = ds)
gf_counts(~ wichtigkeit_freizeit, data = ds, fill = ~ zufriedenheit_status , position = position_dodge())

Mod3 <- aov(q61_1 ~ q1_3, data = ds)
summary(Mod3)

# Test - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
cor.test(q61_1~q1_3, data = ds)
xchisq.test(q61_1~q1_3, data = ds)

# Lohnt sich mehr oder Weniger Arbeit - Frage q11_5
tally(ds$q11_5)
ds$mehr_weniger_arbeit[ds$q11_5==1] <- "Hard Work sehr Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==2] <- "Hard Work sehr Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==3] <- "Hard Work Ziemlich Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==4] <- "Hard Work Ziemlich Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==5] <- "Hard Work Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==6] <- "Hard Work Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==7] <- "Hard Work Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==8] <- "Hard Work Nicht Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==9] <- "Hard Work Nicht Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==10] <- "Hard Work Nicht Wichtig"
ds$mehr_weniger_arbeit[ds$q11_5==98] <- "Neutral"
ds$mehr_weniger_arbeit[ds$q11_5==99] <- "N/A"

# Sortieren der Werte
ds$wichtigkeit_freizeit <- factor(ds$wichtigkeit_freizeit, levels = c ("Hard Work sehr Wichtig","Hard Work Ziemlich Wichtig","Hard Work Wichtig","Hard Work Nicht Wichtig","Neutral", "N/A"))

# überprüfen
tally(ds$mehr_weniger_arbeit)
typeof(ds$mehr_weniger_arbeit)


# Graph - Mehr oder wienger Arbeit: 
gf_counts(~ mehr_weniger_arbeit, data = ds, fill = ~mehr_weniger_arbeit, position = position_dodge() )
gf_jitter(zufriedenheit_status~mehr_weniger_arbeit, data = ds)
gf_counts(~ mehr_weniger_arbeit, data = ds, fill = ~ zufriedenheit_status, position = position_dodge())
mosaicplot(zufriedenheit_status~mehr_weniger_arbeit, data = ds)

# Test - 
Mod4 <- aov(q61_1 ~ q11_5, data = ds)
summary(Mod4)
cor.test(q61_1~q11_5, data = ds)

xchisq.test(zufriedenheit_status~mehr_weniger_arbeit, data = ds)

