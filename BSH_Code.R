library(mosaic)
library(haven)
library(tidyverse)

ds <- read_sav("ZA6702_v1-0-0.sav")
colnames(ds) <- tolower(colnames(ds))


# Wichtigkeit der Familie - Fradse q1_1
ds$q1_1
tally(ds$q1_1)
ds$WichtigkeitFamilie[ds$q1_1==1] <- "SehrWichtig"
ds$WichtigkeitFamilie[ds$q1_1==2] <- "ZiemlichWichtig"
ds$WichtigkeitFamilie[ds$q1_1==3] <- "NichtWichtig"
ds$WichtigkeitFamilie[ds$q1_1==4] <- "NichtWichtig"
ds$WichtigkeitFamilie[ds$q1_1==8] <- "Neutral"
ds$WichtigkeitFamilie[ds$q1_1==9] <- "N/A"
ds$WichtigkeitFamilie
tally(ds$WichtigkeitFamilie)
ds$WichtigkeitFamilie <- factor(ds$WichtigkeitFamilie, levels = c ("SehrWichtig","ZiemlichWichtig","NichtWichtig","Neutral","N/A"))
#as.numeric(ds$WichtigkeitFamilie)

# dsraph - Wichtigkeit der Familie
gf_counts(~ WichtigkeitFamilie, data = ds, fill = ~ WichtigkeitFamilie, position = position_dodge())

# Zufriedenheit der Finanziellen Status - Fradse q61_1
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
tally(ds$ZufriedenheitStatus)
ds$ZufriedenheitStatus <- factor(ds$ZufriedenheitStatus, levels = c ("sehr unzufrieden","unzufrieden","neutral","zufrieden","sehr zufrieden","N/A"))
#ds$ZufriedenheitStatus <- as.numeric(ds$ZufriedenheitStatus)


# dsraph - Wichtigkeit der Familie mit abhändsidskeit der zufriedenheit
gf_jitter(ZufriedenheitStatus~WichtigkeitFamilie, data = ds)

# Test - Wichtigkeit der Familie mit abhändsidskeit der zufriedenheit
cor.test(q61_1~q1_1, data = ds)
xchisq.test(ZufriedenheitStatus~WichtigkeitFamilie, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFamilie, position = position_dodge())



# Wichtigkeit der Freunde - Fradse q1_2
ds$q1_2
tally(ds$q1_2)
ds$WichtigkeitFreunde[ds$q1_2==1] <- "SehrWichtig"
ds$WichtigkeitFreunde[ds$q1_2==2] <- "ZiemlichWichtig"
ds$WichtigkeitFreunde[ds$q1_2==3] <- "NichtWichtig"
ds$WichtigkeitFreunde[ds$q1_2==4] <- "NichtWichtig"
ds$WichtigkeitFreunde[ds$q1_2==8] <- "Neutral"
ds$WichtigkeitFreunde[ds$q1_2==9] <- "N/A"
ds$WichtigkeitFreunde
tally(ds$WichtigkeitFreunde)
ds$WichtigkeitFreunde <- factor(ds$WichtigkeitFreunde, levels = c ("SehrWichtig","ZiemlichWichtig","NichtWichtig","Neutral","N/A"))
#as.numeric(ds$WichtigkeitFreunde)

# dsraph - Wichtigkeit der Freunde 
gf_counts(~ WichtigkeitFreunde, data = ds, fill = ~ WichtigkeitFreunde, position = position_dodge())

# dsraph - Wichtigkeit der Freunde mit abhändsidskeit der zufriedentheit 
gf_jitter(ZufriedenheitStatus~WichtigkeitFreunde, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFreunde, position = position_dodge())

# Test - Wichtigkeit der Freunde mit abhändsidskeit der zufriedentheit 
cor.test(q61_1~q1_1, data = ds)
xchisq.test(q61_1~q1_1, data = ds)

# Wichtigkeit der Freizeit - Fradse q1_3
ds$q1_3
tally(ds$q1_3)
ds$WichtigkeitFreizeit[ds$q1_3==1] <- "SehrWichtig"
ds$WichtigkeitFreizeit[ds$q1_3==2] <- "ZiemlichWichtig"
ds$WichtigkeitFreizeit[ds$q1_3==3] <- "NichtWichtig"
ds$WichtigkeitFreizeit[ds$q1_3==4] <- "NichtWichtig"
ds$WichtigkeitFreizeit[ds$q1_3==8] <- "Neutral"
ds$WichtigkeitFreizeit[ds$q1_3==9] <- "N/A"
ds$WichtigkeitFreizeit
tally(ds$WichtigkeitFreizeit)
ds$WichtigkeitFreizeit <- factor(ds$WichtigkeitFreizeit, levels = c ("SehrWichtig","ZiemlichWichtig","NichtWichtig","Neutral","N/A"))
#as.numeric(ds$WichtigkeitFreunde)

# Graph - Wichtigkeit der Freizeit 
gf_jitter(ZufriedenheitStatus~WichtigkeitFreizeit, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ WichtigkeitFreizeit, position = position_dodge())

# Test - Wichtigkeit der Freunde mit abhändsidskeit der zufriedentheit 
cor.test(q61_1~q1_3, data = ds)
xchisq.test(q61_1~q1_3, data = ds)

# Lohnt sich mehr oder Weniger Arbeit - Fradse q11_5
tally(ds$q11_5)
ds$MehrWenigerArbeit[ds$q11_5==1] <- "Hard Work sehr Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==2] <- "Hard Work sehr Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==3] <- "Hard Work ZiemlichWichtig"
ds$MehrWenigerArbeit[ds$q11_5==4] <- "Hard Work ZiemlichWichtig"
ds$MehrWenigerArbeit[ds$q11_5==5] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==6] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==7] <- "Hard Work Wichtig"
ds$MehrWenigerArbeit[ds$q11_5==8] <- "Hard Work NichtWichtig"
ds$MehrWenigerArbeit[ds$q11_5==9] <- "Hard Work NichtWichtig"
ds$MehrWenigerArbeit[ds$q11_5==10] <- "Hard Work NichtWichtig"
ds$MehrWenigerArbeit[ds$q11_5==98] <- "Neutral"
ds$MehrWenigerArbeit[ds$q11_5==99] <- "N/A"

tally(ds$MehrWenigerArbeit)
ds$WichtigkeitFreizeit <- factor(ds$WichtigkeitFreizeit, levels = c ("Hard Work sehr Wichtig","Hard Work ZiemlichWichtig","Hard Work Wichtig","Hard Work NichtWichtig","Neutral", "N/A"))

# Graph - Mehr oder wienger Arbeit: 
gf_counts(~ MehrWenigerArbeit, data = ds, fill = ~MehrWenigerArbeit, position = position_dodge() )
gf_jitter(ZufriedenheitStatus~MehrWenigerArbeit, data = ds)
gf_counts(~ ZufriedenheitStatus, data = ds, fill = ~ MehrWenigerArbeit, position = position_dodge())
mosaicplot(ZufriedenheitStatus~MehrWenigerArbeit, data = ds)

cor.test(q61_1~q11_5, data = ds)


# geselschaft Kategorien 
ds$q69
tally(ds$q69)
ds$GeselschaftKategorien <- ds$q69
tally(ds$GeselschaftKategorien)
