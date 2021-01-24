library(mosaic)
library(haven)

D <- read_sav("ZA6702_v1-0-0.sav")
colnames(D) <- tolower(colnames(D))

# Excluding extrem Werte
D2 <- subset(D, D$q61_1 != 99 & D$q61_1 != 98, )   # Apply subset function

# Important of the family
tally(D$q1_1)
D$q1_1

# Important of the Friends 
tally(D$q1_2)
D$q1_2

# Important of the Leisure  
tally(D$q1_3)
(D$q1_3)

#  Children suffers when women works
tally(D$q9_2)
D$q9_2

## Zuordnung und Konsolidierung

# Wichtigkeit der Famillie 
tally(D$q1_1)
D$q1_1
D$WichtigkeitFamillie [D$q1_1==1] <- "SehrWichtig"
D$WichtigkeitFamillie [D$q1_1==2] <- "ZiemlichWichtig"
D$WichtigkeitFamillie [D$q1_1==3] <- "NichtWichtig"
D$WichtigkeitFamillie [D$q1_1==4] <- "NichtWichtig"
D$WichtigkeitFamillie [D$q1_1==8] <- "Neutral"
D$WichtigkeitFamillie [D$q1_1==9] <- "N/A"
tally(D$WichtigkeitFamillie)
gf_bar(~ WichtigkeitFamillie, data = D)

# Wichtigkeit der Famillie für Dataset Ohne extrem Wert
tally(D2$q62_1)

D2$WichtigkeitFamillie [D2$q1_1==1] <- "SehrWichtig"
D2$WichtigkeitFamillie [D2$q1_1==2] <- "ZiemlichWichtig"
D2$WichtigkeitFamillie [D2$q1_1==3] <- "NichtWichtig"
D2$WichtigkeitFamillie [D2$q1_1==4] <- "NichtWichtig"
D2$WichtigkeitFamillie [D2$q1_1==8] <- "Neutral"
D2$WichtigkeitFamillie [D2$q1_1==9] <- "N/A"
gf_bar(~ WichtigkeitFamillie, data = D2)

D$WichtigkeitFamillie <- factor(G$WichtigkeitFamillie, levels=c ("SehrWichtig", "ZiemlichWichtig", "NichtWichtig", "Neutral", "N/A"))
tally(D$WichtigkeitFamillie)

# Wichtigkeit der Freunde 
tally(D$q1_2)
D$q1_2
D$WichtigkeitFreunde [D$q1_2==1] <- "SehrWichtig"
D$WichtigkeitFreunde [D$q1_2==2] <- "ZiemlichWichtig"
D$WichtigkeitFreunde [D$q1_2==3] <- "NichtWichtig"
D$WichtigkeitFreunde [D$q1_2==4] <- "NichtWichtig"
D$WichtigkeitFreunde [D$q1_2==8] <- "Neutral"
D$WichtigkeitFreunde [D$q1_2==9] <- "N/A"
D$WichtigkeitFreunde
tally(D$WichtigkeitFreunde)
gf_bar(~ WichtigkeitFreunde, data = D)


# Wichtigkeit der Freizeit
tally(D$q1_3)
D$q1_3
D$WichtigkeitFreizeit [D$q1_3==1] <- "SehrWichtig"
D$WichtigkeitFreizeit [D$q1_3==2] <- "ZiemlichWichtig"
D$WichtigkeitFreizeit [D$q1_3==3] <- "NichtWichtig"
D$WichtigkeitFreizeit [D$q1_3==4] <- "NichtWichtig"
D$WichtigkeitFreizeit [D$q1_3==8] <- "Neutral"
D$WichtigkeitFreizeit [D$q1_3==9] <- "N/A"
D$WichtigkeitFreizeit
tally(D$WichtigkeitFreizeit)
gf_bar(~ WichtigkeitFreizeit, data = D)


#Zufriedenheit finanzieller Status - Zuordnung und Konsolidierung
D$Zufridenheit [D$q61_1==1] <- "SehrUnzufrieden"
D$Zufridenheit [D$q61_1==2] <- "SehrUnzufrieden"
D$Zufridenheit [D$q61_1==3] <- "Unzufrieden"
D$Zufridenheit [D$q61_1==4] <- "Unzufrieden"
D$Zufridenheit [D$q61_1==5] <- "Neutral"
D$Zufridenheit [D$q61_1==6] <- "Neutral"
D$Zufridenheit [D$q61_1==7] <- "Zufrieden"
D$Zufridenheit [D$q61_1==8] <- "Zufrieden"
D$Zufridenheit [D$q61_1==9] <- "SehrZufrieden"
D$Zufridenheit [D$q61_1==10] <- "SehrZufrieden"
#D$Zufridenheit [D$q61_1==98] <- "N/A"
#D$Zufridenheit [D$q61_1==99] <- "N/A"
D$Zufridenheit
tally(D$Zufridenheit)
D$q61_1
tally(D$q61_1)

# ZufriedenheitNumerisch variable
D$ZufriedenheitNumerisch <- D$q61_1
D$ZufriedenheitNumerisch
tally(D$ZufriedenheitNumerisch)

D2$ZufriedenheitNumerisch <- D2$q61_1

# As.nymeric value conversion 
D$ZufriedenheitNumerisch <- as.numeric(D$ZufriedenheitNumerisch)
D2$ZufriedenheitNumerisch <- as.numeric(D2$ZufriedenheitNumerisch)
D2$WichtigkeitFamillie <- as.numeric(D2$WichtigkeitFamillie)



# Wichtigkeit die Famillie
gf_counts(~WichtigkeitFamillie, data = D2, fill = ~WichtigkeitFamillie, position = position_dodge())

gf_boxplot(ZufriedenheitNumerisch~WichtigkeitFamillie , data = D2)
gf_jitter(ZufriedenheitNumerisch~WichtigkeitFamillie, data = D2)

cor.test(ZufriedenheitNumerisch~WichtigkeitFamillie, data = D2)
t.test(ZufriedenheitNumerisch~WichtigkeitFamillie,data=D2)


tally(D$WichtigkeitFamillie)
tally(D$q61_1)
gf_boxplot(q61_1~WichtigkeitFamillie , data = D)
t.test(q61_1~WichtigkeitFamillie,data=D)





