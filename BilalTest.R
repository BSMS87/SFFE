library(foreign)
library(mosaic)
library(haven)
library(tidyverse)
library(rgl)

## STILL TO DO's: 
# 1. Enhance and optimize the script 
# 2. Change and adapt the variables names
# 3. Switch this MehrWenigerArbeitNum to MehrWenigerArbeit and vice versa 
# 4. FamillienKategorien to geselschaft Kategorien
# 5. Compining two plots together https://okanbulut.github.io/bigdata/visualizing-big-data.html#customizing-visualizations
# 6. Try to generate 3-D graphs and https://www.r-graph-gallery.com/3d-surface-plot.html or https://www.r-graph-gallery.com/3d_scatter_plot.html

G <- read.spss("ZA6702_v1-0-0.sav", use.value.labels = TRUE, max.value.labels = Inf, to.data.frame = TRUE)
colnames(G) <- tolower(colnames(G))

summary(G)


# Daten Vorbereitung und Cleansing: 

# Wichtigkeit der Famillie 
G$q1_1
tally(G$q1_1)
G$WichtigkeitFamillie [G$q1_1=="Very important"] <- "SehrWichtig"
G$WichtigkeitFamillie [G$q1_1=="Rather important"] <- "ZiemlichWichtig"
G$WichtigkeitFamillie [G$q1_1=="Not at all important"] <- "NichtWichtig"
G$WichtigkeitFamillie [G$q1_1=="Not very important"] <- "NichtWichtig"
G$WichtigkeitFamillie [G$q1_1=="Do not know"] <- "Neutral"
G$WichtigkeitFamillie [G$q1_1=="No answer"] <- "N/A"
G$WichtigkeitFamillie
tally(G$WichtigkeitFamillie)
G$WichtigkeitFamillie <- factor(G$WichtigkeitFamillie, levels=c ("SehrWichtig", "ZiemlichWichtig", "NichtWichtig", "Neutral", "N/A"))
tally(G$WichtigkeitFamillie)

# Numerische Werte der Wichtigkeit der Famillie 
G$WichtigkeitFamillieNum[G$q1_1=="Very important"] <- 1
G$WichtigkeitFamillieNum[G$q1_1=="Rather important"] <- 2
G$WichtigkeitFamillieNum[G$q1_1=="Not at all important"] <- 3
G$WichtigkeitFamillieNum[G$q1_1=="Not very important"] <- 4
G$WichtigkeitFamillieNum[G$q1_1=="No answer"] <- 9
G$WichtigkeitFamillieNum
tally(G$WichtigkeitFamillieNum)
G$WichtigkeitFamillieNum <- as.numeric(G$WichtigkeitFamillieNum)


# Wichtigkeit der Freunde 
G$q1_2
tally(G$q1_2)
G$WichtigkeitFreunde [G$q1_2=="Very important"] <- "SehrWichtig"
G$WichtigkeitFreunde [G$q1_2=="Rather important"] <- "ZiemlichWichtig"
G$WichtigkeitFreunde [G$q1_2=="Not at all important"] <- "NichtWichtig"
G$WichtigkeitFreunde [G$q1_2=="Not very important"] <- "NichtWichtig"
G$WichtigkeitFreunde [G$q1_2=="Do not know"] <- "Neutral"
G$WichtigkeitFreunde [G$q1_2=="No answer"] <- "N/A"
G$WichtigkeitFreunde
tally(G$WichtigkeitFreunde)
G$WichtigkeitFreunde <- factor(G$WichtigkeitFreunde, levels=c ("SehrWichtig", "ZiemlichWichtig", "NichtWichtig", "Neutral", "N/A"))
tally(G$WichtigkeitFreunde)

# Numerische Werte der Wichtigkeit der Freunde 
G$WichtigkeitFreundeNum[G$q1_2=="Very important"] <- 1
G$WichtigkeitFreundeNum[G$q1_2=="Rather important"] <- 2
G$WichtigkeitFreundeNum[G$q1_2=="Not at all important"] <- 3
G$WichtigkeitFreundeNum[G$q1_2=="Not very important"] <- 4
G$WichtigkeitFreundeNum[G$q1_2=="No answer"] <- 9
G$WichtigkeitFreundeNum
tally(G$WichtigkeitFreundeNum)
G$WichtigkeitFreundeNum <- as.numeric(G$WichtigkeitFreundeNum)


# Wichtigkeit der Freizeit
tally(G$q1_3)
G$q1_3
G$WichtigkeitFreizeit [G$q1_3=="Very important"] <- "SehrWichtig"
G$WichtigkeitFreizeit [G$q1_3=="Rather important"] <- "ZiemlichWichtig"
G$WichtigkeitFreizeit [G$q1_3=="Not at all important"] <- "NichtWichtig"
G$WichtigkeitFreizeit [G$q1_3=="Not very important"] <- "NichtWichtig"
G$WichtigkeitFreizeit [G$q1_3=="Do not know"] <- "Neutral"
G$WichtigkeitFreizeit [G$q1_3=="No answer"] <- "N/A"
G$WichtigkeitFreizeit
tally(G$WichtigkeitFreizeit)
G$WichtigkeitFreizeit <- factor(G$WichtigkeitFreizeit, levels=c ("SehrWichtig", "ZiemlichWichtig", "NichtWichtig", "Neutral", "N/A"))
tally(G$WichtigkeitFreizeit)


# Numerische Werte der Wichtigkeit der Freunde 
G$WichtigkeitFreizeitNum[G$q1_3=="Very important"] <- 1
G$WichtigkeitFreizeitNum[G$q1_3=="Rather important"] <- 2
G$WichtigkeitFreizeitNum[G$q1_3=="Not at all important"] <- 3
G$WichtigkeitFreizeitNum[G$q1_3=="Not very important"] <- 4
G$WichtigkeitFreizeitNum[G$q1_3=="No answer"] <- 9
G$WichtigkeitFreizeitNum
tally(G$WichtigkeitFreizeitNum)
G$WichtigkeitFreizeitNum <- as.numeric(G$WichtigkeitFreizeitNum)


# Lohnt sich mehr oder wienger Arbeit: 
tally(G$q11_5)
str(G$q11_5)
G$MehrWenigerArbeit[G$q11_5=="In the long run, hard work usually brings a better life"] <- 1
G$MehrWenigerArbeit[G$q11_5=="2"] <- 2
G$MehrWenigerArbeit[G$q11_5=="3"] <- 3
G$MehrWenigerArbeit[G$q11_5=="4"] <- 4
G$MehrWenigerArbeit[G$q11_5=="5"] <- 5
G$MehrWenigerArbeit[G$q11_5=="6"] <- 6
G$MehrWenigerArbeit[G$q11_5=="7"] <- 7
G$MehrWenigerArbeit[G$q11_5=="8"] <- 8
G$MehrWenigerArbeit[G$q11_5=="9"] <- 9
G$MehrWenigerArbeit[G$q11_5=="Hard work doesn´t generally bring success- it´s more a matter of luck and connections"] <- 10
G$MehrWenigerArbeit
tally(G$MehrWenigerArbeit)
G$MehrWenigerArbeit <- as.numeric(G$MehrWenigerArbeit)

G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==1] <- "Hard Work sehr wichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==2] <- "Hard Work sehr wichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==3] <- "Hard Work ZiemlichWichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==4] <- "Hard Work ZiemlichWichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==5] <- "Hard Work Wichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==6] <- "Hard Work Wichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==7] <- "Hard Work Wichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==8] <- "Hard Work NichtWichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==9] <- "Hard Work NichtWichtig"
G$MehrWenigerArbeitNum[G$MehrWenigerArbeit==10] <- "Hard Work NichtWichtig"
tally(G$MehrWenigerArbeitNum)


# Zufriedenheit der Finanziellen Status
G$q61_1
tally(G$q61_1)
G$ZufriedenheitFinanziellenStatus[G$q61_1=="completely dissatisfied"] <- 1
G$ZufriedenheitFinanziellenStatus[G$q61_1=="2"] <- 2
G$ZufriedenheitFinanziellenStatus[G$q61_1=="3"] <- 3
G$ZufriedenheitFinanziellenStatus[G$q61_1=="4"] <- 4
G$ZufriedenheitFinanziellenStatus[G$q61_1=="5"] <- 5
G$ZufriedenheitFinanziellenStatus[G$q61_1=="6"] <- 6
G$ZufriedenheitFinanziellenStatus[G$q61_1=="7"] <- 7
G$ZufriedenheitFinanziellenStatus[G$q61_1=="8"] <- 8
G$ZufriedenheitFinanziellenStatus[G$q61_1=="9"] <- 9
G$ZufriedenheitFinanziellenStatus[G$q61_1=="completely satisfied"] <- 10
G$ZufriedenheitFinanziellenStatus
tally(G$ZufriedenheitFinanziellenStatus)
G$ZufriedenheitFinanziellenStatus <- as.numeric(G$ZufriedenheitFinanziellenStatus)
G$q61_1 <- as.numeric(G$q61_1)
tally(G$q61_1)
#G$ZufriedenheitFinanziellenStatus <- factor(G$ZufriedenheitFinanziellenStatus, levels=c ("SehrWichtig", "ZiemlichWichtig", "NichtWichtig", "Neutral", "N/A"))

# Famillien Kategorien
G$q69
tally(G$q69)
G$FamillienKategorien <- G$q69
G$FinanziellenStatusNum <- G$q69
G$FinanziellenStatusNum <- as.numeric(G$FinanziellenStatusNum)
tally(G$FamillienKategorien)

################################################################################

## Graph erstellen und Testen

# Graph - Wichtigkeit der Famillie mit abhängigkeit der zufriedentheit 
gf_bar(~ WichtigkeitFamillie, data = G)
gf_counts(~WichtigkeitFamillie, data = G, fill = ~WichtigkeitFamillie, position = position_dodge())
gf_jitter(ZufriedenheitFinanziellenStatus~WichtigkeitFamillieNum, data = G)

# Test - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
cor.test(ZufriedenheitFinanziellenStatus~WichtigkeitFamillieNum, data = G)
t.test(q61_1~WichtigkeitFamillieNum, data = G)
xchisq.test(Zufrieden~WichtigkeitFamillieNum, data = G)


# Graph - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
gf_bar(~ WichtigkeitFreunde, data = G)
gf_counts(~WichtigkeitFreunde, data = G, fill = ~WichtigkeitFreunde, position = position_dodge())
gf_jitter(ZufriedenheitFinanziellenStatus~WichtigkeitFamillieNum, data = G)

# Test - Wichtigkeit der Freunde mit abhängigkeit der zufriedentheit 
cor.test(ZufriedenheitFinanziellenStatus~WichtigkeitFreundeNum, data = G)
t.test(q61_1~WichtigkeitFreundeNum, data = G)
xchisq.test(Zufrieden~WichtigkeitFamillieNum, data = G)

# Graph - Wichtigkeit der Freizeit mit abhängigkeit der zufriedentheit 
gf_bar(~ WichtigkeitFreizeit, data = G)

boxplot(FinanziellenStatusNum ~ interaction(WichtigkeitFamillie, WichtigkeitFreunde), data = G)
boxplot(ZufriedenheitFinanziellenStatus ~ interaction(WichtigkeitFamillie, WichtigkeitFreunde), data = G)


# Mehr oder wienger Arbeit: 
gf_bar( ~ MehrWenigerArbeit, data = G)
gf_jitter(ZufriedenheitFinanziellenStatus~MehrWenigerArbeit, data = G)
cor.test(ZufriedenheitFinanziellenStatus~MehrWenigerArbeit, data = G)
mosaicplot(ZufriedenheitFinanziellenStatus~MehrWenigerArbeit, data = G)
gf_counts(~ MehrWenigerArbeit, data = G, fill = ~MehrWenigerArbeit, position = position_dodge() )


# Graph Finanziellen Status
gf_counts(~FinanziellenStatus, data = G, fill = ~FinanziellenStatus, position = position_dodge())


# erstellen einen Datensatz, der nur die WichtigkeitFamillie Zufrieden, FinanziellenStatus enthält.
FamillieWichtigZufriedenheitFinanz <- G %>% 
  select(WichtigkeitFamillie, ZufriedenheitFinanziellenStatus, FinanziellenStatus, FinanziellenStatusNum) %>% 
  drop_na()
FamillieWichtigZufriedenheitFinanz


## Graphs!!

## Wichtigkeit der Famillie
ggplot(data = G,
       mapping = aes(x = ZufriedenheitFinanziellenStatus, fill = FamillienKategorien)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "Zufriedenheit Finanziellen Status", y = "Count",
       title = "Geschäfterfolg Scores anhand von Finazilenstatus und Gesellschaftsklasse") +
  facet_grid(. ~ WichtigkeitFamillie) +
  theme_bw()

## Wichtigkeit der Freunde
ggplot(data = G,
       mapping = aes(x = ZufriedenheitFinanziellenStatus, fill = FamillienKategorien)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "Zufriedenheit Finanziellen Status", y = "Count",
       title = "Geschäfterfolg Scores anhand von Finazilenstatus und Gesellschaftsklasse") +
  facet_grid(. ~ WichtigkeitFreunde) +
  theme_bw()

## Wichtigkeit der Freizeit
ggplot(data = G,
       mapping = aes(x = ZufriedenheitFinanziellenStatus, fill = FamillienKategorien)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "Zufriedenheit Finanziellen Status", y = "Count",
       title = "Geschäfterfolg Scores anhand von Finazilenstatus und Gesellschaftsklasse") +
  facet_grid(. ~ WichtigkeitFreizeit) +
  theme_bw()

## Wichtigkeit der Arbeit
ggplot(data = G,
       mapping = aes(x = ZufriedenheitFinanziellenStatus, fill = FamillienKategorien)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "Zufriedenheit Finanziellen Status", y = "Count",
       title = "Geschäfterfolg Scores anhand von Finazilenstatus und Gesellschaftsklasse") +
  facet_grid(. ~ MehrWenigerArbeitNum) +
  theme_bw()


## Per Land 
G$country
tally(G$country)
G$ZufriedenheitFinanziellenStatus
G$FinanziellenStatusNum

# Boxplot finicial status and country
ggplot(data = G, mapping = aes(x = country, y = FinanziellenStatusNum)) +
  geom_boxplot() +
  labs(x=NULL, y="Finanziellen Status") +
  theme_bw()

ggplot(data = G,
       mapping = aes(x = MehrWenigerArbeit, y = FinanziellenStatusNum, fill = country)) +
  geom_point(aes(colour = FamillienKategorien)) +
  geom_smooth(method = "loess") +
  labs(x = "Working  Time", y = "Finanziellen Status ") +
  theme_bw()


ggplot(data = G,
       mapping = aes(x = MehrWenigerArbeit, y = FinanziellenStatusNum)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Weekly Learning Time", y = "Science Scores") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_grid(FamillienKategorien ~ country)

ggplot(data = G,
       mapping = aes(x = country, y = FinanziellenStatusNum, fill = FamillienKategorien)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(x = "", y = "Finanziellen Status ", fill = "Famillien Kategorien") +
  theme_bw()

G$MehrWenigerArbeit


####################################################################################

## Playing around
par(mfrow = c(1, 2))
bar1table = table(G$WichtigkeitFamillie, G$ZufriedenheitFinanziellenStatus)
barplot(bar1table, beside = TRUE, main = "FacVar1=level1")

bar2table = table(G$WichtigkeitFamillie, G$ZufriedenheitFinanziellenStatus)
barplot(bar2table, beside = TRUE, main = "FacVar1=level2", legend = levels(unique(G$WichtigkeitFamillie)))


## Erste graph bezüglich WichtigkeitFamillie, Zufriedenheit und FinanziellenStatus
ErsteGraph <- ggplot(data =  FamillieWichtigZufriedenheitFinanz)

ErsteGraph <- FamillieWichtigZufriedenheitFinanz %>% 
  ggplot(mapping = aes(x = WichtigkeitFamillie, 
                       y = ZufriedenheitFinanziellenStatus, 
                       color = WichtigkeitFamillie,
                       fill = WichtigkeitFamillie))


ZweiteGraph <- FamillieWichtigZufriedenheitFinanz %>% 
  ggplot(mapping = aes(x = WichtigkeitFamillie, 
                       y = FinanziellenStatusNum, 
                       color = WichtigkeitFamillie,
                       fill = WichtigkeitFamillie))





ZweiteGraph + geom_point(size = 3)
ZweiteGraph + geom_jitter()

## Graph erstellen 
ErsteGraph + geom_point(size = 3)
ErsteGraph + geom_jitter()
ErsteGraph + geom_boxplot()
ErsteGraph + geom_violin()
ErsteGraph + geom_boxplot(aes(color = WichtigkeitFamillie))

## Graph Kombinieren 
ErsteGraph +
  geom_violin(aes(fill = WichtigkeitFamillie)) + ZweiteGraph
  geom_jitter(fill = FinanziellenStatusNum, width = 0.2, alpha = 0.6)

## 3D graph 
par(mar=c(0,0,0,0))
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
G$color <- mycolors[ as.numeric(G$WichtigkeitFamillie) ]
plot3d( 
  y=G$WichtigkeitFamillieNum, x=G$ZufriedenheitFinanziellenStatus, z=G$FinanziellenStatusNum, 
  type = 's', 
  col = G$color, 
  radius = .1,
  xlab="WichtigkeitFamillie", ylab="ZufriedenheitFinanziellenStatus", zlab="FinanziellenStatusNum")
  


# Library
library(plotly)
library(htmlwidgets)

# Data: volcano is provided by plotly

# Plot
p <- plot_ly(z = volcano, type = "surface")
str(volcano)
saveWidget(p, file=paste0( getwd(), "/webGL/3dSurface.html"))

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/3dSurface.html"))
  

## Three Variables: Three Numeric Variables
## NumVar4 is 2001 through 2050... possibly, a time variable - use that as
## the x-axis
plot(G$FinanziellenStatusNum, G$ZufriedenheitFinanziellenStatus, type = "o", ylim = c(0, max(G$ZufriedenheitFinanziellenStatus, G$WichtigkeitFamillieNum)))  ## join dots with lines

lines(G$FinanziellenStatusNum, G$ZufriedenheitFinanziellenStatus, type = "o", lty = 2, col = "red")  ## add another line

