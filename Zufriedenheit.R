#Zufriedenheit finanzieller Status
fstatus <- q$q61_1
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
