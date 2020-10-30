## Baseball Reference Data
br_off <- read.csv("team_off_mlb_br.csv", stringsAsFactors=F)[2:30]
br_def <- read.csv("team_def_mlb_br.csv", stringsAsFactors=F)[2:36]
br_info <- read.csv("team_info_mlb_br.csv", stringsAsFactors=F)[2:7]
br_pitWAR <- read.csv("team_pitWAR_mlb_br.csv", stringsAsFactors=F)[2:4]
br_offWAR <- read.csv("team_offWAR_mlb_br.csv", stringsAsFactors=F)[2:4]


## Data Order and Bind
br_off <- br_off[order(br_off$Yr, br_off$Team), ]
br_def <- br_def[order(br_def$Yr, br_def$Team), ]
br_info <- br_info[order(br_info$yr, br_info$Team), ]
br_info$League <- "MLB"

for(i in 1:ncol(br_off)) {
  colnames(br_off)[i] <- paste(colnames(br_off)[i], "_h1", sep="")
}
for(i in 1:ncol(br_def)) {
  colnames(br_def)[i] <- paste(colnames(br_def)[i], "_p1", sep="")
}
br_final <- cbind(br_info, br_off, br_def)
bWAR_p <- rep(NA, 2955)
bWAR_h <- rep(NA, 2955)
br_final <- cbind(br_info, br_off, br_def, bWAR_p, bWAR_h)


## Matching Check
sum(br_final$Team==br_final$Team_h) ; sum(br_final$Team==br_final$Team_p)
sum(br_final$yr==br_final$Yr_h) ; sum(br_final$yr==br_final$Yr_p)


## WAR Data Bind
br_pitWAR <- br_pitWAR[order(br_pitWAR$yr, br_pitWAR$team), ]
br_offWAR <- br_offWAR[order(br_offWAR$yr, br_offWAR$team), ]
br_WAR <- cbind(br_pitWAR, br_offWAR)[, c(1:3, 6)]

WAR <- list()
Final <- list()
for(i in 1901:2020) {
  WAR[[i-1900]] <- subset(br_WAR, br_WAR$yr==i)
  Final[[i-1900]] <- subset(br_final, br_final$yr==i)
}

for(i in 1:120) {
  k <- 1
  for(j in 1:nrow(Final[[i]])) {
    if(Final[[i]]$Team[j]==WAR[[i]]$team[k]) {
      Final[[i]]$bWAR_p[j] <- WAR[[i]]$PitWAR[k]
      Final[[i]]$bWAR_h[j] <- WAR[[i]]$offWAR[k]
      k <- k + 1
    }
  }
}

br_final <- subset(br_final, br_final$yr<=1900)
for(i in 1:120) {
  br_final <- rbind(br_final, Final[[i]])
}


## Team Name Adjustment
for(i in 1:nrow(br_final)) {
  if(br_final$Team2_h1[i]=="Los Angeles Angels of Anaheim") {
    br_final$Team2_h1[i] <- "Los Angeles Angels"
  } else if(br_final$Team2_h1[i]=="Hartfords of Brooklyn") {
    br_final$Team2_h1[i] <- "Hartford Dark Blues"
  } else if(br_final$Team2_h1[i]=="Chicago/Pittsburgh") {
    br_final$Team2_h1[i] <- "Chicago/Pittsburgh Browns/Stogies"
  } else if(br_final$Team2_h1[i]=="Houston Colt .45s") {
    br_final$Team2_h1[i] <- "Houston Colt .45's"
  } else if(br_final$Team2_h1[i]=="Cincinnati Reds" & br_final$yr[i]==1959) {
    br_final$Team2_h1[i] <- "Cincinnati Redlegs"
  } else if(br_final$Team2_h1[i]=="Philadelphia Phillies" & sum(br_final$yr[i]==c(1943, 1944))==1) {
    br_final$Team2_h1[i] <- "Philadelphia Blue Jays"
  } else if(br_final$Team2_h1[i]=="St. Paul White Caps") {
    br_final$Team2_h1[i] <- "St. Paul Apostles"
  } else if(br_final$Team2_h1[i]=="Boston Red Stockings" & sum(br_final$yr[i]==c(1876:1882))==1) {
    br_final$Team2_h1[i] <- "Boston Red Caps"
  }
}


## NA Column Creation
br_final$D <- 0
for(i in 1:nrow(br_final)) {
  br_final$BBHBP_h1[i] <- br_final$BB_h1[i] + ifelse(is.na(br_final$HBP_h1[i])==T, 0, br_final$HBP_h1[i])
  br_final$BBHBP_p1[i] <- br_final$BB_p1[i] + ifelse(is.na(br_final$HBP_p1[i])==T, 0, br_final$HBP_p1[i])
}
br_final$wOBA_h1 <- NA
br_final$WPA_h1 <- NA
br_final$BABIP_h1 <- NA
br_final$wRC_h1 <- NA
br_final$bwRCplus_h1 <- NA
br_final$X2B_p1 <- NA
br_final$X3B_p1 <- NA
br_final$WPA_p1 <- NA
br_final$BABIP_p1 <- NA
br_final$AVG_p1 <- NA
br_final$OBP_p1 <- NA
br_final$SLG_p1 <- NA
br_final$bFIPplus_p1 <- NA
colnames(br_final)

## Baseball-Reference Final Data
MLB_final <- br_final[, c(3, 9, 2, 1, 4, 5, 6, 73, 
                          11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 74, 24, 25, 26, 76, 77, 78, 79, 
                          50, 52, 53, 51, 80, 81, 54, 75, 57, 42, 63, 83, 84, 85, 86, 87, 
                          79, 72, 88, 71)]
colnames(MLB_final) <- c("League", "Team", "Team_abb", "Yr", "G", "W", "L", "D", "G_h1", "PA_h1", "AB_h1", "R_h1", "H_h1", "X2B_h1", "X3B_h1", "HR_h1", 
                         "SB_h1", "CS_h1", "BBHBP_h1", "AVG_h1", "OBP_h1", "SLG_h1", "wOBA_h1", "WPA_h1", "BABIP_h1", "wRC_h1", 
                         "IP_p1", "R_p1", "ER_p1", "H_p1", "X2B_p1", "X3B_p1", "HR_p1", "BBHBP_p1", "SO_p1", "ERA_p1", "FIP_p1", "WPA_p1",
                         "BABIP_p1", "AVG_p1", "OBP_p1", "SLG_p1", "bwRCplus_h", "bWAR_h", "bFIPplus_p", "bWAR_p")


## IP Adjustment
rownames(MLB_final) <- 1:nrow(MLB_final)
for(i in 1:nrow(MLB_final)) {
  if(round(MLB_final$IP_p1[i]-round(MLB_final$IP_p1)[i], 1)==0.1) {
    MLB_final$IP_p1[i] <- MLB_final$IP_p1[i] - 0.1 + 1/3
  } else if(round(MLB_final$IP_p1[i]-round(MLB_final$IP_p1)[i], 1)==0.2) {
    MLB_final$IP_p1[i] <- MLB_final$IP_p1[i] - 0.2 + 2/3
  }
}


## Baseball-Reference Final Data
write.csv(MLB_final, "MLB_final.csv")


## EDA
MLB_final$RA_p1 <- MLB_final$R_p1 / MLB_final$IP_p1 * 9
MLB_final$RAminusERA_p1 <- MLB_final$RA_p1 - MLB_final$ERA_p1
MLB_final$HRP_h1 <- MLB_final$HR_h1 / MLB_final$PA_h1
MLB_final$OPS_h1 <- MLB_final$OBP_h1 + MLB_final$SLG_h1
MLB_final$SO9_p1 <- MLB_final$SO_p1 / MLB_final$IP_p1 * 9
MLB_final$RG_h1 <- MLB_final$R_h1 / MLB_final$G
MLB_final$RAG_p1 <- MLB_final$R_p1 / MLB_final$G

MLB_yr <- list()
for(i in 1871:2020) {
  MLB_yr[[i-1870]] <- subset(MLB_final, MLB_final$Yr==i)
}
Lg_ERA <- c()
Lg_RAMinusERA <- c()
Lg_HRP <- c()
Lg_OPS <- c()
Lg_SO9 <- c()
for(i in 1:length(MLB_yr)) {
  Lg_IP <- sum(MLB_yr[[i]]$IP_p1)
  Lg_ERA[i] <- sum(MLB_yr[[i]]$ER_p1) / Lg_IP * 9
  Lg_RAMinusERA[i] <- sum(MLB_yr[[i]]$R_p1) / Lg_IP * 9 - sum(MLB_yr[[i]]$ER_p1) / Lg_IP * 9
  Lg_SO9[i] <- sum(MLB_yr[[i]]$SO_p1) / Lg_IP * 9
  Lg_PA <- sum(MLB_yr[[i]]$PA_h1)
  Lg_AB <- sum(MLB_yr[[i]]$AB_h1)
  Lg_HRP[i] <- sum(MLB_yr[[i]]$HR_h1) / Lg_PA
  Lg_OPS[i] <- sum(MLB_yr[[i]]$OBP_h1 * MLB_yr[[i]]$PA_h1) / Lg_PA + sum(MLB_yr[[i]]$SLG_h1 * MLB_yr[[i]]$AB_h1) / Lg_AB ## Approximation
}
Yr <- 1871:2020
Lg_Data <- data.frame(Yr, Lg_ERA, Lg_RAMinusERA, Lg_SO9, Lg_HRP, Lg_OPS)

library(ggplot2)
ggplot() + geom_point(size=0.8, color="#4458B0", data=MLB_final, mapping=aes(x=Yr, y=ERA_p1)) + 
  geom_line(mapping=aes(x=Yr, y=Lg_ERA), data=Lg_Data, cex=1.2, color="Navy") + 
  labs(y="ERA") + ggtitle("ERA") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))
ggplot(data=MLB_final, mapping=aes(x=Yr, y=RAminusERA_p1)) + geom_point(size=0.8, color="#4458B0") + 
  geom_line(mapping=aes(x=Yr, y=Lg_RAMinusERA), data=Lg_Data, cex=1.2, color="Navy") + 
  labs(y="RA - ERA") + ggtitle("RA - ERA") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))
ggplot(data=MLB_final, mapping=aes(x=Yr, y=SO9_p1)) + geom_point(size=0.8, color="#4458B0") + 
  geom_line(mapping=aes(x=Yr, y=Lg_SO9), data=Lg_Data, cex=1.2, color="Navy") + 
  labs(y="SO/9") + ggtitle("SO/9") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))

ggplot(data=MLB_final, mapping=aes(x=Yr, y=HRP_h1)) + geom_point(size=0.8, color="#4458B0") + 
  geom_line(mapping=aes(x=Yr, y=Lg_HRP), data=Lg_Data, cex=1.2, color="Navy") + 
  labs(y="HR%") + ggtitle("HR%") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))
ggplot(data=MLB_final, mapping=aes(x=Yr, y=OPS_h1)) + geom_point(size=0.8, color="#4458B0") + 
  geom_line(mapping=aes(x=Yr, y=Lg_OPS), data=Lg_Data, cex=1.2, color="Navy") + 
  labs(y="OPS") + ggtitle("OPS") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))

for(i in 1:nrow(MLB_final)) {
  MLB_final$Ind <- ifelse(MLB_final$Yr<1920, "Before 1920", "After 1920")
}
ggplot(data=MLB_final, mapping=aes(x=OPS_h1, y=RG_h1)) + geom_point(size=0.8, aes(color=Ind)) + 
  labs(y="R/G", x="OPS") + ggtitle("OPS-Run per Game") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))
ggplot(data=MLB_final, mapping=aes(x=FIP_p1, y=RAG_p1)) + geom_point(size=0.8, aes(color=Ind)) + ylim(2, 10) + 
  labs(y="RA/G", x="FIP") + ggtitle("FIP-Run Allowed per Game") + theme(plot.title = element_text(hjust = 0.5, size=18, face="bold"))



## Fangraphs Data Integration - City Seperation
MLB_final <- read.csv("MLB_final.csv", stringsAsFactors=F, row.names=1)
cities <- read.csv("MLBCities.csv", stringsAsFactors=F, header=F)
cities <- cities[, 1]

MLB_final$City <- NA
for(i in 1:nrow(MLB_final)) {
  t <- 0
  p <- 1
  while(t==0 & p<=length(cities)) {
    if(substr(MLB_final$Team[i], 1, nchar(cities[p]))==cities[p]) {
      MLB_final$Team[i] <- substring(MLB_final$Team[i], nchar(cities[p])+2)
      MLB_final$City[i] <- cities[p]
      t <- 1
    } else {p <- p + 1}
  }
}


## Fangraphs Data Integration - Fangraphs Data Order and IP Adjustment
fg_off <- read.csv("Fangraphs_Team_Batting.csv", stringsAsFactors=F)
fg_def1 <- read.csv("Fangraphs_Team_Pitching.csv", stringsAsFactors=F)
fg_def2 <- read.csv("Fangraphs_Team_Pitching2.csv", stringsAsFactors=F)

fg_off <- fg_off[order(fg_off$Season, fg_off$Team), ]
fg_def1 <- fg_def1[order(fg_def1$Season, fg_def1$Team), ]
fg_def2 <- fg_def2[order(fg_def2$Season, fg_def2$Tm), ]

rownames(fg_off) <- 1:nrow(fg_off)
rownames(fg_def1) <- 1:nrow(fg_def1)
rownames(fg_def2) <- 1:nrow(fg_def2)

for(i in 1:nrow(fg_def1)) {
  if(round(fg_def1$IP[i]-round(fg_def1$IP)[i], 1)==0.1) {
    fg_def1$IP[i] <- fg_def1$IP[i] - 0.1 + 1/3
  } else if(round(fg_def1$IP[i]-round(fg_def1$IP)[i], 1)==0.2) {
    fg_def1$IP[i] <- fg_def1$IP[i] - 0.2 + 2/3
  }
}


## Fangraphs Hitting Integration
fg_off$Link <- NA
fg_off$Link2 <- NA
MLB_final <- MLB_final[order(MLB_final$Yr, MLB_final$Team), ]
rownames(MLB_final) <- 1:nrow(MLB_final)
bryr <- list()
fgyr <- list()
for(i in 1871:2020) {
  bryr[[i-1870]] <- subset(MLB_final, MLB_final$Yr==i)
  fgyr[[i-1870]] <- subset(fg_off, fg_off$Season==i)
}

for(i in 1:length(fgyr)) {
  for(j in 1:nrow(fgyr[[i]])) {
    k <- 1
    p <- 0
    while(k <= nrow(fgyr[[i]])) {
      if(fgyr[[i]]$Team[j]==bryr[[i]]$Team[k]) {
        fgyr[[i]]$Link[j] <- ifelse(is.na(fgyr[[i]]$Link[j])==T, k, fgyr[[i]]$Link[j])
        fgyr[[i]]$Link2[j] <- ifelse(is.na(fgyr[[i]]$Link2[j])==T, 
                                     ifelse(paste(fgyr[[i]]$Team[j], fgyr[[i]]$PA[j])==paste(bryr[[i]]$Team[k], bryr[[i]]$PA_h1[k]), k, NA), 
                                     fgyr[[i]]$Link2[j])
        k <- k + 1
      } else {k <- k + 1}
    }
  }
  for(j in 1:nrow(fgyr[[i]])) {
    if(is.na(fgyr[[i]]$Link2[j])==F) {
      fgyr[[i]]$Link[j] <- ifelse(fgyr[[i]]$Link[j]!=fgyr[[i]]$Link2[j], fgyr[[i]]$Link2[j], fgyr[[i]]$Link[j])
    }
  }
  fgyr[[i]] <- fgyr[[i]][order(fgyr[[i]]$Link), ]
}

fg_final <- fgyr[[1]]
for(i in 2:150) {
  fg_final <- rbind(fg_final, fgyr[[i]])
}


## Fangraphs Pitching Integration
fg_def1$Link <- NA
fg_def1$Link2 <- NA
MLB_final <- MLB_final[order(MLB_final$Yr, MLB_final$Team), ]
rownames(MLB_final) <- 1:nrow(MLB_final)
bryr <- list()
fgyr <- list()
for(i in 1871:2020) {
  bryr[[i-1870]] <- subset(MLB_final, MLB_final$Yr==i)
  fgyr[[i-1870]] <- subset(fg_def1, fg_def1$Season==i)
}

for(i in 1:length(fgyr)) {
  for(j in 1:nrow(fgyr[[i]])) {
    k <- 1
    p <- 0
    while(k <= nrow(fgyr[[i]])) {
      if(fgyr[[i]]$Team[j]==bryr[[i]]$Team[k]) {
        fgyr[[i]]$Link[j] <- ifelse(is.na(fgyr[[i]]$Link[j])==T, k, fgyr[[i]]$Link[j])
        fgyr[[i]]$Link2[j] <- ifelse(is.na(fgyr[[i]]$Link2[j])==T, 
                                     ifelse(paste(fgyr[[i]]$Team[j], fgyr[[i]]$IP[j])==paste(bryr[[i]]$Team[k], bryr[[i]]$IP_p1[k]), k, NA), 
                                     fgyr[[i]]$Link2[j])
        k <- k + 1
      } else {k <- k + 1}
    }
  }
  for(j in 1:nrow(fgyr[[i]])) {
    if(is.na(fgyr[[i]]$Link2[j])==F) {
      fgyr[[i]]$Link[j] <- ifelse(fgyr[[i]]$Link[j]!=fgyr[[i]]$Link2[j], fgyr[[i]]$Link2[j], fgyr[[i]]$Link[j])
    }
  }
  fgyr[[i]] <- fgyr[[i]][order(fgyr[[i]]$Link), ]
}

fg_final2 <- fgyr[[1]]
for(i in 2:150) {
  fg_final2 <- rbind(fg_final2, fgyr[[i]])
}
subset(fg_final2, is.na(fg_final2$Link)==T)


## Fangraphs Data Refining
fg_final$BBHBP <- fg_final$BB + ifelse(is.na(fg_final$HBP)==T, 0, fg_final$HBP)
fg_final <- fg_final[, c(2, 1, 4, 5, 7, 17, 18, 19, 20, 8, 9, 27, 11, 12, 13, 14, 24, 10, 23, 15, 16)]
colnames(fg_final) <- c("Team", "Yr", "PA_h2", "AB_h2", "R_h2", "H_h2", "X2B_h2", "X3B_h2", "HR_h2", "SB_h2", "CS_h2", "BBHBP_h2", 
                        "AVG_h2", "OBP_h2", "SLG_h2", "wOBA_h2", "WPA_h2", "BABIP_h2", "wRC_h2", "fwRCplus_h", "fWAR_h")

fg_final2$BBHBP <- fg_final2$BB + ifelse(is.na(fg_final2$HBP)==T, 0, fg_final2$HBP)
fg_final2$X2B <- NA
fg_final2$X3B <- NA
fg_final2$OBP <- NA
fg_final2$SLG <- NA
fg_final2 <- fg_final2[, c(2, 1, 3, 9, 10, 11, 23, 24, 12, 22, 15, 5, 6, 16, 4, 17, 25, 26, 18, 7, 19)]
colnames(fg_final2) <- c("Team", "Yr", "IP_p2", "R_p2", "ER_p2", "H_p2", "X2B_p2", "X3B_p2", "HR_p2", "BBHBP_p2", "SO_p2", "ERA_p2", 
                         "FIP_p2", "WPA_p2", "BABIP_p2", "AVG_p2", "OBP_p2", "SLG_p2", "fFIPminus_p", "fWAR_p", "fRA9WAR_p")

MLB_final <- cbind(MLB_final, fg_final, fg_final2)
MLB_final <- MLB_final[, c(-47, -48, -49, -69, -70)]


## Fangraphs 2B, 3B, OOBP, OOPS Integration
MLB_final_yr <- list()
fg_def2_yr <- list()
for(i in 2002:2020) {
  MLB_final_yr[[i-2001]] <- subset(MLB_final, MLB_final$Yr==i)
  fg_def2_yr[[i-2001]] <- subset(fg_def2, fg_def2$Season==i)
}

for(i in 1:19) {
  for(j in 1:nrow(MLB_final_yr[[i]])) {
    p <- 0
    k <- 1
    while(p==0 & k <= nrow(MLB_final_yr[[i]])) {
      if(MLB_final_yr[[i]]$Team_abb[j]==fg_def2_yr[[i]]$Tm[k]) {
        MLB_final_yr[[i]]$X2B_p2[j] <- fg_def2_yr[[i]]$X2B[k]
        MLB_final_yr[[i]]$X3B_p2[j] <- fg_def2_yr[[i]]$X3B[k]
        MLB_final_yr[[i]]$OBP_p2[j] <- fg_def2_yr[[i]]$OBP[k]
        MLB_final_yr[[i]]$SLG_p2[j] <- fg_def2_yr[[i]]$SLG[k]
        p <- 1
      } else {k <- k + 1}
    }
  }
}


## Final Integrated Data
MLB_ITG <- subset(MLB_final, MLB_final$Yr<=2001)
MLB_final_final <- MLB_final_yr[[1]]
for(i in 2:19) {
  MLB_final_final <- rbind(MLB_final_final, MLB_final_yr[[i]])
}
MLB_ITG <- rbind(MLB_ITG, MLB_final_final)
MLB_ITG <- MLB_ITG[, -3]

write.csv(MLB_ITG, "MLB_Integrated.csv")