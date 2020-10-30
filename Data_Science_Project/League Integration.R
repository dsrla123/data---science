## Data Import
Data <- read.csv("data.csv", header=T, stringsAsFactors=F, fileEncoding="euc-kr", row.names=1)
MLBData <- read.csv("MLB_Integrated.csv", row.names=1)

## ML Separation
nrow(Data)
Data <- subset(Data, Data$League!="MLB")
MLBData <- MLBData[, -8]
Data <- Data[, -67]

Cols <- union(colnames(Data), colnames(MLBData))

NewData <- data.frame(matrix(rep(NA, nrow(Data) * length(Cols)), nrow=nrow(Data)))
colnames(NewData) <- Cols

MLBNewData <- data.frame(matrix(rep(NA, nrow(MLBData) * length(Cols)), nrow=nrow(MLBData)))
colnames(MLBNewData) <- Cols

for(i in 1:ncol(NewData)) {
  for(j in 1:ncol(Data)) {
    if(colnames(NewData)[i]==colnames(Data)[j]) {
      NewData[, i] <- Data[, j]
    }
  }
}

for(i in 1:ncol(MLBNewData)) {
  for(j in 1:ncol(MLBData)) {
    if(colnames(MLBNewData)[i]==colnames(MLBData)[j]) {
      MLBNewData[, i] <- MLBData[, j]
    }
  }
}

Data <- rbind(NewData, MLBNewData)
colnames(Data)[11] <- "2B_h1"
colnames(Data)[12] <- "3B_h1"
colnames(Data)[21] <- "swRC+_h"
colnames(Data)[29] <- "2B_p1"
colnames(Data)[30] <- "3B_p1"
colnames(Data)[41] <- "sWAR_h"
colnames(Data)[42] <- "sWAR_p"
colnames(Data)[43] <- "sFIP+_p"
colnames(Data)[49] <- "2B_h2"
colnames(Data)[50] <- "3B_h2"
colnames(Data)[64] <- "2B_p2"
colnames(Data)[65] <- "3B_p2"
colnames(Data)[67] <- "bwRC+_h"
colnames(Data)[69] <- "bFIP+_p"
colnames(Data)[77] <- "fwRC+_h"
colnames(Data)[84] <- "fFIP-_p"
Data <- Data[, c(1:2, 66, 3:20, 22:40, 45:51, 71:72, 52, 44, 54, 53, 73:76, 56, 61:62, 57, 64:65, 58:60, 55, 79:81, 63, 82:83, 21, 41, 43, 42, 
                     67:70, 77, 78, 84, 85, 86)]

rownames(Data) <- 1:nrow(Data)
for(i in 1:nrow(Data)) {
  if(round(Data$IP_p1[i]-round(Data$IP_p1)[i], 1)==0.1) {
    Data$IP_p1[i] <- Data$IP_p1[i] - 0.1 + 1/3
  } else if(round(Data$IP_p1[i]-round(Data$IP_p1)[i], 1)==0.2) {
    Data$IP_p1[i] <- Data$IP_p1[i] - 0.2 + 2/3
  }
}
write.csv(Data, "Final_Data.csv", fileEncoding="euc-kr")


Data2 <- read.csv("Data_nwar.csv", fileEncoding="euc-kr", row.names=1)
colnames(Data2) <- c("Team", "Yr", "League", "G", "W", "L", "D", "PA_h1", "AB_h1", "R_h1", "H_h1", "2B_h1", "3B_h1", "HR_h1", 
                     "SB_h1", "CS_h1", "BBHBP_h1", "AVG_h1", "OBP_h1", "SLG_h1", "wOBA_h1", "WPA_h1", "BABIP_h1", "wRC_h1", 
                     "IP_p1", "R_p1", "ER_p1", "H_p1", "2B_p1", "3B_p1", "HR_p1", "BBHBP_p1", "SO_p1", "ERA_p1", "FIP_p1", 
                     "WPA_p1", "BABIP_p1", "AVG_p1", "OBP_p1", "SLG_p1", "PA_h2", "AB_h2", "R_h2", "H_h2", "2B_h2", "3B_h2",
                     "HR_h2", "SB_h2", "CS_h2", "BBHBP_h2", "AVG_h2", "OBP_h2", "SLG_h2", "wOBA_h2", "WPA_h2", "BABIP_h2",
                     "wRC_h2", "IP_p2", "R_p2", "ER_p2", "H_p2", "2B_p2", "3B_p2", "HR_p2", "BBHBP_p2", "SO_p2", "ERA_p2", 
                     "FIP_p2", "WPA_p2", "BABIP_p2", "AVG_p2", "OBP_p2", "SLG_p2", "swRC+_h", "sWAR_h", "sFIP+_p", "sWAR_p", 
                     "bwRC+_h", "bWAR_h", "bFIP+_p", "bWAR_p", "fwRC+_h", "fWAR_h", "fFIP-_p", "fWAR_p", "fRA9WAR_p", "nWAR_p", "nWAR_h")
write.csv(Data2, "2nd_Final_Data.csv", fileEncoding="euc-kr")


## EDA
### KBO Data Difference
Data <- read.csv("Final_Data.csv", fileEncoding="euc-kr", row.names=1)
KBO <- subset(Data, Data$League=="KBO")
KBO <- subset(KBO, KBO$Team!="리그")
KBO <- subset(KBO, KBO$Yr!=2020)
KBO_Diff <- KBO[, c(1:3, 8:40)]
for(i in 1:nrow(KBO)) {
  for(j in 1:33) {
    KBO_Diff[i, j+3] <- round(KBO[i, j+7], 3)==round(KBO[i, j+40], 3)
  }
}

no <- c()
yes <- c()
for(i in 4:ncol(KBO_Diff)) {
  t <- data.frame(table(KBO_Diff[, i]))
  yes[i-3] <- ifelse(nrow(t)>=2, t[2, 2], ifelse(nrow(t)>=1, t[1, 2], 0))
  no[i-3] <- ifelse(nrow(t)>=2, t[1, 2], 0)
}
KBO_Diff_table <- data.frame(colnames(KBO_Diff)[4:ncol(KBO_Diff)], yes, no, stringsAsFactors=F)
colnames(KBO_Diff_table) <- c("Stat", "T", "F")
KBO_Diff_table$Ratio <- KBO_Diff_table[, 2] / (KBO_Diff_table[, 2] + KBO_Diff_table[, 3])
for(i in 1:nrow(KBO_Diff_table)) {
  KBO_Diff_table$Stat[i] <- strsplit(KBO_Diff_table$Stat[i], "_")[[1]][1]
}

KBO_Diff_table1 <- KBO_Diff_table[c(1:7, 10), ]
KBO_Diff_table2 <- KBO_Diff_table[c(18:26), ]

ggplot(KBO_Diff_table1, aes(Stat, Ratio, fill = Stat)) + geom_bar(position = "dodge", stat = "identity") +
  labs(y="Freqeuncy", x="Correspondence") + ggtitle("Batting Stats Correspondence(KBO)") + 
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

ggplot(KBO_Diff_table2, aes(Stat, Ratio, fill = Stat)) + geom_bar(position = "dodge", stat = "identity") +
  labs(y="Freqeuncy", x="Correspondence") + ggtitle("Pitching Stats Correspondence(KBO)") + 
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))


### MLB Data Difference
MLB <- subset(Data, Data$League=="MLB")
MLB_Diff <- MLB[, c(1:3, 8:40)]
for(i in 1:nrow(MLB)) {
  for(j in 1:33) {
    MLB_Diff[i, j+3] <- round(MLB[i, j+7], 3)==round(MLB[i, j+40], 3)
  }
}

no <- c()
yes <- c()
for(i in 4:ncol(MLB_Diff)) {
  t <- data.frame(table(MLB_Diff[, i]))
  yes[i-3] <- ifelse(nrow(t)>=2, t[2, 2], ifelse(nrow(t)>=1, t[1, 2], 0))
  no[i-3] <- ifelse(nrow(t)>=2, t[1, 2], 0)
}
MLB_Diff_table <- data.frame(colnames(MLB_Diff)[4:ncol(MLB_Diff)], yes, no, stringsAsFactors=F)
colnames(MLB_Diff_table) <- c("Stat", "T", "F")
MLB_Diff_table$Ratio <- MLB_Diff_table[, 2] / (MLB_Diff_table[, 2] + MLB_Diff_table[, 3])
for(i in 1:nrow(MLB_Diff_table)) {
  MLB_Diff_table$Stat[i] <- strsplit(MLB_Diff_table$Stat[i], "_")[[1]][1]
}

MLB_Diff_table1 <- MLB_Diff_table[c(1:7, 10), ]
MLB_Diff_table2 <- MLB_Diff_table[c(18:21, 24:26), ]

ggplot(MLB_Diff_table1, aes(Stat, Ratio, fill = Stat)) + geom_bar(position = "dodge", stat = "identity") +
  labs(y="Freqeuncy", x="Correspondence") + ggtitle("Batting Stats Correspondence(MLB)") + 
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

ggplot(MLB_Diff_table2, aes(Stat, Ratio, fill = Stat)) + geom_bar(position = "dodge", stat = "identity") +
  labs(y="Freqeuncy", x="Correspondence") + ggtitle("Pitching Stats Correspondence(MLB)") + 
  theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))


### League Difference
Data <- read.csv("Final_Data.csv", fileEncoding="euc-kr", row.names=1, stringsAsFactors = F)
Data$RA_p1 <- Data$R_p1 / Data$IP_p1 * 9
Data$RAminusERA_p1 <- Data$RA_p1 - Data$ERA_p1
Data$HRP_h1 <- Data$HR_h1 / Data$PA_h1
Data$OPS_h1 <- Data$OBP_h1 + Data$SLG_h1
Data$SO9_p1 <- Data$SO_p1 / Data$IP_p1 * 9
Data$RG_h1 <- Data$R_h1 / Data$G
Data$RAG_p1 <- Data$R_p1 / Data$G
for(i in 1:nrow(Data)) {
  if(Data$League[i]=="MLB" & Data$Yr[i]<1920) {
    Data$League[i] <- c("MLBDeadball")
  } else if(Data$League[i]=="NPB" & Data$Yr[i]<1950) {
    Data$League[i] <- c("NPBPast")
  }
}

ggplot(data=Data, mapping=aes(x=OPS_h1, y=RG_h1)) + geom_point(size=0.8, aes(color=League)) + 
  labs(y="R/G", x="OPS") + ggtitle("OPS-Run per Game") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
ggplot(data=Data, mapping=aes(x=FIP_p1, y=RAG_p1)) + geom_point(size=0.8, aes(color=League)) + ylim(2, 10) + 
  labs(y="RA/G", x="FIP") + ggtitle("FIP-Run Allowed per Game") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

ggplot(data=Data, mapping=aes(x=League, y=HRP_h1)) + geom_boxplot(size=0.8, aes(fill=League)) +
  labs(y="HR%", x="League") + ggtitle("HR%") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
ggplot(data=Data, mapping=aes(x=League, y=OPS_h1)) + geom_boxplot(size=0.8, aes(fill=League)) + 
  labs(y="OPS", x="League") + ggtitle("OPS") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
ggplot(data=Data, mapping=aes(x=League, y=AVG_h1)) + geom_boxplot(size=0.8, aes(fill=League)) + 
  labs(y="AVG", x="League") + ggtitle("AVG") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

ggplot(data=Data, mapping=aes(x=League, y=RAG_p1)) + geom_boxplot(size=0.8, aes(fill=League)) +
  labs(y="RA/G", x="League") + ggtitle("RA/G") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
ggplot(data=Data, mapping=aes(x=League, y=RAminusERA_p1)) + geom_boxplot(size=0.8, aes(fill=League)) + ylim(0, 2.5) + 
  labs(y="RA - ERA", x="League") + ggtitle("RA - ERA") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))
ggplot(data=Data, mapping=aes(x=League, y=SO9_p1)) + geom_boxplot(size=0.8, aes(fill=League)) + 
  labs(y="SO/9", x="League") + ggtitle("SO/9") + theme(plot.title = element_text(hjust = 0.5, size=12, face="bold"))

