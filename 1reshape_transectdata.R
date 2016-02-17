# Lynx Project #
# This script reshapes the transect data #

# ---- Load Packages ----
library(reshape2)

# ---- Set Working Directory ----
setwd("C:/Users/Danielle/OneDrive/DQ_Documents/Lynx/Data/Working Data")

# ---- Read in Data ----
tdata<-read.csv("transectdata.csv")

# ---- Melt Data ----
tdata2<-melt(tdata, id.vars=c("transect_id","year","pass"))
tdata2<-tdata2[!tdata2$variable=="tot_len",]

# ---- Split variable into habitat and buffer ----
# Habitat #
tdata2$habitat<-NA
reftab1<-data.frame(habitat=c("road","clearcut","natural","treated"), letter=c("r","c","n","t"))
for(i in unique(reftab1$letter))
{
  tdata2$habitat[which(substr(tdata2$variable, 1, 1)==as.character(i))]<-as.character(reftab1$habitat[reftab1$letter==i])
}

# Buffer #
tdata2$buffer<-NA
for(i in 1:nrow(tdata2))
{
  tdata2$buffer[i]<-substr(tdata2$variable[i], gregexpr('_',tdata2$variable[i])[[1]][1], nchar(as.character(tdata2$variable[i])))
}
tdata2$buffer[tdata2$buffer=="_nobuf"]<-"no"
tdata2$buffer[tdata2$buffer=="_buf"]<-"yes"

# ---- Save Data File ----
names(tdata2)[names(tdata2)=="value"]<-"length"
tdata3<-tdata2[,names(tdata2) %in% c("year","pass","transect_id","length","habitat","buffer")]
write.csv(tdata3, "transectdata_reshaped.csv")
