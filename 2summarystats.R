# ---- Load packages ----
library(dplyr)
library(ggplot2)

# ---- Set working directory ----
setwd("C:/Users/danie/OneDrive/DQ_Documents/Lynx")
setwd("C:/Users/Danielle/OneDrive/DQ_Documents/Lynx")

# ---- Read in data ----
tdata<-read.csv("Data/Working Data/transectdata_reshaped.csv")
pdata<-read.csv("Data/Working Data/pointdata.csv")

# ---- Edit Data ----
pdata$habitat<-tolower(pdata$habitat)
pdata$buffer<-tolower(pdata$buffer)
pdata$description<-tolower(pdata$description)
pdata<-pdata[pdata$point_type=="track",]
pdata$description[pdata$description=="small mammals"]<-"small mammal"

tdata<-droplevels(tdata[!tdata$habitat=="road",])

# ---- Only Certain Species ----
pdata<-pdata[pdata$description %in% c("hare","lynx","squirrel","marten"),]

# ---- Buffer Labels ----
pdata$bufferL<-NA
pdata$bufferL[pdata$buffer=="no"]<-"non buffer"
pdata$bufferL[pdata$buffer=="yes"]<-"buffer"

tdata$bufferL<-NA
tdata$bufferL[tdata$buffer=="no"]<-"non buffer"
tdata$bufferL[tdata$buffer=="yes"]<-"buffer"

# ---- Set Factor Order ----
# Transects #
torder<-read.csv("Data/Working Data/TransectOrder_Input.csv")
tdata$transect_id<-with(tdata, factor(transect_id, levels=torder$transect_id))
pdata$transect_id<-with(pdata, factor(transect_id, levels=torder$transect_id))

# Buffer #
pdata$bufferL<-with(pdata, factor(bufferL, levels=c("buffer","non buffer")))
tdata$bufferL<-with(tdata, factor(bufferL, levels=c("buffer","non buffer")))

# Habitat #
pdata$habitat<-with(pdata, factor(habitat, levels=c("natural","treated", "clearcut")))
tdata$habitat<-with(tdata, factor(habitat, levels=c("natural","treated", "clearcut")))

# ---- Transects Per Year ----
trans<-data.frame(tdata%>%
                    group_by(year, pass)%>%
                    summarise(transects=n_distinct(transect_id)))
trans

# ---- Create Summary Tables ----
tdata2<-tdata[tdata$length>0,]
tdata2$el<-NA
tdata2$bd<-NA
tdata2$snow<-NA
for(i in 1:nrow(tdata2))
{
  use.rows<-which(as.character(pdata$transect_id)==as.character(tdata2$transect_id)[i] &
                    pdata$year==tdata2$year[i] &
                    pdata$pass==tdata2$pass[i] &
                    as.character(pdata$habitat)==as.character(tdata2$habitat[i]) &
                    as.character(pdata$bufferL)==as.character(tdata2$bufferL[i]))
  tdata2$el[i]<-round(mean(pdata$elevation[use.rows]),1)
  tdata2$bd[i]<-round(mean(pdata$buffer_distance[use.rows]),1)
  tdata2$snow[i]<-round(mean(pdata$hrs_snowfall[use.rows]),1)
}

newdata<-tdata2
newdata$animal<-NA
newdata$count<-0

pdata$check<-0
use.animals<-c("hare", "marten", "squirrel", "lynx")
for(i in use.animals)
{
  add.me<-tdata2

  add.me$animal<-as.character(i)
  for(ii in 1:nrow(add.me))
  {
    use.rows<-which(as.character(pdata$description)==as.character(i) &
               as.character(pdata$transect_id)==as.character(add.me$transect_id)[ii] &
               pdata$year==add.me$year[ii] &
               pdata$pass==add.me$pass[ii] &
               pdata$habitat==add.me$habitat[ii] &
               pdata$bufferL==add.me$bufferL[ii])
      pdata$check[use.rows]<-1
      add.me$count[ii]<-length(use.rows)
  }
  newdata<-rbind(newdata, add.me)
}
newdata<-newdata[!is.na(newdata$animal),]
newdata<-droplevels(newdata)

# Check for Errors #
pdata$check[!pdata$description %in% use.animals]<-1
errors<-pdata[pdata$check==0,]
nrow(errors)
if(nrow(errors)>0) {write.csv(errors, "Data/Working Data/mergingerrors.csv")}
# Errors fixed #

# ---- FIGURE 1 ----
figure1.input<-data.frame(tdata%>%
                            filter(length>0 & pass==1)%>%
                            group_by(transect_id, year, habitat)%>%
                            summarise(length=sum(length)))


figure1<-ggplot(figure1.input)+
  theme_bw(25)+
  geom_bar(aes(y=length, x=transect_id, fill=habitat), col='black', stat="identity")+
  theme(axis.text.x=element_text(angle=90, vjust=0, hjust=1))+
  facet_grid(year~.)+ylab("Survey Length (m)")+xlab("Transect")+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat")
figure1

# ---- FIGURE 2 ----
figure2.input<-data.frame(tdata%>%
                            filter(length>0 & pass==1)%>%
                            group_by(transect_id, year, bufferL)%>%
                            summarise(length=sum(length)))

figure2<-ggplot(figure2.input)+
  theme_bw(25)+
  geom_bar(aes(y=length, x=transect_id, fill=bufferL), col='black', stat="identity")+
  theme(axis.text.x=element_text(angle=90, vjust=0, hjust=1))+
  facet_grid(year~.)+ylab("Survey Length (m)")+xlab("Transect")+
  scale_fill_manual(values=c("burlywood3", "cadetblue2"), name="Buffer")
figure2

# ---- FIGURE 3 ----
figure3.input<-data.frame(tdata%>%
                   group_by(year, pass, habitat)%>%
                   summarise(total_length=sum(length)))
figure3.input$pass[figure3.input$pass==1]<-"pass 1"
figure3.input$pass[figure3.input$pass==2]<-"pass 2"

figure3<-ggplot(figure3.input)+
  geom_bar(aes(x=year, y=total_length/1000, fill=habitat), col='black', stat="identity")+
  theme_bw(25)+xlab("Year")+ylab("Total Distance (km)")+
  facet_grid(pass~.)+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat")
figure3

# ---- FIGURE 4 ----
figure4.input<-data.frame(tdata%>%
                   group_by(year, pass, bufferL)%>%
                   summarise(total_length=sum(length)))
figure4.input$pass[figure4.input$pass==1]<-"pass 1"
figure4.input$pass[figure4.input$pass==2]<-"pass 2"

figure4<-ggplot(figure4.input)+
  geom_bar(aes(x=year, y=total_length/1000, fill=bufferL), col='black', stat="identity")+
  theme_bw(25)+xlab("Year")+ylab("Total Distance (km)")+
  facet_grid(pass~.)+
  scale_fill_manual(values=c("burlywood3", "cadetblue2"), name="Buffer")
figure4

# ---- FIGURE 5 ----
figure5.input<-data.frame(tdata%>%
                   group_by(year, pass, habitat, bufferL)%>%
                   summarise(total_length=sum(length)))
figure5.input$pass[figure5.input$pass==1]<-"pass 1"
figure5.input$pass[figure5.input$pass==2]<-"pass 2"

figure5<-ggplot(figure5.input)+
  geom_bar(aes(x=year, y=total_length/1000, fill=habitat), col='black', stat="identity")+
  theme_bw(25)+xlab("Year")+ylab("Total Distance (km)")+
  facet_grid(pass~bufferL)+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat")
figure5

# ---- FIGURE 6 ----
figure6<-ggplot(newdata)+
  geom_boxplot(aes(x=habitat, y=length, fill=habitat))+
  theme_bw(25)+xlab("Year")+ylab("Survey Length (m)")+
  facet_grid(year~bufferL)+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat")
figure6

# ---- FIGURE 7 ----
labels7<-data.frame(pdata%>%
                      group_by(description)%>%
                      summarise(count=length(description)))

figure7<-ggplot(pdata)+
  geom_bar(aes(x=description), fill='grey50', col='black')+
  theme_bw(25)+ylab("Number of Tracks")+xlab("")+
  geom_text(aes(x=description, y=count+150, label=count), size=7, data=labels7)
figure7

# ---- FIGURES 8 to 10 ----
# By Habitat #
par(mar=c(6,5,1,4), las=3)
spineplot(as.factor(pdata$habitat)~as.factor(pdata$description),
          off=1, ylevels=c("natural","treated","clearcut"),
          col=c("aquamarine4", "darkorchid2", "brown3"),
          xlab="", ylab="", las=3)
mtext(side=4, "Proportion", line=2)

pdata3<-pdata[!pdata$description %in% c("hare", "squirrel"),]
par(mar=c(6,5,1,4), las=3)
spineplot(as.factor(pdata3$habitat)~as.factor(pdata3$description),
          off=1, ylevels=c("natural","treated","clearcut"),
          col=c("aquamarine4", "darkorchid2", "brown3"),
          xlab="", ylab="", las=3)
mtext(side=4, "Proportion", line=2)

# By Buffer #
par(mar=c(6,5,1,4), las=3)
spineplot(as.factor(pdata$bufferL)~as.factor(pdata$description),
          off=1, ylevels=c("buffer", "non buffer"),
          col=c("burlywood3", "cadetblue2"),
          xlab="", ylab="", las=3)
mtext(side=4, "Proportion", line=2)

par(mar=c(6,5,1,4), las=3)
spineplot(as.factor(pdata3$bufferL)~as.factor(pdata3$description),
          off=1, ylevels=c("buffer", "non buffer"),
          col=c("burlywood3", "cadetblue2"),
          xlab="", ylab="", las=3)
mtext(side=4, "Proportion", line=2)


par(mfrow=c(2,2), cex.lab=1.5, oma=c(6,3,3,3))
for(i in unique(pdata$description))
{
  if(!i %in% c("lynx", "marten")){xticklabel=""}
  if(i %in% c("marten","lynx")){xticklabel=c("clearcut", "natural","treated")}
  if(i %in% c("squirrel","lynx")){newtext="Proportion"}
  if(!i %in% c("squirrel","lynx")){newtext=""}
  par(mar=c(1,5,1,4), las=3)
  spineplot(as.factor(pdata$bufferL[pdata$description==i])~
              as.factor(pdata$habitat[pdata$description==i]),
            off=1, ylevels=c("buffer", "non buffer"),
            col=c("burlywood3", "cadetblue2"),
            xaxlabels=xticklabel,
            xlab="", ylab=i, las=3)
  mtext(side=4, newtext, line=2.5)  
}

# ---- FIGURE 11 ----
myscale_x<-max(pdata$easting[pdata$description=="lynx"])-5000
myscale_xend<-max(pdata$easting[pdata$description=="lynx"])
myscale_y<-min(pdata$northing_utm[pdata$description=="lynx"])
myscale_yend<-min(pdata$northing_utm[pdata$description=="lynx"])
  
map1<-ggplot(pdata[pdata$bufferL=="buffer",])+
  geom_point(aes(x=easting_utm, y=northing_utm, colour=habitat,shape=bufferL), size=7, alpha=0.8, data=pdata[pdata$description=='lynx',])+
  geom_point(aes(x=easting_utm, y=northing_utm, fill=habitat,shape=bufferL), colour='black', size=7, alpha=0.8, data=pdata[pdata$description=='lynx',])+
  theme_bw(20)+facet_wrap(~year)+
  xlab("Easting (UTM)")+ylab("Northing (UTM)")+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat", guide=FALSE)+
  scale_colour_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat")+
  scale_shape_manual(values=c(25,21), name="Buffer")+
  coord_fixed(ratio=1)+
  geom_segment(aes(x=myscale_x, xend=myscale_xend, y=myscale_y, yend=myscale_yend))+
  geom_text(aes(x=mean(c(myscale_xend, myscale_x)), y=myscale_y+1000, label="5 km"), hjust=0.5)
map1

# ---- FIGURE 12 ----
figure12<-ggplot(pdata[pdata$buffer=="no",])+
  geom_boxplot(aes(x=habitat, y=buffer_distance/1000, fill=habitat))+
  scale_fill_manual(values=c("aquamarine4","darkorchid2","brown3"), name="Habitat", guide=FALSE)+
  theme_bw(25)+xlab("Habitat")+ylab("Distance to Buffer (km)")+
  facet_grid(year~description)
figure12

# ---- FIGURE 13 ----
buf_nobuf<-data.frame(pdata%>%
  filter(pass==1)%>%
  group_by(description, bufferL, year)%>%
  summarise(count=length(description)))

figure13<-ggplot(buf_nobuf)+
  geom_bar(aes(x=year, y=count, fill=bufferL),col='black', stat="identity")+
  facet_wrap(~description, scales='free')+
  theme_bw(25)+ylab("Count")+xlab("")+
  scale_fill_manual(values=c("burlywood3", "cadetblue2"), name="Buffer")
figure13

# ---- FIGURE 14 ----
hab<-data.frame(pdata%>%
                        filter(pass==1)%>%
                        group_by(description, habitat, year)%>%
                        summarise(count=length(description)))

figure14<-ggplot(hab)+
  geom_bar(aes(x=year, y=count, fill=habitat),col='black', stat="identity")+
  facet_wrap(~description, scales='free')+
  theme_bw(25)+ylab("Count")+xlab("")+
  scale_fill_manual(values=c("aquamarine4", "darkorchid2", "brown3"), name="Habitat")
figure14

# ---- UNUSED FIGURE: COYOTE MAP ----
coy<-newdata[newdata$animal=="coyote",]
coy%>%
  group_by(year)%>%
  summarise(total=sum(count))

myscale_x<-max(pdata$easting[pdata$description=="coyote"])-5000
myscale_xend<-max(pdata$easting[pdata$description=="coyote"])
myscale_y<-min(pdata$northing_utm[pdata$description=="coyote"])
myscale_yend<-min(pdata$northing_utm[pdata$description=="coyote"])

map2<-ggplot(pdata[pdata$buffer=="yes",])+
  #geom_point(aes(x=easting_utm, y=northing_utm), colour="grey80", fill="grey80", shape=21, size=7, alpha=0.5)+
  geom_point(aes(x=easting_utm, y=northing_utm, colour=habitat,shape=buffer), size=7, alpha=0.8, data=pdata[pdata$description=='coyoyte',])+
  geom_point(aes(x=easting_utm, y=northing_utm, fill=habitat,shape=buffer), colour='black', size=7, alpha=0.8, data=pdata[pdata$description=='coyote',])+
  theme_bw(15)+facet_wrap(~year)+
  xlab("Easting (UTM)")+ylab("Northing (UTM)")+
  scale_fill_manual(values=c("brown3","aquamarine4", "darkorchid2"), name="Habitat", guide=FALSE)+
  scale_colour_manual(values=c("brown3","aquamarine4", "darkorchid2"), name="Habitat")+
  scale_shape_manual(values=c(25,21), name="Buffer")+
  coord_fixed(ratio=1)+
  geom_segment(aes(x=myscale_x, xend=myscale_xend, y=myscale_y, yend=myscale_yend))+
  geom_text(aes(x=mean(c(myscale_xend, myscale_x)), y=myscale_y+1000, label="5 km"), hjust=0.5)
map2

both2<-pdata[pdata$description %in% c("lynx",'coyote'),]
figure8<-ggplot(both2[both2$buffer=="no",])+
  geom_boxplot(aes(x=habitat, y=buffer_distance/1000, fill=habitat))+
  scale_fill_manual(values=c("brown3","aquamarine4", "darkorchid2"), name="Habitat", guide=FALSE)+
  theme_bw(15)+xlab("Habitat")+ylab("Distance to Buffer (km)")+
  facet_grid(.~description)
figure8
figure8b<-figure8+facet_grid(year~.)
figure8b

both_nobuf<-both2[both2$buffer=="no",]
# clearcut # p = 0.833
wilcox.test(both_nobuf$buffer_distance[both_nobuf$description=="lynx" & both_nobuf$habitat=="clearcut"],
            both_nobuf$buffer_distance[both_nobuf$description=="coyote" & both_nobuf$habitat=="clearcut"])
# treated # p = 0.154
wilcox.test(both_nobuf$buffer_distance[both_nobuf$description=="lynx" & both_nobuf$habitat=="treated"],
            both_nobuf$buffer_distance[both_nobuf$description=="coyote" & both_nobuf$habitat=="treated"])
# natural # p = 0.002
wilcox.test(both_nobuf$buffer_distance[both_nobuf$description=="lynx" & both_nobuf$habitat=="natural"],
            both_nobuf$buffer_distance[both_nobuf$description=="coyote" & both_nobuf$habitat=="natural"])
# overall #
wilcox.test(both_nobuf$buffer_distance[both_nobuf$description=="lynx"],
            both_nobuf$buffer_distance[both_nobuf$description=="coyote"])

nrow(pdata[pdata$description=="lynx" & pdata$buffer=="no",])
nrow(pdata[pdata$description=="lynx",])

nrow(pdata[pdata$description=="coyote" & pdata$buffer=="no",])
nrow(pdata[pdata$description=="coyote",])

# ---- UNUSED: LYNX to COYOTE ----
lynx.loc<-pdata[pdata$description=="lynx",]
coy.loc<-pdata[pdata$description=="coyote",]

lhabitat<-c()
lbuffer<-c()
chabitat<-C()
cbuffer<-c()
distance<-c()
for(i in 1:nrow(lynx.loc))
{
  results<-as.matrix(dist(rbind(data.frame(x=lynx.loc$easting_utm[i], y=lynx.loc$northing_utm[i]), data.frame(x=coy.loc$easting_utm, y=coy.loc$northing_utm))))
  distance.in<-min(results[-1,1])
  distance<-c(distance, distance.in)
}
boxplot(distance)
range(distance)
plot(jitter(sort(distance)))
abline(1000,0)
abline(500,0)
hist(distance, breaks=100)

tryme<-data.frame(pdata%>%
                    filter(description %in% c("lynx","coyote"))%>%
                    group_by(transect_id, year, description)%>%
                    select(transect_id, year, description)%>%
                    summarise(count=length(description)))
tryme

ggplot(tryme)+
  geom_point(aes(x=transect_id, y=description, size=count))+
  facet_grid(year~.)+theme_bw(15)

length(unique(tryme$transect_id)) # 56

# 15A_2, 2013 #
tran1<-pdata[pdata$transect_id=="15A_2" & pdata$year==2013 & pdata$description %in% c("lynx","coyote"),]
dist(rbind(data.frame(x=tran1$easting_utm[tran1$description=="coyote"], y=tran1$northing_utm[tran1$description=="coyote"]),
           data.frame(x=tran1$easting_utm[tran1$description=="lynx"], y=tran1$northing_utm[tran1$description=="lynx"])))

# 8B_2, 2015 #
tran1<-pdata[pdata$transect_id=="8B_2" & pdata$year==2015 & pdata$description %in% c("lynx","coyote"),]
dist(rbind(data.frame(x=tran1$easting_utm[tran1$description=="coyote"], y=tran1$northing_utm[tran1$description=="coyote"]),
           data.frame(x=tran1$easting_utm[tran1$description=="lynx"], y=tran1$northing_utm[tran1$description=="lynx"])))

# 12B_2, 2015 #
tran1<-pdata[pdata$transect_id=="12B_2" & pdata$year==2015 & pdata$description %in% c("lynx","coyote"),]
dist(rbind(data.frame(x=tran1$easting_utm[tran1$description=="coyote"], y=tran1$northing_utm[tran1$description=="coyote"]),
           data.frame(x=tran1$easting_utm[tran1$description=="lynx"], y=tran1$northing_utm[tran1$description=="lynx"])))

# How much prey availability? #
usethese<-unique(pdata$transect_id[pdata$description %in% c("lynx","coyote")])
mydata<-data.frame(pdata%>%
                     filter(transect_id %in% usethese)%>%
                     filter(description %in% c("hare","squirrel","grouse"))%>%
                     group_by(transect_id, year, pass)%>%
                     summarise(prey=length(description)))

mydata$length<-NA
for(i in 1:nrow(mydata))
{
  mydata$length[i]<-sum(tdata$length[tdata$year==mydata$year[i] &
                                       tdata$pass==mydata$pass[i] &
                                       as.character(tdata$transect_id)==as.character(mydata$transect_id)[i]])
}

ggplot(mydata[mydata$year %in% c(2013, 2015),])+
  geom_bar(aes(x=transect_id, y=prey/length), fill='grey50', col='black', stat='identity')+
  theme_bw(15)+
  facet_grid(year~., scales="free")

# It doesn't appear to be a result of prey abundance #


