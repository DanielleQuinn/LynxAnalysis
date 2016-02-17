# ---- Set Up Values for Predict ----
snowvals<-seq(from=min(surveydata$hrs_snowfall, na.rm=TRUE),
              to=max(surveydata$hrs_snowfall, na.rm=TRUE),
              length=25)
bdvals<-seq(from=min(surveydata$buffer_distance, na.rm=TRUE),
            to=max(surveydata$buffer_distance, na.rm=TRUE),
                   length=25)
elvals<-seq(from=min(surveydata$elevation,na.rm=TRUE),
            to=max(surveydata$elevation, na.rm=TRUE),
            length=25)

# ---- Remove Cells With Missing Covariate Values ----
remove1<-unique(c(which(is.na(cellcovs$habitat)),
                  which(is.na(cellcovs$buffer)),
                  which(is.na(cellcovs$buffer_distance)),
                  which(is.na(cellcovs$elevation))))
ind<-apply(o.snow, 1, function(x) all(is.na(x)))
remove2<-which(ind==TRUE)
removeall<-unique(remove1, remove2)

cellcovs2<-cellcovs[-removeall,]
cellcovs2$habitat<-droplevels(cellcovs2$habitat)
samplecovs2<-samplecovs
samplecovs2$o.prey<-samplecovs2$o.prey[-removeall,]
samplecovs2$o.comp<-samplecovs2$o.comp[-removeall,]
samplecovs2$o.snow<-samplecovs2$o.snow[-removeall,]

# ---- FUNCTION: Generate all occupancy only models ----
basicinfo<-function(SPECIES)
{
  if(!paste0(SPECIES,".y") %in% ls()) {assign(paste0(SPECIES,".y"),create.matrix(SPECIES), envir=.GlobalEnv)}

  tempdata<-eval(as.name(paste0(SPECIES, ".y")))
  tempdata2<-tempdata[-removeall,]
  
  model.setup<-unmarkedFrameOccu(y=tempdata2, siteCovs=cellcovs2, obsCovs=samplecovs2)
  
  rm(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16, envir=.GlobalEnv)
  try(assign("m1", occu(~1~1, data=model.setup), envir=.GlobalEnv))
  mymodels<-list(m1)
  
  try(assign("m2", occu(~1~habitat, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m2)
  try(assign("m3", occu(~1~buffer, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m3)
  try(assign("m4", occu(~1~buffer_distance, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m4)
  try(assign("m5", occu(~1~elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m5)
  
  try(assign("m6", occu(~1~habitat+buffer, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m6)
  try(assign("m7", occu(~1~habitat+buffer_distance, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m7)
  try(assign("m8", occu(~1~habitat+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m8)
  try(assign("m9", occu(~1~buffer+buffer_distance, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m9)
  try(assign("m10", occu(~1~buffer+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m10)
  try(assign("m11", occu(~1~buffer_distance+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m11)
  
  try(assign("m12", occu(~1~habitat+buffer+buffer_distance, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m12)
  try(assign("m13", occu(~1~habitat+buffer+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m13)
  try(assign("m14", occu(~1~habitat+buffer_distance+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m14)
  try(assign("m15", occu(~1~buffer+buffer_distance+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m15)
  
  try(assign("m16", occu(~1~habitat+buffer+buffer_distance+elevation, data=model.setup), envir=.GlobalEnv)); try(mymodels[[length(mymodels)+1]]<-m16)
  
  allmods<-paste0("m",1:16)
  usemods<-allmods[allmods %in% ls(envir=.GlobalEnv)]
  names(mymodels)<-usemods
  
  compareme<-fitList(fits=mymodels)
  return(modSel(compareme))
}

# ---- Run Occupancy Models ----
useme<-"marten"
basicinfo(useme)

# ---- MODEL 1: Null Model ----
summary(m1)
backTransform(m1, type="state")
backTransform(m1, type="det")
# I'm still unsure as to why we get a occupancy probability of 100% with the lynx data

# ---- MODEL 2: Habitat ----
newdata<-data.frame(habitat=unique(cellcovs2$habitat))
newdata$predicted<-predict(m2, newdata, type="state")[,1]
newdata$se<-predict(m2, newdata, type="state")[,2]

plot2<-ggplot(newdata)+
  geom_point(aes(x=habitat, y=predicted, col=habitat))+
  geom_errorbar(aes(x=habitat, ymin=predicted-se, ymax=predicted+se, col=habitat), width=0.25)+
  theme_bw(15)+
  xlab("Habitat")+
  ylab("Predicted Occupancy")+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"), guide=FALSE)+
  ylim(0,1)
plot2

# ---- MODEL 3: Buffer ----
newdata<-data.frame(buffer=sort(unique(cellcovs2$buffer)))
newdata$predicted<-predict(m3, newdata, type="state")[,1]
newdata$se<-predict(m3, newdata, type="state")[,2]

plot3<-ggplot(newdata)+
  geom_point(aes(x=buffer, y=predicted, col=buffer))+
  geom_errorbar(aes(x=buffer, ymin=predicted-se, ymax=predicted+se, col=buffer), width=0.25)+
  theme_bw(15)+
  xlab("Buffer")+
  ylab("Predicted Occupancy")+
  scale_colour_manual(values=c("burlywood3","cadetblue3"), guide=FALSE)+
  ylim(0,1)
plot3

# ---- MODEL 4: Buffer Distance ----
newdata<-data.frame(buffer_distance=sort(unique(cellcovs2$buffer_distance)))
newdata$predicted<-predict(m4, newdata, type="state")[,1]
newdata$se<-predict(m4, newdata, type="state")[,2]

plot4<-ggplot(newdata)+
  geom_line(aes(x=buffer_distance, y=predicted))+
  geom_point(aes(x=buffer_distance, y=predicted))+
  geom_line(aes(x=buffer_distance, y=predicted-se), linetype='dashed')+
  geom_line(aes(x=buffer_distance, y=predicted+se), linetype='dashed')+
  theme_bw(15)+
  xlab("Buffer Distance")+
  ylab("Predicted Occupancy")+
  ylim(0,1)
plot4

# ---- MODEL 5: Elevation ----
newdata<-data.frame(elevation=sort(unique(cellcovs2$elevation)))
newdata$predicted<-predict(m5, newdata, type="state")[,1]
newdata$se<-predict(m5, newdata, type="state")[,2]

plot5<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted))+
  geom_point(aes(x=elevation, y=predicted))+
  geom_line(aes(x=elevation, y=predicted-se), linetype='dashed')+
  geom_line(aes(x=elevation, y=predicted+se), linetype='dashed')+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  ylim(0,1)
plot5

# ---- MODEL 6: Habitat + Buffer ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat), buffer=unique(cellcovs2$buffer))
newdata$predicted<-predict(m6, newdata, type="state")[,1]
newdata$se<-predict(m6, newdata, type="state")[,2]

newdata<-newdata[-which(newdata$buffer=="yes" & newdata$habitat=="clearcut"),]

plot6<-ggplot(newdata)+
  geom_point(aes(x=habitat, y=predicted, col=habitat), size=5)+
#  geom_errorbar(aes(x=habitat, ymin=predicted-se, ymax=predicted+se, col=habitat), width=0.25)+
  theme_bw(15)+
  xlab("Habitat")+
  ylab("Predicted Occupancy")+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"), guide=FALSE)+
  ylim(0,1)+
  facet_grid(buffer~.)
plot6

# ---- MODEL 7: Habitat + Buffer Distance ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat), buffer_distance=sort(unique(cellcovs2$buffer_distance)))
newdata$predicted<-predict(m7, newdata, type="state")[,1]
newdata$se<-predict(m7, newdata, type="state")[,2]

newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$habitat=="clearcut"),]

plot7<-ggplot(newdata)+
  geom_line(aes(x=buffer_distance, y=predicted, col=habitat))+
  theme_bw(15)+
  xlab("Buffer Distance (m)")+
  ylab("Predicted Occupancy")+
  ylim(0,1)+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"))
plot7

# ---- MODEL 8: Habitat + Elevation ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat), elevation=sort(unique(cellcovs2$elevation)))
newdata$predicted<-predict(m8, newdata, type="state")[,1]
newdata$se<-predict(m8, newdata, type="state")[,2]

plot8<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=habitat))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  ylim(0,1)+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"))
plot8

# ---- MODEL 9: Buffer + Buffer Distance ----
newdata<-expand.grid(buffer=unique(cellcovs2$buffer), buffer_distance=sort(unique(cellcovs2$buffer_distance)))
newdata$predicted<-predict(m9, newdata, type="state")[,1]
newdata$se<-predict(m9, newdata, type="state")[,2]

newdata<-newdata[-which(newdata$buffer=="yes" & newdata$buffer_distance>0),]
newdata<-newdata[-which(newdata$buffer=="no" & newdata$buffer_distance==0),]

plot9<-ggplot(newdata)+
  geom_line(aes(x=buffer_distance, y=predicted, col=buffer))+
  geom_point(aes(x=buffer_distance, y=predicted, col=buffer))+
  theme_bw(15)+
  xlab("Buffer Distance (m)")+
  ylab("Predicted Occupancy")+
  ylim(0,1)+
  scale_colour_manual(values=c("burlywood3","cadetblue2"))
plot9

# ---- MODEL 10: Buffer + Elevation ----
newdata<-expand.grid(buffer=unique(cellcovs2$buffer), elevation=sort(unique(cellcovs2$elevation)))
newdata$predicted<-predict(m10, newdata, type="state")[,1]
newdata$se<-predict(m10, newdata, type="state")[,2]

plot10<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=buffer))+
  geom_point(aes(x=elevation, y=predicted, col=buffer))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  ylim(0,1)+
  scale_colour_manual(values=c("burlywood3","cadetblue2"))
plot10

# ---- MODEL 11: Buffer Distance + Elevation ----
newdata<-expand.grid(buffer_distance=c(0,500,1000,1500),
                     elevation=elvals)
predictions<-predict(m11, newdata, type="state")
newdata$predicted<-predictions[,1]
newdata$se<-predictions[,2]

plot11<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  facet_grid(.~buffer_distance)+
  ylim(c(0,1))
plot11

# ---- MODEL 12: Habitat + Buffer + Buffer Distance ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat),
                     buffer=unique(cellcovs2$buffer),
                     buffer_distance=sort(unique(cellcovs2$buffer_distance)))
newdata$predicted<-predict(m12, newdata, type="state")[,1]
newdata$se<-predict(m12, newdata, type="state")[,2]

newdata<-newdata[-which(newdata$buffer=="yes" & newdata$buffer_distance>0),]
newdata<-newdata[-which(newdata$buffer=="no" & newdata$buffer_distance==0),]
newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$habitat=="clearcut"),]

plot12<-ggplot(newdata)+
  geom_line(aes(x=buffer_distance, y=predicted, col=habitat))+
  geom_point(aes(x=buffer_distance, y=predicted, col=habitat))+
  theme_bw(15)+
  xlab("Buffer Distance (m)")+
  ylab("Predicted Occupancy")+facet_grid(.~buffer)+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"))
plot12

# ---- MODEL 13: Habitat + Buffer + Elevation ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat),
                     buffer=unique(cellcovs2$buffer),
                     elevation=sort(unique(cellcovs2$elevation)))
newdata$predicted<-predict(m13, newdata, type="state")[,1]
newdata$se<-predict(m13, newdata, type="state")[,2]

newdata<-newdata[-which(newdata$buffer=="yes" & newdata$habitat=="clearcut"),]

plot13<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=habitat))+
  geom_point(aes(x=elevation, y=predicted, col=habitat))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+facet_grid(.~buffer)+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"))
plot13

# ---- MODEL 14: Habitat + Buffer Distance + Elevation ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat),
                     buffer_distance=c(0,250,500,750,1000,1250,1500),
                     elevation=unique(cellcovs2$elevation))
predictions<-predict(m14, newdata, type="state")
newdata$predicted<-predictions[,1]
newdata$se<-predictions[,2]

newdata$buffer_distance<-as.factor(newdata$buffer_distance)
newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$habitat=="clearcut"),]

plot14<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=habitat), size=2)+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  facet_grid(.~buffer_distance)+
  ylim(c(0,1))+
  scale_colour_manual(values=c("aquamarine4", "darkorchid2", "brown3"))+
  ggtitle(useme)
plot14
ggsave(paste0("Figures and Tables/fullocc_",useme,".png"), plot14, width=25, height=15, units="cm")

# ---- MODEL 15: Buffer + Buffer Distance + Elevation ----
newdata<-expand.grid(buffer=unique(cellcovs2$buffer),
                     buffer_distance=c(0,500,1000,1500),
                     elevation=unique(cellcovs2$elevation))
predictions<-predict(m15, newdata, type="state")
newdata$predicted<-predictions[,1]
newdata$se<-predictions[,2]

newdata$buffer_distance<-as.factor(newdata$buffer_distance)
newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$buffer=="no"),]
newdata<-newdata[-which(!newdata$buffer_distance==0 & newdata$buffer=="yes"),]

plot15<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=buffer))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  facet_grid(.~buffer_distance)+
  ylim(c(0,1))+
  scale_colour_manual(values=c("burlywood2","cadetblue3"))
plot15

# ---- MODEL 16: Habitat + Buffer + Buffer Distance + Elevation ----
newdata<-expand.grid(habitat=unique(cellcovs2$habitat),
                     buffer=unique(cellcovs2$buffer),
                     buffer_distance=c(0,500,1000,1500),
                     elevation=elvals)
predictions<-predict(m16, newdata, type="state")
newdata$predicted<-predictions[,1]
newdata$se<-predictions[,2]

newdata$buffer_distance<-as.factor(newdata$buffer_distance)
newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$buffer=="no"),]
newdata<-newdata[-which(newdata$buffer_distance==0 & newdata$habitat=="clearcut"),]
newdata<-newdata[-which(!newdata$buffer_distance==0 & newdata$buffer=="yes"),]

plot16<-ggplot(newdata)+
  geom_line(aes(x=elevation, y=predicted, col=habitat))+
  theme_bw(15)+
  xlab("Elevation (m)")+
  ylab("Predicted Occupancy")+
  facet_grid(.~buffer_distance)+
  ylim(c(0,1))+
  scale_colour_manual(values=c("brown3","aquamarine4","darkorchid2"))
plot16

# ---- FUNCTION: Model detectability with snow as sample covariate ----
det.snow<-function(SPECIES, SCALE="free")
{
  
  if(!paste0(SPECIES,".y") %in% ls()) {assign(paste0(SPECIES,".y"),create.matrix(SPECIES), envir=.GlobalEnv)}
  
  tempdata<-eval(as.name(paste0(SPECIES, ".y")))
  tempdata2<-tempdata[-removeall,]
  
  model.setup<-unmarkedFrameOccu(y=tempdata2, siteCovs=cellcovs2, obsCovs=samplecovs2)
  
  mymodel<<-occu(~o.snow~1, data=model.setup)
  
  newdata<<-expand.grid(o.snow=snowvals)
  newdata$predicted<<-predict(mymodel, type = 'det', newdata)$Predicted
  newdata$se<<-predict(mymodel, type = 'det', newdata)$SE

  plot1<-ggplot(newdata)+
    geom_line(aes(x=o.snow, y=predicted))+
    theme_bw(15)+
    xlab("Hours Since Snowfall")+
    ylab("Predicted Detectability")+
    ggtitle(SPECIES)
  if(SCALE=="fixed") {plot1<-plot1+ylim(c(0,1))}
  return(plot1)
  return(newdata)
  return(mymodel)
}

# ---- Run Detection Model ----
det.snow("marten", SCALE="fixed")
mymodel
