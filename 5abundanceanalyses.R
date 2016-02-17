# ---- To Do (Feb 1, 2016) ----
# Update all plots - DONE
# Test abundance vs habitat and buffer - DONE
# GLMs of abundance by habitat and buffer - DONE
# Zero Inflated GLMs on abundance
# Check variance of occupancy models 

# ---- Load Packages ----
library(MASS)
library(reshape2)
library(pscl)
library(lmtest)

# ---- Set Up Data Frame of Abundance ----
# elevation and buffer_distance will be the average of all points across all years and passes #
abundata<-tdata[tdata$length>0 & tdata$pass==1,]
abundata$elevation<-NA
abundata$buffer_distance<-NA
abundata$lynx<-NA
abundata$hare<-NA
abundata$squirrel<-NA
abundata$marten<-NA

for(i in 1:nrow(abundata))
{
  y<-pdata[pdata$transect_id==abundata$transect_id[i] &
             pdata$habitat==abundata$habitat[i] &
             pdata$bufferL==abundata$bufferL[i],]
  x<-pdata[pdata$transect_id==abundata$transect_id[i] &
             pdata$year==abundata$year[i] &
             pdata$pass==1 &
             pdata$habitat==abundata$habitat[i] &
             pdata$bufferL==abundata$bufferL[i],]
  abundata$buffer_distance[i]<-mean(y$buffer_distance, na.rm=TRUE)
  abundata$elevation[i]<-mean(y$elevation, na.rm=TRUE)
  abundata$lynx[i]<-nrow(x[x$description=="lynx",])
  abundata$hare[i]<-nrow(x[x$description=="hare",])
  abundata$squirrel[i]<-nrow(x[x$description=="squirrel",])
  abundata$marten[i]<-nrow(x[x$description=="marten",])
}

abundata2<-melt(abundata, id.vars=names(abundata)[1:9])
names(abundata2)[names(abundata2)=="variable"]<-"description"
names(abundata2)[names(abundata2)=="value"]<-"abundance"
abundata2$cpue<-abundata2$abundance/abundata2$length*1000

# Eight cells with missing elevation and/or buffer_distance
# Remove
abundata2<-abundata2[-which(is.na(abundata2$elevation)),]

# Center Elevetion #
abundata2$elevation2<-as.vector(scale(abundata2$elevation, scale=FALSE))
# Log Length #
abundata2$Llength<-log(abundata2$length)

# ---- Buffer vs Non-Buffer ----
# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="lynx",]) # p = 0.175
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="lynx",]) # p = 0.186

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="hare",]) # p = 0.445
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="hare",]) # p = 964

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="squirrel",]) # p = 0.954
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="squirrel",]) # p = 0.817

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="marten",]) # p = 0.610
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="marten",]) # p = 0.776

# ---- Habitat ----
meanbyhab<-data.frame(abundata2%>%
                        group_by(description, habitat)%>%
                        summarise(m.cpue=mean(cpue), m.abun=mean(abundance)))

# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="lynx",]) # p = 0.048
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="lynx",]) # p = 0.053

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.015
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.399
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 0.652

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.019
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.415
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 0.656

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="hare",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="hare",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.381
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.174
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="squirrel",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="squirrel",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.790
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.872
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="marten",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="marten",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.020
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.111
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 0.014

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.0185
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.111
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 0.014

# ---- Buffer Distance ----
# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_point(aes(x=buffer_distance, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_point(aes(x=buffer_distance, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_point(aes(x=buffer_distance, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_point(aes(x=buffer_distance, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# ---- Elevation ----
# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_point(aes(x=elevation, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_point(aes(x=elevation, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_point(aes(x=elevation, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_point(aes(x=elevation, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Distance to Buffer (m)")

# ---- Dot Plots ----
# Dot Plots #
par(mfrow=c(1,4), mar=c(4,2,2,1))
dotchart(abundata2$abundance[abundata2$description=="lynx"], main="Lynx")
dotchart(abundata2$abundance[abundata2$description=="hare"], main="Hare")
dotchart(abundata2$abundance[abundata2$description=="squirrel"], main="Squirrel")
dotchart(abundata2$abundance[abundata2$description=="marten"], main="Marten")

# Frequency Plots #
par(mfrow=c(1,4), mar=c(4,2,2,1))
plot(table(abundata2$abundance[abundata2$description=="lynx"]), type="h", main="Lynx")
plot(table(abundata2$abundance[abundata2$description=="hare"]), type="h", main="Hare")
plot(table(abundata2$abundance[abundata2$description=="squirrel"]), type="h", main="Squirrel")
plot(table(abundata2$abundance[abundata2$description=="marten"]), type="h", main="Marten")

# ---- GLMs ----
# Poisson GLM #
model1<-glm(abundance~habitat+buffer_distance+elevation,
            data=abundata2[abundata2$description=="lynx",],
            family="poisson")
dispersion(model1, modeltype="poisson") # 6.83 (overdispersed)

# NB GLM #
model2<-glm.nb(abundance~habitat+buffer_distance+elevation,
            data=abundata2[abundata2$description=="lynx",])
dispersion(model2, modeltype="nb") # 1.17
plot(model2)
summary(model2)
drop1(model2, test="Chi")
# Non-significant habitat and elevation effect, nearly significant buffer effect

# ---- ZIP ----
zip1<-zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                 habitat+buffer_distance+elevation2,
               data=abundata2[abundata2$description=="lynx",],
               dist="poisson")
summary(zip1)
dispersion(zip1, modeltype='zp') # 1.72

# ---- ZI Negative Binomial ----
# Set up some variables for plotting #
mylimits<-c(325-mean(abundata2$elevation), 500-mean(abundata2$elevation))
mybreaks<-c(325-mean(abundata2$elevation), 400-mean(abundata2$elevation), 475-mean(abundata2$elevation))
mylabels<-c(350, 400, 450)

rm(zinb1)
rm(plotpredicted)
rm(plotpredicted2)
rm(plotzeros)
rm(plotzeros2)

# Set Animal #
useme<-"squirrel"
if(useme %in% c("hare","lynx")) {usehab=c("natural","treated","clearcut");
                                 mycols=c("aquamarine4","darkorchid2","brown3");
                                 usedata=abundata2}
if(useme %in% c("squirrel","marten")) {usehab=c("natural","treated");
                                       mycols=c("aquamarine4","darkorchid2");
                                       usedata=droplevels(abundata2[abundata2$habitat %in% usehab,])}


# Quick Look at the Data #
usedata$pa<-"0"
usedata$pa[usedata$abundance>0]<-"> 0"
usedata$pa<-with(usedata, factor(pa, levels=c("0","> 0")))

preZINB<-ggplot(usedata[usedata$description==useme,])+
  geom_point(aes(x=buffer_distance, y=elevation, col=pa), size=3, alpha=0.5)+
  scale_colour_manual(values=c("red","blue"), name="Track Count")+
  facet_grid(.~habitat)+theme_bw(15)+
  ggtitle(useme)+xlab("Distance to Buffer (m)")+
  ylab("Elevation (m)")
ggsave(paste0("Figures and Tables/preZINB_",useme,".png"), preZINB,
       width=25, height=15, units="cm")

# Run Full Model #
zinb1<-zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                  habitat+buffer_distance+elevation2,
                data=usedata[usedata$description==useme,],
                dist="negbin")
summary(zinb1)
dispersion(zinb1, modeltype='znb')

# lrtest(zip1, zinb1) # Negative binomial model is better

newdata<-expand.grid(habitat=usehab,
                     buffer_distance=seq(from=0, to=1500, by=250),
                     elevation2=seq(from=min(usedata$elevation2),
                                     to=max(usedata$elevation2),
                                     length=25),
                     Llength=mean(usedata$Llength))
newdata$abundance<-predict(zinb1, newdata, type="response")
newdata$zero<-predict(zinb1, newdata, type="zero")

# Predicted Abundance #
plotpredicted<-ggplot(newdata)+
  geom_line(aes(x=elevation2, y=abundance), size=1)+
  facet_grid(habitat~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Abundance")+
  xlab("Elevation (m)")+
  ggtitle(useme)+
  scale_x_continuous(limits=mylimits, breaks=mybreaks, labels=mylabels)
ggsave(paste0("Figures and Tables/fullZINB_predict_",useme,".png"), plotpredicted,
       width=25, height=15, units="cm")
plotpredicted2<-ggplot(newdata)+
  geom_line(aes(x=elevation2, y=abundance, col=habitat), size=1)+
  facet_grid(.~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Abundance")+
  xlab("Elevation (m)")+
  ggtitle(useme)+
  scale_colour_manual(values=mycols, name="Habitat")+
  scale_x_continuous(limits=mylimits, breaks=mybreaks, labels=mylabels)
ggsave(paste0("Figures and Tables/fullZINB_predict2_",useme,".png"), plotpredicted2,
       width=25, height=10, units="cm")
# Why are there peaks?

# Predicted False Zeros #
plotzeros<-ggplot(newdata)+
  geom_line(aes(x=elevation2, y=zero), size=1)+
  facet_grid(habitat~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Probability of False Zeros")+
  xlab("Elevation (m)")+
  ggtitle(useme)+
  scale_x_continuous(limits=mylimits, breaks=mybreaks, labels=mylabels)
ggsave(paste0("Figures and Tables/fullZINB_falsezeros_",useme,".png"), plotzeros,
       width=25, height=15, units="cm")
plotzeros2<-ggplot(newdata)+
  geom_line(aes(x=elevation2, y=zero, col=habitat), size=1)+
  facet_grid(.~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Probability of False Zeros")+
  xlab("Elevation (m)")+
  ggtitle(useme)+
  scale_colour_manual(values=mycols, name="Habitat")+
  scale_x_continuous(limits=mylimits, breaks=mybreaks, labels=mylabels)
ggsave(paste0("Figures and Tables/fullZINB_falsezeros2_",useme,".png"), plotzeros2,
       width=25, height=10, units="cm")











# ---- Model Comparisons ----
mytheta<-zinb1$theta

# Function from Zurr for likelihood ratio test #
LikRatioTest<-function(L1,L2,df)
{
  # L1 is the full model, L2 is the nested model
  # df is the difference in parameters
  L<-2*(L1-L2)
  L<-abs(as.numeric(L))
  p<-1-pchisq(L, df)
  round(c(L, p), digits=4)
}

source("Code/ZurrFunction_myzeroinfl.R")
# (1) Drop habitat from count model
zinb1a<-my.zeroinfl(abundance~buffer_distance+elevation2+offset(Llength)|
                   habitat+buffer_distance+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                dist="negbin", fixed.log.theta=log(mytheta))
# (2) Drop buffer_distance from count model
zinb1b<-my.zeroinfl(abundance~habitat+elevation2+offset(Llength)|
                   habitat+buffer_distance+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))
# (3) Drop elevation2 from count model
zinb1c<-my.zeroinfl(abundance~habitat+buffer_distance+offset(Llength)|
                   habitat+buffer_distance+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))
# (4) Drop offset(Llength) from count model
zinb1d<-my.zeroinfl(abundance~habitat+buffer_distance+elevation2|
                   habitat+buffer_distance+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))
  # DOESN'T CONVERGE #
# (5) Drop habitat from binary model
zinb1e<-my.zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                   buffer_distance+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))
# (6) Drop buffer_distance from binary model
zinb1f<-my.zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                   habitat+elevation2,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))
# (7) Drop elevation2 from binary model
zinb1g<-my.zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                   habitat+buffer_distance,
                 data=abundata2[abundata2$description=="lynx",],
                 dist="negbin", fixed.log.theta=log(mytheta))

Za<-LikRatioTest(logLik(zinb1), logLik(zinb1a), 2)
Zb<-LikRatioTest(logLik(zinb1), logLik(zinb1b), 1)
Zc<-LikRatioTest(logLik(zinb1), logLik(zinb1c), 1)
Ze<-LikRatioTest(logLik(zinb1), logLik(zinb1e), 2)
Zf<-LikRatioTest(logLik(zinb1), logLik(zinb1f), 1)
Zg<-LikRatioTest(logLik(zinb1), logLik(zinb1g), 1)

Z<-rbind(Za, Zb, Zc, Ze, Zf, Zg)
row.names(Z)<-c("hab.count","bd.count","ele.count","hab.bin","bd.bin","ele.bin")
Z
### This shows that only elevation is significant in the count and binary models ###
### Does this make sense? Could this be the influence of centering the data? ###