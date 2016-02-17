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

# ---- Buffer vs Non-Buffer ----
# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="lynx",]) # p = 0.127
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="lynx",]) # p = 0.136

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="hare",]) # p = 0.177
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="hare",]) # p = 0.493

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="squirrel",]) # p = 0.691
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="squirrel",]) # p = 0.814

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_boxplot(aes(x=bufferL, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("")

wilcox.test(cpue~bufferL, data=abundata2[abundata2$description=="marten",]) # p = 0.481
wilcox.test(abundance~bufferL, data=abundata2[abundata2$description=="marten",]) # p = 0.622

# ---- Habitat ----
meanbyhab<-data.frame(abundata2%>%
                        group_by(description, habitat)%>%
                        summarise(m.cpue=mean(cpue), m.abun=mean(abundance)))

# Lynx #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="lynx",]) # p = 0.033
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="lynx",]) # p = 0.037

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.015
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.172
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 1

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.016
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.179
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="lynx" & abundata2$habitat %in% c("clearcut","treated"),]) # p = 1

# Hare #
ggplot(abundata2[abundata2$description=="hare",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="hare",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="hare",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.422
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.204
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="hare" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

# Squirrel #
ggplot(abundata2[abundata2$description=="squirrel",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="squirrel",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="squirrel",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.834
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.914
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("natural","clearcut"),]) # p < 0.01
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="squirrel" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

# Marten #
ggplot(abundata2[abundata2$description=="marten",])+
  geom_boxplot(aes(x=habitat, y=cpue))+theme_bw(25)+
  ylab("Tracks per Km")+xlab("Habitat")

kruskal.test(cpue~habitat, data=abundata2[abundata2$description=="marten",]) # p < 0.01
kruskal.test(abundance~habitat, data=abundata2[abundata2$description=="marten",]) # p < 0.01

wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.022
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.058
wilcox.test(cpue~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","treated"),]) # p = 0.020
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("natural","clearcut"),]) # p = 0.058
wilcox.test(abundance~habitat,data=abundata2[abundata2$description=="marten" & abundata2$habitat %in% c("clearcut","treated"),]) # p < 0.01

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
# Center Elevetion #
abundata2$elevation2<-as.vector(scale(abundata2$elevation, scale=FALSE))
# Log Length #
abundata2$Llength<-log(abundata2$length)

zip1<-zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                 habitat+buffer_distance+elevation2,
               data=abundata2[abundata2$description=="lynx",],
               dist="poisson")
summary(zip1)
dispersion(zip1, modeltype='zp') # 1.72

# ---- ZI Negative Binomial ----
# Quick Look at the Data #
ggplot(abundata2[abundata2$description=="lynx",])+
  geom_point(aes(x=buffer_distance, y=elevation), col="red", size=3, alpha=0.5,
             data=abundata2[abundata2$description=="lynx" & abundata2$abundance==0,])+
  geom_point(aes(x=buffer_distance, y=elevation), col="blue", size=3, alpha=0.5,
             data=abundata2[abundata2$description=="lynx" & abundata2$abundance>0,])+
  facet_grid(.~habitat)+theme_bw(15)

# Run Full Model #
zinb1<-zeroinfl(abundance~habitat+buffer_distance+elevation2+offset(Llength)|
                  habitat+buffer_distance+elevation2,
                data=abundata2[abundata2$description=="lynx",],
                dist="negbin")
summary(zinb1)
dispersion(zinb1, modeltype='znb') # 0.936

lrtest(zip1, zinb1) # Negative binomial model is better

newdata<-expand.grid(habitat=c("natural","treated","clearcut"),
                     buffer_distance=seq(from=0, to=1500, by=250),
                     elevation2=seq(from=min(abundata2$elevation2),
                                     to=max(abundata2$elevation2),
                                     length=25),
                     Llength=mean(abundata2$Llength))
newdata$abundance<-predict(zinb1, newdata, type="response")
newdata$zero<-predict(zinb1, newdata, type="zero")

# Predicted Abundance #
ggplot(newdata)+
  geom_line(aes(x=elevation2, y=abundance), size=2)+
  facet_grid(habitat~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Abundance")+
  xlab("Eleveation (Centered)")
# Why are there peaks?

# Predicted False Zeros #
ggplot(newdata)+
  geom_line(aes(x=elevation2, y=zero), size=2)+
  facet_grid(habitat~buffer_distance)+
  theme_bw(15)+
  ylab("Predicted Probability of False Zeros")+
  xlab("Elevation (Centered)")

### Show these to Trevor. What do they mean? ###

# Model Comparisons #
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