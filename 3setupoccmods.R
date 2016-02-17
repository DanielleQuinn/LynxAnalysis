# ---- Load packages ----
library(dplyr)
library(unmarked)
library(ggplot2)

# ---- Set up cells for occupancy models ----
# Describe all cells
allcells<-data.frame(tdata%>%
                       filter(pass==1 & length>0)%>%
                       group_by(transect_id, habitat, buffer)%>%
                       summarise(rmme=mean(year)))
allcells<-allcells[,-which(names(allcells)=="rmme")]
# Describe the cells that were actually surveyed
celldata<-tdata[tdata$length>0 & tdata$pass==1,]
# Subset to only have pass 1 surveys
surveydata<-pdata[pdata$pass==1 & pdata$point_type=="track",]

# Assign cell IDs #
allcells$id<-paste(allcells$transect_id,
                   substr(allcells$habitat, 1, 1),
                   substr(allcells$buffer, 1, 1), sep="-")
celldata$id<-paste(celldata$transect_id,
                   substr(celldata$habitat, 1, 1),
                   substr(celldata$buffer, 1, 1), sep="-")
surveydata$id<-paste(surveydata$transect_id,
                   substr(surveydata$habitat, 1, 1),
                   substr(surveydata$buffer, 1, 1), sep="-")

# ---- Cell survey history ----
history<-data.frame(table(celldata$id, celldata$year))
names(history)<-c("id","year","check")
historysum<-data.frame(history%>%
                       group_by(id)%>%
                       summarise(total=sum(check)))
table(historysum$total)

# I've confirmed that all cells identified in the survey data 
# have a corresponding cell record in the cell data (this is great!)
# There are 8 cells that were surveyed and have no points
# associated with them (this is fine - true zeros)

# ---- Create vectors of segment_id by visit ----
all.vector<-unique(celldata$id)
v2013<-unique(celldata$id[celldata$year==2013])
v2014<-unique(celldata$id[celldata$year==2014])
v2015<-unique(celldata$id[celldata$year==2015])

# ---- Cell covariates (data frame) ----
cellcovs<-allcells[,c(4,1,2,3)]
cellcovs$elevation<-NA
cellcovs$buffer_distance<-NA
cellcovs$length<-NA
for(i in 1:nrow(cellcovs))
{
  x<-surveydata[surveydata$id==cellcovs$id[i],]
  cellcovs$elevation[i]<-mean(x$elevation, na.rm=TRUE)
  cellcovs$buffer_distance[i]<-mean(x$buffer_distance, na.rm=TRUE)
  cellcovs$length[i]<-mean(celldata$length[celldata$id==cellcovs$id[i]])
}

# ---- Observed data ----
blankdata<-data.frame(id=allcells$id, v2013=NA, v2014=NA, v2015=NA)

for(i in 1:nrow(blankdata))
{
  if(nrow(celldata[celldata$id==blankdata$id[i] &
                   celldata$year==2013,])>0) {blankdata$v2013[i]<-100}
  if(nrow(celldata[celldata$id==blankdata$id[i] &
                   celldata$year==2014,])>0) {blankdata$v2014[i]<-100}
  if(nrow(celldata[celldata$id==blankdata$id[i] &
                   celldata$year==2015,])>0) {blankdata$v2015[i]<-100}
}

# blankdata will be the template for observational data #

create.matrix<-function(description)
{
  useme<-blankdata
  for(i in 1:nrow(useme))
  {
    if(!is.na(useme$v2013[i])) {useme$v2013[i]<-nrow(surveydata[surveydata$year==2013 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    if(!is.na(useme$v2014[i])) {useme$v2014[i]<-nrow(surveydata[surveydata$year==2014 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    if(!is.na(useme$v2015[i])) {useme$v2015[i]<-nrow(surveydata[surveydata$year==2015 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    
  }
  useme$v2013[useme$v2013>1 & !is.na(useme$v2013)]<-1
  useme$v2014[useme$v2014>1 & !is.na(useme$v2014)]<-1
  useme$v2015[useme$v2015>1 & !is.na(useme$v2015)]<-1

  return(as.matrix(useme[,2:4]))
  description<<-description
}

lynx.y<-create.matrix("lynx")
hare.y<-create.matrix("hare") 
coyote.y<-create.matrix("coyote")
moose.y<-create.matrix("moose")
prey.y<-create.matrix(c("squirrel","grouse","hare"))

# ---- Sample covariates ----
# hours since snow
o.snow_pre<-blankdata
for(i in 1:nrow(o.snow_pre))
{
  if(!is.na(o.snow_pre$v2013[i])) {o.snow_pre$v2013[i]<-round(mean(surveydata$hrs_snowfall[surveydata$year==2013 &
                                                                                       surveydata$id==o.snow_pre$id[i]], na.rm=TRUE),0)}
  if(!is.na(o.snow_pre$v2014[i])) {o.snow_pre$v2014[i]<-round(mean(surveydata$hrs_snowfall[surveydata$year==2014 &
                                                                                       surveydata$id==o.snow_pre$id[i]], na.rm=TRUE),0)}
  if(!is.na(o.snow_pre$v2015[i])) {o.snow_pre$v2015[i]<-round(mean(surveydata$hrs_snowfall[surveydata$year==2015 &
                                                                                       surveydata$id==o.snow_pre$id[i]], na.rm=TRUE),0)}
}
o.snow_pre$v2013[is.na(o.snow_pre$v2013)]<-NA
o.snow_pre$v2014[is.na(o.snow_pre$v2014)]<-NA
o.snow_pre$v2015[is.na(o.snow_pre$v2015)]<-NA

o.snow<-as.matrix(o.snow_pre[,2:4])

# create function for prey and competition matrix
create.matrix2<-function(description)
{
  useme<-blankdata
  for(i in 1:nrow(useme))
  {
    if(!is.na(useme$v2013[i])) {useme$v2013[i]<-nrow(surveydata[surveydata$year==2013 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    if(!is.na(useme$v2014[i])) {useme$v2014[i]<-nrow(surveydata[surveydata$year==2014 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    if(!is.na(useme$v2015[i])) {useme$v2015[i]<-nrow(surveydata[surveydata$year==2015 &
                                                                  surveydata$id==useme$id[i] &
                                                                  surveydata$description %in% description,])}
    
  }
  
  return(as.matrix(useme[,2:4]))
  description<<-description
}

# prey
o.prey<-create.matrix2(c("squirrel","grouse","hare"))

# competition
o.comp<-create.matrix2("coyote")

# list observation covariate matrices
samplecovs<-list(o.prey, o.comp, o.snow)
samplecovs<-setNames(samplecovs, c("o.prey", "o.comp", "o.snow"))