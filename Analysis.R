####################### TRANSPLANTATION OF CORALS ######################################

#### Packages used:
library(moments) #for skewness
library(ggfortify) #autoplot for PCA
library(ggplot2) #for graph










#### Physical parameteres (Figure 3) ####
###upload data set
data.env <-read.csv('Physical_parameters.csv',header=TRUE)

###cleaning data
data.env2<-data.env[data.env$Chlorophyll.a>0,] # clean form negatives chl a
data.env3<-data.env2[data.env2$Turbidity>0,] # clean form null data Turbidity
data.env3$Date<-as.POSIXct(data.env3$Date,format="%Y/%m/%d%H:%M",tz='Asia/Taipei')
data.env3$Sites<-as.factor(data.env3$Sites)

#removing NA's (mostly from WMS)
data.env3 <-data.env3[complete.cases(data.env3),]

## skewness
skewness(data.env3$Turbidity)
skewness(data.env3$Chlorophyll.a)
skewness(log(data.env3$Turbidity+1)) # fixed for turbidity
skewness(log(data.env3$Chlorophyll.a+1)) # fixed for chlorophyll

## log
data.env3$log_turb<-log(data.env3$Turbidity+1)
data.env3$log_chl<-log(data.env3$Chlorophyll.a+1)

### PCA
var.pca<-data.env3[,c('Temperature','Oxygen_concentration','log_turb','log_chl','Salinity' )]


my.pca<-prcomp(var.pca, scale=T)

autoplot(my.pca, data.env3, colour='Sites', loadings=TRUE,loadings.label.vjust = -0.4,
         loadings.colour='black', loadings.label=TRUE, loadings.label.size=3,loadings.label.colour='black', 
         frame=TRUE, frame.colour ='Sites')+
  ggtitle(label = "") +
  scale_color_manual(values=c("Xiehe"="#CC0033", "Waimushan"="#0000FF","Heping Island"="#99CC00")) +
  scale_fill_manual(values=c("Xiehe"="#CC0033", "Waimushan"="#0000FF","Heping Island"="#99CC00"))




