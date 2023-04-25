####################### TRANSPLANTATION OF CORALS ######################################

#### Packages used:
library(moments) #skewness
library(ggfortify) #autoplot for PCA
library(ggplot2) #graphs
library(readxl) #read xlsx files
library(FSA) #Dunn multiple comparison after Kruskal-Wallis








#### Physical parameters (Figure 3) ####
###upload data set
data.env <-read.csv('Physical_parameters.csv',header=TRUE)

###cleaning data
data.env2<-data.env[data.env$Chlorophyll.a>0,] # clean form negatives chl a
data.env3<-data.env2[data.env2$Turbidity>0,] # clean form null data Turbidity
data.env3$Date<-as.POSIXct(data.env3$Date,format="%Y/%m/%d%H:%M",tz='Asia/Taipei')
data.env3$Sites<-as.factor(data.env3$Sites)

##removing NA's (mostly from WMS)
data.env3 <-data.env3[complete.cases(data.env3),]

##skewness
skewness(data.env3$Turbidity)
skewness(data.env3$Chlorophyll.a)
skewness(log(data.env3$Turbidity+1)) # fixed for turbidity
skewness(log(data.env3$Chlorophyll.a+1)) # fixed for chlorophyll

##log
data.env3$log_turb<-log(data.env3$Turbidity+1)
data.env3$log_chl<-log(data.env3$Chlorophyll.a+1)

###PCA
var.pca<-data.env3[,c('Temperature','Oxygen_concentration','log_turb','log_chl','Salinity' )]


my.pca<-prcomp(var.pca, scale=T)

autoplot(my.pca, data.env3, colour='Sites', loadings=TRUE,loadings.label.vjust = -0.4,
         loadings.colour='black', loadings.label=TRUE, loadings.label.size=3,loadings.label.colour='black', 
         frame=TRUE, frame.colour ='Sites')+
  ggtitle(label = "") +
  scale_color_manual(values=c("Xiehe"="#CC0033", "Waimushan"="#0000FF","Heping Island"="#99CC00")) +
  scale_fill_manual(values=c("Xiehe"="#CC0033", "Waimushan"="#0000FF","Heping Island"="#99CC00"))



#### Relative growth of corals per species (Figure 4) ####
###Acropora japonica
aj <- read_excel("Acropora_japonica_Relative_Growth.xlsx", col_names = TRUE)
f <-c(5)
aj[ , f] <- apply(aj[ , f], 2,            
                  function(x) as.numeric(as.character(x)))
aj$To <-factor(aj$To, levels = c("ToHeping.A", "ControlXiehe.A", "ToWaimushan.A","ToHeping.B", "ControlXiehe.B", "ToWaimushan.B")) #pa cambiarle el orden!
aj$Time <-factor(aj$Time,levels = c("1w.A","1w.B","1m.A","1m.B","2m.A","2m.B","3m.A","3m.B","6m.A","6m.B","9m.A","9m.B","12m.A","12m.B"))

##graph
 ggplot(aj, aes(x=Time, color=To))+
    geom_boxplot(aes(y=relative_growth), show.legend = FALSE) + 
    scale_colour_manual(name="",  values = c("ControlXiehe.A"="#CC0033", "ToWaimushan.A"="#0000FF","ToHeping.A"="#99CC00",
                                             "ControlXiehe.B"="#CC0033", "ToWaimushan.B"="#0000FF","ToHeping.B"="#99CC00")) +
    xlab("Time") +
    ylab("Relative growth (%)") +
    ggtitle ("Acropora japonica") +
    ylim(-100,221) +
    theme(axis.text.x = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1.5))) +
    theme(plot.title = element_text(size=30)) +
    theme(axis.title.x = element_text(size = rel(2))) +
    theme(axis.title.y = element_text(size = rel(2))) +
    theme(axis.line = element_line(color='black'),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
 

###Acropora cf. cerealis:
a2 <- read_excel("Acropora_cf_cerealis_Relative_Growth.xlsx", col_names = TRUE)
f <-c(4)
a2[ , f] <- apply(a2[ , f], 2,            
                  function(x) as.numeric(as.character(x)))
a2$To <-factor(a2$To, levels = c("ToHeping.A", "ControlXiehe.A", "ToWaimushan.A","ToHeping.B", "ControlXiehe.B", "ToWaimushan.B")) #pa cambiarle el orden!
a2$Time <-factor(a2$Time,levels = c("1w.A","1w.B","1m.A","1m.B","2m.A","2m.B","3m.A","3m.B","6m.A","6m.B","9m.A","9m.B","12m.A","12m.B"))

##graph
 ggplot(a2, aes(x=Time, color=To))+
    geom_boxplot(aes(y=relative_growth), show.legend = FALSE) + 
    scale_colour_manual(name="",  values = c("ControlXiehe.A"="#CC0033", "ToWaimushan.A"="#0000FF","ToHeping.A"="#99CC00",
                                             "ControlXiehe.B"="#CC0033", "ToWaimushan.B"="#0000FF","ToHeping.B"="#99CC00")) + #color verde claro:CCFFCC
    xlab("Time") +
    ylab("Relative growth (%)") +
    ggtitle ("Acropora cf. cerealis") +
    ylim(-100,221) +
    theme(axis.text.x = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1.5))) +
    theme(plot.title = element_text(size=30)) +
    theme(axis.title.x = element_text(size = rel(2))) +
    theme(axis.title.y = element_text(size = rel(2))) +
    theme(axis.line = element_line(color='black'),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
 

###Cyphastrea microphthalma
cm <- read_excel("Cyphastrea_microthpthalma_Relative_Growth.xlsx", col_names = TRUE)
f <-c(4)
cm[ , f] <- apply(cm[ , f], 2,            
                  function(x) as.numeric(as.character(x)))

cm$To <-factor(cm$To, levels = c("ToHeping.A", "ControlXiehe.A", "ToWaimushan.A","ToHeping.B", "ControlXiehe.B", "ToWaimushan.B")) #pa cambiarle el orden!
cm$Time <-factor(cm$Time,levels = c("1w.A","1w.B","1m.A","1m.B","2m.A","2m.B","3m.A","3m.B","6m.A","6m.B","9m.A","9m.B","12m.A","12m.B"))

##graph
ggplot(cm, aes(x=Time, color=To))+
    geom_boxplot(aes(y=relative_growth), show.legend = FALSE) + 
    scale_colour_manual(name="",  values = c("ControlXiehe.A"="#CC0033", "ToWaimushan.A"="#0000FF","ToHeping.A"="#99CC00",
                                             "ControlXiehe.B"="#CC0033", "ToWaimushan.B"="#0000FF","ToHeping.B"="#99CC00")) +
    xlab("Time") +
    ylab("Relative growth (%)") +
    ggtitle ("Cyphastrea microphthalma") +
    ylim(-100,221) +
    theme(axis.text.x = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1.5))) +
    theme(plot.title = element_text(size=30)) +
    theme(axis.title.x = element_text(size = rel(2))) +
    theme(axis.title.y = element_text(size = rel(2))) +
    theme(axis.line = element_line(color='black'),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())


###Favites pentagona:
fp <- read_excel("Favites_pentagona_Relative_Growth.xlsx", col_names = TRUE)
f <-c(4)
fp[ , f] <- apply(fp[ , f], 2,            
                  function(x) as.numeric(as.character(x)))
fp$To <-factor(fp$To, levels = c("ToHeping.A", "ControlXiehe.A", "ToWaimushan.A","ToHeping.B", "ControlXiehe.B", "ToWaimushan.B")) #pa cambiarle el orden!
fp$Time <-factor(fp$Time,levels = c("1w.A","1w.B","1m.A","1m.B","2m.A","2m.B","3m.A","3m.B","6m.A","6m.B","9m.A","9m.B","12m.A","12m.B"))

##graph
ggplot(fp, aes(x=Time, color=To))+
    geom_boxplot(aes(y=relative_growth), show.legend = FALSE) + 
    scale_colour_manual(name="",  values = c("ControlXiehe.A"="#CC0033", "ToWaimushan.A"="#0000FF","ToHeping.A"="#99CC00",
                                             "ControlXiehe.B"="#CC0033", "ToWaimushan.B"="#0000FF","ToHeping.B"="#99CC00")) +
    xlab("Time") +
    ylab("Relative growth (%)") +
    ggtitle ("Favites pentagona") +
    ylim(-100,221) +
    theme(axis.text.x = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1.5))) +
    theme(plot.title = element_text(size=30)) +
    theme(axis.title.x = element_text(size = rel(2))) +
    theme(axis.title.y = element_text(size = rel(2))) +
    theme(axis.line = element_line(color='black'),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())


###Psammocora albopicta:
pa <- read_excel("Psammocora_albopicta_Relative_Growth.xlsx", col_names = TRUE)
f <-c(4)
pa[ , f] <- apply(pa[ , f], 2,            
                  function(x) as.numeric(as.character(x)))
pa$To <-factor(pa$To, levels = c("ToHeping.A", "ControlXiehe.A", "ToWaimushan.A","ToHeping.B", "ControlXiehe.B", "ToWaimushan.B")) #pa cambiarle el orden!
pa$Time <-factor(pa$Time,levels = c("1w.A","1w.B","1m.A","1m.B","2m.A","2m.B","3m.A","3m.B","6m.A","6m.B","9m.A","9m.B","12m.A","12m.B"))

##graph
ggplot(pa, aes(x=Time, color=To))+
    geom_boxplot(aes(y=relative_growth)) + 
    scale_colour_manual(name="",  values = c("ControlXiehe.A"="#CC0033", "ToWaimushan.A"="#0000FF","ToHeping.A"="#99CC00",
                                             "ControlXiehe.B"="#CC0033", "ToWaimushan.B"="#0000FF","ToHeping.B"="#99CC00")) +
    xlab("Time") +
    ylab("Relative growth (%)") +
    ggtitle ("Psammocora albopicta") +
    ylim(-100,221) +
    theme(axis.text.x = element_text(size = rel(1.5))) +
    theme(axis.text.y = element_text(size = rel(1.5))) +
    theme(legend.key.size = unit(1, "cm")) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.title = element_text(size=15)) +
    theme(legend.key = element_rect(fill = "white", colour = "white")) + #change legend's box
    theme(plot.title = element_text(size=30)) +
    theme(axis.title.x = element_text(size = rel(2))) +
    theme(axis.title.y = element_text(size = rel(2))) +
    theme(axis.line = element_line(color='black'),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())

#### STATS ####
gr <- read_excel("Corals_Relative_Growth.xlsx", col_names = TRUE)
f <-c(5:11) 
gr[ , f] <- apply(gr[ , f], 2,            
                  function(x) as.numeric(as.character(x)))
#### Welsh t-test of relative growth between frame vs. seabed (Table S2) ####
t.test(RG_1w ~ Group, data = gr) #t = 0.13836, df = 205.41, p-value = 0.8901
t.test(RG_1m ~ Group, data = gr) #t = -0.67304, df = 198.38, p-value = 0.5017
t.test(RG_2m ~ Group, data = gr) #t = 0.9999, df = 177.56, p-value = 0.3187
t.test(RG_3m ~ Group, data = gr) #t = 0.13, df = 188.48, p-value = 0.8967
t.test(RG_6m ~ Group, data = gr) #t = -1.9905, df = 149.53, p-value = 0.04836
t.test(RG_9m ~ Group, data = gr) #t = -0.78141, df = 133.77, p-value = 0.4359
t.test(RG_12m ~ Group, data = gr) #t = 0.2076, df = 112.06, p-value = 0.8359

#### ANOVA and Mann-Whitney-Wilcoxon tests of relative growth between SITES (Table S3) ####
###1w:
gr.1w <- gr[ ,c("Group", "Species", "To", "RG_1w")]
gr.1w <- gr.1w[complete.cases(gr.1w),]

#check normality:
qqnorm(gr.1w$RG_1w)
qqline(gr.1w$RG_1w)
fligner.test(gr.1w$RG_1w~gr.1w$To) #chi-squared = 3.2954, df = 2, p-value = 0.1925

#ANOVA:
week <- aov(RG_1w~To, data = gr.1w)
summary(week) #p=0.108

###1m:
gr.1m <- gr[ ,c("Group", "Species", "To", "RG_1m")]
gr.1m <- gr.1m[complete.cases(gr.1m),]

#check normality:
qqnorm(gr.1m$RG_1m)
qqline(gr.1m$RG_1m)
fligner.test(gr.1m$RG_1m~gr.1m$To) #chi-squared = 9.7616, df = 2, p-value = 0.007591

#NonPara:
kruskal.test(RG_1m~To, data = gr.1m) #chi-squared = 8.9994, df = 2, p-value = 0.01111
dunnTest(RG_1m~To, data = gr.1m, method = "bonferroni")

###2m:
gr.2m <- gr[ ,c("Group", "Species", "To", "RG_2m")]
gr.2m <- gr.2m[complete.cases(gr.2m),]

#check normality:
qqnorm(gr.2m$RG_2m)
qqline(gr.2m$RG_2m)
fligner.test(gr.2m$RG_2m~gr.2m$To) #chi-squared = 4.5025, df = 2, p-value = 0.1053

#ANOVA:
month2 <- aov(RG_2m~To, data = gr.2m)
summary(month2) #p=0.146

###3m:
gr.3m <- gr[ ,c("Group", "Species", "To", "RG_3m")]
gr.3m <- gr.3m[complete.cases(gr.3m),]

#check normality:
qqnorm(gr.3m$RG_3m)
qqline(gr.3m$RG_3m)
fligner.test(gr.3m$RG_3m~gr.3m$To) #chi-squared = 0.15898, df = 2, p-value = 0.9236

#ANOVA:
month3 <- aov(RG_3m~To, data = gr.3m)
summary(month3) #p=0.00992 **
TukeyHSD(month3)

###6m:
gr.6m <- gr[ ,c("Group", "Species", "To", "RG_6m")]
gr.6m <- gr.6m[complete.cases(gr.6m),]

#check normality:
qqnorm(gr.6m$RG_6m)
qqline(gr.6m$RG_6m)
fligner.test(gr.6m$RG_6m~gr.6m$To) #chi-squared = 9.3472, df = 2, p-value = 0.009338

#NonPara:
kruskal.test(RG_6m~To, data = gr.6m) #chi-squared = 16.684, df = 2, p-value = 0.0002383
dunnTest(RG_6m~To, data = gr.6m, method = "bonferroni")

###9m:
gr.9m <- gr[ ,c("Group", "Species", "To", "RG_9m")]
gr.9m <- gr.9m[complete.cases(gr.9m),]

#check normality:
qqnorm(gr.9m$RG_9m)
qqline(gr.9m$RG_9m)
fligner.test(gr.9m$RG_9m~gr.9m$To) #chi-squared = 4.522, df = 2, p-value = 0.1042

#ANOVA:
month9 <- aov(RG_9m~To, data = gr.9m)
summary(month9) #p=0.0246 *
TukeyHSD(month9)

###12m:
gr.12m <- gr[ ,c("Group", "Species", "To", "RG_12m")]
gr.12m <- gr.12m[complete.cases(gr.12m),]

#check normality:
qqnorm(gr.12m$RG_12m)
qqline(gr.12m$RG_12m)
fligner.test(gr.12m$RG_12m~gr.12m$To) #chi-squared = 1.1995, df = 2, p-value = 0.5489

#ANOVA:
month12 <- aov(RG_12m~To, data = gr.12m)
summary(month12) #p=0.129












