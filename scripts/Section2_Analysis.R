#setwd

#load packages
library(dplyr)
library(lme4)
library(lmerTest)
library(effects)
library(ggplot2)
library(scales)
library(ggpubr)
library(standardize)
library(egg)
library(sjPlot)
library(forcats)

#Section1-----------Prepare data------------------------------------------------

#Read in data

df_models_avg <- read.csv("df_models_avg_23May2024.csv")
df_models_avg = subset(df_models_avg, select = -c(X))

df_models_max <- read.csv("df_models_max_23May2024.csv")
df_models_max = subset(df_models_max, select = -c(X))

names(df_models_avg)[14]<-'Position_in_range'
names(df_models_avg)[15]<-'Binary_change'
names(df_models_max)[14]<-'Position_in_range'
names(df_models_max)[15]<-'Binary_change'

#One final filter to make avg and max dataframes match

df_models_avg$Binary_change <- as.factor(df_models_avg$Binary_change)
df_models_max$Binary_change <- as.factor(df_models_max$Binary_change)
df_models_both <- left_join(df_models_max, df_models_avg, by=c("TimeSeriesID", "Species", "Year", "Duration","Latitude","Longitude"))
df_models_avg <-  subset(df_models_both[,c(1:2,16:17,5:13,23:24)])
names(df_models_avg)[3]<-'Slope_value'
names(df_models_avg)[4]<-'Cor_value'
names(df_models_avg)[9]<-'BioRealm'
names(df_models_avg)[10]<-'Elevation'
names(df_models_avg)[11]<-'Troph'
names(df_models_avg)[12]<-'Length'
names(df_models_avg)[13]<-'Range'
names(df_models_avg)[14]<-'Position_in_range'
names(df_models_avg)[15]<-'Binary_change'

#clean house
rm(df_models_both)

#some summary data

n_distinct(df_models_avg$TimeSeriesID)
n_distinct(df_models_avg$Species)
n_distinct(df_models_avg$Latitude, df_models_avg$Longitude)

n_distinct(df_models_max$TimeSeriesID)
n_distinct(df_models_max$Species)
n_distinct(df_models_max$Latitude, df_models_max$Longitude)

#Subset data base into three sets: timeseries of ten years or longer (all data), 20 years or longer, 30 years or longer

df_a10 <- df_models_avg
df_a20 <- subset(df_models_avg,df_models_avg$Duration>19)
df_a30 <- subset(df_models_avg,df_models_avg$Duration>29)

df_m10 <- df_models_max
df_m20 <- subset(df_models_max, df_models_max$Duration>19)
df_m30 <- subset(df_models_max, df_models_max$Duration>29)

#Section2-----------Models testing for position in range------------------------

#modelling - naming convention is as follows, modelxy, where x = the model formula (e.g., Binary_change~Position_in_range), and y = the subset applied to (10/20/30), so model1 applied to 20 = model120
model110_avg <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_a10)
summary(model110_avg)
model120_avg <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_a20)
summary(model120_avg)
model130_avg <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_a30)
summary(model130_avg)

model110_max <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_m10)
summary(model110_max)
model120_max <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_m20)
summary(model120_max)
model130_max <- glmer(Binary_change~Position_in_range + (1 |Species), family=binomial("logit"), data=df_m30)
summary(model130_max)

#Section3-----------Plots showing effect of position in range on responses------

#create dataframes of effects from models

#effect for standard models using average temperature
effect_model110avg <- Effect("Position_in_range",model110_avg)
effect_model110avg <- as.data.frame(effect_model110avg)
effect_model120avg <- Effect("Position_in_range",model120_avg)
effect_model120avg <- as.data.frame(effect_model120avg)
effect_model130avg <- effect("Position_in_range",model130_avg)
effect_model130avg <- as.data.frame(effect_model130avg)

#effects for standard models using maximum temperature data
effect_model110max <- effect("Position_in_range",model110_max)
effect_model110max <- as.data.frame(effect_model110max)
effect_model120max <- effect("Position_in_range",model120_max)
effect_model120max <- as.data.frame(effect_model120max)
effect_model130max <- effect("Position_in_range",model130_max)
effect_model130max <- as.data.frame(effect_model130max)

effect_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 10, vjust=0.1),
  axis.title.y = element_text(size = 10),
  axis.text = element_text(size = 8, colour='black'),
  axis.text.x = element_text(vjust = 0),
  plot.title = element_text(lineheight=1, size = 11, hjust=0.5, vjust=0.000001),
  panel.background = element_rect(fill = "white",colour = "white"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  legend.justification = c("right", "bottom"),legend.position = c(.90,.10),legend.text = element_text(size = 10))

#plots for standard models using average temperature data

plotmodel110_avg <- ggplot(data=effect_model110avg, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("10+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel110_avg

plotmodel120_avg <- ggplot(data=effect_model120avg, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("20+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel120_avg

plotmodel130_avg <- ggplot(data=effect_model130avg, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("30+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel130_avg

cols <- c("30"="chocolate2","20"="darkgrey","10"="#0072B2")

plot_avg_effects <- ggplot(data=effect_model130avg, aes(x=Position_in_range,y=fit)) + 
  geom_line(aes(colour="30")) +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="chocolate2", fill="chocolate2") +
  scale_y_continuous(name="Abundance Change (Probability of positive trend)",breaks=c(0.3,0.4,0.5,0.6,0.7), limits=c(0.25,0.75),labels=c("0.3","0.4","0.5","0.6","0.7")) +
  scale_x_continuous(name="Position in Range (0 = equatorward limit, 1 = poleward limit)",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  geom_line(data = effect_model110avg,linetype=4,aes(colour="10"))+geom_ribbon(data = effect_model110avg,aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#0072B2", fill="#0072B2")+
  geom_line(data = effect_model120avg,linetype=5,aes(colour="20"))+geom_ribbon(data = effect_model120avg,aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="darkgrey", fill="darkgrey")+
  effect_theme +
  scale_color_manual(values=cols,labels=c("10+ Years","20+ Years","30+ Years"),name="Length of timeseries") + theme(axis.title.x = element_text(hjust=-0.1))
plot_avg_effects

#plots for standard models using maximum temperature data

plotmodel110_max <- ggplot(data=effect_model110max, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("10+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel110_max

plotmodel120_max <- ggplot(data=effect_model120max, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("20+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel120_max

plotmodel130_max <- ggplot(data=effect_model130max, aes(x=Position_in_range,y=fit)) + 
  geom_line(colour="#0072B2") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#56B4E9", fill="#56B4E9") +
  ggtitle("30+ Years") +
  scale_y_continuous(name="Abundance Change",breaks=c(0.25,0.5,0.75), limits=c(0.25,0.75),labels=c("-1","0","1")) +
  scale_x_continuous(name="Position in Range",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme
plotmodel130_max

cols <- c("30"="chocolate2","20"="darkgrey","10"="#0072B2")

plot_max_effects <- ggplot(data=effect_model130max, aes(x=Position_in_range,y=fit)) + 
  geom_line(aes(colour="30")) +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="chocolate2", fill="chocolate2") +
  scale_y_continuous(name="Abundance Change (Probability of positive trend)",breaks=c(0.3,0.4,0.5,0.6,0.7), limits=c(0.25,0.75),labels=c("0.3","0.4","0.5","0.6","0.7")) +
  scale_x_continuous(name="Position in Range (0 = equatorward limit, 1 = poleward limit)",breaks=c(0.0,0.25,0.5,0.75,1.0), limits=c(0,1)) +
  theme_bw() +
  effect_theme +
  geom_line(data = effect_model120max,linetype=5,aes(colour="20"))+geom_ribbon(data = effect_model120max,aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="darkgrey", fill="darkgrey")+
  geom_line(data = effect_model110max,linetype=4,aes(colour="10"))+geom_ribbon(data = effect_model110max,aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#0072B2", fill="#0072B2")+
  scale_color_manual(values=cols,labels=c("10+ Years","20+ Years","30+ Years"),name="Length of timeseries") + theme(axis.title.x = element_text(hjust=-0.1))
plot_max_effects

#plots for frequency distributions
  
plot_avgdist_110<-ggplot(df_a10, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_avgdist_110

plot_avgdist_120<-ggplot(df_a20, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_avgdist_120

plot_avgdist_130<-ggplot(df_a30, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_avgdist_130

plot_maxdist_110<-ggplot(df_m10, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_maxdist_110

plot_maxdist_120<-ggplot(df_m20, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_maxdist_120

plot_maxdist_130<-ggplot(df_m30, aes(Position_in_range,fill = fct_rev(Binary_change), alpha=0.8)) +
  geom_density(adjust = 1/5,linewidth =0.15, alpha=.5) +
  scale_fill_manual(values=c("orange", "steelblue2")) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_classic() + theme(legend.position="none")
plot_maxdist_130

Avg_Grid<-grid.arrange(plot_avg_effects, arrangeGrob(plot_avgdist_110, plot_avgdist_120, plot_avgdist_130, ncol=1), 
                       ncol=2, widths=c(1,1.2))
  
Max_Grid<-grid.arrange(plot_max_effects, arrangeGrob(plot_maxdist_110, plot_maxdist_120, plot_maxdist_130, ncol=1), 
                    ncol=2, widths=c(1,1.2))

#export as 9.5 x 4.75 inches

#Section4-----------Tests of elevation and traits on responses------------------

###Testing for an elevation effect

df_a10$Elevation_scaled <- scale(df_a10$Elevation)
df_a20$Elevation_scaled <- scale(df_a20$Elevation)
df_a30$Elevation_scaled <- scale(df_a30$Elevation)
df_m10$Elevation_scaled <- scale(df_m10$Elevation)
df_m20$Elevation_scaled <- scale(df_m20$Elevation)
df_m30$Elevation_scaled <- scale(df_m30$Elevation)

model210_avg <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_a10)
summary(model210_avg)
model220_avg <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_a20)
summary(model220_avg)
model230_avg <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_a30)
summary(model230_avg)
model210_max <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_m10)
summary(model210_max)
model220_max <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_m20)
summary(model220_max)
model230_max <- glmer(Binary_change~Position_in_range + Elevation_scaled + (1|Species),family=binomial("logit"),data=df_m30)
summary(model230_max)

###Testing for a body size effect

df_a10$Length_scaled <- scale(log10(df_a10$Length))
df_a20$Length_scaled <- scale(log10(df_a20$Length))
df_a30$Length_scaled <- scale(log10(df_a30$Length))
df_m10$Length_scaled <- scale(log10(df_m10$Length))
df_m20$Length_scaled <- scale(log10(df_m20$Length))
df_m30$Length_scaled <- scale(log10(df_m30$Length))

model310_avg <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_a10)
summary(model310_avg)
model320_avg <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_a20)
summary(model320_avg)
model330_avg <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_a30)
summary(model330_avg)
model310_max <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_m10)
summary(model310_max)
model320_max <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_m20)
summary(model320_max)
model330_max <- glmer(Binary_change~Position_in_range*Length_scaled + (1|Species),family=binomial("logit"),data=df_m30)
summary(model330_max)

###Testing for a trophic level effect

df_a10$Troph_scaled <- scale(df_a10$Troph)
df_a20$Troph_scaled <- scale(df_a20$Troph)
df_a30$Troph_scaled <- scale(df_a30$Troph)
df_m10$Troph_scaled <- scale(df_m10$Troph)
df_m20$Troph_scaled <- scale(df_m20$Troph)
df_m30$Troph_scaled <- scale(df_m30$Troph)

model410_avg <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_a10)
summary(model410_avg)
model420_avg <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_a20)
summary(model420_avg)
model430_avg <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_a30)
summary(model430_avg)
model410_max <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_m10)
summary(model410_max)
model420_max <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_m20)
summary(model420_max)
model430_max <- glmer(Binary_change~Position_in_range*Troph_scaled + (1|Species),family=binomial("logit"),data=df_m30)
summary(model430_max)

###Testing for a latitudinal range effect

df_a10$Range_scaled <- scale(log10(df_a10$Range))
df_a20$Range_scaled <- scale(log10(df_a20$Range))
df_a30$Range_scaled <- scale(log10(df_a30$Range))
df_m10$Range_scaled <- scale(log10(df_m10$Range))
df_m20$Range_scaled <- scale(log10(df_m20$Range))
df_m30$Range_scaled <- scale(log10(df_m30$Range))

model510_avg <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_a10)
summary(model510_avg)
model520_avg <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_a20)
summary(model520_avg)
model530_avg <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_a30)
summary(model530_avg)
model510_max <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_m10)
summary(model510_max)
model520_max <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_m20)
summary(model520_max)
model530_max <- glmer(Binary_change~Position_in_range*Range_scaled + (1|Species),family=binomial("logit"),data=df_m30)
summary(model530_max)

#Section5-----------Plots of elevation and traits on responses------------------

effect_theme <- theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 10, vjust=0.1),
  axis.title.y = element_text(size = 10),
  axis.text = element_text(size = 8, colour='black'),
  axis.text.x = element_text(vjust = 0),
  plot.title = element_text(lineheight=1, size = 11, hjust=0.5, vjust=0.000001),
  panel.background = element_rect(fill = "white",colour = "white"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 6),legend.title=element_blank())

#save below as 3.5x3 inches portrait

elevation.plot_avg <- plot_model(model210_avg,type = "pred", terms = c("Position_in_range [all]", "Elevation_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
elevation.plot_avg <- elevation.plot_avg + scale_color_manual(values=c("#0072B2","darkgrey","chocolate") , labels=c("Low Elevation","Medium Elevation","High Elevation"))
elevation.plot_avg <- elevation.plot_avg + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
elevation.plot_avg <- elevation.plot_avg + scale_y_continuous(limits=c(0.42,0.58),breaks = c(0.42, 0.46, 0.50, 0.54, 0.58),labels=c(0.42, 0.46, 0.50, 0.54, 0.58))
elevation.plot_avg <- elevation.plot_avg + effect_theme
elevation.plot_avg

length.plot_avg <- plot_model(model310_avg,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Length_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
length.plot_avg <- length.plot_avg + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Small body length","Medium body length","Large body length"))
length.plot_avg <- length.plot_avg + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
length.plot_avg <- length.plot_avg + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
length.plot_avg <- length.plot_avg + effect_theme
length.plot_avg

trophic.plot_avg <- plot_model(model410_avg,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Trophic_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
trophic.plot_avg <- trophic.plot_avg + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Low trophic level","Medium trophic level","High trophic level"))
trophic.plot_avg <- trophic.plot_avg + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
trophic.plot_avg <- trophic.plot_avg + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
trophic.plot_avg <- trophic.plot_avg + effect_theme
trophic.plot_avg

rangesize.plot_avg <- plot_model(model510_avg,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Latitudinal_range_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
rangesize.plot_avg <- rangesize.plot_avg + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Small range","Medium range","Large range"))
rangesize.plot_avg <- rangesize.plot_avg + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
rangesize.plot_avg <- rangesize.plot_avg + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
rangesize.plot_avg <- rangesize.plot_avg + effect_theme
rangesize.plot_avg

elevation.plot_max <- plot_model(model210_max,type = "pred", terms = c("Position_in_range [all]", "Elevation_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
elevation.plot_max <- elevation.plot_max + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate") , labels=c("Low Elevation","Medium Elevation","High Elevation"))
elevation.plot_max <- elevation.plot_max + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
elevation.plot_max <- elevation.plot_max + scale_y_continuous(limits=c(0.42,0.58),breaks = c(0.42, 0.46, 0.50, 0.54, 0.58),labels=c(0.42, 0.46, 0.50, 0.54, 0.58))
elevation.plot_max <- elevation.plot_max + effect_theme
elevation.plot_max

length.plot_max <- plot_model(model310_max,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Length_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
length.plot_max <- length.plot_max + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Small body length","Medium body length","Large body length"))
length.plot_max <- length.plot_max + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
length.plot_max <- length.plot_max + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
length.plot_max <- length.plot_max + effect_theme
length.plot_max

trophic.plot_max <- plot_model(model410_max,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Trophic_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
trophic.plot_max <- trophic.plot_max + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Low trophic level","Medium trophic level","High trophic level"))
trophic.plot_max <- trophic.plot_max + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
trophic.plot_max <- trophic.plot_max + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
trophic.plot_max <- trophic.plot_max + effect_theme
trophic.plot_max

rangesize.plot_max <- plot_model(model510_max,type = "int", mdrt.values = "quart", terms = c("Position_in_range [all]", "Latitudinal_range_scaled"),title="")+ylab("")+xlab("") + scale_fill_manual(values=c("#0072B2","darkgrey","chocolate"))
rangesize.plot_max <- rangesize.plot_max + theme_classic() + scale_color_manual(values=c("#0072B2","darkgrey","chocolate"),labels=c("Small range","Medium range","Large range"))
rangesize.plot_max <- rangesize.plot_max + theme(legend.justification = c("right", "bottom"),legend.position = c(.95,.05),legend.text = element_text(size = 12),axis.text = element_text(size=12))
rangesize.plot_max <- rangesize.plot_max + scale_y_continuous(limits=c(0.40,0.60),breaks = c(0.40, 0.45, 0.50, 0.55, 0.60),labels=c(0.40, 0.45, 0.50, 0.55, 0.60))
rangesize.plot_max <- rangesize.plot_max + effect_theme
rangesize.plot_max

#Section6-------Tests of key hypotheses using slopes of standardised abundance x temperature------

model110_slope_avg <- lmer(Slope_value~Position_in_range + (1 |Species), data=df_a10)
summary(model110_slope_avg)
model110_slope_max <- lmer(Slope_value~Position_in_range + (1 |Species), data=df_m10)
summary(model110_slope_max)

model210_slope_avg <- lmer(Slope_value~Position_in_range + Elevation_scaled + (1|Species),data=df_a10)
summary(model210_slope_avg)
model210_slope_max <- lmer(Slope_value~Position_in_range + Elevation_scaled + (1|Species),data=df_m10)
summary(model210_slope_max)

model310_slope_avg <- lmer(Slope_value~Position_in_range * Length_scaled + (1|Species),data=df_a10)
summary(model310_slope_avg)
model310_slope_max <- lmer(Slope_value~Position_in_range * Length_scaled + (1|Species),data=df_m10)
summary(model310_slope_max)

model410_slope_avg <- lmer(Slope_value~Position_in_range * Troph_scaled + (1|Species),data=df_a10)
summary(model410_slope_avg)
model410_slope_max <- lmer(Slope_value~Position_in_range * Troph_scaled + (1|Species),data=df_m10)
summary(model410_slope_max)

model510_slope_avg <- lmer(Slope_value~Position_in_range * Range_scaled + (1|Species),data=df_a10)
summary(model510_slope_avg)
model510_slope_max <- lmer(Slope_value~Position_in_range * Range_scaled + (1|Species),data=df_m10)
summary(model510_slope_max)

#end of analysis
