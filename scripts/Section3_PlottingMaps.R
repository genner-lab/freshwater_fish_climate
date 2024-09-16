#setwd

library(rfishbase)
library(dplyr)
library(ggplot2)
library(viridis)
library(lme4)
library(effects)

#Section1-----------Mapmaking---------------------------------------------------

#Read in data

df_models_max <- read.csv("df_models_max_03May2024.csv")
df_models_max <- subset(df_models_max, select = -c(X))

world <- map_data("world")
latitude_df_max <- df_models_max$Latitude
longitude_df_max <- df_models_max$Longitude

hex_map <- ggplot(df_models_max, aes(x=longitude_df_max, y=latitude_df_max))+
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", colour="grey70", linewidth=0.2, alpha=0.3) +
  geom_hex(bins=75) +
  theme_void() +
  #ylim(33, 71) +
  #xlim(-13,34)+
  scale_fill_viridis(
    breaks = c(1,10,50,250,500,1000,2000,3000,5000),
    trans = 'log10',
    name="Number of Timeseries", 
    guide = guide_legend(keyheight = unit(2.0, units = "mm"), keywidth=unit(10, units = "mm"), label.position = 'bottom', title.position = 'top', nrow=1)
  )+theme(legend.position = "bottom", legend.text = element_text(size = 14), legend.title = element_text(size = 16))
hex_map

ggsave(hex_map, file="map.pdf",width = 40, height = 20, unit = "cm", dpi = 300)

#Section2-------------------Obtaining and plotting Order data-------------------

order_data <- load_taxa(server=getOption("fishbase"))#get taxonomic data from fishbase
order_data <- order_data[,c(2,6)]
order_data <- left_join(df_models_max, order_data, by=c("Species"))
order_data <- order_data[,c(2,15)]
order_data <- unique(order_data)
rownames(order_data) = NULL

#Need to manually complete some NAs
order_data["45", "Order"] <- "Cypriniformes"
order_data["272", "Order"] <- "Perciformes"
order_data["427", "Order"] <- "Characiformes"
order_data["465", "Order"] <- "Cypriniformes"
order_data["466", "Order"] <- "Cypriniformes"
order_data["540", "Order"] <- "Characiformes"
order_data["546", "Order"] <- "Characiformes"
order_data["555", "Order"] <- "Characiformes"
order_data["594", "Order"] <- "Characiformes"
order_data["599", "Order"] <- "Siluriformes"

#simplifying the Perciformes to Order level

order_data["Order"][order_data["Order"] == "Perciformes/Cottoidei"] <- "Perciformes"
order_data["Order"][order_data["Order"] == "Perciformes/Percoidei"] <- "Perciformes"
order_data["Order"][order_data["Order"] == "Perciformes/Gasterosteoidei"] <- "Perciformes"
order_data["Order"][order_data["Order"] == "Perciformes/Scorpaenoidei"] <- "Perciformes"

#creating a barplot showing frequency of timeseries in dataset by order

#summarise dataframe_complete to provide a count of species and timeseries, then add the order information

order_plot_data <- df_models_max %>% 
  group_by(Species, TimeSeriesID) %>% 
  summarise(n = n())

order_plot_data <- left_join(order_plot_data, order_data, by = c("Species"))

order_plot_data2 <- order_plot_data %>% 
  group_by(Order) %>% 
  summarise(n = n())

order_plot_data2 <- filter(order_plot_data2, n > 500)

orderbarplot <- ggplot(data = order_plot_data2, aes(x = reorder(Order, -n), y=n))+ 
  geom_bar(stat = "identity", width=0.5,fill='red4') +
  coord_flip() + theme_classic() +
  xlab("Taxonomic Order")+ylab("Number of population abundance timeseries")+
  scale_y_continuous(limits = c(0,50000), expand = c(0, 0)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))
orderbarplot

ggsave(orderbarplot, file="barplot.pdf",width = 40, height = 20, unit = "cm", dpi = 300)

#Section3-----------Temperature change plots and analyses-----------------------

Tclim_input <- read.csv(file="Tclim_input.csv")
Tclim_input <- subset(Tclim_input, select = -c(1) )
temp_data <- Tclim_input
temp_data$Location <- paste(temp_data$x,temp_data$y)

#linear mixed models
mixed.lmer_avg <- lmer(Average.temperature~Year + (1|Location),data=temp_data)
summary(mixed.lmer_avg)

mixed.lmer_max <- lmer(Maximum.temperature~Year + (1|Location),data=temp_data)
summary(mixed.lmer_max)

#making plots of temperature rise
effect_avg.temp<-effect("Year",mixed.lmer_avg)
effect_avg.temp<-as.data.frame(effect_avg.temp)

effect_max.temp<-effect("Year",mixed.lmer_max)
effect_max.temp<-as.data.frame(effect_max.temp)


#colour palette
cbPalette <- c("#E69F00", "#56B4E9", "#0072B2", "#D55E00")#orange,light blue, dark blue, red

effect_theme <- theme(
  axis.title.x = element_text(size = 14, vjust=0.1, hjust=0.5),
  axis.title.y = element_text(size = 14),
  axis.text = element_text(size = 13, colour='black'),
  axis.text.x = element_text(vjust = 0.5),
  plot.title = element_text(lineheight=.8, face="bold", size = 10, hjust=0.5, vjust=0.2),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 13),
  legend.position.inside = c(0.9,0.1),
  legend.justification = c("right","bottom"))

plot_max.temp1 <- ggplot(data=effect_max.temp, aes(x=Year,y=fit)) + 
  geom_line(colour="#D55E00") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#E69F00", fill="#E69F00") +
  #geom_segment(aes(x=0,xend=1,y=0.5,yend=0.5),linetype=3, alpha=1, colour="#969696") +
  #ggtitle("The trend in maximum temperature") +
  scale_y_continuous(name="Tmax (°C)",limits=c(22,24)) +
  scale_x_continuous(name="Year",limits=c(1958,2020),breaks = c(1960,1970,1980,1990,2000,2010,2020))+
  theme_bw() +
  effect_theme
plot_max.temp1

#save as 5x5 inches

plot_avg.temp <- ggplot(data=effect_avg.temp, aes(x=Year,y=fit)) + 
  geom_line(colour="#D55E00") +
  geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.3, colour="#E69F00", fill="#E69F00") +
  #geom_segment(aes(x=0,xend=1,y=0.5,yend=0.5),linetype=3, alpha=1, colour="#969696") +
  #ggtitle("Average temperature") +
  scale_y_continuous(name="Tavg (°C)",limits=c(13,15.2)) +
  scale_x_continuous(name="Year",limits=c(1958,2020),breaks = c(1960,1970,1980,1990,2000,2010,2020))+
  theme_bw() +
  effect_theme
plot_avg.temp

#save as 5x5 inches

#end of analysis
