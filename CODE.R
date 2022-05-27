library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
#datvis: https://ourcodingclub.github.io/tutorials/dataviz-beautification/

datWide <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/Data_Extract_From_World_Development_Indicators/8cc86dd2-5b26-46aa-acb9-c83e00c3a923_Data.csv")
colnames(datWide)<-gsub("X","",colnames(datWide))
#View(datWide)

datLong <- datWide %>% gather(year, value, -c(Series.Name, Country))
#View(datLong)

ggplot(datLong, aes(year, as.numeric(value), color=Series.Name)) +
  geom_point() +
  geom_line(size = 1.5, aes(group=Series.Name)) +
  labs(x= "Year", y = "Metric Tons", title = "Wild Catch vs Aquaculture in the United States 1971-2018") +
  scale_color_hue(labels = c("Aquaculture", "Wild Catch", "Total Fishery Production")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

datLong1 <- read.csv("/Users/rishigurjar/datLong1.csv")

ggplot(datLong1, aes(x=year, y=percentage, fill=Series.Name)) + 
  geom_area(alpha=0.6 , size=1, colour="black")+
  scale_fill_viridis(discrete = T) +
  labs(x= "Year", y = "Percentage (Volume measured in metric tons)", title = "Percentage Seafood Production Share in the US 1971-2018")

#-------
importsLong <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/fosslandings.csv")
#View(importsLong)
#head(importsLong)

ggplot(importsLong, aes(Year, as.numeric(as.character(MetricTons)), group = 1)) +
  geom_point() +
  geom_line(size = 1.5) +
  labs(x= "Year", y = "Metric Tons", title = "US Commercial Seafood Landings 1950-2020")
#------
consumpLong <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/percapconsumption.csv")

ggplot(consumpLong) +
  geom_path(aes(x = Year, y = Val, color = Type)) +
  facet_grid(Type ~ ., scales = "free_y", 
             labeller = label_parsed,
             switch = "y") +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = rel(1))) +
  guides(color = "none") +
  labs(title = "US Population Growth and Annual Per Capita Consumption of Seafood Products 1910-2019")

#---------
aquaCap <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/oced_aquacultureprodmod1.csv")
#View(aquaCap)

ggplot(aquaCap, aes(x=TIME, y=PerCapita, color=LOCATION)) + 
  geom_line(size = 1.5)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Per Capita Aaquaculture Production Volume (1995-2019)") + 
  scale_color_hue(labels = c("China", "Indonesia", "Thailand", "United States", "Vietnam"))

#---------
discards <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/discards.csv")

ggplot(discards, aes(x=Year, y=Discards, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish discards by country 1950-2018",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")


ggplot(discards, aes(x=Year, y=PerCap, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish discards per capita by country 1960-2018",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")

#---------
captureprod <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/API_ER.FSH.CAPT.MT_DS2_en_csv_v2_3692271.csv")

#---------
imports <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/usimps.csv")


ggplot(imports, aes(x=Year, y=Value)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="US Seafood Imports 1976-2019")
#---------
toppop <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/toppop.csv")
#head(toppop)
#toppop
topone <- filter(toppop, name != 'Net')

ggplot(topone, aes(x=Time, y=Per.Capita, linetype=name, color=country)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Per Capita Volume (mt)",
        title ="Top 6 Population Countries Aquaculture vs Capture Fishery Per Capita")

topnet <- filter(toppop, name == 'Net')
#view(topnet)

ggplot(topnet, aes(x=Time, y=NetVal, color=country)) + 
  geom_point()+
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Net Volume (Aquaculture minus Capture Fishery) (mt)",
        title ="Top 6 Population Countries Net (Aquaculture vs Capture Fishery) Per Capita")