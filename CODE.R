library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(cowplot)

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
#view(datLong1)


# OLD ONE
#ggplot(datLong1, aes(x=year, y=percentage, fill=Series.Name)) + 
#  geom_area(alpha=0.6 , size=1, colour="black")+
#  labs(x= "Year", y = "Percentage (Volume measured in metric tons)", title = "Percentage Seafood Production Share in the US 1971-2018")+
#  scale_fill_manual("Fishery Types", labels = c("Aquaculture" , "Capture Fishery"), values = c("#440154", "#5ec962"))

ggplot(datLong1) +
  aes(x = year, y = value, fill = Series.Name) +
  geom_area(size = 1.5) +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  labs(
    x = "Year",
    y = "Volume (Metric Tons)",
    title = "US Seafood Production Stratified by Production Type (1971-2018)",
  ) +
  scale_fill_manual("Production Type", labels = c("Aquaculture" , "Capture Fishery"), values = c("#440154", "#5ec962")) +
  theme_minimal()



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
#view(consumpLong)
#ggplot(consumpLong) +
#  geom_path(aes(x = Year, y = Val, color = Type)) +
#  facet_grid(Type ~ ., scales = "free_y", 
#             labeller = label_parsed,
#             switch = "y") +
#  theme(strip.background = element_blank(),
#        axis.title.y = element_blank(),
#        strip.text = element_text(size = rel(1))) +
#  guides(color = "none") +
#  labs(title = "US Population Growth and Annual Per Capita Consumption of Seafood Products 1910-2019")

coeff <- 15

ggplot(consumpLong, aes(x = Year))+
  geom_line(aes(y = Pop), size = 1, color = "blue")+
  geom_line(aes(y = Pounds*coeff), size = 1, color = "red")+
  scale_y_continuous(
    name = "Population (in millions)",
    sec.axis = sec_axis(trans=~./coeff, name = "Pounds (lbs)")) + 
labs(title = "US Population Growth and Annual Consumption of Seafood Products & Red Meat Per Capita (1910-2019)")+
  theme(
    axis.title.y = element_text(color = "blue", size=15),
    axis.title.y.right = element_text(color = "red", size=0),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.title.x = element_blank())

coeff2 <- 2
ggplot(consumpLong, aes(x = Year))+
  geom_line(aes(y = Pop), size = 1, color = "blue")+
  geom_line(aes(y = TotalRedMeatConsumpPounds*coeff2), size = 1, color = "red")+
  scale_y_continuous(
    name = "Population (in millions)",
  theme(
#    axis.title.y = element_text(color = "blue", size=10),
    axis.title.y.right = element_text(color = "red", size=25),
    axis.text.x=element_text(size=20),
    axis.text.y=element_text(size=20),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()))

#---------
aquaCap <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/FoodResearch/SeafoodResearch/oced_aquacultureprodmod1.csv")
view(aquaCap)

#cap <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFINAL/cap.csv")
#names(cap) <- sub("^X", "", names(cap))

#view(cap)

#write.csv(cap,"/Users/rishigurjar/Desktop/aquaresearchFinal/cap1.csv", row.names = FALSE)

#cap <- cap %>% pivot_longer(cols = 2:27, names_to = "Year", values_to = "Population")


library(plyr)

names <- c('AQUAPROD' = "Aquaculture Production", 'CAPNLD' = "Capture Fishery Landings")

p1 <- ggplot(aquaCap, aes(x=TIME, y=PerCapita, linetype=INDICATOR, color=LOCATION)) + 
  geom_line(size = 1)+
  labs( x = "Year",
        y = "log Volume (tonnes)",
        title ="Total") + 
  theme(legend.position = "none")

p2 <- ggplot(aquaCap, aes(x=TIME, y=log10(Value), linetype=INDICATOR, color=LOCATION)) + 
  geom_line(size = 1)+
  labs( x = "Year",
        y = "Volume (tonnes)",
        title ="Per Capita") + 
  scale_color_hue("Country", labels = c("China", "Indonesia", "Thailand", "United States", "Vietnam"))+
  labs(
    linetype = ""
  )
p <- plot_grid(p1, p2, labels = c('a', 'b'), label_size = 15)
title <- ggdraw() + draw_label("Domestic Aquaculture Production and Capture Fishery Landings of Select Countries (1995-2019)")
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins


#facet_grid(INDICATOR ~ ., scales = "free_y", labeller = as_labeller(names)) 


#---------
fatal <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/fatal.csv")
view(fatal)

library(ggplot2)

ggplot(fatal) +
 aes(x = Year, y = value, colour = type, group = thing) +
 geom_line(size = 1) +
 scale_color_hue(direction = 1) +
 labs(x = "Year", y = "Rate & Number of injuries per 100,000 full-time workers", title = "Rate & Number of injuries per 100K workers in the private crop production & animal production and aquaculture industries") +
  scale_color_discrete(name = "Key", labels=c('Animal production and aquaculture', 'Crop production'))+
  facet_grid(thing2 ~ ., scales = "free_y") +
     theme_minimal() + theme(legend.position="bottom") +
  theme(strip.text = element_text(size = 12))

#---------
discards <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/discards.csv")
#view(discards)

ggplot(discards, aes(x=Year, y=Discards, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish discards by country 1950-2018",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")

clean_discards <- filter(discards, Year >=1960)
#view(clean_discards)
ggplot(clean_discards, aes(x=Year, y=PerCap, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish Discards Per Capita By Country (1960-2018)",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")+
  scale_color_discrete(name = "Country", labels = c("China", "Great Britain", "Indonesia", "India", "United States", "Vietnam"))

ggplot(clean_discards, aes(x=Year, y=DiscardsOverCap, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish Discards/Capture Fishery Produced-Fish By Country (1960-2018)",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")+
  scale_color_discrete(name = "Country", labels = c("China", "Great Britain", "Indonesia", "India", "United States", "Vietnam"))

disv2 <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/discardsv2.csv")

disv3 <- pivot_longer(disv2, cols = 4:5, names_to = "type", values_to = "tonnes")
view(disv3)

ggplot(disv2, aes(x=Year, y=discards/landings, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Year",
        y = "Volume (tonnes)",
        title ="Fish Discards Per Capture Fishery-Produced Fish By Country",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")
#  scale_color_discrete(name = "Country", labels = c("China", "Great Britain", "Indonesia", "India", "United States", "Vietnam"))


ggplot(disv3) +
 aes(x = Year, y = tonnes, fill = type) +
 geom_area(size = 1.5) +
 scale_fill_viridis_d(option = "viridis", 
 direction = 1) +
 labs(x = "Year",
      y = "Volume (tonnes)",
      title ="Fish Discards and Fish Landings By Country",
      subtitle ="Landings represent aquatic animals that are caught and brought ashore for use.\nDiscards are animals thrown back (alive or dead) into the sea after being caught during
fishing activities.",
      fill = "Type") +
 theme_minimal() +
  scale_fill_manual(values=c('#3a4e68', '#b1cd69'), labels=c('Discards', 'Landings')) +
  theme(legend.title=element_blank())+
 facet_wrap(vars(Entity))


#Per Capita Version of above plot
ggplot(clean_discards, aes(x=Year, y=PerCapDiscardsOverCapturedFish, color=Code)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Fish Discards/Capture Fishery Produced-Fish By Country Per Capita (1960-2018)",
        subtitle ="Discards are animals thrown back (alive or dead) into the sea after being caught during fishing activities.")+
  scale_color_discrete(name = "Country", labels = c("China", "Great Britain", "Indonesia", "India", "United States", "Vietnam"))


#---------
captureprod <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/API_ER.FSH.CAPT.MT_DS2_en_csv_v2_3692271.csv")

#---------
imports <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/usimps.csv")

uspop <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/USPOPDATA_WorldBank.csv")

testdat <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/testdata.csv")

allpop <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/ALLPOP_WorldBank_WDI.csv")
view(allpop)
library(stringr)
library(magrittr)
library(scales)

view(uspop)

uspop1 <- uspop %>% pivot_longer(cols = 2:63, names_to = "Year", values_to = "Population")
view(uspop1)

allpop <- allpop %>% pivot_longer(cols = 2:63, names_to = "Year", values_to = "Population")
view(allpop)

allpop <- allpop %>%
  transform(Year=str_replace(Year,"X",""))

write.csv(allpop,"/Users/rishigurjar/Desktop/aquaresearchFinal/cleaned_allpop_WorldbankWDI.csv", row.names = FALSE)


ggplot(imports, aes(x=Year, y=Value)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="US Seafood Imports 1976-2019")

coeff3 <- 140
ggplot(imports, aes(x = Year))+
  geom_line(aes(y = Population), size = 1, color = "blue")+
  geom_line(aes(y = Value*coeff3), size = 1, color = "red")+
  scale_y_continuous(
    name = "Population",
    labels = scales::label_number(scale_cut = scales::cut_short_scale()),
    sec.axis = sec_axis(trans=~./coeff3,
    name = "Volume (tonnes)",
    labels = scales::label_number(scale_cut = scales::cut_short_scale()))) + 
  labs(title = "US Population and Seafood Imports (1976-2019)")+
  theme(
    axis.title.y = element_text(color = "blue", size=15),
    axis.title.y.right = element_text(color = "red", size=15)) 


#---------
toppop <- read.csv("/Users/rishigurjar/Desktop/aquaresearchFinal/toppop.csv")
#head(toppop)
#toppop
topone <- filter(toppop, name != 'Net')
view(topone)


library(ggforce)

df=data.frame(point=c("AB","AC","ABC"),
              x=1:3, # x coordinate of point center
              y=c(0.5,2,0.5),# y coordinate of point center
              A=c(0.5,0.5,1/3), # I chose different values 
              B=c(0.7,0,1/4),   # to make the resulting plot
              C=c(0,0.5,1/3))   # a little more interesting

view(df)
df=pivot_longer(df,cols=c(A,B,C),names_to = "colorby",values_to = "amount")
df$radius=sqrt(df$amount) # since we have a number represented by area
ggplot(df)+
  geom_arc_bar(aes(x0=x,y0=y,r0=0,r=radius,amount=amount,fill=colorby),stat="pie")+
  coord_equal()+ # so one gets actually circles
  scale_fill_manual(values = c("A"="red","B"="blue","C"="green"))

ggplot(topone)+
  geom_arc_bar(aes(x0=x,y0=y,r0=0,r=radius,amount=amount,fill=colorby),stat="pie")+
  coord_equal()+ # so one gets actually circles
  scale_fill_manual(values = c("A"="red","B"="blue","C"="green"))






ggplot(topone, aes(x=Time, y=Per.Capita, linetype=name, color=country)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Per Capita Volume (mt)",
        title ="Top 6 Population Countries Aquaculture vs Capture Fishery Per Capita")

topnet <- filter(toppop, name == 'Net')
#view(topnet)

ggplot(topnet, aes(x=Time, y=NetVal, color=country)) + 
  geom_line(size = 1)+
  labs( x = "Time (yrs)",
        y = "Volume Differential (metric tons)",
        title ="Top 6 Population Countries Seafood Production Differential (Aquaculture - Capture Fishery) Per Capita")+
  scale_color_discrete("Country") + 
  theme_minimal() 
  
 # facet_grid(country ~ ., scales = "free_y")
