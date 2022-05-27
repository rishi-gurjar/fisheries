library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)


dat1 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/oced_aquacultureprodmod1.csv")

#Aquaculture Production Over Time By State(1)
dat1 %>%
  ggplot(aes(x=TIME, y=Value, color=LOCATION)) + 
  geom_line()+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title = "Aquaculture Production Value over time (No Ecuador in Dataset)",
        color = "States") 

#Aquaculture Production Over Time Per Capita By State(2)
ggplot(dat1, aes(x=TIME, y=PerCapita, color=LOCATION)) + 
  geom_line()+
  labs( x = "Time (yrs)",
        y = "Volume (tonnes)",
        title ="Per Capita Aquaculture Production Value over time")

#******"CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU"
#------------------------------------------------------------------------------------------------------
dat <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/fao_consumptionpercapita.csv")
View(dat)

#Fish and Seafood Consumption Over Time Per Capita By State (3)
filter(dat, Code==c("CAN","CHN", "GBR", "IND", "USA", "THA", "IDN", "VNM", "ECU"))%>%
  ggplot(aes(Year, Value, color=Code)) + 
  geom_point()+
  geom_line()+
  geom_smooth(method=lm, se=FALSE, col='red', size=1.5)+
  labs( x = "Time (yrs)", y = "Food supply quantity (kg/capita/yr) (FAO, 2020)", title ="Fish and seafood consumption per capita over time")

#------------------------------------------------------------------------------------------------------
dat2 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/faoseafoodprod_selected.csv")
View(dat2)
#names(dat2)
#View(dat_gp)

#************************ Note: compute percentages without dplyr:
#my_fun <- function(vec){ 
# as.numeric(vec[2]) / sum(data$value[data$time==vec[1]]) *100 
#} 
#data$percentage <- apply(data , 1 , my_fun)
#************************
dat_gp <- dat2 %>% 
  group_by(Year, Entity) %>% 
  summarise(n=sum(Value)) %>% 
  mutate(percentage = n/sum(n))

#Global Seafood Production Over Time By State (4)
ggplot(dat_gp, aes(x=Year, y=percentage, fill=Entity))+
  geom_area(alpha=0.6 , size=0.6, colour="black")+
  labs( x = "Time (yrs)", y = "Percentage (Commodity Balances calculated in tonnes)", title ="Global Seafood production over time based on area (FAO)")

#Global Seafood Production Over Time (5)
ggplot(dat2, aes(Year, Value)) + 
  geom_col()+
  labs( x = "Time (yrs)", y = "Commodity Balances in tonnes", title ="Global Seafood production over time (FAO)")

dat2mod1 <- dat2 %>% 
  group_by(Year, Fish) %>% 
  summarise(n=sum(Value)) %>% 
  mutate(percentage = n/sum(n))

#Global Seafood Production Over Time by Seafood Group (6)
ggplot(dat2mod1, aes(x=Year, y=percentage, fill=Fish))+
  geom_area(alpha=0.6 , size=0.6, colour="black")+
  labs( x = "Time (yrs)", y = "Percentage", title ="Global Seafood production over time based on seafood group (FAO)")

#Asian Seafood Production Over Time by Seafood Group (7)
filter(dat2, Entity=="Asia")%>%
  ggplot(aes(Year, Value, color=Fish)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Commodity Balances in tonnes", title ="Asian Seafood production over time grouped by seafood group (FAO)", subtitle="No formal FAO freshwater fish defintition; Google: Freshwater fish are those that spend some or all of their lives in fresh water, such as rivers and lakes, with a salinity of less than 1.05%")
#Separation by Asian Country Appears in Later Plots

dat2mod2 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/worldbankasianfisheryprod.csv")

dat2long <-  pivot_longer(dat2mod2,
                          starts_with("X"),
                          names_to = "Year", values_to = "Value")

#Asian Seafood Production Over Time By Asian State (8)
ggplot(dat2long, aes(Year, Value, color=Country)) + 
  geom_point()+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Asian Seafood production over time By State; CAN and USA placed as reference (FAO)")+
  theme(axis.text.x = element_text(angle = 90))


#Global Seafood Production Over Time by Region and Seafood Group (9)
ggplot(dat2, aes(Year, Value, fill=Fish)) + 
  geom_col()+
  labs( x = "Time (yrs)", y = "Commodity Balances in tonnes", title ="Global Seafood production over time in major regions and seafood group (FAO)")+
  facet_wrap(~ Entity) 

#Global Seafood Production Over Time by Region and Seafood Group Per Capita (10)
ggplot(dat2, aes(Year, PerCapita, fill=Fish)) + 
  geom_col()+
  labs( x = "Time (yrs)", y = "Commodity Balances in tonnes", title ="Global Seafood production over time in major regions and seafood group Per Capita (FAO)")+
  facet_wrap(~ Entity) 

#------------------------------------------------------------------------------------------------------
noaapercap <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/noaa_percapconsumption.csv")
#names(noaapercap) <- tolower(names(noaapercap))
#head(noaapercap)
#View(noaapercap)

aql <- melt(noaapercap)
aql1 <- melt(noaapercap, id.vars = "year")
#View(aql1)
aql2 <- aql1 %>% filter(year > 1991, variable != "total.canned..pounds.")

#US Consumption of Fishery Products By Seafood Type Per Capita (11)
ggplot(aql2, aes(year, value, color=variable)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Pounds", title ="US Per Capita Consumption of Fishery Products (NOAA)")
#********check what "pounds" means

#dataql2 <- aql2 %>% 
# group_by(year, variable) %>% 
# summarise(n=sum(value)) %>% 
# mutate(percentage = n/sum(n))
#ggplot(dataql2, aes(x=year, y=percentage, fill=variable))+
#geom_area(alpha=0.6 , size=0.6, colour="black")+
#labs( x = "Time (yrs)", y = "Percentage (Calc. in Pounds)", title ="US Per Capita Consumption of Fishery Products (NOAA)")
#------------------------------------------------------------------------------------------------------
dat3 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/worldbank_prod.csv")

#Global Capture vs Aquaculture Over Time (12)
ggplot(dat3, aes(Year, Value, fill=Group)) + 
  geom_bar(stat='identity', position= "dodge")+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Global Capture vs Aquaculture over time (Worldbank)")+
  scale_fill_discrete(name = "Type",
                      labels = c("Aquaculture Production", "Capture Fisheries"))

#Capture vs Aquaculture Over Time By State (13)
filter(dat3, Code==c("CAN", "USA", "THA", "IDN", "VNM", "ECU", "IND"))%>%
  ggplot(aes(Year, Value, color=Group)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Capture vs Aquaculture Over Time By State (Worldbank)", subtitle="Capture Fisheries: The aquatic life they support is not artificial controlled in any meaningful way and needs to be captured; exist primarily in the oceans, coasts, continental shelves, lakes, and rivers.")+
  facet_wrap(~ Code)

#Capture vs Aquaculture Over Time By State Per Capita (14)
filter(dat3, Code==c("CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU", "IND"))%>%
  ggplot(aes(Year, PerCapita, color=Group)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Capture vs Aquaculture Over Time By State Per Capita (Worldbank)", subtitle="Capture Fisheries: The aquatic life they support is not artificial controlled in any meaningful way and needs to be captured; exist primarily in the oceans, coasts, continental shelves, lakes, and rivers.")+
  facet_wrap(~ Code)
#------------------------------------------------------------------------------------------------------
dat4 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/worldbank_captureprod.csv")

#Capture Fishery Catch Over Time By State (15)
filter(dat4, Code==c("CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU"))%>%
  ggplot(aes(Year, Capture.fisheries.production, color=Code)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Capture Fishery Catch Over Time By State (Worldbank)")

#Capture Fishery Catch Over Time By State Per Capita (16)
filter(dat4, Code==c("CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU"))%>%
  ggplot(aes(Year, PerCapita, color=Code)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Metric Tons", title ="Capture Fishery Catch Over Time By State Per [2020] Capita (Worldbank)")

#PROBLEM?? = Use country pop values (Use every 5 years— take average, put in that 5 year range)
#Is that accurate? ^
#------------------------------------------------------------------------------------------------------
dat5 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/pauly&zeller_fisherycatchsector.csv")
#View(dat5)
#View(dat5mod1)

#Below not really necessary
ggplot(dat5, aes(Year, Value, fill=Group)) + 
  geom_col()+
  labs( x = "Time (yrs)", y = "Catch (Tons)", title ="Reconstructed global catch by fisheries sectors (Pauly and Zeller, 2016)")

dat5mod1 <- dat5 %>% 
  group_by(Year, Group) %>% 
  summarise(n=sum(Value)) %>% 
  mutate(percentage = n/sum(n))

#Below not really necessary
ggplot(dat5mod1, aes(x=Year, y=percentage, fill=Group))+
  geom_area(alpha=0.6 , size=0.6, colour="black")+
  labs( x = "Time (yrs)", y = "Percentage (Calc. in Pounds)", title ="US Per Capita Consumption of Fishery Products (NOAA); Species Not in Dataset/Supp. Info")

#------------------------------------------------------------------------------------------------------
dat6 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/worldbank_aquacultureprod.csv")
#dat6mod <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/worldbank_aquacultureprodmod.csv")
#View(dat6)
#names(dat6)
#View(dat_gp1)

#Aquaculture Production Over Time By State (17)
filter(dat6, Code==c("CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU"))%>%
  ggplot(aes(Year, AqProdMetricTons, color=Code)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Aquaculture Production (metric tons)", title ="Aquaculture Production Over Time By State  (Worldbank)")

#Aquaculture Production Over Time By State Per Capita (18)
filter(dat6, Code==c("CAN", "USA", "CHN", "THA", "IDN", "VNM", "ECU"))%>%
  ggplot(aes(Year, PerCapita, color=Code)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Aquaculture Production (metric tons)", title ="Aquaculture Production Over Time By State Per Capita (Worldbank)")

#dat_gp1 <- dat6mod %>%
# group_by(Year, Code) %>% 
#summarise(n=sum(AqProdMetricTons)) %>% 
#mutate(percentage = n/sum(n))
#ggplot(dat_gp1, aes(x=Year, y=percentage, fill=Code))+
#geom_area(alpha=0.6 , size=0.6, colour="black")+
#labs( x = "Time (yrs)", y = "Aquaculture Production (metric tons)", title ="Aquaculture production over time in percent including major countries that are supplying the US with Seafood (Worldbank)")

#------------------------------------------------------------------------------------------------------
dat7 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/fao_fishploitation.csv")

#Below not really necessary
ggplot(dat7, aes(TimePeriod, Value)) + 
  geom_point()+
  geom_line()+
  labs( x = "Time (yrs)", y = "Percent Exploited", title ="Proportion of global fish stocks within biologically sustainable levels (not overexploited) over time (FAO 2020)", subtitle = "UN SD GOAL 14: measures the sustainability of the world's marine capture fisheries by their abundance. A fish stock of which abundance is at or greater than the level, that can produce the maximum sust. yield (MSY) is classified as biologically sustainable-- when abundance falls below the MSY level, the stock is considered biologically unsustainable.")
#------------------------------------------------------------------------------------------------------
#dat8 <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/oecd_agriandfisherytrademod2.csv")
#names(dat8)
#View(dat8)

#ggplot(dat8, aes(Year, as.numeric(ValueUSDollarUncorrected), fill=TradeFlow)) + 
# geom_bar()+
# labs( x = "Time (yrs)", y = "Value(US Dollar Uncorrected)", title ="International Trade Flows in aggregate fisheries products in Not Corrected $USD (FAO 2020)")
#****problem in plot; filter for countries where us is depedent on (seperate in panels, one for each country)
#------------------------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

mydata <- readr::read_csv("/Users/rishigurjar/Desktop/SeafoodResearch/usda_worldexports1.csv")
#View(mydata)

mapdata <- map_data("world")

mapdata1 <- left_join(mapdata, mydata, by= "region")
#view(mapdata1)

#mapdata2 <- mapdata1 %>% filter(mapdata1$Value = "0")
#view(mapdata)
mapdata1mod <- mapdata1 %>% filter(mapdata1$Value != 0)

#US Export Partner Countries Represented in USD of Seafood Exports from the US --> the country (2020) (19)
ggplot(mapdata1mod, aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Export(domestic) Partner Countries Represented in Thousands of Dollars of Seafood Exports (from the US to the country) in 2020 (FAS US Trade/USDA)", subtitle = "US Export Partner Countries = Countries Exporting Seafood to the US; include (1) commodities which are grown, produced, or manufactured in the United States and (2) commodities of foreign origin which have undergone substantial transformation in the United States from the form in which they were imported ")
#**** look for another gradient (scale discrete gradient)

#------------------------------------------------------------------------------------------------------
mydata1 <- readr::read_csv("/Users/rishigurjar/Desktop/SeafoodResearch/usdaimportsmod.csv")
#View(mapdata1)

mapdata1 <- map_data("world")

mapdat <- left_join(mapdata1, mydata1, by= "region")
#view(mapdat)

mapdat2 <-  pivot_longer(mapdat,
                         starts_with("X"),
                         names_to = "Year", values_to = "Value")

mapdat2 <- mapdat2 %>% filter(mapdat2$Value != 0)
#view(mapdat2)

#US Import Partner Countries Represented in USD of Seafood Import to the US (2016) (20)
filter(mapdat2, Year == "X2016") %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Import(general) Partner Countries Represented in Thousands of Dollars of Seafood Import to the US in 2016 (FAS US Trade/USDA)", subtitle = "US Import Partner Countries = Countries Importing US Seafood; measures the total value of merchandise shipments that arrive in the U.S. from foreign countries")

#US Import Partner Countries Represented in Dollars of Seafood Import to the US (2017) (20)
filter(mapdat2, Year == "X2017") %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Import(general) Partner Countries Represented in Thousands of Dollars of Seafood Import to the US in 2017 (FAS US Trade/USDA)", subtitle = "US Import Partner Countries = Countries Importing US Seafood; measures the total value of merchandise shipments that arrive in the U.S. from foreign countries")

#US Import Partner Countries Represented in Dollars of Seafood Import to the US (2018) (20)
filter(mapdat2, Year == "X2018") %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Import(general) Partner Countries Represented in Thousands of Dollars of Seafood Import to the US in 2018 (FAS US Trade/USDA)", subtitle = "US Import Partner Countries = Countries Importing US Seafood; measures the total value of merchandise shipments that arrive in the U.S. from foreign countries")

#US Import Partner Countries Represented in Dollars of Seafood Import to the US (2019) (20)
filter(mapdat2, Year == "X2019") %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Import(general) Partner Countries Represented in Thousands of Dollars of Seafood Import to the US in 2019 (FAS US Trade/USDA)", subtitle = "US Import Partner Countries = Countries Importing US Seafood; measures the total value of merchandise shipments that arrive in the U.S. from foreign countries")

#US Import Partner Countries Represented in Dollars of Seafood Import to the US (2020) (20)
filter(mapdat2, Year == "X2020") %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill= Value), color = "black")+
  labs( x = "Longitude", y = "Latitude", title ="US Import(general) Partner Countries Represented in Thousands of Dollars of Seafood Import to the US in 2020 (FAS US Trade/USDA)", subtitle = "US Import Partner Countries = Countries Importing US Seafood; measures the total value of merchandise shipments that arrive in the U.S. from foreign countries")
#GIF IN GOOGLE!

#GIS IN GOOGLE MAPS AND GOOGLE EARTH!


dat <- read.csv("/Users/rishigurjar/Desktop/SeafoodResearch/test1.csv")
names(dat)
View(dat)
dat$Country
#library(tidyverse)
#& |
dat1 <- dat %>% filter(Country==c("1") | Country==c( "2"))
dat2 <- dat %>% filter(Country=="1" | Country=="2")
#dat3 <- dat %>% filter(Country==c(1,2))

View(dat3)
View(dat2)
View(dat1)
