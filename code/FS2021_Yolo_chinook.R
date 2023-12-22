library(dplyr)
library(reshape2)
library(ggplot2)


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/233/4/4488201fee45953b001f70acf30f7734" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "EventID",     
                 "StationCode",     
                 "Datetime",     
                 "SampleDate",     
                 "WaterTemp",     
                 "SpecificConductance",     
                 "Conductivity",     
                 "Turbidity",     
                 "DO",     
                 "pH",     
                 "Secchi",     
                 "Tide",     
                 "WeatherCode",     
                 "VegetationRank",     
                 "SubstrateCode",     
                 "HabitatType",     
                 "MicrocystisRank",     
                 "MethodCode",     
                 "GearCode",     
                 "GearConditionCode",     
                 "SampleAltered",     
                 "FieldComments",     
                 "Flag_WQ",     
                 "Comment_WQ",     
                 "Duplicated"    ), check.names=TRUE)
##YBFMP salmon genetics data

salmon_gen <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.4&entityid=fd6b801591175db195661e09d2f52ba8")

fish_len <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.4&entityid=402732c0e6c782db8b8229c3b9310afa")

total_catch <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.4&entityid=b2b92d9dbfb78cfb1a5716174dfceab1")

total_catch$Date <- as.Date(substr(total_catch$SampleID, 1, 8), format = "%Y%m%d")
# fish_len <- merge(fish_len, total_catch[, c("EventID", 'Date')], by = "EventID", all.x = T)
fish_len <- merge(fish_len, dt2[, c("EventID", 'SampleDate')], by = "EventID", all.x = T)
# subset to CHN

mydata <- data.frame(Day=c(-31:150))
mydata$func1 <- sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+26)))
mydata$func2 <-  sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+71)))
mydata$func3 <-  sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+160)))

darkcols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2") 
set.seed(51684)

# Chinook salmon ----------------------------------------------------------

chinook <- fish_len[fish_len$OrganismCode == "CHN",]
chinook <- merge(chinook, salmon_gen, by = "FishGenID", all.x = T)
chinook$Date <- as.Date(chinook$SampleDate, format = "%m/%d/%Y")

chinook_gen <- chinook[is.na(chinook$GeneticID) == F,]

chinook$K <- 10^5 * chinook$Weight*chinook$ForkLength^-3

chinook %>% group_by(GeneticID) %>% summarize(Count = length(ForkLength))

##Double check this with Lisa and Naoaki
chinook$Adipose <- ifelse(grepl(pattern = "Ad_plus", chinook$FishGenID), "intact", "clipped")
chinply <- chinook %>% group_by(RaceByLength, Adipose) %>% summarize(Count = length(ForkLength))
chinply

chinook$Day <- as.integer(format(chinook$Date, format = "%j"))
chinook$Year <- as.integer(format(chinook$Date, format = "%Y"))
chinook$GeneticID <- ifelse(chinook$GeneticID == "n/p", NA, chinook$GeneticID)
chinook1519 <- chinook[chinook$Year %in% 2015:2019,]

png("output/Yolo_chinook_2023_%03d.png",
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(chinook, aes(x = Date, y = ForkLength, color = RaceByLength)) + 
  geom_jitter(alpha = .5) + theme_bw()

ggplot(chinook, aes(x = Day, y = ForkLength)) + 
  geom_jitter(alpha = .3) + theme_bw() +
  ylim(30,200) +
  scale_x_continuous(limits = c(-0, 102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")



ggplot() + 
  # geom_ribbon(data = mydata, aes(ymin = min(chinook$ForkLength, na.rm = T), ymax = func1), fill = darkcols[1], alpha = .2) +
  # geom_ribbon(data = mydata, aes(ymin = func2, ymax = func1), fill = darkcols[3], alpha = .2) + 
  # geom_ribbon(data = mydata, aes(ymin = func2, ymax = func3), fill = darkcols[4], alpha = .2) + 
  # geom_ribbon(data = mydata, aes(ymin = func3, ymax = 200), fill = darkcols[2], alpha = .2) +
  geom_jitter(data = chinook1519[is.na(chinook1519$GeneticID) == T,],
              aes(x = Day, y = ForkLength, shape = GeneticID), 
              color = "black", height = 0, size = 1, alpha = .1) +
  geom_jitter(data = chinook1519[is.na(chinook1519$GeneticID) == F,], 
              aes(x = Day, y = ForkLength, color = GeneticID, shape = GeneticID),
              height = 0, alpha = .5) + 
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +  
  theme(legend.position = "bottom") +
  ylim(30,150) + facet_wrap(Year ~ .) +
  scale_x_continuous(limits = c(-0,150),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")

dev.off()

png("output/Yolo_chinook_ay_%03d.png",
    family = "serif", width = 7.5, height= 7.5, units = "in", res = 1000)

ggplot(chinook, aes(x = Day, y = ForkLength)) + 
  geom_jitter(alpha = .125) + theme_bw() +
  ylim(30,200) + facet_wrap(Year ~ .) +
  scale_x_continuous(limits = c(-0, 102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")

ggplot(chinook, aes(x = Day, y = ForkLength)) + 
  stat_density2d_filled() +
  #geom_jitter(alpha = .3) + theme_bw() +
  ylim(30,200) + facet_wrap(Year ~ .) +
  scale_x_continuous(limits = c(-0, 102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")


dev.off()

sites <- unique(chinook$Location)

# Subset for genetics analysis --------------------------------------------
# To run or not to run, that is the question
# Objectives for identifying 142 samples to be run :
#   1) isolate potential winter un for PBT - give to Rachel (does not count against the 142 wells)
#   2) Run all Butte watershed caught Spring and Fall run size fish
#   3) Subsample ~25% Sac river (Willow and main channel sites) fall run fish

#Logical binning
chinook$Genrun <- "Backson"

chinook[chinook$Trib %in% "Butte", "Genrun"] <- "Mariah"
chinook[chinook$ForkLength > 50 & chinook$Day < 31, "Genrun"] <- "PBT"
chinook[chinook$Trib %in% "Sac River" & chinook$RaceByLength %in% c("Fall"), "Genrun"] <- "Mariah_sub"

ggplot(chinook, aes(x = Day, y = ForkLength, color = GeneticID, shape = Adipose)) + 
  geom_jitter(alpha = .9) + theme_bw() +
  scale_x_continuous(limits = c(-0, 105),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black", alpha = .5) +
  geom_hline(aes(yintercept = 50)) + geom_vline(aes(xintercept = 31)) + theme(legend.position = "bottom")

ggdark::invert_geom_defaults()

## Manual method using a spreadsheet printout
chindateloc <- unique(paste(chinook$Date, chinook$Location))

chinook <- data.frame(chinook)
paste(sort(chinook[paste(chinook$Date, chinook$Location) %in% chindateloc[3],"ForkLength"]))
chinooksort <- chinook[order(chinook$ForkLength, decreasing = T),]
chinookprint <- data.frame()

for(i in 1:length(chindateloc)){
  print(chindateloc[i])
  
  chinookprint <- rbind(chinookprint, 
                        data.frame(dateloc = chindateloc[i], 
                                   FLs = paste(data.frame(chinooksort[paste(chinooksort$Date, chinooksort$Location) %in% 
                                                                        chindateloc[i], "ForkLength"])))
  )
}

chinookprint
# write.csv(chinookprint, "C:/Users/ejholmes/Desktop/Sutter2023_Chinook_for_genetics.csv")
# chinotes <- readxl::read_excel("C:/Users/ejholmes/Desktop/Sutter2023_Chinook_for_genetics.xlsx")

chinooksort$sitedate <- paste(chinooksort$Date, chinooksort$Location)
# chinooksort2 <- merge(chinooksort, chinotes, by = "sitedate", all.x = T)

# chinooksort2 %>% group_by(Notes) %>% summarise(count = length(ForkLength))

# Refuge Chinook patterns -------------------------------------------------

refuge <- chinook[chinook$Location %in% sites[grep(pattern = "Refuge", unique(chinook$Location))],]
refuge.fllm <- lm(ForkLength ~ Day, data = refuge)
refuge.wtlm <- lm(Weight ~ Day, data = refuge)
summary(refuge.fllm)
summary(refuge.wtlm)
refuge %>% group_by(RaceByLength, Adipose) %>% summarize(Count = sum(Count))

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Sutter2023_Refugechinook_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(refuge, aes(x = Day, y = ForkLength)) + 
  geom_boxplot(aes(group = Day), outlier.alpha = 0) +
  geom_jitter(aes(color = RaceByLength),alpha = .5, width = .1) + theme_bw() +
  labs(x = "Day of year", y = "Fork length (mm)", title = "Sutter NWR juvenile Chinook Salmon")+
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  # stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "grey60") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "grey60")+
  stat_smooth(method = "lm", se = F)

ggplot(refuge, aes(x = Day, y = K)) + 
  geom_boxplot(aes(group = Day), outlier.alpha = 0) +
  geom_jitter(aes(color = RaceByLength),alpha = .5, width = .1) + theme_bw() +
  labs(x = "Day of year", y = "Condition", title = "Sutter NWR juvenile Chinook Salmon") +
  stat_smooth(method = "lm", se = F)

ggplot(refuge, aes(x = ForkLength, y = Weight)) + geom_point() +
  stat_function(fun = function(x) x^3*10^-5, color = "black") +
  theme_bw() +
  labs(x = "Fork length (mm)", y = "Weight (g)", title = "Sutter NWR juvenile Chinook Salmon")

dev.off()

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Dark_themed/Sutter2023_Refugechinook_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(refuge, aes(x = Day, y = ForkLength)) + 
  # geom_boxplot(aes(group = Day), outlier.alpha = 0) +
  geom_jitter(aes(color = RaceByLength),alpha = .9, width = .1) + ggdark::dark_theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Day of year", y = "Fork length (mm)", title = "Sutter NWR juvenile Chinook Salmon")+
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  # stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "grey60") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "grey60")

ggplot(chinook, aes(x = Day)) + 
  geom_ribbon(data = mydata, aes(ymin = min(chinook$ForkLength), ymax = func1), fill = darkcols[1], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func1), fill = darkcols[3], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func3), fill = darkcols[4], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func3, ymax = 200), fill = darkcols[2], alpha = .2) +
  geom_jitter(alpha = .1, show.legend = F,aes(y = ForkLength, color = RaceByLength, shape = Adipose)) + 
  geom_jitter(data = refuge, aes(y = ForkLength, color = RaceByLength, shape = Adipose), alpha = .9, width = .1, show.legend = F) +
  ggdark::dark_theme_bw() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(3, 19, 19)) +
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  coord_cartesian(ylim = c(32,130)) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "white", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "white", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "white", alpha = .5)

ggdark::invert_geom_defaults()

dev.off()


