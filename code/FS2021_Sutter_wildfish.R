library(googlesheets4)
library(dplyr)
library(reshape2)
library(ggplot2)
library(plotly)
library(lubridate)

#Set up fisher LAD class breaks
mydata <- data.frame(Day=c(-31:150))
mydata$func1 <- sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+26)))
mydata$func2 <-  sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+71)))
mydata$func3 <-  sapply(mydata$Day, FUN = function(x) exp(3.516464 + 0.006574*(x+160)))

# Set up a color palette
darkcols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2") 
set.seed(51684)

## Load wild chinook data from 2017-2023
chinook <- read.csv("data/CV_wild_salmon_Holmes.csv")

chinook$Adipose <- ifelse(chinook$Adipose %in% "Present", "Intact",
                          ifelse(chinook$Adipose %in% "Partial", "Clipped", chinook$Adipose))
chinook$Race_table <- ifelse(chinook$Race_table %in% "Late fall", "Late-Fall", chinook$Race_table)

chinook$Gen_assign <- ifelse(chinook$Genetics %in% c("", "No Call"), "Unknown",
                             ifelse(chinook$Genetics %in% c("SpButte"), "Spring",
                                    ifelse(chinook$Genetics %in% c("LateFall", "Late fall"), "Late-fall", chinook$Genetics)))

chinook$Date <- as.Date(chinook$Date, "%m/%d/%Y")
chinook$Day <- as.integer(format(chinook$Date, format = "%j"))
chinook$Year <- as.integer(format(chinook$Date, format = "%Y"))

chinook$K <- 10^5 * chinook$Weight_g*chinook$Fork_length_mm^-3
chinook$Count <- 1
# chinook$Genetics_lab <- ifelse(is.na(chinook$Genetics_lab) == T, "wait", chinook$Genetics_lab)

# chinook %>% group_by(Genetics_lab) %>% summarize(Count = sum(Count))

chinply <- chinook %>% group_by(Gen_assign, Adipose, Year) %>% summarize(Count = sum(Count))
chinply
unique(chinook$Genetics)

chinook$Trib <- ifelse(chinook$Location %in% c("Colusa Weir", "Willow Bend east side", "Sacramento River side channel ds Willow Bend", 
                                               "Moulton Weir", "Willow Bend East side", "Willow Bend swale", 
                                               "Sutter Bypass at Franklin road west side", 
                                               "Tisdale Bypass at Reclamation Road bridge", "Willow Bend east margin", 
                                               "Willow Bend outlet", "Sacramento River at Colusa Bend gravel bar", 
                                               "Tisdale Bypass at Reclamation Rd", "Tisdale weir sill north corner"),"Sac River",
                       ifelse(chinook$Location %in% c("Laux Road rice field", "Butte Sink north wetland at Mallard Ranch", 
                                               "Butte Creek at Mallard Ranch", "Refuge southern wetland", "Refuge wetland north of Hughes Road", 
                                               "Refuge inlet canal", "Lundberg Farms sothwest rice field outlet", "Refuge T8:3 outlet under Hughes Rd.",
                                               "Refuge northern wetland inlet", "Refuge northern wetland east drain", 
                                               "Refuge southern wetland east outlet", "Refuge southern wetland northwest inlet", 
                                               "Refuge southern wetland north inlet from other wetland", "Refuge Northern wetland south check", 
                                               "Refuge Southern wetland light bulb inlet", "Refuge northern weltand at inlet", 
                                               "Refuge northeastern wetland at inlet", "Butte Creek at Laux Rd. boat launch", 
                                               "Refuge north wetland T2:1 to T2:2 east structure", "Refuge Field 11:2 southwest main drain", 
                                               "Refuge Northern wetland", "Butte Creek at Meridian Pass Rd pad", "Laux road rice field", 
                                               "Refuge T5 wetland inlet", "Refuge T2:1 inlet", "Refuge T2:1 to T2:2 east culvert", 
                                               "Refuge south entrance parking lot", "Refuge at flooded Oswald Rd",
                                               "Refuge T17 northwest inlet", "Refuge T2-1 to T2-2 culvert", 
                                               "Refuge T16 to T17 culvert", "Refuge inlet canal lightbulb south outlet"), "Butte", "Raccoons"))


png("output/Sutter_wildchinook_Jan2023_%03d.png",
    family = "serif", width = 7, height= 5, units = "in", res = 1000)

ggplot(chinook, aes(x = Date, y = Fork_length_mm, color = Race_table)) + 
  geom_jitter(alpha = .5) + theme_bw()

ggplot(chinook[chinook$Year %in% 2019:2023,], aes(x = Day, y = Fork_length_mm, color = Gen_assign, shape = Adipose)) + 
  geom_jitter(alpha = .5) + theme_bw() +
  scale_x_continuous(limits = c(-0, 102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  scale_shape_manual(values = c(3, 19)) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")

ggplot(chinook[chinook$Year %in% 2023,], aes(x = Day, y = Fork_length_mm, shape = Adipose)) + 
  geom_jitter(height = 0) + theme_bw() + facet_grid(. ~ Trib) +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(3, 1)) +
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black")
unique(chinook$Gen_assign)
chinook$genfac <- factor(chinook$Gen_assign, levels = c("Fall", "Late-fall", "Winter", "Spring","Unknown"))

namedf <- data.frame(Day = c(95, 10, 10, 10, 10), 
                     FL = c(35, 50, 100, 107, 35), 
                     Lab = c("Fall", "Spring", "Winter", "Late-fall", "Unknown"),
                     Trib = rep("Butte", 5),
                     ang = c(0, 20, 37, 39, NA),
                     col = c(darkcols[1], darkcols[3], darkcols[4], darkcols[2], "grey80"))

namedf$genfac <- factor(namedf$Lab, levels = c("Fall", "Late-fall", "Winter", "Spring", "Unknown"))

ggplot() + 
  geom_ribbon(data = mydata, aes(x = Day, ymin = 30, ymax = func1), fill = darkcols[1], alpha = .2) +
  geom_ribbon(data = mydata, aes(x = Day, ymin = func2, ymax = func1), fill = darkcols[3], alpha = .2) +
  geom_ribbon(data = mydata, aes(x = Day, ymin = func2, ymax = func3), fill = darkcols[4], alpha = .2) +
  geom_ribbon(data = mydata, aes(x = Day, ymin = func3, ymax = 200), fill = darkcols[2], alpha = .2) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "grey80") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "grey80") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "grey80") +
  geom_text(data = namedf, aes(x = Day, y = FL, label = genfac, color = genfac, angle = ang), show.legend = F) +
  geom_jitter(data = chinook[chinook$Year %in% 2023,],
              aes(x = Day, y = Fork_length_mm, shape = Adipose, color = genfac), height = 0) +
  theme_bw() + facet_grid(. ~ Trib) +
  labs(y = "Fork length (mm)", x = NULL, color = "Genetic assignment") +
  coord_cartesian(ylim = c(30,130)) +
  theme(legend.position="bottom", legend.box="vertical", legend.spacing.y = unit(-.2, 'cm')) +
  scale_shape_manual(values = c(3, 19)) +
  scale_color_manual(values = c("Fall" = darkcols[1], "Spring" = darkcols[3], "Unknown" = "grey", 
                            "Late-fall" = darkcols[2], "Winter" = darkcols[4])) +
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1"))

dev.off()

ggplotly(ggplot(chinook, aes(x = Day, y = Fork_length_mm, color = Race_table, shape = Adipose)) + 
  geom_jitter(alpha = .5) + theme_bw() +
  scale_x_continuous(
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  scale_shape_manual(values = c(3, 3, 19)) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black"), layerdata = 3)

ggplotly(ggplot(chinook, aes(x = Day, y = Fork_length_mm, color = Trib, shape = Adipose)) + 
           geom_jitter(alpha = .5) + theme_bw() +
           scale_y_continuous(limits = c(30,130),breaks = seq(0,200, 20)) +
           scale_x_continuous(
             breaks = c(-32, 1, 32, 60, 91, 122, 152), 
             labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
           scale_shape_manual(values = c(3, 3, 19)) +
           stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
           stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black") +
           stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black"), layerdata = 3)

# png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Dark_themed/Sutter_wildchinook_Jan2023_%03d.png", 
#     family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(chinook, aes(x = Date, y = Fork_length_mm, color = Race_table)) + 
  geom_jitter(alpha = .5, show.legend = F) + ggdark::dark_theme_bw()

ggplot(chinook, aes(x = Day)) + 
  geom_ribbon(data = mydata, aes(ymin = min(chinook$Fork_length_mm), ymax = func1), fill = darkcols[1], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func1), fill = darkcols[3], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func3), fill = darkcols[4], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func3, ymax = 200), fill = darkcols[2], alpha = .2) +
  geom_jitter(alpha = .9, show.legend = F,aes(y = Fork_length_mm, color = Race_table, shape = Adipose)) + 
  # ggdark::dark_theme_bw() + 
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(3, 19, 19)) +
  scale_x_continuous(limits = c(-0, 75),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  coord_cartesian(ylim = c(32,130)) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "white", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "white", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "white", alpha = .5)

ggdark::invert_geom_defaults()

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
chinook[chinook$Fork_length_mm > 50 & chinook$Day < 31, "Genrun"] <- "PBT"
chinook[chinook$Trib %in% "Sac River" & chinook$Race_table %in% c("Fall"), "Genrun"] <- "Mariah_sub"

ggplot(chinook, aes(x = Day, y = Fork_length_mm, color = Genetics_lab, shape = Adipose)) + 
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
paste(sort(chinook[paste(chinook$Date, chinook$Location) %in% chindateloc[3],"Fork_length_mm"]))
chinooksort <- chinook[order(chinook$Fork_length_mm, decreasing = T),]
chinookprint <- data.frame()

for(i in 1:length(chindateloc)){
  print(chindateloc[i])
  
  chinookprint <- rbind(chinookprint, 
                        data.frame(dateloc = chindateloc[i], 
                                   FLs = paste(data.frame(chinooksort[paste(chinooksort$Date, chinooksort$Location) %in% 
                                                                    chindateloc[i], "Fork_length_mm"])))
  )
}

chinookprint
# write.csv(chinookprint, "C:/Users/ejholmes/Desktop/Sutter2023_Chinook_for_genetics.csv")
# chinotes <- readxl::read_excel("C:/Users/ejholmes/Desktop/Sutter2023_Chinook_for_genetics.xlsx")

chinooksort$sitedate <- paste(chinooksort$Date, chinooksort$Location)
# chinooksort2 <- merge(chinooksort, chinotes, by = "sitedate", all.x = T)

# chinooksort2 %>% group_by(Notes) %>% summarise(count = length(Fork_length_mm))

# Refuge Chinook patterns -------------------------------------------------

refuge <- chinook[chinook$Location %in% sites[grep(pattern = "Refuge", unique(chinook$Location))],]
refuge.fllm <- lm(Fork_length_mm ~ Day, data = refuge)
refuge.wtlm <- lm(Weight_g ~ Day, data = refuge)
summary(refuge.fllm)
summary(refuge.wtlm)
refuge %>% group_by(Race_table, Adipose) %>% summarize(Count = sum(Count))

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Sutter2023_Refugechinook_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(refuge, aes(x = Day, y = Fork_length_mm)) + 
  geom_boxplot(aes(group = Day), outlier.alpha = 0) +
  geom_jitter(aes(color = Race_table),alpha = .5, width = .1) + theme_bw() +
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
  geom_jitter(aes(color = Race_table),alpha = .5, width = .1) + theme_bw() +
  labs(x = "Day of year", y = "Condition", title = "Sutter NWR juvenile Chinook Salmon") +
  stat_smooth(method = "lm", se = F)

ggplot(refuge, aes(x = Fork_length_mm, y = Weight_g)) + geom_point() +
  stat_function(fun = function(x) x^3*10^-5, color = "black") +
  theme_bw() +
  labs(x = "Fork length (mm)", y = "Weight (g)", title = "Sutter NWR juvenile Chinook Salmon")

dev.off()

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Dark_themed/Sutter2023_Refugechinook_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(refuge, aes(x = Day, y = Fork_length_mm)) + 
  # geom_boxplot(aes(group = Day), outlier.alpha = 0) +
  geom_jitter(aes(color = Race_table),alpha = .9, width = .1) + ggdark::dark_theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Day of year", y = "Fork length (mm)", title = "Sutter NWR juvenile Chinook Salmon")+
  scale_x_continuous(limits = c(-0,102),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  # stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "grey60") +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "grey60")

ggplot(chinook, aes(x = Day)) + 
  geom_ribbon(data = mydata, aes(ymin = min(chinook$Fork_length_mm), ymax = func1), fill = darkcols[1], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func1), fill = darkcols[3], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func3), fill = darkcols[4], alpha = .2) + 
  geom_ribbon(data = mydata, aes(ymin = func3, ymax = 200), fill = darkcols[2], alpha = .2) +
  geom_jitter(alpha = .1, show.legend = F,aes(y = Fork_length_mm, color = Race_table, shape = Adipose)) + 
  geom_jitter(data = refuge, aes(y = Fork_length_mm, color = Race_table, shape = Adipose), alpha = .9, width = .1, show.legend = F) +
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

# Willow Bend -------------------------------------------------------------

wbendfish <- wildfish[grepl(pattern = "Willow Bend", wildfish$Location),]
wbendply <- wbendfish %>% group_by(Species, Native) %>% summarize(Totcount = sum(Count))

wbendply$Fork_length_mm <- 150

wbendfish %>% group_by(Native) %>% summarize(Totcount = sum(Count))
wbendfish[wbendfish$Species == "Chinook",] %>% group_by(Race_table) %>% summarize(Totcount = sum(Count))

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Sutter_wildfish_Jan2023_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(wbendply, aes(x = reorder(Species, (-Totcount)), y = Totcount, fill = Native)) + geom_bar(stat = "identity") +
  labs(x = NULL, y = "Count") + theme_bw() + geom_text(aes(label = Totcount, y = Totcount +2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(.88,.85), legend.title = element_blank()) 

ggplot(wbendfish, aes(x = Fork_length_mm, fill = Native)) + geom_histogram(binwidth = 2) + facet_wrap(Species ~ .) + theme_bw() +
  geom_text(data = wbendply, aes(label = paste0("N = ", Totcount), y = 25)) + theme(legend.position = "bottom") +
  labs(y = "Count", x = "Fork length (mm)")

ggplot(wbendfish[wbendfish$Species %in% "Chinook",], aes(x = Date, y = Fork_length_mm)) + 
  geom_jitter(width = 10000, height = 0, alpha = .5) + theme_bw()

ggplot(wbendfish[wbendfish$Species %in% "Chinook",], aes(x = Date, y = Weight_g)) + 
  geom_jitter(width = 10000, height = 0, alpha = .5) + theme_bw()

dev.off()

wb_chinook<-chinook %>% 
  filter(Project == "Willow Bend 2023")

ggplot(wb_chinook, aes(x = Day, y = Fork_length_mm, color=Race_table)) + 
  geom_jitter(alpha = .9) + theme_bw() +
  scale_x_continuous(limits = c(-0, 105),
                     breaks = c(-32, 1, 32, 60, 91, 122, 152), 
                     labels = c("Dec-1", "Jan-1", "Feb-1", "Mar-1", "Apr-1", "May-1", "Jun-1")) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+160)), color = "black", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+71)), color = "black", alpha = .5) +
  stat_function(fun = function(x) exp(3.516464 + 0.006574*(x+26)), color = "black", alpha = .5) +
  geom_hline(aes(yintercept = 50)) + geom_vline(aes(xintercept = 31)) + theme(legend.position = "bottom")


# Willow bend water quality -----------------------------------------------

wbdobo <- read.csv("C:/Users/jeffres/Box/PROJECTS/Sutter/DATA/Water_quality/Loggers/2023/DOBO/20073063_WillowBend_structure_2023_all_data.csv", skip = 1,
                   col.names = c("rowid", "Datetime", "DO_mgl", "Temp_c","Garbage1", "Garbage2", "Garbage3", "Garbage4", "Garbage5"))

wbdobo$Datetime <- mdy_hm(wbdobo$Datetime)
#wbdobo <- wbdobo[wbdobo$Datetime < as.POSIXct("2023-01-27"),]
ggplot(wbdobo, aes(x = Datetime, y = DO_mgl)) + geom_line() + theme_bw()

# Levellogger data --------------------------------------------------------

wbstage <- read.csv("C:/Users/jeffres/Box/PROJECTS/Sutter/DATA/Water_quality/Loggers/2023/Levellogger/32096980_Willow_Bend_all_2023_data.csv",
                  header = T, stringsAsFactors = F, skip = 11)
wbbaro <- read.csv("C:/Users/jeffres/Box/PROJECTS/Sutter/DATA/Water_quality/Loggers/2023/Levellogger/2101610_Willow_Bend_all_2023_BARO.csv",
                   header = T, stringsAsFactors = F, skip = 11)

colnames(wbstage) <- c("Date", "Time", "Garbage", "LEVEL", "Temp_c")
colnames(wbbaro) <- c("Date", "Time", "Garbage", "BARO", "Temp_c")

wbstage$Datetime <- as.POSIXct(paste(wbstage$Date, wbstage$Time), format = "%m/%d/%Y %I:%M:%S %p")
wbbaro$Datetime <- as.POSIXct(paste(wbbaro$Date, wbbaro$Time), format = "%m/%d/%Y %I:%M:%S %p")

wbbaro$MWD <- ifelse(wbbaro$BARO < 40, wbbaro$BARO * 0.703070, wbbaro$BARO * 0.101972)

wbstage <- merge(wbstage, wbbaro[, c("Datetime", "MWD")], by = "Datetime", all.x = T)

wbstage$stage <- wbstage$LEVEL - wbstage$MWD

wb_daily_stage<- wbstage %>% 
  group_by(Date) %>% 
  summarise("daily" =mean(stage)) %>% 
  filter(daily>.25)

png("C:/Users/ejholmes/Box/PROJECTS/Sutter/Output/Report_figures/Sutter_2023/Willowbend_WQ_Jan2023_%03d.png", 
    family = "serif", width = 6.5, height= 4.5, units = "in", res = 1000)

ggplot(wbstage, aes(x = Datetime, y = stage)) + 
  geom_vline(xintercept = as.POSIXct(unique(wbendfish$Date)), color = "red") +
  labs(y = "Stage (m)", x = NULL) + theme_bw() +
  geom_ribbon(aes(ymin = 0, ymax = stage), fill = "skyblue") + geom_line(size = 2)

cowplot::plot_grid(ggplot(wbdobo, aes(x = Datetime, y = DO_mgl)) + geom_line() + theme_bw() +
  xlim(range(wbstage$Datetime)) + labs(x = NULL, y = "Dissolved Oxygen (mg/L)"),
  ggplot(wbdobo, aes(x = Datetime, y = Temp_c)) + geom_line() + theme_bw() +
    xlim(range(wbstage$Datetime)) + labs(x = NULL, y = "Temperature (C)"),nrow = 2, align = "v")

dev.off()
  
