library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# Load datasets -----------------------------------------------------------

testdat <- read.csv("data/Tisdale_rst_raw/TisRSTrawFall_unclipped_2010.csv", 
                    skip = 1, header = F, col.names = c("Date", "Catch"))
files <- list.files("data/Tisdale_rst_raw")
# winterfiles <- list.files("data/Tisdale_rst", pattern = "Winter")
# sflffiles <- files[!(files %in% winterfiles)]

dat <- data.frame()

for(i in files){
  tmp <- read.csv(paste0("data/Tisdale_rst_raw/", i),skip = 1, header = F, col.names = c("Date", "Catch"))
  tmp$Filename <- i
  tmp$BroodYear <- as.numeric(substr(tmp$Filename, nchar(tmp$Filename)-7, nchar(tmp$Filename)-4))
  dat <- rbind(dat, tmp)
} 

dat$Date <- as.Date(dat$Date)
dat$Year <- as.numeric(format(dat$Date, format = "%Y"))
dat$Month <- as.numeric(format(dat$Date, format = "%m"))
dat$WY <- ifelse(dat$Month %in% c(10,11,12), dat$Year+1, dat$Year)
dat$jday <- as.numeric(format(dat$Date, format = "%j"))
dat$wyjday <- hydro.day.new(dat$Date)
dat$adjday <- ifelse(dat$wyjday>300, dat$wyjday - 365, dat$wyjday)

datply <- dat %>% group_by(Date, wyjday, WY) %>% summarize(SumCatch = sum(Catch, na.rm = T)) %>% ungroup()

datply <- datply %>%
  group_by(WY) %>%
  mutate(freq = 100*SumCatch / sum(SumCatch, na.rm = T)) %>% filter(WY >2009 & WY<2022) %>% ungroup()

ggplot(datply, aes(x = wyjday, y = freq)) + geom_bar(stat = "identity") + facet_grid(WY ~ .)
# save(von, file = "C:/Users/ejholmes/Box/Holmes/Presentations/Floodplain_symposium/Data_output/von_floodtyped.Rdata")


# VonHydro ----------------------------------------------------------------
load("data/von_floodtyped.Rdata")
von <- von[is.na(von$Flow) == F & von$Flow > 1000,]
# saveplots = TRUE
if(saveplots == TRUE){png("output/chinook_VON_river_panel_%03d.png", 
                          family = "serif", res = 700, height = 6, width = 5, units = "in")}

ggplot(von[von$WY %in% c(2012:2021),], aes(x = wyjday, y = Flow)) + geom_line(color = "black") +
  geom_ribbon(data = von[von$WY %in% c(2012:2021),],
              aes(ymin = 0, ymax = Flow), fill = "#56B4E9") + 
  scale_fill_brewer(palette = "Set1") + labs(x = NULL, y = "Discharge (cfs)") +
  theme_classic() + labs("Month of water year") +
  theme(legend.position = "bottom", panel.border = element_blank(), strip.text.y = element_blank()) + 
  facet_grid(WY ~ .) +
  geom_bar(data = datply, aes(x = wyjday, y = freq), stat = "identity") +
  scale_y_continuous(breaks = seq(0,75000, 25000), labels = paste(seq(0,75,25),"k", sep = "")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), limits = c(0,365), 
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

facet_labs <- data.frame(WY = c(2010:2021),
                         wytype = c("BN", "W", "BN", "D", "C", "C", "BN", "W", "BN", "W", "D", "C"))
facet_labs$Lab <- paste0(facet_labs$WY, " (", facet_labs$wytype, ")")
coeff = 2000

ggplot() + 
  geom_line(data = von[von$WY %in% c(2010:2021),], aes(x = wyjday, y=Flow / coeff), color="skyblue") +
  geom_ribbon(data = von[von$WY %in% c(2010:2021) & von$Flow > 25000,],
              aes(x = wyjday, ymin = 0, ymax = Flow / coeff, group = Flood_ID), fill = "skyblue") + 
  geom_bar(data = von[von$WY %in% c(2010:2021) & von$Flow > 50000,], aes(x = wyjday, y = Flow / coeff), 
           fill = "red", stat = "identity", alpha = .1, width = 3) +
  facet_grid(WY ~ .) +
  geom_text(data = facet_labs,aes(x = 330, y = 39, label = Lab), fontface = "bold", size = 3) +
  scale_y_continuous( name = "Chinook % of total catch", breaks = seq(0,max(datply$freq, na.rm = T), 10),# first axis title
                      sec.axis = sec_axis(~.*coeff, breaks = seq(0,100000,25000), 
                                          labels = paste(seq(0,100,25),"K", sep = ""), name = "Discharge (cfs)")) + 
  labs(x = NULL) +
  geom_bar(data = datply, aes(x = wyjday, y = freq), stat = "identity", width = 2, fill = "black") + 
  theme_classic() + theme(strip.text.y = element_blank()) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), 
                     labels = c("O", "N", "D", "J", "F", "M", "A", "M", "J", "J", "A", "S")) 
dev.off()

png("output/chinook_VON_river_wrap_%03d.png", 
    family = "serif", res = 1000, height = 4, width = 6.5, units = "in")
ggplot() + 
  geom_ribbon(data = von[von$WY %in% c(2010:2021) & von$Flow > 25000,],
              aes(x = wyjday, ymin = 0, ymax = Flow / coeff, group = Flood_ID), fill = "skyblue") + 
  geom_bar(data = von[von$WY %in% c(2010:2021) & von$Flow > 50000,], aes(x = wyjday, y = Flow / coeff), 
           fill = "red", stat = "identity", alpha = .1, width = 3) +
  geom_line(data = von[von$WY %in% c(2010:2021),], aes(x = wyjday, y=Flow / coeff), color="grey") +
  facet_wrap(WY ~ .) + labs(x = "Month of water year") +
  geom_text(data = facet_labs,aes(x = 60, y = 39, label = Lab), fontface = "bold", size = 2.5) +
  scale_y_continuous( name = "Chinook % of total WY catch", breaks = seq(0,max(datply$freq, na.rm = T), 10),# first axis title
                      sec.axis = sec_axis(~.*coeff, breaks = seq(0,100000,25000), 
                                          labels = paste(seq(0,100,25),"K", sep = ""), name = "Discharge (cfs)")) + 
  labs(x = NULL) +
  geom_bar(data = datply, aes(x = wyjday, y = freq), stat = "identity", width = 2, fill = "black") + 
  theme_classic() + theme(strip.text.x = element_blank()) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), 
                     labels = c("O", "", "D", "", "F", "", "A", "", "J", "", "A", "")) 

von$Flow.cms <- 0.028316847 * von$Flow
coeff.cms = 0.028316847 *2000

ggplot() + 
  geom_ribbon(data = von[von$WY %in% c(2010:2021) & von$Flow.cms > 25000*0.028316847,],
              aes(x = wyjday, ymin = 0, ymax = Flow.cms / coeff.cms, group = Flood_ID), fill = "skyblue") + 
  geom_bar(data = von[von$WY %in% c(2010:2021) & von$Flow.cms > 50000*0.028316847,], aes(x = wyjday, y = Flow.cms / coeff.cms), 
           fill = "red", stat = "identity", alpha = .1, width = 3) +
  geom_line(data = von[von$WY %in% c(2010:2021),], aes(x = wyjday, y=Flow.cms / coeff.cms), color="grey") +
  facet_wrap(WY ~ .) + labs(x = "Month of water year") +
  geom_text(data = facet_labs,aes(x = 60, y = 39, label = Lab), fontface = "bold", size = 2.5) +
  scale_y_continuous( name = "Chinook % of total WY catch", breaks = seq(0,max(datply$freq, na.rm = T), 10),# first axis title
                      sec.axis = sec_axis(~.*coeff.cms, breaks = seq(0,3000,500), name = "Discharge (cms)")) + 
  geom_bar(data = datply, aes(x = wyjday, y = freq), stat = "identity", width = 2, fill = "black") + 
  theme_classic() + theme(strip.text.x = element_blank()) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), 
                     labels = c("O", "", "D", "", "F", "", "A", "", "J", "", "A", "")) 
dev.off()

ggplot() + 
  geom_line(data = von[von$WY %in% c(2011:2021),], aes(x = wyjday, y=Flow / coeff), color="skyblue")+
facet_wrap(WY ~ .)

ggplot(von[von$WY %in% c(2012:2021),], aes(x = wyjday, y = Flow)) + geom_line(color = "black") +
  geom_ribbon(data = von[von$WY %in% c(2012:2021) & von$Flow > 25000,],
              aes(ymin = 0, ymax = Flow, group = Flood_ID), fill = "#56B4E9") + 
  scale_fill_brewer(palette = "Set1") + labs(x = NULL, y = "Discharge (cfs)") +
  theme_classic() + theme(legend.position = "bottom", panel.border = element_blank()) + facet_grid(WY ~ .) +
  scale_y_continuous(breaks = seq(0,75000, 25000), labels = paste(seq(0,75,25),"k", sep = "")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), limits = c(0,365),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

ggplot(von[von$WY %in% c(2012:2021),], aes(x = wyjday, y = Flow)) + geom_line(color = "black") +
  geom_ribbon(data = von[von$WY %in% c(2012:2021) & von$Flow > 25000,],
              aes(ymin = 0, ymax = Flow, group = Flood_ID), fill = "#56B4E9") + 
  geom_bar(data = von[von$WY %in% c(2012:2021) & von$Flow > 50000,], fill = "red", stat = "identity") +
  scale_fill_brewer(palette = "Set1") + labs(x = NULL, y = "Discharge (cfs)") +
  theme_classic() + theme(legend.position = "bottom", panel.border = element_blank()) + facet_grid(WY ~ .) +
  scale_y_continuous(breaks = seq(0,75000, 25000), labels = paste(seq(0,75,25),"k", sep = "")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), limits = c(0,365),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))


ggplot(von[von$WY %in% c(2012:2021),], aes(x = wyjday, y = Flow)) + geom_line(color = "black") +
  geom_ribbon(data = von[von$WY %in% c(2012:2021) & von$Flow > 50000,],
              aes(ymin = 0, ymax = Flow, group = Flood_ID), fill = "#56B4E9") + 
  scale_fill_brewer(palette = "Set1") + labs(x = NULL, y = "Discharge (cfs)") +
  theme_classic() + theme(legend.position = "bottom", panel.border = element_blank()) + facet_grid(WY ~ .) +
  scale_y_continuous(breaks = c(0,40000, 80000), labels = c("0", "40k", "80k")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), limits = c(0,365),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
dev.off()