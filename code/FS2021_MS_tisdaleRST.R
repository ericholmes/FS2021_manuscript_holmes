## Tisdale RST cummulative unclipped juvenile salmon catch
## Data from SACPAS: http://www.cbr.washington.edu/sacramento/data/query_sampling_graph.html
## Author: Eric Holmes

# Load packages + functions -----------------------------------------------

library(ggplot2)
library(lubridate)
library(tidyr)

hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

# Load datasets -----------------------------------------------------------

testdat <- read.csv("data/Tisdale_rst/TisRSTFall_unclipped_2009.csv", 
                    skip = 1, header = F, col.names = c("Date", "Catch"))
files <- list.files("data/Tisdale_rst")
winterfiles <- list.files("data/Tisdale_rst", pattern = "Winter")
sflffiles <- files[!(files %in% winterfiles)]

windat <- data.frame()
for(i in winterfiles){
  tmp <- read.csv(paste0("data/Tisdale_rst/", i),skip = 1, header = F, col.names = c("Date", "Catch"))
  tmp$Filename <- i
  tmp$BroodYear <- as.numeric(substr(tmp$Filename, nchar(tmp$Filename)-7, nchar(tmp$Filename)-4))
  tmp$Catch_nona <- tmp$Catch
  for(j in 1:nrow(tmp)){
    tmp[j, "Catch_nona"] <- ifelse(is.na(tmp[j, "Catch_nona"]) == T, tmp[j - 1, "Catch_nona"], tmp[j, "Catch_nona"])
  }
  tmp$StandCatch <- (tmp$Catch_nona - min(tmp$Catch_nona, na.rm = T)) / 
    (max(tmp$Catch_nona, na.rm = T) - min(tmp$Catch_nona, na.rm = T))
  tmp$DailyCatch <- tmp$Catch_nona - c(0, head(tmp$Catch_nona, -1))
  tmp$DailyStandCatch <- tmp$StandCatch - c(NA, head(tmp$StandCatch, -1))
  print(paste(i, nrow(tmp)))
  windat <- rbind(windat, tmp)
}

sflfdat <- data.frame()
for(y in 2010:2021){
  subfiles <- sflffiles[grep(sflffiles, pattern = y)]
  for(i in subfiles){
    assign(i, read.csv(paste0("data/Tisdale_rst/", i),skip = 1, header = F, col.names = c("Date", "Catch")))
  }
  tmpdat <- 
    # merge(
    merge(get(subfiles[1]), get(subfiles[2]), by = "Date", all = T)
    # , Cget(subfiles[3]), by = "Date", all = T)
  
  tmpdat$sumcumcatch <- rowSums(tmpdat[,c(2:3)], na.rm = T)
  tmp <- tmpdat[, c(1,4)]
  colnames(tmp) <- c("Date", "Catch")
  tmp$Filename <- y
  tmp$BroodYear <- y
  tmp$Catch_nona <- tmp$Catch
  for(j in 2:nrow(tmp)){
    # tmp[j, "Catch_nona"] <- ifelse(is.na(tmp[j, "Catch_nona"]) == T, tmp[j - 1, "Catch_nona"], tmp[j, "Catch_nona"])
    print(paste(j, tmp[j, "Catch_nona"], tmp[j - 1, "Catch_nona"] - tmp[j, "Catch_nona"]))
    tmp[j, "Catch_nona"] <- ifelse(tmp[j - 1, "Catch_nona"] - tmp[j, "Catch_nona"] == tmp[j, "Catch_nona"], 
                                   tmp[j - 1, "Catch_nona"], tmp[j, "Catch_nona"])
    
  }
  tmp$StandCatch <- (tmp$Catch_nona - min(tmp$Catch_nona, na.rm = T)) / 
    (max(tmp$Catch_nona, na.rm = T) - min(tmp$Catch_nona, na.rm = T))
  tmp$DailyCatch <- tmp$Catch_nona - c(0, head(tmp$Catch_nona, -1))
  tmp$DailyStandCatch <- tmp$StandCatch - c(NA, head(tmp$StandCatch, -1))
  sflfdat <- rbind(sflfdat, tmp)
}
windat$Run <- "Winter"
sflfdat$Run <- "SFLF"
dat <- rbind(windat, sflfdat)

dat$Date <- as.Date(dat$Date)
dat$Year <- as.numeric(format(dat$Date, format = "%Y"))
dat$jday <- as.numeric(format(dat$Date, format = "%j"))
dat$wyjday <- hydro.day.new(dat$Date)
dat$adjday <- ifelse(dat$wyjday>300, dat$wyjday - 365, dat$wyjday)
# dat$binom <- dat$Pass2/100

# dat$Run <- ifelse(grepl(dat$Filename, pattern = "Winter"), "Winter",
#                   ifelse(grepl(dat$Filename, pattern = "Spring"), "Spring",
#                          ifelse(grepl(dat$Filename, pattern = "Fall"), "Fall",
#                                 ifelse(grepl(dat$Filename, pattern = "LateFall"), "LateFall", "Other"))))
# dat$BroodYear <- as.numeric(substr(dat$Filename, nchar(dat$Filename)-7, nchar(dat$Filename)-4))

# Cummulative catch curves ------------------------------------------------

if(saveplots == TRUE){png("C:/Users/ejholmes/Box/Holmes/Presentations/Floodplain_symposium/Output/Migration_timing_%03d.png", 
                          family = "serif", res = 700, height = 6, width = 5, units = "in")}

ggplot(dat[dat$Run == "Winter",], aes(x = adjday, y = StandCatch)) + 
  # geom_vline(data = dat[dat$Run != "LF" & dat$Pass2 == 50,], aes(xintercept = adjday), alpha = .5) +
  # geom_ribbon(data = dat[dat$Run != "LF" & dat$Pass2 %in% c(25,75),], 
  #             aes(ymax = 1, ymin = 0, group = Year), alpha = .15) +
  geom_path(aes(group = BroodYear), alpha = .2) + 
  # facet_grid(BroodYear ~ .) + 
  labs(y = "Proportion of total catch", x = NULL) +
  theme(axis.text.x = element_text(face = "bold")) + theme_classic() +
  scale_x_continuous(breaks = c(-30, 0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304), 
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "black")

sflf <- dat[dat$Run == "SFLF",]

ggplot(dat[dat$Run == "SFLF",], aes(x = wyjday, y = StandCatch)) + 
  # geom_vline(data = dat[dat$Run != "LF" & dat$Pass2 == 50,], aes(xintercept = adjday), alpha = .5) +
  # geom_ribbon(data = dat[dat$Run != "LF" & dat$Pass2 %in% c(25,75),], 
  #             aes(ymax = 1, ymin = 0, group = Year), alpha = .15) +
  geom_path(aes(group = BroodYear), alpha = .2) + 
  facet_grid(BroodYear ~ .) +
  labs(y = "Proportion of total catch", x = NULL) +
  theme(axis.text.x = element_text(face = "bold")) + theme_classic() +
  scale_x_continuous(breaks = c(-30, 0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304), 
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "black")


dev.off()

rethinking::dens(dat[dat$Run == "Winter", "adjday"])
rethinking::dens(dat[dat$Run == "Winter", "wyjday"])


ggplot(dat[dat$Run == "Winter",] ) + 
  stat_smooth(aes(x = wyjday, y = DailyStandCatch))# + stat_density(adjust = 2)

ggplot(dat[dat$Run == "Winter",] ) + 
  stat_smooth(aes(x = wyjday, y = Catch_nona))

dat[dat$Run == "Winter",] %>% uncount(Catch_nona)
datuncount <- dat[dat$Run == "Winter",] %>% uncount(DailyCatch)

ggplot(dat[dat$Run == "Winter",] %>% uncount(DailyCatch) ) + 
  geom_density(aes(x = adjday), adjust = .2) + facet_grid(BroodYear ~ ., scales = "free") +
  scale_x_continuous(breaks = c(-30, 0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304), 
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))

ggplot(dat[dat$Run == "Spring",] %>% uncount(DailyCatch) ) + 
  geom_density(aes(x = adjday, group = BroodYear), adjust = .1) + facet_grid(Year ~ ., scales = "free") +
  scale_x_continuous(breaks = c(-30, 0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304), 
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))

denthing <- density(datuncount[datuncount$BroodYear == 2021, "adjday"] )
plot(denthing)
