saveplots = F
library(tidyverse)
wytype <- read.csv("data/CDEC_wsihist.csv")

downloadNWISdaily <- function(site_no, parameterCd, startDT, endDT){
  temp <- read.table(paste0("https://waterservices.usgs.gov/nwis/dv/?sites=", site_no,
                            "&parameterCd=", parameterCd,
                            "&startDT=", startDT,
                            "&endDT=", endDT,
                            "&siteStatus=all&format=rdb"),
                     skip = 30, fill = TRUE, stringsAsFactors = F, 
                     col.names = c("Agency", "Site_no", "Date", "Param_val", "qc_code"))
  temp$parameterCd <- parameterCd
  return(temp)
}

hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

paramsdf <- data.frame("parameterCd" = c("00060"),
                       "parameterLabel" = c("Discharge"))

nwis_sitesdf <-data.frame(Site_no = c("11342000", "11370500", "11377100", "11425500"),
                          site_code = c("SRDELTA", "KESWICK", "BEND", "VERONA"))
startDT = "1890-01-01"
endDT = "2023-12-31"
##janky for-loop, replace with vectorized apply function?
datparams <- data.frame()
for(site in nwis_sitesdf$Site_no){
  for(i in paramsdf$parameterCd){
    print(paste("site:", site, "param:", i))
    tempdat <- downloadNWISdaily(site, i, startDT, endDT)
    datparams <- rbind(datparams, tempdat)
  }
}

datparams <- merge(merge(datparams, paramsdf, by = "parameterCd", all.x = T),
                   nwis_sitesdf, by = "Site_no", all.x = T)
datparams$Date <- as.Date(datparams$Date, format = "%Y-%m-%d")
datparams$Year <- as.numeric(format(datparams$Date, format = "%Y"))
datparams$Month <- as.numeric(format(datparams$Date, format = "%m"))
datparams$WY <-  ifelse(datparams$Month %in% c(10:12), datparams$Year + 1, datparams$Year)
datparams$wyjday <- hydro.day.new(datparams$Date)
datparams$Param_val <- as.numeric(datparams$Param_val)

datparams <- datparams[is.na(datparams$parameterLabel) == F &
                         is.na(datparams$Date) == F &
                         is.na(datparams$Param_val) == F,]

##Download SHA reservoir storage

temp <- read.table("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=SHA&SensorNums=15&dur_code=M&Start=1953-10-1&End=2023-12-1",
                   header=FALSE, sep=",", skip=1, stringsAsFactors = F)

temp <- temp[,c(5,7)]
colnames(temp) <- c("Datetime", "Param_val")
temp$Site_no <- "SHA"
temp$parameterCd <- "Storage"
temp$Datetime <- as.POSIXct(temp$Datetime, format = "%Y%m%d %H%M")
temp$Date <- as.Date(temp$Datetime, format = "%Y-%m-%d")
temp$Year <- as.numeric(format(temp$Date, format = "%Y"))
temp$Month <- as.numeric(format(temp$Date, format = "%m"))
temp$WY <-  ifelse(temp$Month %in% c(10:12), temp$Year + 1, temp$Year)
temp$Storage <- as.numeric(temp$Param_val)

# calculate CT ------------------------------------------------------------

##Center of water year timing
datparams$cttop <- datparams$wyjday*datparams$Param_val

dat_ct <- datparams %>% filter(WY<2024) %>% group_by(site_code) %>% 
  mutate(cttop = wyjday*Param_val) 

CT <- dat_ct %>% group_by(site_code, WY) %>% 
  summarize(t = sum(cttop), b = sum(Param_val), CT = t/b)

CT <- merge(CT, wytype, by = "WY", all.x = T)

if(saveplots == T){png("/Output/Verona/Verona_Flood_typing_CT_trends%03d.png", 
                       family = "serif", width = 6.5, height= 4.5, units = "in", res = 500, pointsize = 7.5)}

(CTdam <- ggplot(CT, aes(y = CT, x = WY, color = site_code, fill = site_code)) + geom_point() +
    geom_vline(xintercept = c(1944,1968)) + 
    labs(y = "Water year center of mass", x = "year") +
    stat_smooth(method = "loess", se = T, span = .6) + theme_bw())

(CTdamsrd <- ggplot(CT[CT$site_code == "SRDELTA",], 
                 aes(y = CT, x = WY, color = Yr.type, fill = Yr.type)) + geom_point() +
    geom_vline(xintercept = c(1944,1968)) + 
    labs(y = "Water year center of mass", x = "year") +
    stat_smooth(method = "lm", se = F, span = .6) + theme_bw())

(CTdamver <- ggplot(CT[CT$site_code == "VERONA",], 
                 aes(y = CT, x = WY, color = Yr.type, fill = Yr.type)) + geom_point() +
    geom_vline(xintercept = c(1944,1968)) + 
    labs(y = "Water year center of mass", x = "year") +
    stat_smooth(method = "lm", se = F, span = .6) + theme_bw())

dev.off()

# compare spring and fall flow trends -------------------------------------

springflow <- datparams %>% filter(WY < 2024 & Month %in% c(1:2)) %>% group_by(site_code, WY) %>% 
  summarize(meanflow = mean(Param_val)) %>% group_by(site_code) %>% 
  mutate(percmax = meanflow/max(meanflow))

ggplot(springflow[springflow$site_code %in% c("SRDELTA", "KESWICK"),], 
       aes(x = WY, y = percmax, color = site_code)) + 
  geom_point(alpha = .1) + labs(title = "Winter/spring (Dec-Apr) Flow percent of max") +
  geom_line(stat = "smooth", span = .1) + theme_bw()

springflowwide <- springflow %>% pivot_wider(id_cols = WY, names_from = site_code, values_from = percmax)
springstorage <- temp %>% filter(Month %in% c(1:2)) %>%  group_by(WY) %>% 
  summarize(meanstorage = mean(Storage))

springflowwide <- merge(springflowwide, springstorage, by = "WY", all.x = T)
springflowwide$inoutdiff <- springflowwide$SRDELTA/springflowwide$KESWICK

ggplot(springflowwide, aes(x = SRDELTA, y = KESWICK, color = meanstorage)) + geom_point() + theme_bw()
ggplot(springflowwide, aes(x = meanstorage, y = inoutdiff, color = WY)) + geom_point() + theme_bw()
ggplot(springflowwide, aes(x = WY, y = inoutdiff, color = meanstorage)) + geom_point() + theme_bw()

summerflow <- datparams %>% filter(WY < 2024 & WY > 1891 & Month %in% 7:9) %>% group_by(site_code, WY) %>% 
  summarize(meanflow = mean(Param_val)) %>% group_by(site_code) %>% 
  mutate(percmax = meanflow/max(meanflow))

ggplot(summerflow[summerflow$site_code %in% c("SRDELTA", "BEND"),], 
       aes(x = WY, y = percmax, color = site_code)) + 
  geom_point(alpha = .5) + labs(title = "Summer (Jul-Oct) Flow percent of max") +
  geom_line(stat = "smooth", span = .6) + theme_bw()

ggplot(summerflow[summerflow$site_code %in% c("SRDELTA", "BEND"),], 
       aes(x = WY, y = meanflow, color = site_code)) + 
  geom_point(alpha = .5) + labs(title = "Summer (Jul-Oct) Flow percent of max") +
  geom_line(stat = "smooth", span = .6) + theme_bw()

summerflowwide <- summerflow %>% pivot_wider(id_cols = WY, names_from = site_code, values_from = percmax)
summerstorage <- temp %>% filter(Month %in% c(7:10)) %>%  group_by(WY) %>% 
  summarize(meanstorage = mean(Storage))

summerflowwide <- merge(summerflowwide, summerstorage, by = "WY", all.x = T)
summerflowwide$inoutdiff <- summerflowwide$SRDELTA/summerflowwide$KESWICK

ggplot(summerflowwide, aes(x = SRDELTA, y = KESWICK, color = meanstorage)) + geom_point() + theme_bw()
ggplot(summerflowwide, aes(x = meanstorage, y = inoutdiff, color = WY)) + geom_point() + theme_bw()
ggplot(summerflowwide, aes(x = WY, y = inoutdiff, color = meanstorage)) + geom_point() + theme_bw()

bend <- datparams[datparams$site_code == "BEND",]

# disproportionate effect on early season floodplain access ----
von <- datparams[datparams$site_code == "VERONA",]
von$Flow <- von$Param_val
##Water year julian day
hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

#Plot full time series to view SHasta construction and 
ggplot(von, aes(x = Date, y = Flow)) + geom_line() + 
  geom_vline(xintercept = as.Date("1968-10-01"), color = "red") + 
  geom_hline(yintercept = 40000) + theme_bw()

ggplot(von[von$Date > as.Date("1997-10-01") & von$Date < as.Date("1999-9-30"),], aes(x = Date, y = Flow)) + 
  geom_ribbon(aes(ymax = Flow, ymin = 0), alpha = .5, fill = "skyblue") + geom_line() + 
  geom_hline(yintercept = 40000, linetype = 2) + theme_bw() + scale_x_date(date_breaks = "4 months")

#Set flooding threshold
von$flood <- ifelse(von$Flow > 40000, "over", "under")
von$dam <- ifelse(von$Date >= as.Date("1968-10-1"), "Post", "Pre")
von$dam <- factor(von$dam, levels = c("Pre", "Post"))

#Discretize contiguous flood events
von$runs <- rep(seq_along(rle(von$flood)$lengths), rle(von$flood)$lengths)

#Add water year
von$Year <- as.numeric(format(von$Date, format = "%Y"))
von$Month <- as.numeric(format(von$Date, format = "%m"))
von$WY <-  ifelse(von$Month %in% c(10:12), von$Year + 1, von$Year)
von$wyjday <- hydro.day.new(von$Date)

#Provide each flood event with a unique ID
von$Flood_ID <- paste(von$WY, sprintf("%06d", von$runs), sep = "_")

# Calculate metrics for each flood event ----------------------------------

##Calculate rate of change
von$ratechange <- c(NA, diff(von$Flow))

#Identify rising or descending limbs of hydrograph
von$limb <- ifelse(von$ratechange > 0, "rising", "descending")

#Identify flood peaks and troughs
von <- merge(von, data.frame(Date = von[cumsum(rle(von$limb)$lengths), "Date"], 
                             Peak = ifelse(rle(von$limb)$values == "descending", "trough", "peak")), 
             by = "Date", all.x = T)

##Add peak location and number of peaks per flood
vonpeaks <- von %>% filter(flood == "over" & Peak == "peak") %>% group_by(Flood_ID, Peak) %>% 
  add_tally() %>% group_by(Flood_ID) %>% summarise(Peaks = mean(n))

#Calculate flood event summary metrics
vonsum <- von %>% filter(flood == "over") %>% group_by(Flood_ID, dam, WY) %>%
  summarise(startdate = min(Date), enddate = max(Date), 
            maxflow = max(Flow), meanflow = mean(Flow), sumflow = sum(Flow),
            riserate = max(ratechange), fallrate = min(ratechange))

##Merge flood summary metrics with flood peak metrics
vonsum <- merge(vonsum, vonpeaks, by = "Flood_ID", all.x = T)

##Calculate flood centroid date
vonsum$centdate <- vonsum$startdate + floor((vonsum$enddate-vonsum$startdate)/2)

##Calculate flood duration
vonsum$duration <- as.numeric(vonsum$enddate - vonsum$startdate) + 1

##Convert dates to julian water days
vonsum$startday <- hydro.day.new(vonsum$startdate)
vonsum$endday <- hydro.day.new(vonsum$enddate)
vonsum$centday <- hydro.day.new(vonsum$enddate)

##Center of flood timing prep
von$cttop <- von$wyjday*von$Flow

annfloods <- vonsum %>% filter(duration>6) %>% group_by(WY) %>% 
  summarize(totfloods = length(unique(Flood_ID)))
range(annfloods$WY)


for(i in 1930:2023){
  print(i)
  if(!(i %in% annfloods$WY)){
    annfloods <- rbind(annfloods,data.frame(WY = i, totfloods = 0))
    }
}
annfloods$Flyr <- ifelse(annfloods$totfloods > 0, 1, 0)
ggplot(annfloods, aes(x = WY, y = totfloods)) + geom_point() + stat_smooth()

1-(sum(annfloods[annfloods$WY >= 1943, "Flyr"])/nrow(annfloods[annfloods$WY >= 1943, "Flyr"]))/
  (sum(annfloods[annfloods$WY < 1943, "Flyr"])/nrow(annfloods[annfloods$WY < 1943, "Flyr"]))

von$decade <- substr(von$WY, 1, 3)

ggplot(von, aes(x = Flow)) + geom_histogram(fill = "black") + facet_grid(decade ~ Month, scales = "free_x") 

# Weibull distribution ----------------------------------------------------

library(fitdistrplus)      # we'll use this package

# day = rep(1:16, each=100)  # 1 through 16, each repeated 100x
# y   = c()                  # an empty vector to hold the data
# for(i in 1:16){            # generates data from Weibull distributions w/ the
#   y = c(y,                 #  shape parameter increasing (but decelerating) by day
#         rweibull(n=100, shape=(.5 + .1*i - .002*(i^2)), scale=1))
# }

# estimate Weibull shape & scale parameters by MLE for each WY
dt = t(sapply(split(von[von$Month %in% c(12, 1:4), "Flow"], 
                    von[von$Month %in% c(12, 1:4), "WY"]), function(x){ fitdist(x, distr="weibull")$estimate }))

range(von[von$Month %in% c(5:8), "WY"])
dt = data.frame(day=1929:2023, dt)
dt
plot(shape~day, dt)
lines(lowess(dt$day, dt$shape), col="red")

dq = t(sapply(split(von[von$Month %in% c(5:8), "Flow"], von[von$Month %in% c(5:8), "WY"]), 
              function(x){ quantile(x, probs=c(0.25, 0.5, 0.75, 0.95)) }))
dq = data.frame(day=1930:2023, dq)
names(dq) = c("day", "25%", "50%", "75%", "95%")
plot(1,1, xlim=c(1929,2023), ylim=c(0, 20000), xlab="day", ylab="value", type="n")
lines(dq$day, dq$`25%`, col="red",    lty=2)
lines(dq$day, dq$`50%`, col="black",  lty=1)
lines(dq$day, dq$`75%`, col="blue",   lty=3)
lines(dq$day, dq$`95%`, col="purple", lty=4)
legend("topright", legend=c("95%", "75%", "50%", "25%"), lty=c(4,3,1,2),
       col=c("purple", "blue", "black", "red"))


library(ggridges)
von$WYfac <- factor(von$WY)
ggplot(von, aes(x = Flow, y = WYfac)) + #scale_x_log10() +
  geom_density_ridges(scale = 4) + theme_ridges()

ggplot(von[von$WY == 1930,], aes(x = Flow)) + #scale_x_log10() +
  geom_histogram()

# Median flow hydrograph --------------------------------------------------

bend <- datparams[datparams$site_code == "BEND",]

# Robbing Peter to pay Paul  ----
# disproportionate effect on early season floodplain access
bend$Flow <- bend$Param_val
##Water year julian day
hydro.day.new = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}


#Plot full time series to view SHasta construction and 
ggplot(bend, aes(x = Date, y = Flow)) + geom_line() + 
  geom_vline(xintercept = as.Date("1968-10-01"), color = "red") + 
  geom_hline(yintercept = 40000) + theme_bw()

ggplot(bend[bend$Date > as.Date("1997-10-01") & bend$Date < as.Date("1999-9-30"),], aes(x = Date, y = Flow)) + 
  geom_ribbon(aes(ymax = Flow, ymin = 0), alpha = .5, fill = "skyblue") + geom_line() + 
  geom_hline(yintercept = 40000, linetype = 2) + theme_bw() + scale_x_date(date_breaks = "4 months")

#Set flooding threshold
bend$flood <- ifelse(bend$Flow > 40000, "over", "under")
bend$dam <- ifelse(bend$Date >= as.Date("1944-10-1"), "Post", "Pre")
bend$dam <- factor(bend$dam, levels = c("Pre", "Post"))

#Discretize contiguous flood events
bend$runs <- rep(seq_along(rle(bend$flood)$lengths), rle(bend$flood)$lengths)

#Add water year
bend$Year <- as.numeric(format(bend$Date, format = "%Y"))
bend$Month <- as.numeric(format(bend$Date, format = "%m"))
bend$WY <-  ifelse(bend$Month %in% c(10:12), bend$Year + 1, bend$Year)
bend$wyjday <- hydro.day.new(bend$Date)

medflow <- bend %>% group_by(dam, wyjday) %>% 
  summarise(medianflow = median(Flow), meanflow = mean(Flow, na.rm = TRUE),
            sdflow = sd(Flow, na.rm = TRUE),
            nflow = n()) %>%
  mutate(seflow = sdflow / sqrt(nflow),
         lower.ciflow = meanflow - qt(1 - (0.05 / 2), nflow - 1) * seflow,
         upper.ciflow = meanflow + qt(1 - (0.05 / 2), nflow - 1) * seflow)

# bend$period <- cut(bend$WY, seq(1890, 2030, 10), labels = paste(seq(1890, 2020, 10), "s", sep = ""))
bend$period <- case_when(bend$WY < 1915 ~ "prediv",
                         bend$WY > 1915 & bend$WY < 1944 ~ "divert",
                         bend$WY > 1944 ~ "distribute")

periodflow <- bend %>% filter(WY < 2024 & wyjday < 365) %>% 
  group_by(period, wyjday) %>% 
  summarise(medianflow = median(Flow), meanflow = mean(Flow, na.rm = TRUE),
            sdflow = sd(Flow, na.rm = TRUE),
            nflow = n()) %>%
  mutate(seflow = sdflow / sqrt(nflow),
         lower.ciflow = meanflow - qt(1 - (0.05 / 2), nflow - 1) * seflow,
         upper.ciflow = meanflow + qt(1 - (0.05 / 2), nflow - 1) * seflow,
         lower.qiflow = meanflow - qt(1 - (0.05 / 2), nflow),
         upper.qiflow = meanflow + qt(1 - (0.05 / 2), nflow))

meanflow <- data.frame(bend %>% filter(Month %in% c(12,1:4)) %>% group_by(dam, WY, period) %>% 
                         summarise(meanflow = mean(Flow, na.rm = T), medianflow = median(Flow, na.rm = T)))

summerflow <- data.frame(bend %>% filter(Month %in% c(5:10)) %>% group_by(dam, WY, period) %>% 
                           summarise(meanflow = mean(Flow, na.rm = T), medianflow = median(Flow, na.rm = T)))
# meanflow <- bend %>% filter(Month %in% c(12,1:4)) %>% group_by(dam, WY, period) %>% summarise(meanflow = sum(Flow, na.rm = T))
# wytype <- read.csv("C:/Users/ejholmes/Box/Holmes/CV_floodtyping/Data/CDEC_Water_year_class.csv")
# wytype$dacr <- wytype$Dec + wytype$Jan + wytype$Feb + wytype$Mar + wytype$Apr

meanflow <- merge(meanflow, wytype, by = "WY", all.x = T)
# meanflow$Type <- ifelse(meanflow$WY %in% 2021, "C", meanflow$Yr.Type)
meanflow$Yr.typefac <- factor(meanflow$Yr.type, levels = c("W", "AN", "BN",   "D",  "C"))
levels(meanflow$Yr.typefac) <- c("Wet", "Above normal", "Below normal", "Dry", "Critical")

# png("C:/Users/ejholmes/Box/Holmes/CV_floodtyping/Output/Verona/Verona_meanflow%03d.png", 
#     family = "serif", width = 6.5, height= 5, units = "in", res = 700, pointsize = 7.5)

(postdam <- ggplot(meanflow[meanflow$WY > 1968,], aes(x = meanflow)) + geom_histogram(binwidth = 2000, fill = "#0097d4", alpha = .5) +
    geom_vline(xintercept = unlist(meanflow[meanflow$WY %in% c("2019", "2020", "2021"), "meanflow"]), 
               color = c("navy","darkgreen", "red"), size = 2) + theme_bw() +
    coord_cartesian(xlim=range(meanflow[meanflow$WY > 8,"meanflow"])) +
    labs(x = "Mean Dec-Apr discharge (cfs)", y = "Frequency") + 
    annotate("text", x = 48000, y = 6.5, label = "Post Oroville dam", fontface = "bold") +
    annotate("text", x = c(37000, 11900, 7400), y = 7, 
             label = c("2019", "2020", "2021"), 
             color = c("navy","darkgreen", "red"), angle = 90, fontface = "bold"))

(predam <- ggplot(meanflow[meanflow$WY <= 1968,], aes(x = meanflow)) + geom_histogram(binwidth = 2000, fill = "brown", alpha = .5) +
    xlim(range(meanflow[meanflow$WY > 1968,"meanflow"])) +
    annotate("text", x = 48000, y = 5.5, label = "Pre Oroville dam", fontface = "bold") +
    theme_bw() + labs(x = NULL, y = "Frequency"))

vals <- unlist(meanflow[meanflow$WY %in% 2011:2021, "meanflow"])

virpal <- viridis::plasma(n = 5)

nrow(meanflow[meanflow$meanflow < 24000,])/nrow(meanflow)
nrow(meanflow[meanflow$meanflow > 24000 & meanflow$meanflow < 42000,])/nrow(meanflow)
nrow(meanflow[meanflow$meanflow > 42000,])/nrow(meanflow)

png("output/SacR_at_Bend_medianflow%03d.png",
    family = "serif", width = 5, height= 4, units = "in", res = 1000, pointsize = 7.5)

ggplot(medflow, aes(x = wyjday, fill = dam)) + #facet_grid(dam ~ ., scales = "fixed") +
  geom_ribbon(aes(ymin = lower.ciflow, ymax = upper.ciflow), alpha = .25) +
  geom_hline(yintercept = 20000, color = "black", linetype = 2) +
  labs(x = NULL) +
  # geom_line(stat = "smooth", aes(y = meanflow, color = dam), method = "loess", span = .07, size = 1, se = F) +
  geom_line(aes(y = meanflow, color = dam), size = 1) +
  theme_bw() + 
  scale_color_manual(values = c("#8EA7E9", "#B06161")) +
  scale_fill_manual(values = c("#8EA7E9", "#B06161")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), 
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

medflow$meanflow.cms <- 0.028316847 * medflow$meanflow
medflow$upper.ciflow.cms <- 0.028316847 * medflow$upper.ciflow
medflow$lower.ciflow.cms <- 0.028316847 * medflow$lower.ciflow
medflow$damfac <- factor(medflow$dam, levels = c("Pre", "Post"), labels = c("Pre-Shasta dam: 1891-1943",
                                                                            "Post-Shasta dam: 1944-2023"))

ggplot(medflow, aes(x = wyjday, fill = damfac)) + #facet_grid(dam ~ ., scales = "fixed") +
  geom_ribbon(aes(ymin = lower.ciflow.cms, ymax = upper.ciflow.cms), alpha = .25) +
  labs(x = NULL, y = "Discharge (cms)", fill = NULL, color = NULL) +
  # geom_line(stat = "smooth", aes(y = meanflow.cms, color = dam), method = "loess", span = .07, size = 1, se = F) +
  geom_line(aes(y = meanflow.cms, color = damfac), size = 1) +
  theme_bw() + theme(legend.justification = c(1, 1), legend.position = c(1, 1),legend.box.margin=margin(c(5,5,5,5))) +
  scale_y_continuous(breaks = seq(0,1000, 200)) +
  scale_color_manual(values = c("#8EA7E9", "#B06161")) +
  scale_fill_manual(values = c("#8EA7E9", "#B06161")) +
  scale_x_continuous(breaks = c(0, 31, 61, 92, 123, 151, 182, 212, 243, 273, 304, 334), 
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
dev.off()
