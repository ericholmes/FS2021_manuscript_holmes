library(ggplot2)
library(plyr)
library(viridis)
library(reshape2)

saveplots = TRUE
load("data/Sutter2018_flow_data.RDATA")

inundate <- read.csv("data/Inundation_timesteps2.csv", header = T, stringsAsFactors = F)

inundate$Acres <- inundate$Water_prop * 4729.367326
inundate$Hectares <- inundate$Water_prop * 1913.907054

vdcdec <- ddply(na.omit(von[von$Flow > 0, ]), .(Date), summarize, Flow = mean(Flow), Stage = mean(Stage))

# fit <- lm(Acres ~ log(Flow), data = inundate)
fit <- lm(Water_prop ~ poly(Flow, 2, raw = T), data = inundate)

stagefit <- lm(Water_prop ~ poly(Stage, 2, raw = T), data = inundate)
# fit <- lm(Water_prop ~ -1/Stage, data = inundate)
summary(fit)

a <- as.numeric(coef(fit)[3])
b <- as.numeric(coef(fit)[2])
c <- as.numeric(coef(fit)[1])

zero <- (-b + sqrt(b^2 - 4 * a * c))/(2*a)
fifty <- (-b + sqrt(b^2 - 4 * a * (c-.5)))/(2*a)
fre <- 50000

vd$Acres <- predict(fit, newdata = list(Flow = vd$Flow))
vd$Acres <- ifelse(vd$Acres >= 0, vd$Acres, ifelse(is.na(vd$Acres) == T, NA, 0))
vd$Day <- as.numeric(format(vd$Date, format = "%j"))
vd$Month <- as.numeric(format(vd$Date, format = "%m"))
vd$Monthname <- format(vd$Date, format = "%b")
vd$Year <- as.numeric(format(vd$Date, format = "%Y"))
vd$wy <- ifelse(vd$Month %in% c(10:12), vd$Year + 1, vd$Year)
vd$wyday <- ifelse(vd$Month %in% c(10:12), vd$Day - 273, vd$Day + 92)

vdsub <- vd[vd$Month %in% c(1:3),]
vdsub$Flowrank <- rank(-vdsub$Flow)
vdsub$prob <- 100 * (vdsub$Flowrank / (nrow(vdsub)+1))
vdsub <- vdsub[order(vdsub$Flow),]

probfun <- approxfun(vdsub$prob ~ vdsub$Flow, method = "linear")
flowfun <- approxfun(vdsub$Flow ~ vdsub$prob, method = "linear")

if(saveplots == TRUE){png("output/Sutter_inundate-flow_%03d.png", 
                          height = 3.5, width = 6.5, units = "in", res = 500, family = "serif")}

ggplot(inundate, aes(x = Stage, y = Water_prop)) + geom_point() + 
  stat_smooth(method = "lm", formula =  y ~ poly(x,2), se = F) + ylim(c(0,1.05)) +
  theme_bw()

ggplot(inundate, aes(x = Flow, y = Water_prop)) + geom_point() + 
  stat_smooth(method = "lm", formula =  y ~ poly(x,2), se = F) +
  theme_bw()

##two panel inundation stage relationship and flow duration curve
par(mfrow = c(1,2))
par(mar = c(4,4,0,1), oma = c(0,0,1,0))

##inundation flow relationship
plot(Water_prop ~ Flow, inundate, ylim = c(0,1), xlim = c(zero, max(inundate$Flow)), xaxt = "n",
     xlab = "Flow (1000 cfs)", ylab = "Proportion inundated")
axis(1, at = seq(0,100000, 10000), labels = seq(0,100, 10))
abline(h = seq(0, 1, .10), lty = 3, col = rgb(0,0,0,.2))
abline(v = seq(0, 100000, 10000), lty = 3, col = rgb(0,0,0,.2))

lines(seq(from=20000,to=80000,length.out=1000),
      predict(fit,newdata=list(Flow=seq(from=20000,to=80000,length.out=1000))))

polygon(c(seq(from = zero, to = fre, length.out = 1000), rev(seq(from = zero, to = fre, length.out = 1000))),
        c(predict(fit,newdata=list(Flow=seq(from=zero,to=fre,length.out=1000))), 
          rep(0,1000)), col = rgb(1,0,0,.3))

polygon(c(seq(from = fre, to = 85000, length.out = 1000), rev(seq(from = fre, to = 85000, length.out = 1000))),
        c(predict(fit,newdata=list(Flow=seq(from=fre,to=85000,length.out=1000))), 
          rep(0,1000)), col = rgb(0,0,1,.3))
abline(0,0)

##flow duration curve
plot(Flow ~ prob, data = vdsub, type = "l", ylim = c(0, max(vdsub$Flow)), yaxt = "n", 
     xlab = "Probablity", ylab = "Flow (1000 cfs)")
axis(2, at = seq(0,100000, 20000), labels = seq(0,100, 20))
abline(v = seq(0, 100, 10), lty = 3, col = rgb(0,0,0,.2))
abline(h = seq(0, 100000, 10000), lty = 3, col = rgb(0,0,0,.2))
# segments(x0 = probfun(zero), y0 = 0, x1 = probfun(zero), y1 = zero, lwd = 1)
segments(x0 = -20, y0 = zero, x1 = probfun(zero), y1 = zero, lwd = 1)
# segments(x0 = probfun(fre), y0 = 0, x1 = probfun(fre), y1 = fre, lwd = 1)
segments(x0 = -20, y0 = fre, x1 = probfun(fre), y1 = fre, lwd = 1)

polygon(c(seq(from = probfun(zero), to = probfun(fre), length.out = 2006), 
          rev(seq(from = probfun(zero), to = probfun(fre), length.out = 2006))),
        c(vdsub[vdsub$Flow >= flowfun(probfun(zero)) & vdsub$Flow <= flowfun(probfun(fre)), "Flow"],
          rep(-1,2006)), col = rgb(1,0,0,.3))

polygon(c(seq(from = probfun(fre), to = 0, length.out = 1869), 
          rev(seq(from = probfun(fre), to = 0, length.out = 1869))),
        c(vdsub[vdsub$Flow >= flowfun(probfun(fre)), "Flow"],
          rep(-1,1869)), col = rgb(0,0,1,.3))

if(saveplots == TRUE)dev.off()

plot(vd$Flow ~ vd$Date, type = "l")

ggplot(vd[vd$wy == 2018,], aes(x = wyday, y = Acres)) + geom_ribbon(aes(ymax = Acres, ymin = 0)) + facet_grid(wy ~ .)

nrow(vd[vd$wy == 2018 & vd$Acres > 0,])
vy <- ddply(vd, .(wy), summarize, adpy = sum(Acres))
nrow(vy[vy$adpy > 1, ])/nrow(vy)
nrow(vy[vy$adpy > 10, ])/nrow(vy)

ad <- ddply(vd[is.na(vd$Acres)==F,], .(Month, Monthname, wy), summarize, acredays = sum(Acres))
unique(ad$Monthname)

ad$Monthfac <- factor(ad$Monthname, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
ad$thousacredays <- ad$acredays/1000
ad$thousacredays <- ifelse(ad$thousacredays > 0, ad$thousacredays, NA)

if(saveplots == TRUE){png("X:/knaggs/Experiment_2018/Sutter_cages/Output/Sutter_acre-days_usgs_%03d.png", 
                          height = 4.5, width = 6.5, units = "in", res = 500, family = "serif")}

ggplot(ad, aes(x = Monthfac, y = wy)) + geom_tile(aes(fill = thousacredays)) + 
  scale_x_discrete( c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  scale_fill_viridis("1000 acre-days", option = "C",breaks = c(seq(0, 150, 50)), guide = guide_colorbar(direction = "horizontal"), 
                     na.value = NA, direction = 1) +
  scale_y_continuous(breaks=seq(1930,2020,5)) +
  theme_bw() + labs(list(x = "", y = "Water year\n")) + theme(legend.position = "bottom",axis.title.x=element_blank())

if(saveplots == TRUE)dev.off()

adcast <- dcast(data = ad[, c("Monthname", "wy", "acredays")], wy ~ Monthname, value.var = "acredays")

adcast$Peaksalmon <- adcast$Jan + adcast$Feb + adcast$Mar
barplot(adcast$Peaksalmon)
abline(h = 50000)

sum(ifelse(adcast$Peaksalmon > 18000, 1, 0))/length(adcast$Peaksalmon)

# Stage version -----------------------------------------------------------

a <- as.numeric(coef(stagefit)[3])
b <- as.numeric(coef(stagefit)[2])
c <- as.numeric(coef(stagefit)[1])

zero <- (-b + sqrt(b^2 - 4 * a * c))/(2*a)
fifty <- (-b + sqrt(b^2 - 4 * a * (c-.5)))/(2*a)
fre <- 29.521

vdcdec$Acres <- predict(stagefit, newdata = list(Stage = vdcdec$Stage))
vdcdec$Acres <- ifelse(vdcdec$Acres >= 0, vdcdec$Acres, ifelse(is.na(vdcdec$Acres) == T, NA, 0))
vdcdec$Acres <- vdcdec$Acres * 4729.367326
vdcdec$Date <- as.Date(vdcdec$Date)
vdcdec$Day <- as.numeric(format(vdcdec$Date, format = "%j"))
vdcdec$Month <- as.numeric(format(vdcdec$Date, format = "%m"))
vdcdec$Monthname <- format(vdcdec$Date, format = "%b")
vdcdec$Year <- as.numeric(format(vdcdec$Date, format = "%Y"))
vdcdec$wy <- ifelse(vdcdec$Month %in% c(10:12), vdcdec$Year + 1, vdcdec$Year)
vdcdec$wyday <- ifelse(vdcdec$Month %in% c(10:12), vdcdec$Day - 273, vdcdec$Day + 92)

vdcdecsub <- vdcdec[vdcdec$Month %in% c(1:3),]
vdcdecsub$Stagerank <- rank(-vdcdecsub$Stage)
vdcdecsub$prob <- 100 * (vdcdecsub$Stagerank / (nrow(vdcdecsub)+1))
vdcdecsub <- vdcdecsub[order(vdcdecsub$Stage),]

stageprobfun <- approxfun(vdcdecsub$prob ~ vdcdecsub$Stage, method = "linear")
stagefun <- approxfun(vdcdecsub$Stage ~ vdcdecsub$prob, method = "linear")

stageflowpoly <- lm(Stage ~ poly(Flow, 3), data = vdcdec)
predict(stageflowpoly, newdata = list(Flow = 50000))


if(saveplots == TRUE){png("X:/knaggs/Experiment_2018/Sutter_cages/Output/Sutter_inundate-stage_%03d.png", 
                          height = 3.5, width = 6.5, units = "in", res = 500, family = "serif")}

##two panel inundation stage relationship and flow duration curve
par(mfrow = c(1,2))
par(mar = c(4,4,0,1), oma = c(0,0,1,0))

##inundation flow relationship
plot(Water_prop ~ Stage, inundate, ylim = c(0,1), xlim = c(zero, max(inundate$Stage)), #xaxt = "n",
     xlab = "Stage (ft)", ylab = "Proportion inundated")
axis(1, at = seq(20,40, 5), labels = seq(20,40, 5))
abline(h = seq(0, 1, .10), lty = 3, col = rgb(0,0,0,.2))
abline(v = seq(20,40, 5), lty = 3, col = rgb(0,0,0,.2))

lines(seq(from=20,to=40,length.out=1000),
      predict(stagefit,newdata=list(Stage=seq(from=20,to=40,length.out=1000))))

polygon(c(seq(from = zero, to = fre, length.out = 1000), rev(seq(from = zero, to = fre, length.out = 1000))),
        c(predict(stagefit,newdata=list(Stage=seq(from=zero,to=fre,length.out=1000))), 
          rep(0,1000)), col = rgb(1,0,0,.3))

polygon(c(seq(from = fre, to = 40, length.out = 1000), rev(seq(from = fre, to = 40, length.out = 1000))),
        c(predict(stagefit,newdata=list(Stage=seq(from=fre,to=40,length.out=1000))), 
          rep(0,1000)), col = rgb(0,0,1,.3))
abline(0,0)

##flow duration curve
plot(Stage ~ prob, data = vdcdecsub, type = "l", ylim = c(0, max(vdcdecsub$Stage)), #yaxt = "n", 
     xlab = "Probability", ylab = "Stage (ft)")
# axis(2, at = seq(0,100000, 20000), labels = seq(0,100, 20))
abline(v = seq(0, 100, 10), lty = 3, col = rgb(0,0,0,.2))
abline(h = seq(0, 40, 10), lty = 3, col = rgb(0,0,0,.2))
# segments(x0 = probfun(zero), y0 = 0, x1 = probfun(zero), y1 = zero, lwd = 1)
segments(x0 = -20, y0 = zero, x1 = stageprobfun(zero), y1 = zero, lwd = 1)
# segments(x0 = probfun(fre), y0 = 0, x1 = probfun(fre), y1 = fre, lwd = 1)
segments(x0 = -20, y0 = fre, x1 = stageprobfun(fre), y1 = fre, lwd = 1)

polygon(c(seq(from = stageprobfun(zero), to = stageprobfun(fre), length.out = 627), 
          rev(seq(from = stageprobfun(zero), to = stageprobfun(fre), length.out = 627))),
        c(vdcdecsub[vdcdecsub$Stage >= stagefun(stageprobfun(zero)) & vdcdecsub$Stage <= stagefun(stageprobfun(fre)), "Stage"],
          rep(0,627)), col = rgb(1,0,0,.3))

polygon(c(seq(from = stageprobfun(fre), to = 0, length.out = 613), 
          rev(seq(from = stageprobfun(fre), to = 0, length.out = 613))),
        c(vdcdecsub[vdcdecsub$Stage >= stagefun(stageprobfun(fre)), "Stage"],
          rep(0,613)), col = rgb(0,0,1,.3))

if(saveplots == TRUE)dev.off()

# heatmap ----------------------------------------------------------------
ggplot(vdcdec[vdcdec$wy > 1991,], aes(x = wyday, y = Acres)) + geom_ribbon(aes(ymax = Acres, ymin = 0)) + facet_grid(wy ~ .)
plot(Flow ~ Date, vdcdec[vdcdec$Year == 1994,], type = "l")
plot(Stage ~ Date, vdcdec[vdcdec$Year == 1994,], type = "l")

ad <- ddply(vdcdec[is.na(vdcdec$Acres)==F,], .(Month, Monthname, wy), summarize, acredays = sum(Acres))
unique(ad$Monthname)

ad$Monthfac <- factor(ad$Monthname, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
ad$thousacredays <- ad$acredays/1000
ad$thousacredays <- ifelse(ad$thousacredays > 0, ad$thousacredays, NA)

if(saveplots == TRUE){png("X:/knaggs/Experiment_2018/Sutter_cages/Output/Sutter_acre-days_cdec_%03d.png", 
                          height = 4.5, width = 6.5, units = "in", res = 500, family = "serif")}

ggplot(ad, aes(x = Monthfac, y = wy)) + geom_tile(aes(fill = thousacredays)) + 
  scale_x_discrete( c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) +
  # scale_fill_viridis("1000 acre-days", option = "C", na.value = NA) +
  scale_fill_viridis("1000 acre-days", option = "C", breaks = c(seq(0, 150, 50)), guide = guide_colorbar(direction = "horizontal"),
                     na.value = NA, direction = 1) +
  scale_y_continuous(breaks=seq(1930,2020,5)) +
  theme_bw() + labs(list(x = "", y = "Water year\n")) + theme(legend.position = "bottom", axis.title.x=element_blank())

if(saveplots == TRUE)dev.off()
