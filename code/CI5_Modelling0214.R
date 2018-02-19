library(reshape2)
library(ggplot2)
library(MASS)
library(gnm)
library(dplyr)
library(RColorBrewer)

setwd("C:/Users/KINGQB7/Desktop/KHNP/Cancer Incidence/Incidence-Rate")
K.dat <- read.csv("CI5_KR.csv")
J.dat <- read.csv("CI5_JP.csv")
K.dat$Cancer <- factor(K.dat$Cancer, levels = K.dat$Cancer[1:32])
K.dat$Age <- K.dat$Age + 2.5
head(K.dat)
K.dat <- within(K.dat, Rate <- Case * 100000 / PY)
K.dat <- within(K.dat, Version <- factor(Version, levels = 9:11))
K.dat <- within(K.dat, Version <- relevel(Version, ref = "9"))
summary(K.dat)
## Period (CI5 Version) effect comparison
### can change code for male/female
### for count data
gg <- ggplot(subset(K.dat, Sex == "female"), aes(x = Age, y = Case, group = Version, colour = factor(Version))) + 
  geom_line(size = 1.5) +
  labs(title = "Period effects among incidence cases", subtitle = "Female") +
  facet_wrap(~Cancer, scales = "free_y") +
  theme(plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom",
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(title = "CI5 version"))
png("./plot/PeriodEffect_female_count.png", width = 1200, height = 800)
print(gg) # there exists the period effects.
dev.off()

gg <- ggplot(subset(K.dat, Sex == "male"), aes(x = Age, y = log(Case+1), group = Version, colour = factor(Version))) + 
  geom_line(size = 1.5) +
  labs(title = "Period effects among log(incidence cases)", subtitle = "Male") +
  facet_wrap(~Cancer, scales = "free_y") +
  theme(plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom",
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(title = "CI5 version"))
png("./plot/PeriodEffect_male_count_log.png", width = 1200, height = 800)
print(gg) # there exists the period effects.
dev.off()

### for incidence rate data
gg <- ggplot(subset(K.dat, Sex == "male"), aes(x = Age, y = Rate, group = Version, colour = factor(Version))) + 
  geom_line(size = 1.5) +
  labs(title = "Period effects among incidence rates", subtitle = "Male") +
  facet_wrap(~Cancer, scales = "free_y") +
  theme(plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom",
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(title = "CI5 version"))
png("./plot/PeriodEffect_male_rate.png", width = 1200, height = 800)
print(gg) # there exists the period effects.
dev.off()

gg <- ggplot(subset(K.dat, Sex == "female"), aes(x = Age, y = log(Rate+1), group = Version, colour = factor(Version))) + 
  geom_line(size = 1.5) +
  labs(title = "Period effects among log(incidence rates)", subtitle = "Female") +
  facet_wrap(~Cancer, scales = "free_y") +
  theme(plot.title = element_text(size = 50, face = "bold"),
        plot.subtitle = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.position = "bottom",
        strip.text = element_text(size = 15)) +
  guides(colour = guide_legend(title = "CI5 version"))
png("./plot/PeriodEffect_female_rate_log.png", width = 1200, height = 800)
print(gg) # there exists the period effects.
dev.off()
# The period effect is more large in Count data, rather than rate. 
# It seems to be the cause of the population
# We conclude that we will consider the version effect in our model.
# And perhaps, the version effect is not additive, it looks like multiplicative effect.
# With log transformation, however, the version seems to have additive effect.
# Note that almost cancer incidence are much higher in male than female, except thyroid.
# It seems better to fit model seperately for each sex.

cc = 1
cancer.dat <- subset(K.dat, Code==cc)
cancer.male <- subset(cancer.dat, Sex=="male")
cancer.female <- subset(cancer.dat, Sex=="female")


try(model.poly.a <- lm(log(Case + 1) ~ poly(Age,3) + Version, data = cancer.male))
try(model.poly.m <- lm(log(Case + 1) ~ poly(Age,3) * Version, data = cancer.male))
try(model.poi.a <- glm(Case ~ poly(Age,3) + Version, offset = log(PY/100000), data = cancer.male, family = poisson(link = log)))
try(model.poi.m <- glm(Case ~ poly(Age,3) * Version, offset = log(PY/100000), data = cancer.male, family = poisson(link = log)))
try(model.nb.a) <- glm.nb(Case ~ poly(Age,3) + Version + offset(log(PY/100000), data = cancer.male))
try(model.nb.m) <- glm.nb(Case ~ poly(Age,3) * Version + offset(log(PY/100000), data = cancer.male))

#plot(model.poi$fitted, model.poi.f$fitted)
try(model.nb <- glm.nb(Incidence ~ poly(Age,3) + Year + offset(log(Offset/100000)), data = cancer.dat))
gnm.male <- within(cancer.male, {
  lp100k <- log(PY/100000)
  lage70 <- log(Age/70)
  lage70sq <- lage70^2
  lage70qsp <- lage70sq*(Age>70)
  #yr08 <- (floor(Year-Age)-2008)/10
})
try(model.gnm <- gnm(Case ~ lage70 + lage70sq + lage70qsp + offset(lp100k), # + yr08
                     family = poisson, data = gnm.male))
