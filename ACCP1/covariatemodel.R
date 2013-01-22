#read nonmem output
setwd('C:/Users/Devin/SkyDrive/UMSOP/tutorials/accp1nonmem/test')
dat <- read.table (file='CS1_CLcr_Wt.fit', skip=1, header=T)
head(dat)

#time may need to be set to 0 for IPRED and PRED vs DV
out <- dat[dat$TIME>0,]
out <- dat
#set code results to output to repository wd
setwd('C:/Users/Devin/Documents/nonmem/ACCP1')

# #code from datainspector
# dat <- subset (dat, ID %in% c(1))
# plot (x=dat$TIME, y=dat$CONC, type='p', pch=19, col='darkblue',
#       xlab='TIME', ylab='CONC', main='CS1_IV1ESTFPDF.fit' )

# Conc-time plots---------------------------------------------------------------
library(ggplot2)
library(grid)


head(out)
out$ID <- as.factor(out$ID)
out$DOSE <- as.factor(out$DOSE)
out$ISM <- as.factor(out$ISM)

#for individual
out1 <- subset (out, ID %in% c(1))
ggplot(data = out1, aes(x = TIME, y = CONC)) +
  geom_point() + 
  geom_line(aes(x = TIME, y = IPRED), color = "black", size = 1) +
  geom_line(aes(x = TIME, y = PRED), color = "blue", size = 1)

#for small group of individuals in dose group 100
outinds <- subset (out, ID %in% c(1, 8, 14, 20, 26, 30))
summary(outinds)
ggplot(data = outinds, aes(x = TIME, y = CONC)) +
  geom_point() + 
  geom_line(aes(x = TIME, y = IPRED), color = "black", size = 1) +
  geom_line(aes(x = TIME, y = PRED), color = "blue", size = 1, linetype = "dashed") +
  facet_wrap(~ID, ncol=3) +
  scale_x_continuous(name = "Time (hr)") +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.x = element_text(size=16)) +
  scale_y_continuous(name = "Concentration (ug/mL)") +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.title.y = element_text(size=16))

#observed vs predicted concentrations base
ggplot(data = out) +
  geom_point(aes(x = CONC, y = IPRED), color = "#56B4E9") + 
  geom_point(aes(x = CONC, y = PRED), color = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  xlim(0,125) + xlab("Observed Conc (ug/mL)") +
  ylim(0,125) + ylab("Predicted Conc (ug/mL)") +
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.x = element_text(size=16)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.title.y = element_text(size=16))

#Covariate Relationship(s) on Clearance----------------------------------------
clcr_cl <- ggplot(data = out, aes(x = CLCR, y = ETA1)) +
  geom_point() + geom_smooth(se = FALSE, method = "loess", size = 1.3) +
  scale_x_continuous(name = "Creatine Clearance (ml/min") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on CL") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

wt_cl <- ggplot(data = out, aes(x = WT, y = ETA1)) +
  geom_point() + geom_smooth(se = FALSE, method = "loess", size = 1.3) +
  scale_x_continuous(name = "WT (kg)") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on CL") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

age_cl <- ggplot(data = out, aes(x = AGE, y = ETA1), size = 1.3) +
  geom_point() + geom_smooth(se = FALSE, method = "loess") +
  scale_x_continuous(name = "Age (yrs)") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on CL") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

gender_cl <- qplot(x = ISM, y = ETA1, data = out, geom="boxplot") + 
  xlab("GENDER: 0 = Female, 1 = Male") +
  ylab("Estimated Random Effect ETA on CL") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

multiplot(clcr_cl, wt_cl, age_cl, gender_cl, cols = 2)

#Covariate Relationship(s) on Volume--------------------------------------------
clcr_cl <- ggplot(data = out, aes(x = CLCR, y = ETA2)) +
  geom_point() + geom_smooth(se = FALSE, method = "loess", size = 1.3) +
  scale_x_continuous(name = "Creatine Clearance (ml/min") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on V") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

wt_cl <- ggplot(data = out, aes(x = WT, y = ETA2)) +
  geom_point() + geom_smooth(se = FALSE, method = "loess", size = 1.3) +
  scale_x_continuous(name = "WT (kg)") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on V") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

age_cl <- ggplot(data = out, aes(x = AGE, y = ETA2), size = 1.3) +
  geom_point() + geom_smooth(se = FALSE, method = "loess") +
  scale_x_continuous(name = "Age (yrs)") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  scale_y_continuous(name = "Estimated Random Effect ETA on V") +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

gender_cl <- qplot(x = ISM, y = ETA2, data = out, geom="boxplot") + 
  xlab("GENDER: 0 = Female, 1 = Male") +
  ylab("Estimated Random Effect ETA on V") +
  theme(axis.text.x = element_text(size=13)) +
  theme(axis.title.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=13)) +
  theme(axis.title.y = element_text(size=15))

multiplot(clcr_cl, wt_cl, age_cl, gender_cl, cols = 2)