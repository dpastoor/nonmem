#read nonmem output
setwd('C:/Users/Devin/SkyDrive/UMSOP/nonmem/piranaoutput/test1/')
dat <- read.table (file='CS1_IV1ESTFPDF.fit', skip=1, header=T)
head(dat)
out <- dat[dat$TIME>0,]

#set code results to output to repository wd
setwd('C:/Users/Devin/Documents/nonmem/ACCP1')

# #code from datainspector
# dat <- subset (dat, ID %in% c(1))
# plot (x=dat$TIME, y=dat$CONC, type='p', pch=19, col='darkblue',
#       xlab='TIME', ylab='CONC', main='CS1_IV1ESTFPDF.fit' )

# Conc-time plots---------------------------------------------------------------
library(ggplot2)
plot <- ggplot(data = y, aes(x = xaxis, y = value, color = V5)) +
  geom_point(aes(shape = V4), size = 5) +
  geom_line(aes(group = variable, linetype = V4), size = 1.2) +
  labs(color = "# per group", shape = "Method", linetype = "Method") +    #if don't set linetype to same name will separate to 3 legends
  geom_hline(aes(yintercept = 0.9), linetype="dashed", size = 1.05) +
  scale_color_discrete(name = "# per group",
                       labels = c("20/group", "30/group", "45/group", "60/group")) +
  scale_linetype_discrete(name = "Method", labels = c("t-test", "Model")) +
  scale_shape_discrete(name = "Method", labels = c("t-test", "Model")) +
  scale_x_continuous(name = "% Effect due to Clobazam") +
  scale_y_continuous(name = "Power") +
  #to edit out grid lines
  #theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.title.x = element_text(size=20)) +
  theme(axis.title.y = element_text(size=20)) +
  theme(axis.text.x = element_text(size=20)) +
  theme(axis.text.y = element_text(size=20)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) + 
  theme(legend.key.width= unit(2, "lines")) #part of grid

head(out)
out$ID <- as.factor(out$ID)
out$DOSE <- as.factor(out$DOSE)

#for individual
outind <- subset (out, ID %in% c(1))
ggplot(data = outind, aes(x = TIME, y = CONC)) +
  geom_point() + 
  geom_line(aes(x = TIME, y = IPRED), color = "black", size = 1) +
  geom_line(aes(x = TIME, y = PRED), color = "blue", size = 1)
  