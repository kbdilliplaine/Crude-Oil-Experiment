library(ggplot2)
library(lubridate)
library(dplyr)

all30 = read.csv("C:/Users/Dilliplaine/Desktop/Most Recent Manuscript/Eric Collins 112019/all30min.csv",stringsAsFactors=F)
all30$at650 = -log(0.17/as.numeric(all30$linePARMean))/2.66
all30$at450 = -log(0.17/as.numeric(all30$linePARMean))/21.8
all30$at650[all30$at650<0] = 0
all30$at650min = -log(0.17/as.numeric(all30$linePARMinimum))/21.8
all30$at650max = -log(0.17/as.numeric(all30$linePARMaximum))/21.8
all30$at650max[all30$at650max<0] = 0
all30$at650min[all30$at650min<0] = 0

#Make Date only
all30$Day=as.Date(all30$startDateTime)
library(plyr)
allday = ddply(all30, c("Day"), summarise,
                mean = mean(at650, na.rm = TRUE),
                sd   = sd(at650, na.rm = TRUE),
                max=max(at650max, na.rm=TRUE),
                min=min(at650min, na.rm=TRUE))
                allday$ymax=allday$mean+allday$sd
                allday$ymin=allday$mean-allday$sd
                allday$ymin[allday$ymin<0] = 0


                NEONlight=  ggplot(allday, aes(Day, mean, ymax=ymax, ymin=ymin))+geom_line()+geom_ribbon(alpha=0.4)+
                    labs(x="Year", y="Oil Thickness (mm)")+
                    theme(axis.text=element_text(size=8, color="black"),
                          axis.title=element_text(size=10,face="bold"),
                          legend.key.size =unit(3.5, "mm"),
                          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          panel.border = element_rect(color="black", fill=NA))

                ggsave("NEONlightlarge.tiff", NEONlight,
                       path="C:/Users/Dilliplaine/Desktop", height=42, width=84, units="mm", dpi=600)
