library(readxl)
#require (ggplot2)
library(lsmeans)
library(lme4)
library(data.table)
library(multcompView)
library(ggpubr)
library(tidyverse)
library(egg)
library(dplyr)
library(reshape2)
library(plyr)
#The stacked and dodged cannot be done

setwd("C:/Users/Dilliplaine/Documents/Thesis")
df1=read_excel("MD_3302019.xlsx", col_names=T)
#Subset pertinent data
Ice =subset(df1, Year == "2015" & Core %in% c("A","B","C") & Day %in% c("OR-2", "OR10") & Tank %in% c("1", "2", "4", "5", "6"))
Ice=droplevels(Ice) #drops the levels not included in the newly subsetted dataframe
Ice[, c(8,10,17:98)] <- lapply(Ice[, c(8,10,17:98)], as.numeric)
#Rename levels
Ice$Trmnt=plyr::revalue(Ice$Trmnt, c("EOY2"="PD", "BCY2"="BC", "BLY2"="OL"))
Ice$Day=plyr::revalue(Ice$Day, c("OR-2"="Pre-Oil", "OR10"="Post-Oil"))

Ice[, c(1:6)] <- sapply(Ice[, c(1:6)], as.factor)



Ice$WP_m2=Ice$Wpsec/((pi*2.5*2.5)/10000)/1000 #Scales per m2 unit
Ice$WP_m3=Ice$WP_m2*(1/(Ice$SL/100)) #Scales per m3 using section length
Ice$WOP_m2=Ice$WOPsec/((pi*2.5*2.5)/10000)/1000
Ice$WOP_m3=Ice$WOP_m2*(1/(Ice$SL/100))
Ice$Frus_m2=Ice$Frussec/((pi*2.5*2.5)/10000)/1000
Ice$Frus_m3=Ice$Frus_m2*(1/(Ice$SL/100))
Ice$XGEQV= (0.975*Ice$'EPS (ug glucose/l Ice)')+0.879 #Xanthum Gum Equivalents
which(colnames(Ice)=="XGEQV")
Ice2 =subset(Ice[,c(2,4,9,12,5,61,100,102,104,105)]) #Further subset
setnames(Ice2, old=c("XGEQV","Chl per_l_ice"), new=c("EPS_mgm3", "Chl_mgm3"))
Ice2$P1=round(Ice2$WP_m3, 0)
Ice2$P2=round(Ice2$WOP_m3, 0)
Ice2$Frusrawml=round(Ice2$Frus_m3/100000, 0)
Ice2$Frustule=round(Ice2$Frus_m3, 0)
Ice2$Chl_mgm3=round(Ice2$Chl_mgm3, 1)
Ice2$EPS_mgm3=round(Ice2$EPS_mgm3, 0)
Ice2$Tot_Dia=Ice2$WP_m3+Ice2$WOP_m3

Ice2$FrusPropl=round(((Ice2$Frus_m3/Ice2$Tot_Dia)/1000), 2)
Ice2$WPPropl=round(((Ice2$WP_m3/Ice2$Tot_Dia)/1000), 2)
Ice2$WOPPropl=round(((Ice2$WP_m3/Ice2$Tot_Dia)/1000), 2)

Ice2$S_ordinal=plyr::revalue(Ice2$S_ordinal, c("A"="Upper", "B"="Supra-oil", "C"="Sub-oil"))



Ice3=melt(Ice2)
Ice4=subset(Ice3, variable %in%  c("P1", "P2", "Frustule"))
Ice5=ddply(Ice4, c("Day", "S_ordinal", "Trmnt", "variable"), summarize, mean=mean(value))


sums=aggregate(mean~Day+S_ordinal+Trmnt, Ice5, sum)
colnames(sums)[4] <- "total"

Ice6=merge(Ice5,sums, all=T)
colnames(Ice6)[4] <- "Cell Bins"

Ice6$Rel=Ice6$mean/Ice6$total
MeanCore=aggregate(Rel~Day+Trmnt+`Cell Bins`, Ice6, mean)

  Relabun=ggplot(MeanCore, aes(Trmnt, Rel,  fill=Day, alpha=`Cell Bins`))+geom_bar(stat="identity", color="black",fill="Black", position=position_stack(reverse=TRUE))+
    theme_bw()+labs(x="Treatments", y="Relative Abundance")+scale_y_continuous(expand = c(0, .02), limits = c(0, 1))+
    theme(  legend.position="top",
          legend.box="horizontal",
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(0,-10,-10,-10),
          strip.background = element_rect(fill="white"),
          strip.text.y=element_text(size=10,face="bold"),
          axis.text=element_text(size=8, color="black"),
          axis.title=element_text(size=10,face="bold"),
          axis.text.y=element_text(face="bold"),
          legend.background=element_blank(),
          legend.title = element_text(size=8,face="bold"),
            legend.key.size =unit(3.5, "mm"),
          legend.text = element_text( size=8,face="bold"),
          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          plot.background=element_blank(),
          panel.border = element_rect(color="black"), axis.ticks = element_line(color="black"))+

    facet_wrap(~Day, ncol=1, strip.position=c("right"))+coord_flip()


    ggsave("Rellarge.tiff", Relabun,
    path="C:/Users/Dilliplaine/Desktop/Most Recent Manuscript/FINAL 01272020/Polar Biology/BODIL+ROLF", height=42, width=84, units="mm", dpi=600)
