#Finglesham early medieval cemetery graphs and stats
#Author: S.A. Leggett
#Date: 2021 
library(readxl)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(qqplotr)
library(ggExtra)
library(viridis)
library(ggsci)
library(ggpubr)
library(dplyr)
library(latex2exp)
library(gridExtra)
library(cellWise)
library(BEST) #make sure you have the latest JAGS and rjags installed, if you're having problems, quit R, reinstall JAGS, open R and reinstall both rjags and BEST

#need the geom_bag() function from Ben Marwick's GitHub - https://gist.github.com/benmarwick/00772ccea2dd0b0f1745
devtools::source_gist("00772ccea2dd0b0f1745", filename = "000_geom_bag.r")
devtools::source_gist("00772ccea2dd0b0f1745", filename = "001_bag_functions.r")

#import Finglesham only data for cellwise to reduce chances of problems, limited to cells with numeric data that is individualized for burials and does not pertain to the site, and isn't free text
FING_Finglesham_Kent <- read_excel("~/ESM2.xlsx", 
                                   sheet = "cellWise_trial")
View(FING_Finglesham_Kent)

finglesham_enamel_CO3_scatter<- ggplot(FING_Finglesham_Kent,aes(enamel_d13C, enamel_d18O_chenery))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_enamel_CO3_scatter

finglesham_CO3_scatter_marg<-ggMarginal(finglesham_enamel_CO3_scatter)

ggsave(filename="Fig3.tiff", plot=finglesham_CO3_scatter_marg, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)

finglesham_bone_scatter<- ggplot(FING_Finglesham_Kent,aes(bone_d13C,bone_d15N))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_bone_scatter
finglesham_bone_scatter_marg<-ggMarginal(finglesham_bone_scatter)

ggsave(filename="Fig4.tiff", plot=finglesham_bone_scatter_marg, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)

finglesham_dentine_scatter<- ggplot(FING_Finglesham_Kent,aes(dentine_d13C,dentine_d15N))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_dentine_scatter
finglesham_dentine_scatter_marg<-ggMarginal(finglesham_dentine_scatter)

ggsave(filename="Fig5.tiff", plot=finglesham_dentine_scatter_marg, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)

#change over time Finglesham
fing_violin_date_DELTA18O<-ggplot(data=FING_Finglesham_Kent, aes(x=date_cat, y=FING_Finglesham_Kent$DELTA18O_dwMAP, fill=date_cat))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-5,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))
fing_violin_date_DELTA18O

ggsave(filename="Fig6.tiff", plot=fing_violin_date_DELTA18O, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)

#bagplot dentine vs. bone Finglesham
Finglesham_match_bone_dent<-read_excel("~/ESM2.xlsx", 
                                       sheet = "matched_bone_dent")

View(Finglesham_match_bone_dent)

Finglesham_Bone_Dent_Bag<- ggplot(Finglesham_match_bone_dent, aes(d13C, d15N, colour = `Bone or Dentine`, fill = `Bone or Dentine`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Finglesham_Bone_Dent_Bag

ggsave(filename="Fig7.tiff", plot=Finglesham_Bone_Dent_Bag, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)


#BEST test of this
Bone_FING_matched_all<-subset(Finglesham_match_bone_dent, `Bone or Dentine`=="Bone") #English human bone C&N matched
Dentine_FING_matched_all<-subset(Finglesham_match_bone_dent, `Bone or Dentine`=="Dentine")#English human dentine C&N matched 
FINGBoneC_matchedall<-c(Bone_FING_matched_all$d13C)
FINGDentineC_matchedall<-c(Dentine_FING_matched_all$d13C)
carbonFING1_matched<-na.omit(FINGBoneC_matchedall)
carbonFING2_matched<-na.omit(FINGDentineC_matchedall)
BESTout_FING_collC_matched_all <- BESTmcmc(carbonFING1_matched, carbonFING2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_collC_matched_all)
plotAll(BESTout_FING_collC_matched_all)

FINGBoneN_matched_all<-c(Bone_FING_matched_all$d15N)
FINGDentineN_matched_all<-c(Dentine_FING_matched_all$d15N)
nitrogenFING1_matched<-na.omit(FINGBoneN_matched_all)
nitrogenFING2_matched<-na.omit(FINGDentineN_matched_all)
BESTout_FING_collN_matched_all<- BESTmcmc(nitrogenFING1_matched, nitrogenFING2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_collN_matched_all)
plotAll(BESTout_FING_collN_matched_all)

FING_carboffset_dent_bag<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_enamel_dent, dentine_d15N)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{15},N["dentine coll"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carboffset_dent_bag

FING_carboffset_dent_bagC<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_enamel_dent, enamel_d13C)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carboffset_dent_bagC

FING_change_overlife_bag<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_dent_bone, DELTA15N_dent_bone)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{15},N["dentine-bone"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_change_overlife_bag

FING_carbonD_carbdent_bag<-ggplot(FING_Finglesham_Kent, aes(dentine_d13C, DELTA13C_enamel_dent)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine coll (PDB)"]," (\u2030)")))+
  scale_x_continuous(limits=c(-20.5,-19.2),breaks=seq(-20.5,-19.2,0.5))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carbonD_carbdent_bag

FING_carb_offsets_x4<-ggarrange(FING_carboffset_dent_bag, FING_carboffset_dent_bagC, FING_change_overlife_bag, FING_carbonD_carbdent_bag,  labels = c("A", "B", "C", "D"), font.label=list(size=22), ncol = 2, nrow = 2)
FING_carb_offsets_x4

ggsave(filename="Fig8.tiff", plot=FING_carb_offsets_x4, device="tiff",
       path="~/", height=10.82, width=20, units="in", dpi=600)

#differences between males and females
FING_Finglesham_Kent$SimpleSex<-FING_Finglesham_Kent$age_cat
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F4', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F4/5', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F5', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F5/6', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F6', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M4', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M4/5', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M5', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M6', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))

#oxygen
FING_sex_DELTA18O<-ggplot(data=FING_Finglesham_Kent, aes(x=`SimpleSex`, y=DELTA18O_dwMAP, fill=SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, colour="white")+
  geom_jitter(position = position_jitter(width = 0.001, height = 0.001))+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Osteological Sex")))+
  scale_y_continuous(limits=c(-4,3),breaks=seq(-4,3,1))+
  theme(legend.position = "none",axis.title = element_text(size = 20), axis.text = element_text(size=18))
FING_sex_DELTA18O

ggsave(filename="Fig9.tiff", plot=FING_sex_DELTA18O, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)


#best test for that
FING_Female<-subset(FING_Finglesham_Kent, SimpleSex=="F")
FING_Male<-subset(FING_Finglesham_Kent, SimpleSex=="M")
FING_sex1 <- c(FING_Female$DELTA18O_dwMAP)
FING_sex2 <- c(FING_Male$DELTA18O_dwMAP)
FING_sex1<-na.omit(FING_sex1)
FING_sex2<-na.omit(FING_sex2)
BESTout_FING_sex_mobility <- BESTmcmc(FING_sex1, FING_sex2, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_sex_mobility)
plotAll(BESTout_FING_sex_mobility)

#collagen
FING_Bone_sex_Bag<- ggplot(FING_Finglesham_Kent, aes(bone_d13C, bone_d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco(name="Osteological Sex")+
  scale_colour_jco(name="Osteological Sex")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_Bone_sex_Bag

ggsave(filename="Fig10.tiff", plot=FING_Bone_sex_Bag, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)

FING_dent_sex_Bag<- ggplot(FING_Finglesham_Kent, aes(dentine_d13C, dentine_d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco(name="Osteological Sex")+
  scale_colour_jco(name="Osteological Sex")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_dent_sex_Bag

ggsave(filename="Fig11.tiff", plot=FING_dent_sex_Bag, device="tiff",
       path="~/", height=10.76, width=18.04, units="in", dpi=600)


#cellWise
FING_DDC<-FING_Finglesham_Kent
FING_DDC <- data.frame(FING_DDC)
rownames(FING_DDC) <- FING_DDC[,1] #make ID numbers row numbers 
View(FING_DDC)

# Default options for DDC:
DDCpars = list(fracNA = 0.5, numDiscrete = 3, precScale = 1e-12,
               cleanNAfirst = "automatic", tolProb = 0.99, 
               corrlim = 0.5, combinRule = "wmean",
               returnBigXimp = FALSE, silent = FALSE,
               nLocScale = 25000, fastDDC = FALSE,
               standType = "1stepM", corrType = "gkwls",
               transFun = "wrap", nbngbrs = 100)

# A small list giving the same results:
DDCpars = list(fastDDC = FALSE)

DDC_finglesham = DDC(FING_DDC,DDCpars)

remX = DDC_finglesham$remX; dim(remX)

cellMap(D=remX, R=DDC_finglesham$stdResid, rowlabels = rownames(remX), 
        columnlabels = colnames(remX))
#now to make it prettier
ggpDDC = cellMap(D=remX, 
                 R=DDC_finglesham$stdResid,
                 indcells=DDC_finglesham$indcells,
                 indrows=DDC_finglesham$indrows,
                 rowlabels=rownames(remX),
                 columnlabels=colnames(remX),                 
                 mTitle="DetectDeviatingCells",
                 rowtitle = "Grave Number",
                 columntitle = "Variable",
                 sizetitles = 2.0,
                 autolabel=T)
plot(ggpDDC) 
# Red cells have higher value than predicted, blue cells lower,
# white cells are missing values, and all other cells are yellow.
#have to chang column and row names to make it look ok
rowlabels=c("6","8","15","18","21A","21B", "26A","30","47B","48","57", "61","62B","63","64", "72", "73","82","84","105",  
            "113", "116", "121", "123","124","125A","129A", "129B", "135", "138","144", "145A", "150", "158", "165","175",
            "180", "193", "199","208")
columnlabels=c("height","number of grave goods", "grave orientation", "bone d13C", "bone d15N","dentine d13C","dentine d15N","enamel d13C","enamel d18O (Chenery)",
               "D13C dent-bone","D15N dent-bone","D13C enamel-dent","D18O dw-MAP")

ggpDDC2 = cellMap(D=remX, 
                  R=DDC_finglesham$stdResid,
                  indcells=DDC_finglesham$indcells,
                  indrows=DDC_finglesham$indrows,
                  rowlabels=rowlabels,
                  columnlabels=columnlabels,                 
                  mTitle="DetectDeviatingCells",
                  rowtitle = "Grave Number",
                  columntitle = "Variable",
                  sizetitles = 2.0,
                  autolabel=T)
FING_DDC2<-plot(ggpDDC2)

ggsave(filename="Fig12.tiff", plot=FING_DDC2, device="tiff",
       path="~/", height=16, width=20, units="in", dpi=600)
