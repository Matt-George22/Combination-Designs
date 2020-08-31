setwd("//lancs/homes/45/georgem1/My Documents/DISS")
library(magrittr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(xtable)

load("boin_sim.RData")
load("key_sim.RData")
load("pipe_sim.RData")
load("sfd_sim.RData")
load("blrm_sim.RData")

#######################################################################################

# Data for SFD and BLRM

#load("sim_sfd1.RData")
#load("sim_sfd2.RData")
#load("sim_sfd3.RData")
load("sim_blrm1.RData")
load("sim_blrm2.RData")
load("sim_blrm2a.RData")
load("sim_blrm3.RData")
m_PCS=c(PCS1,PCS2,PCS2a,PCS3)
m_PAS=c(PAS1,PAS2,PAS2a,PAS3)
m_PCE=c(PCE1,PCE2,PCE2a,PCE3)
m_PAE=c(PAE1,PAE2,PAE2a,PAE3)
m_OVER=c(OVER1,OVER2,OVER2a,OVER3)
m_PEOVER=c(PEOVER1,PEOVER2,PEOVER2a,PEOVER3)
m_ZERO=c(ZERO1,ZERO2,ZERO2a,ZERO3)
m_ntox=c(ntox1,ntox2,ntox2a,ntox3)
m_npat=c(npat1,npat2,npat2a,npat3)
m_EXTRA=c(EXTRA1,EXTRA2,EXTRA2a,EXTRA3)
m_SEL=c(SEL1,SEL2,SEL2a,SEL3)
m_EXP=c(EXP1,EXP2,EXP2a,EXP3)

#save(s_PCS,s_PAS,s_PCE,s_PAE,s_OVER,s_PEOVER,s_ZERO,s_ntox,s_npat,s_EXTRA,s_SEL,s_EXP,file="sfd_sim.RData")
#save(m_PCS,m_PAS,m_PCE,m_PAE,m_OVER,m_PEOVER,m_ZERO,m_ntox,m_npat,m_EXTRA,m_SEL,m_EXP,file="blrm_sim.RData")

####################################################################################

## PCS and PAS - Graph 1
b_PCS=b_PCS[-c(14,15)]; b_PAS=b_PAS[-c(14,15)]
k_PCS=k_PCS[-c(14,15)]; k_PAS=k_PAS[-c(14,15)]
p_PCS=p_PCS[-c(14,15)]; p_PAS=p_PAS[-c(14,15)]
s_PCS=s_PCS[-c(14,15)]; s_PAS=s_PAS[-c(14,15)]
m_PCS=m_PCS[-c(14,15)]; m_PAS=m_PAS[-c(14,15)]
av1=mean(b_PCS); av2=mean(k_PCS); av3=mean(p_PCS); av4=mean(s_PCS); av5=mean(m_PCS)
av11=mean(b_PAS); av22=mean(k_PAS); av33=mean(p_PAS); av44=mean(s_PAS); av55=mean(m_PAS)
sc=factor(c(1:13,"Mean"))
scen=factor(sc,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","Mean"))

df4 <- data.frame(sc=scen, b_PCS=c(b_PCS,av1), k_PCS=c(k_PCS,av2), p_PCS=c(p_PCS,av3), s_PCS=c(s_PCS,av4), m_PCS=c(m_PCS,av5))
dfmelt4 <- melt(df4, id.vars="sc", value.name="Selection", variable.name = "Method")
df5 <- data.frame(sc=scen, b_PAS=c(b_PAS,av11), k_PAS=c(k_PAS,av22), p_PAS=c(p_PAS,av33), s_PAS=c(s_PAS,av44), m_PAS=c(m_PAS,av55))
dfmelt5 <- melt(df5, id.vars="sc", value.name="Selection", variable.name = "Method")

pdf(file="fig_pcs.pdf",width=10,height=6)
ggplot() + geom_col(data=dfmelt4, aes(x=sc, y=Selection, fill=dfmelt4$Method, alpha=factor(1)), width=0.75, position="dodge") +
  geom_col(data=dfmelt5, aes(x=sc, y=Selection, fill=dfmelt4$Method, alpha=factor(0.5)), width=0.75, position="dodge") +
  labs(x="Scenario", y="Proportion of Selections (%)") + ylim(c(0,100)) + 
  scale_alpha_discrete(name="Selection", labels=c("Acceptable","Correct"), range=c(0.17,1)) +
  scale_fill_manual(name="Design", values=c("red","black","cyan","green","purple"), labels=c("BOIN", "KEY", "PIPE", "SFD", "BLRM")) +
  theme(axis.text=element_text(size=14), axis.title = element_text(size=14), legend.title = element_text(size=14), legend.text = element_text(size=14))
dev.off()

####################################################################################

## Overtoxic Recommendations - Graph 2
ave1=mean(b_OVER); ave2=mean(k_OVER); ave3=mean(p_OVER); ave4=mean(s_OVER); ave5=mean(m_OVER)
sc=factor(c(1:15,"Mean"))
scen=factor(sc,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","Mean"))
df0 <- data.frame(sc=scen, b_OVER=c(b_OVER,ave1), k_OVER=c(k_OVER,ave2), p_OVER=c(p_OVER,ave3), s_OVER=c(s_OVER,ave4), m_OVER=c(m_OVER,ave5))
dfmelt0 <- melt(df0, id.vars="sc", value.name="Proportion", variable.name = "Method")

pdf(file="fig_toxsel.pdf",width=10,height=6)
ggplot() + geom_col(data=dfmelt0, aes(x=sc, y=Proportion, fill=Method), width=0.75, position="dodge") +
  labs(x="Scenario", y="Proportion of Overly Toxic Selections (%)") + ylim(c(0,100)) +
  scale_fill_manual(name="Design", values=c("red","black","cyan","green","purple"), labels=c("BOIN", "KEY", "PIPE", "SFD", "BLRM")) +
  geom_hline(yintercept=25, linetype="dashed", col="red") +
  theme(axis.text=element_text(size=14), axis.title = element_text(size=14), legend.title = element_text(size=14), legend.text = element_text(size=14))
dev.off()

####################################################################################

## Overtoxic Experimentation - Graph 3
ave11=mean(b_PEOVER); ave22=mean(k_PEOVER); ave33=mean(p_PEOVER); ave44=mean(s_PEOVER); ave55=mean(m_PEOVER)
sc=factor(c(1:15,"Mean"))
scen=factor(sc,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","Mean"))
df1 <- data.frame(sc=scen, b_PEOVER=c(b_PEOVER,ave11), k_PEOVER=c(k_PEOVER,ave22), p_PEOVER=c(p_PEOVER,ave33),
                  s_OVER=c(s_PEOVER,ave44), m_PEOVER=c(m_PEOVER,ave55))
dfmelt1 <- melt(df1, id.vars="sc", value.name="Proportion", variable.name = "Method")

pdf(file="fig_toxexp.pdf",width=10,height=6)
ggplot() + geom_col(data=dfmelt1, aes(x=sc, y=Proportion, fill=Method), width=0.75, position="dodge") +
  labs(x="Scenario", y="Proportion of Overly Toxic Experimentation (%)") + ylim(c(0,100)) +
  scale_fill_manual(name="Design", values=c("red","black","cyan","green","purple"), labels=c("BOIN", "KEY", "PIPE", "SFD", "BLRM")) +
  theme(axis.text=element_text(size=14), axis.title = element_text(size=14), legend.title = element_text(size=14), legend.text = element_text(size=14))
dev.off()

####################################################################################

## Variation in Selection
library(gridExtra)
des=factor(c("BOIN", "KEY", "PIPE", "SFD", "BLRM"))
design=factor(des,levels=c("BOIN", "KEY", "PIPE", "SFD", "BLRM"))

dat2 <- data.frame(
  Design = design,
  Dose1 = c(24.4,25.9,16.4,23.8,27.1),
  Dose2 = c(24.4,27.5,18.9,25.5,32.3)
)
dat_long2 <- dat2 %>% gather("Stat", "Value", -Design)
g1=ggplot(dat_long2, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 2") +
  scale_fill_manual(values = c("cyan", "darkblue"), labels = c("(3,2)", "(2,3)")) + lims(y=c(0,60))

dat6 <- data.frame(
  Design = design,
  Dose1 = c(19.0,16.8,22.8,17.5,7.8),
  Dose2 = c(22.1,24.0,19.2,28.3,52.1),
  Dose3 = c(18.6,18.8,20.2,16.0,7.9)
)
dat_long6 <- dat6 %>% gather("Stat", "Value", -Design)
g2=ggplot(dat_long6, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 6") +
  scale_fill_manual(values = c("cyan", "blue", "darkblue"), labels = c("(3,1)", "(2,2)","(1,3)")) + lims(y=c(0,60))

dat7 <- data.frame(
  Design = design,
  Dose1 = c(22.7,19.8,23.3,21.5,9.5),
  Dose2 = c(29.8,31.4,20.1,30.3,54.4)
)
dat_long7 <- dat7 %>% gather("Stat", "Value", -Design)
g3=ggplot(dat_long7, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 7") +
  scale_fill_manual(values = c("cyan", "darkblue"), labels = c("(3,1)", "(2,2)")) + lims(y=c(0,60))

dat8 <- data.frame(
  Design = design,
  Dose1 = c(14.9,16.8,23.2,20.9,10.1),
  Dose2 = c(33.0,35.9,16.6,30.9,34.0)
)
dat_long8 <- dat8 %>% gather("Stat", "Value", -Design)
g4=ggplot(dat_long8, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 8") +
  scale_fill_manual(values = c("cyan", "darkblue"), labels = c("(3,1)", "(2,3)")) + lims(y=c(0,60))

dat9 <- data.frame(
  Design = design,
  Dose1 = c(16.7,18.8,17.8,24.5,24.4),
  Dose2 = c(30.9,29.9,24.6,28.0,14.5)
)
dat_long9 <- dat9 %>% gather("Stat", "Value", -Design)
g5=ggplot(dat_long9, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 9") +
  scale_fill_manual(values = c("cyan", "darkblue"), labels = c("(2,1)", "(1,3)")) + lims(y=c(0,60))

dat10 <- data.frame(
  Design = design,
  Dose1 = c(27.4,26.4,26.7,28.9,31.5),
  Dose2 = c(24.7,27.5,25.1,30.3,31.9)
)
dat_long10 <- dat10 %>% gather("Stat", "Value", -Design)
g6=ggplot(dat_long10, aes(x = Design, y = Value, fill = Stat, width=0.6)) + geom_col(position = "dodge") +
  labs(y="Proportion of Selections (%)", fill="Dose", x="Scenario 10") +
  scale_fill_manual(values = c("cyan", "darkblue"), labels = c("(2,1)", "(1,2)")) + lims(y=c(0,60))

pdf(file="fig_variation.pdf",width=8,height=9)
grid.arrange(g1,g2,g3,g4,g5,g6,nrow=3)
dev.off()

#############################################################################################

## Table 1

sc=factor(c(1:15,"Mean"))
scen=factor(sc,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","Mean"))

df2=data.frame(sc=scen, BOIN=round(c(b_PCE,mean(b_PCE)),1), KEY=round(c(k_PCE,mean(k_PCE)),1), PIPE=round(c(p_PCE,mean(p_PCE)),1),
               SFD=round(c(s_PCE,mean(s_PCE)),1), BLRM=round(c(m_PCE,mean(m_PCE)),1))
df3=data.frame(sc=scen, BOIN=round(c(b_PAE,mean(b_PAE)),1), KEY=round(c(k_PAE,mean(k_PAE)),1), PIPE=round(c(p_PAE,mean(p_PAE)),1),
               SFD=round(c(s_PAE,mean(s_PAE)),1), BLRM=round(c(m_PAE,mean(m_PAE)),1))
xtable(t(df2))
xtable(t(df3))

df2=data.frame(sc=scen, BOIN=round(c(b_PCS,mean(b_PCS)),1), KEY=round(c(k_PCS,mean(k_PCS)),1), PIPE=round(c(p_PCS,mean(p_PCS)),1),
               SFD=round(c(s_PCS,mean(s_PCS)),1), BLRM=round(c(m_PCS,mean(m_PCS)),1))
df3=data.frame(sc=scen, BOIN=round(c(b_PAS,mean(b_PAS)),1), KEY=round(c(k_PAS,mean(k_PAS)),1), PIPE=round(c(p_PAS,mean(p_PAS)),1),
               SFD=round(c(s_PAS,mean(s_PAS)),1), BLRM=round(c(m_PAS,mean(m_PAS)),1))
xtable(t(df2))
xtable(t(df3))

df2=data.frame(sc=scen, BOIN=round(c(b_OVER,mean(b_OVER)),1), KEY=round(c(k_OVER,mean(k_OVER)),1), PIPE=round(c(p_OVER,mean(p_OVER)),1),
               SFD=round(c(s_OVER,mean(s_OVER)),1), BLRM=round(c(m_OVER,mean(m_OVER)),1))
df3=data.frame(sc=scen, BOIN=round(c(b_PEOVER,mean(b_PEOVER)),1), KEY=round(c(PEOVER,mean(k_PEOVER)),1), PIPE=round(c(p_PEOVER,mean(p_PEOVER)),1),
               SFD=round(c(s_PEOVER,mean(s_PEOVER)),1), BLRM=round(c(m_PEOVER,mean(m_PEOVER)),1))

xtable(t(df2))
xtable(t(df3))

df2=data.frame(sc=scen, BOIN=round(c(b_ZERO,mean(b_ZERO)),1), KEY=round(c(k_ZERO,mean(k_ZERO)),1), PIPE=round(c(p_ZERO,mean(p_ZERO)),1),
               SFD=round(c(s_ZERO,mean(s_ZERO)),1), BLRM=round(c(m_ZERO,mean(m_ZERO)),1))
xtable(t(df2))


df2=data.frame(sc=scen, BOIN=round(c(b_npat,mean(b_npat)),1), KEY=round(c(k_npat,mean(k_npat)),1), PIPE=round(c(p_npat,mean(p_npat)),1),
               SFD=round(c(s_npat,mean(s_npat)),1), BLRM=round(c(m_npat,mean(m_npat)),1))
df3=data.frame(sc=scen, BOIN=round(c(b_ntox,mean(b_ntox)),1), KEY=round(c(k_ntox,mean(k_ntox)),1), PIPE=round(c(p_ntox,mean(p_ntox)),1),
               SFD=round(c(s_ntox,mean(s_ntox)),1), BLRM=round(c(m_ntox,mean(m_ntox)),1))
xtable(t(df2))
xtable(t(df3))

####################################################################################

###############################################################################################

## PIPE

load("pipe_sim.RData")
load("pipe_sima.RData")

p_PCS=p_PCS[-c(14,15)]; p_PAS=p_PAS[-c(14,15)]
p_PCSa=p_PCSa[-c(14,15)]; p_PASa=p_PASa[-c(14,15)]
sc=factor(c(1:13,"Mean"))
scen=factor(sc,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","Mean"))

df40 <- data.frame(sc=scen, p_PCS=c(p_PCS,mean(p_PCS)), p_PCSa=c(p_PCSa,mean(p_PCSa)))
dfmelt40 <- melt(df40, id.vars="sc", value.name="Selection", variable.name = "Method")
df50 <- data.frame(sc=scen, p_PAS=c(p_PAS,mean(p_PAS)), p_PASa=c(p_PASa,mean(p_PASa)))
dfmelt50 <- melt(df50, id.vars="sc", value.name="Selection", variable.name = "Method")

pdf(file="fig_pipe_pcs.pdf",width=10,height=6)
ggplot() + geom_col(data=dfmelt40, aes(x=sc, y=Selection, fill=dfmelt40$Method, alpha=factor(1)), width=0.75, position="dodge") +
  geom_col(data=dfmelt50, aes(x=sc, y=Selection, fill=dfmelt40$Method, alpha=factor(0.5)), width=0.75, position="dodge") +
  labs(x="Scenario", y="Proportion of Selections (%)") + ylim(c(0,100)) + 
  scale_alpha_discrete(name="Selection", labels=c("Acceptable","Correct"), range=c(0.17,1)) +
  scale_fill_manual(name="Design", values=c("cyan","midnightblue"), labels=c("PIPE", "PIPE (ii)")) +
  theme(axis.text=element_text(size=14), axis.title = element_text(size=14), legend.title = element_text(size=14), legend.text = element_text(size=14))
dev.off()


