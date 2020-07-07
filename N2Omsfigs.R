setwd("/Users/katebuckeridge/OneDrive - University of Edinburgh/R/NLBELT/N2Oms")
setwd("C:/Users/kbuckeri/OneDrive - University of Edinburgh/R/NLBELT/N2Oms") ## at work


library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(lattice)
library(forcats)

#scale_shape_manual(values=c(21,21,23), name = "MAT:") + # pch
#scale_fill_manual(values=c("white","grey", "black"), name = "MAT:") + 


#### FIGURE 2

Fig2 <- read.csv("Fig2.csv", header=TRUE, sep=",") 

nh4o <- ggplot(Fig2, 
               aes(x=Temp, 
                   y=NH4o, 
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +   
  geom_errorbar(aes(ymin=NH4o-NH4ose, ymax=NH4o+NH4ose),
                colour = "black",
                size=.3,                     
                width=.2) +    
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  labs(x=NULL, y= expression(Ammonium~(mu*g~NH[4]^{"+"}-N~g^{"-1"}))) +
  theme_bw() +
  theme(legend.position = c(0.3,0.75),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

nh4o
nh4m <- ggplot(Fig2, 
               aes(x=Temp, 
                   y=NH4m,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=NH4m-NH4mse, ymax=NH4m+NH4mse),
                colour = "black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

nh4m

no3o <- ggplot(Fig2, 
             aes(x=Temp, 
                 y=NO3o,
                 colour=Region, 
                 shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=NO3o-NO3ose, ymax=NO3o+NO3ose),
                colour = "black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y= expression(Nitrate~(mu*g~NO[3]^{"-"}-N~g^{"-1"}))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("Pre-inc.","5","15","25")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

no3o

no3m <- ggplot(Fig2, 
               aes(x=Temp, 
                   y=NO3m,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=NO3m-NO3mse, ymax=NO3m+NO3mse),
                colour = "black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("Pre-inc.","5","15","25")) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

no3m

Fig2ms <- ggarrange(nh4o, nh4m, no3o, no3m,
                   ncol = 2, nrow = 2, align = "hv")
png(file="Fig2.png", units="in", width=6, height=6, res=300)
Fig2ms
dev.off()

##### FIGURE 3

Fig3 <- read.csv("Fig3.csv", header=TRUE, sep=",") 

O <- ggplot(Fig3, 
               aes(x=Temp, 
                   y=RateO,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +                      
  geom_errorbar(aes(ymin=RateO-RateOse, ymax=RateO+RateOse),
                colour="black",
                size=.3,                     
                width=.2) +                   
  labs(x=NULL, y=expression(Net~N[2]*O~production~rate~(ng~N[2]*O-N~g^{"-1"}~h^{"-1"}))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(0, 30)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

O
C <- ggplot(Fig3, 
               aes(x=Temp, 
                   y=RateC,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +                      
  geom_errorbar(aes(ymin=RateC-RateCse, ymax=RateC+RateCse),
                colour="black",
                size=.3,                     
                width=.2) +  
  labs(x=NULL, y=NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(0, 30)) +
  theme_bw() +
  theme(legend.position = c(0.3,0.75),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

C

M <- ggplot(Fig3, 
               aes(x=Temp, 
                   y=Ratem,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +   
  geom_errorbar(aes(ymin=Ratem-RateMse, ymax=Ratem+RateMse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(0, 30)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

M

Fig3ms <- ggarrange(O, C, M,
                    ncol = 3, nrow = 1, align = "hv")
png(file="Fig3.png", units="in", width=9, height=3, res=300)
Fig3ms
dev.off()

#### FIGURE 4

Fig4 <- read.csv("Fig4.csv", header=TRUE, sep=",") 

ME <- ggplot(Fig4, 
            aes(x=Temp, 
                y=Rate,
                colour=Region, 
                shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=Rate-Ratese, ymax=Rate+Ratese),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   
  labs(x=NULL, y=expression(Combination~effect~(ng~N[2]*O-N~g^{"-1"}~h^{"-1"}))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(-10, 0.9)) +
  theme_bw() +
  theme(legend.position = c(0.3,0.25),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

ME
summary(Fig4)

Fig4p <- read.csv("Fig4percent.csv", header=TRUE, sep=",") 

MEp <- ggplot(Fig4p, 
             aes(x=Temp, 
                 y=ME,
                 colour=Region, 
                 shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=ME-Mese, ymax=ME+Mese),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   
  labs(x=NULL, y=expression(Combination~effect~("%")) ) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  #scale_y_continuous(limits = c(-8, 1)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

MEp

Fig4ms <- ggarrange(ME, MEp,
                    ncol = 2, nrow = 1, align = "hv")
png(file="Fig4.png", units="in", width=6, height=3, res=300)
Fig4ms
dev.off()

##### FIGURE 5

Fig5 <- read.csv("Fig5.csv", header=TRUE, sep=",") 

O5 <- ggplot(Fig5, 
            aes(x=Temp, 
                y=O,
                colour=Region, 
                shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=O-Ose, ymax=O+Ose),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=expression(Change~"in"^{"15"}*N[2]*O~("%"))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(-16, 100)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

O5
C5 <- ggplot(Fig5, 
            aes(x=Temp, 
                y=C,
                colour=Region, 
                shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=C-Cse, ymax=C+Cse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(-16, 100)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

C5

M5 <- ggplot(Fig5, 
            aes(x=Temp, 
                y=M,
                colour=Region, 
                shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=M-Mse, ymax=M+Mse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(-16, 100)) +
  theme_bw() +
  theme(legend.position = c(0.25,0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 

M5

Fig5ms <- ggarrange(O5, C5, M5,
                    ncol = 3, nrow = 1, align = "hv", common.legend = TRUE, legend = "right")
png(file="Fig5.png", units="in", width=11, height=3, res=300)
Fig5ms
dev.off()

#### FIGURE 6 

Fig6 <- read.csv("Fig6.csv", header=TRUE, sep=",") 

niro <- ggplot(Fig6, 
             aes(x=Temp, 
                 y=nirO,
                 colour=Region, 
                 shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nirO-nirOse, ymax=nirO+nirOse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=expression(italic(nirS)~abundance~(x10^{"6"}~g^{"-1"}))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(limits = c(0, 1.4)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
niro

nirm <- ggplot(Fig6, 
               aes(x=Temp, 
                   y=nirM,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nirM-nirMse, ymax=nirM+nirMse),
                colour = "black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(limits = c(0, 1.4)) +
  theme_bw() +
  theme(legend.position = c(0.3,0.75),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
nirm

noso <- ggplot(Fig6, 
               aes(x=Temp, 
                   y=nosO,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nosO-nosOse, ymax=nosO+nosOse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=expression(italic(nosZ)~abundance~(x10^{"6"}~g^{"-1"}))) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
noso

nosm <- ggplot(Fig6, 
               aes(x=Temp, 
                   y=nosM,
               colour=Region, 
               shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nosM-nosMse, ymax=nosM+nosMse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
nosm

nno <- ggplot(Fig6, 
               aes(x=Temp, 
                   y=nnO,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nnO-nnOse, ymax=nnO+nnOse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y=expression(italic(nirS):italic(nosZ)~ratio)) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
nno

nnm <- ggplot(Fig6, 
               aes(x=Temp, 
                   y=nnM,
                   colour=Region, 
                   shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=nnM-nnMse, ymax=nnM+nnMse),
                colour="black",
                size=.3,                     # thinner lines
                width=.2) +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_discrete(labels=c("5","15","25")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
nnm

Fig6ms <- ggarrange(niro, nirm, noso, nosm, nno, nnm,
                    ncol = 2, nrow = 3, align = "hv")
png(file="Fig6.png", units="in", width=6, height=9, res=300)
Fig6ms
dev.off()

##### SUPP 1

FigS1 <- read.csv("Supp1.csv", header=TRUE, sep=",")

o5 <- ggplot(FigS1, 
              aes(x=Time, 
                  y=O5,
                  colour=Region,
                  shape=Region)) + 
  geom_hline(yintercept=0,
             linetype="dotted") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=O5-O5se, ymax=O5+O5se),
                size=.3,                     
                width=1.0,
                colour = "black") +                 
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(organic~soil~"5"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = c(0.3,0.7),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
o5

o15 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=O15,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=O15-O15se, ymax=O15+O15se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(organic~soil~"15"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
o15
o25 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=O25,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=O25-O25se, ymax=O25+O25se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(organic~soil~"25"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
o25
c5 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=C5,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=C5-C5se, ymax=C5+C5se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= expression(N[2]*O~concentration~(mu*g~N[2]*O-N~g^{"-1"}))) +
  annotate("text",
           label = expression(combined~"5"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
c5
c15 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=C15,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=C15-C15se, ymax=C15+C15se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(combined~"15"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
c15
c25 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=C25,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=C25-C25se, ymax=C25+C25se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(combined~"25"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
c25
m5 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=M5,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=M5-M5se, ymax=M5+M5se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(mineral~soil~"5"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
m5
m15 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=M15,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=M15-M15se, ymax=M15+M15se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                  
  labs(x="Time (h)", y= NULL) +
  annotate("text",
           label = expression(mineral~soil~"5"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
m15
m25 <- ggplot(FigS1, 
             aes(x=Time, 
                 y=M25,
                 colour=Region,
                 shape=Region)) + 
  geom_hline(yintercept=0,
             colour = "grey") +
  geom_line(colour = "black") +
  geom_point(size=3) +  
  geom_errorbar(aes(ymin=M25-M25se, ymax=M25+M25se),
                size=.3,                     # thinner lines
                width=1.0,
                colour = "black") +                   # Width of the error bars
  labs(x=NULL, y= NULL) +
  annotate("text",
           label = expression(mineral~soil~"25"*degree*C),
           x = 30,
           y = 2,
           size = 4) +
  scale_colour_manual(name = "MAT:",
                      labels=c("coolest","intermediate","warmest"),
                      values=c("#ffeda0","#feb24c","#f03b20")) + 
  scale_shape_manual(name = "MAT:",
                     labels=c("coolest","intermediate","warmest"),
                     values=c(19,15,17)) +
  scale_x_continuous(limits =c(0,60)) +
  scale_y_continuous(limits = c(-0.05, 2)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill = "white", size=1)) 
m25

FigS1ms <- ggarrange(o5,o15,o25,c5,c15,c25,m5,m15,m25,
                    ncol = 3, nrow = 3, align = "hv")
png(file="Supp1.png", units="in", width=9, height=9, res=300)
FigS1ms
dev.off()
