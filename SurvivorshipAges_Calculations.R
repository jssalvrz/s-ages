#############################################################################
# 
# Code to calculate survivorship ages using data from the Human Mortality Database
#
# Reference: Mortality as a Function of Survival (Alvarez and Vaupel, 2023)
# https://doi.org/10.1215/00703370-10429097 
#
# Last updated: 05-08-2023
# Author: Jesus-Adrian Alvarez
#
# ############################################################################

library(tidyverse)
library(splines)
library(data.table)
library(pracma)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("SurvivorshipAges_Functions.R")


# Read and clean HMD data -------------------------------------------------

path <- paste0(getwd(), "/data/")
# Read all the names of th e
names    <- list.files(path = path, pattern="*.txt")
# Put everything in a list
HMDlist <- lapply(names,getHMDdata, path=path)
HMDdata   <-do.call(rbind.data.frame, HMDlist)
Dx <- subset(HMDdata, Type =="Deaths")
Nx <- subset(HMDdata, Type == "Exposures")
Dx <- Dx[,c("Country","Year","Age","Female","Male","Total")]
Nx <- Nx[,c("Country","Year","Age","Female","Male","Total")]
names(Dx) <- c("Country","Year","Age","Dx.f", "Dx.m","Dx.t" )
names(Nx) <- c("Country","Year","Age","Nx.f", "Nx.m","Nx.t" )

Mx <- merge(Dx,Nx, by =c("Country","Year", "Age"))
Mx <- data.frame(lapply(Mx, as.character), stringsAsFactors=FALSE)
Mx$Age[Mx$Age == "110+"] <-110
Mx[, c(2:9)] <- sapply(Mx[, c(2:9)], as.numeric)
Mx           <- arrange(Mx, Year, Age)
Mx$Mx.f <- Mx$Dx.f / Mx$Nx.f
Mx$Mx.m <- Mx$Dx.m / Mx$Nx.m
Mx$Mx.t <- Mx$Dx.t / Mx$Nx.t
Mx[is.na(Mx)]<-0

# Long format
Mxf <- Mx[,c("Country","Year", "Age","Dx.f","Nx.f","Mx.f")]
Mxm <- Mx[,c("Country","Year", "Age","Dx.m","Nx.m","Mx.m")]
Mxt <- Mx[,c("Country","Year", "Age","Dx.t","Nx.t","Mx.t")]

names(Mxf) <- c("Country","Year","Age","Dx","Nx","Mx")
names(Mxm) <- c("Country","Year","Age","Dx","Nx","Mx")
names(Mxt) <- c("Country","Year","Age","Dx","Nx","Mx")

Mxf$Sex <- "Females"
Mxm$Sex <- "Males"
Mxt$Sex <- "Total"

data <- data.table(rbind(Mxf,Mxm,Mxt))

# Smooth data to the finest age interval ----------------------------------

smoothData<- data[,ageInterpolationSpline(Dx = Dx, Nx = Nx, Age= Age), by =list(Country,  Sex, Year)] 

# Calculate survivorship ages (s-ages) ------------------------------------

survival <- smoothData[, calculateSurvival(Age=Age,hx=Mx), by = list(Country,  Sex, Year)]
survivalAges <- survival[, calculateSurvivalAges(Age,fx,Sx,hx), by = list(Country,  Sex, Year)]
survivalAges$Year <- as.integer(survivalAges$Year)

# Charts ------------------------------------------------------------------

brks <- seq(1900,2020,by = 20)
labs <- c("1900","'20","'40","'60","'80","2000", "'20")

# Survivorship ages. Figure 2 of Alvarez and Vaupel (2023)
ggplot(subset(survivalAges,
              s>=1 ))+
  geom_line(aes(Year, Age, group = s), colour = "grey50",alpha=0.7, size = 0.3)+

  geom_line(data=subset(survivalAges,s  %in% 100 ),
            aes(Year,  Age, group = s)
            ,colour= "black", size =0.3)+
  
  
  geom_line(data=subset(survivalAges, s  %in% seq(10,90, by =10)),
            aes(Year,  Age, group =s)
            ,colour= "firebrick3", size =0.3)+
  scale_color_viridis_c(direction = -1,option = "D",
                        begin = 0.5,end = 1,limits = c(0,110))+
  facet_grid(Country~Sex)+
  scale_y_continuous(breaks = seq(0,120,by = 10), expand = c(0,0))+
  scale_x_continuous(breaks = brks,labels = labs, expand = c(0,0))+
  theme_classic()+
  coord_cartesian(ylim = c(0,110), xlim=c(1900,2022))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background =element_blank(),
        strip.text = element_text(size=12, face = "bold"),
        text = element_text(size = 12,  colour = "black"),
        aspect.ratio = 1.5,
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(vjust=1),
        axis.title.y = element_text(vjust=2),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(2,"lines"))



