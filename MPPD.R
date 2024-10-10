library(ggplot2)
library(RColorBrewer)
library(viridis)
data<-read.csv("E:/Results/PAH air/MPPD/MPPD_air.csv")
View(data)
data$Age<- factor(data$Age, levels = c(14,18,21,30))
virid = scale_fill_viridis(direction = T)
p<-ggplot(data, aes(x=Age, y = DM, ymin = Non_burningDM, ymax = BurningDM, col = PM, fill = PM))+
  geom_linerange(aes(ymin = Non_burningDM, ymax = BurningDM), position = position_dodge(width = 0.5))+
  geom_point(size=2, shape=21, position=position_dodge(width = 0.5))+
  coord_flip()+
  geom_point(aes(x = Age, y = BurningDM),size =5, position = position_dodge(width = 0.5))+
  ylab(expression(bold(paste("PM deposited mass" (ng.s^-1)))))+
  theme_light()+theme(axis.line = element_line(color = "black"))+
  theme(legend.position = "none")+
  theme(axis.title = element_text(face = "bold"))+
  theme(axis.text = element_text(colour = "black"))
  
p

group.colors <- c(PM10 = "#333BFF", PM2.5 = "yellow")

q<-ggplot(data, aes(x=Age, y = MPA, ymin = Non_burningMPA, ymax = BurningMPA, col = PM, fill = PM))+
  geom_linerange(aes(ymin = Non_burningMPA, ymax = BurningMPA), position = position_dodge(width = 0.5))+
  geom_point(aes(x = Age, y = Non_burningMPA, fill = PM),size=2, shape=21, position=position_dodge(width = 0.5))+
  coord_flip()+
  geom_point(aes(x = Age, y = BurningMPA, fill = PM),size =5, position = position_dodge(width = 0.5))+
  theme(axis.line = element_line(color = "black"))+
  theme(axis.title = element_text(face = "bold"))+
  ylab(expression(bold(paste("PM mass deposited per area (ng.m"^{-2}, "s"^{-1}, ")"))))+
  theme_light()+theme(axis.line = element_line(color = "black"))+
  theme(axis.title = element_text(face = "bold"))+
  theme(axis.text = element_text(colour = "black"))
q


library(ggpubr)
figs<-ggarrange(p,q, ncol =2, align ="h", vjust = 0.1, hjust = 0.1)
figs
##########################################################################################################################################################
p<-ggplot(data)+
  scale_x_discrete(breaks=factor(c("14","18","21", "30")),
                   limits = c("14","18","21", "30"))+
  ylim(0,1.7700)+
  geom_point(aes(x = Age, y = Non_burningDM,colour=Non_burning,fill = Burning),size=2, shape =16, position=position_dodge(width = 0.5))+
  geom_linerange(aes(x = Age, ymin = Non_burningDM, ymax = BurningDM, fill = Burning), position = position_dodge(width = 0.5))+
  coord_flip()+
  geom_point(aes(x = Age, y = BurningDM, fill = Non_burning),size =5, shape = 21, position = position_dodge(width = 0.5))+
  ylab(expression(bold(paste("PM deposited mass" (ng.s^-1)))))+
  theme_light()+theme(axis.line = element_line(color = "black"))+
  theme(axis.title = element_text(face = "bold"))+
  theme(axis.text = element_text(colour = "black"))+
  scale_fill_manual(values = c("B"="blue","W" = "yellow"))+
  scale_fill_manual(values = c("PM10" = "red", "PM2.5" = "darkblue"))+theme(legend.position = "none")
  

p


q<-ggplot(data)+
  scale_x_discrete(breaks=factor(c("14","18","21", "30")),
                   limits = c("14","18","21", "30"))+
  ylim(0, 1500)+
  geom_point(aes(x = Age, y = Non_burningMPA,colour=Non_burning,fill = Burning),size=2, shape =16, position=position_dodge(width = 0.5))+
  geom_linerange(aes(x = Age,ymin = Non_burningMPA, ymax = BurningMPA, fill = Burning), position = position_dodge(width = 0.5))+
  coord_flip()+
  geom_point(aes(x = Age, y = BurningMPA,fill = Non_burning),size =5, shape = 21, position = position_dodge(width = 0.5))+
  theme(axis.line = element_line(color = "black"))+
  theme(axis.title = element_text(face = "bold"))+
  ylab(expression(bold(paste("PM mass deposited per area (ng.m"^{-2}, "s"^{-1}, ")"))))+
  theme_light()+theme(axis.line = element_line(color = "black"))+
  theme(axis.title = element_text(face = "bold"))+
  theme(axis.text = element_text(colour = "black"))+
  scale_fill_manual(values = c("B"="blue","W" = "yellow"))+
  scale_fill_manual(values = c("PM10" = "red", "PM2.5" = "darkblue"))+
  labs(fill='Burning', colour = "Non burning")+
  theme(legend.text = element_text(face = "bold"))+
  theme(legend.title = element_text(face = "bold"))
q


library(ggpubr)
figs<-ggarrange(p,q, ncol =2)
figs
