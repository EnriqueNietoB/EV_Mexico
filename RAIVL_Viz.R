library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gtable)
  
###### Dando formato ####

#Cargando base
EnriqueNietoB/EV_Mexico
#Formato de datos 
RAIAVL_11$Año<-as.character(RAIAVL_11$Año)
#Colocando nombres
colnames(RAIAVL_11)[4:6]<-c("EV","Plug-in","Hibrídos") 
#From long to short
EVs<-RAIAVL_11 %>% gather(Tipo, Ventas, EV:Hibrídos, factor_key=TRUE)
EVs$Ventas<-as.numeric(EVs$Ventas)
#Eliminando NAs
EVs<-na.omit(EVs)
#Agrupando
EVs<-EVs %>% group_by(Año,Tipo) %>% summarise(Ventas=sum(Ventas)) 
EVs1<-EVs %>% group_by(Año) %>% summarise(Ventas=sum(Ventas)) 
EVs1$Ventas<-EVs$Ventas/1000
EVs$Ventas<-EVs$Ventas/1000

##### Vizualizando #####
#Tema Dark
dark_theme<-theme(text=element_text(size=14,  
                                     family="Times New Roman", 
                                     colour ="White"),
                   plot.background = element_rect(fill = 'black', colour = 'black'), 
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   legend.background = element_rect(fill = 'black', colour = 'black'),
                   legend.text = element_text(color = "White", size=14),
                   panel.background = element_rect(fill = 'black', colour = 'white'),
                   axis.text = element_text(size=14, family="Times New Roman", colour="white"),
                   axis.title = element_text(size=18, family="Times New Roman", colour="white"),
                   axis.line = element_line(colour="white")) 



#Datos por year
Year<-ggplot(EVs, aes(Año, Ventas, fill=Tipo)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  scale_y_continuous(breaks = seq(0,40,5)) + 
dark_theme + theme(legend.position="none") +
   labs(
    title="Ventas de automóviles de baterías en México", 
    subtitle = "(Miles)") +  scale_fill_manual(values=c("#0AEA4A","#F00707","#0A9CEA"))
Year

#Datos por tipo
Tipo<-ggplot(EVs %>% group_by(Tipo) %>% summarise(Ventas=sum(Ventas)), 
       aes(reorder(Tipo, -Ventas), Ventas, fill=Tipo)) +
  geom_bar(stat="identity") + geom_text(aes(label=Ventas),
                                        vjust=-.5, col="white") +
dark_theme + 
  scale_y_continuous(limits = c(0,125),breaks = seq(0,160,20)) +
  labs(
    title="Ventas acumuladas", 
    subtitle = "2016-2021 (Miles)", 
    caption = "INEGI: RAIVL, Información hasta 10/2021. Datos no incluyen Tesla",
    x="Tipo de automóvil") +
  scale_fill_manual(values=c("#0AEA4A","#F00707","#0A9CEA")) 

#Combinated  
Dash<-ggarrange(Year,Tipo, ncol = 2)
Dash

#### Fin ###
