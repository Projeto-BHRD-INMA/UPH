# ============ Figura Veg nativa + UC  para cada UPH da Bacia ================
# =======================   Talita - julho 2020  ==========================
#  =======================================================================

#pacotes
library(plyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

#lendo tables
veg <- read.csv(file = "Data/veg_uc.csv", sep = ";", dec = ".", header = T)

#barras lado a lado
f1 <- ggplot(data= veg, aes(x=name, y=per,  fill = x, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c('lightgray','black'))+
  xlab("") +
  ylab("Vegetação nativa  e área protegida (%)") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=6),
         axis.text.x=element_text(size=7, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="none")

ggsave("Figs/plot1.png", f1, width = 5, height = 3.5)

#stacked barplot
uph <- c("Piranga","Piranga","Piracicaba", "Piracicaba", "Sto Antonio","Sto Antonio", "Suaçui Gde", "Suaçui Gde","Caratinga", "Caratinga", "Manhuaçu","Manhuaçu", "São José", "São José","Guandu","Guandu", "Sta Maria", "Sta Maria")
uph1 <- as.factor(uph)

#na verdade esses values estao errados, tem q subtrair e fazer veg fora de UC e dentro de UC - senao nao da certo...fazer depois.
values <- c("9.7", "33.2", "38.5", "46.7", "38.6", "41.1", "2.7", "24.4", "7.8" ,"14", "4.5", "32.1", "10.1", "27.6","1.2",  "29.2", "2.3", "28.1")
value1 <- as.numeric(values)

type <- c("vegTot", "VegProt", "vegTot", "VegProt", "vegTot", "VegProt","vegTot", "VegProt", "vegTot", "VegProt", "vegTot", "VegProt", "vegTot", "VegProt", "vegTot", "VegProt", "vegTot", "VegProt")
tipo <- as.factor(type)

data <-data.frame(uph1, value1, tipo)

f2 <-ggplot(data= data, aes(x=uph1, y=value1, fill = tipo,  width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c('lightgray','black'))+
  xlab("") +
  ylab("Vegetação Nativa (%)") +
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=6),
         axis.text.x=element_text(size=7, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  theme(legend.position="right")

