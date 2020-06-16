######################################################
# uso do solo da ANA - por UPHs
# Talita
# 10.junho.2020
######################################################

# loading pck ####

library(rgdal)
library(raster)
library(rgeos)

#reading shps e converting into dataframes
guandu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_guandu_uso2", encoding = 'UTF-8')
guandu.df <- as.data.frame(guandu)

caratinga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_caratinga_uso2", encoding = 'UTF-8')
cara.df <- as.data.frame(caratinga)

manhuacu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_manhuacu_uso2", encoding = 'UTF-8')
manhu.df <- as.data.frame(manhuacu)

pira<- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piracicaba_uso2", encoding = 'UTF-8')
pira.df <- as.data.frame(pira)

piranga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piranga_uso2", encoding = 'UTF-8')
piranga.df <- as.data.frame(piranga)

maria <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_Santa_Maria_do_Doce_uso2", encoding = 'UTF-8')
maria.df <- as.data.frame(maria)

antonio <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_santo_antonio_uso2", encoding = 'UTF-8')
antonio.df<- as.data.frame(antonio)

jose <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_sao_jose_uso2", encoding = 'UTF-8')
jose.df<- as.data.frame(jose)

suacui <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_suasui_grande_uso2", encoding = 'UTF-8')
suacui.df<- as.data.frame(suacui)

#area em km2 (a area original é dada em m2)

guandu.df$area_km <-guandu.df$area2/1e+6 #em km2
cara.df$area_km <- cara.df$area2/1e+6
manhu.df$area_km <- manhu.df$area2/1e+6
pira.df$area_km <- pira.df$area2/1e+6
piranga.df$area_km <- piranga.df$area2/1e+6
maria.df$area_km <- maria.df$area2/1e+6
antonio.df$area_km <- antonio.df$area2/1e+6
jose.df$area_km <- jose.df$area2/1e+6
suacui.df$area_km <- suacui.df$area2/1e+6



#summarise and make graphs ####
library(dplyr)
library(ggplot2)
library(gridExtra)

g0 <- filter(guandu.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!

g1 <- g0 %>%
  group_by(Uso) %>%
  summarise (sum = sum(area_km))


#to add levels that are missing in df (pq no total são 10 classes - tirando o aeroporto-, mas nem toda UPH tem todas as classes...)
#text clean para poder arrange em ordem alfabetica. otherwise ele nao entende os caracteres especiais...

g2 <- g1 %>%
add_row(Uso = "Áreas Agrícolas", sum = 0)%>%
add_row(Uso = "Áreas de Mineração", sum = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para Caratinga
cara1 <- cara.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

cara1 <-  cara1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para manhuacu
manhu1 <- manhu.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

manhu1 <- manhu1 %>%
   add_row(Uso = "Áreas de Mineração", sum = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para piracicaba
pira0 <- filter(pira.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
pira1 <- pira0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))
pira1 <- pira1 %>%
add_row(Uso = "Áreas Agrícolas", sum = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para Piranga
piranga0 <- filter(piranga.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!

piranga1 <- piranga0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

piranga1 <-  piranga1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para Sta Maria do Doce
maria1 <- maria.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

maria1 <- maria1 %>%
  add_row(Uso = "Rodovias", sum = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para Sto Antonio
antonio1 <- antonio.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

antonio1 <-  antonio1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para São Jose
jose0 <- filter(jose.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
jose1 <- jose0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

jose1 <- jose1 %>%
  add_row(Uso = "Rodovias", sum = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

#para Suasui Grande
suacui0 <- filter(suacui.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
suacui1 <- suacui0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))

suacui1 <-  suasui1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)


#barplots ####

f1 <- ggplot(data = g2, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 3000),breaks=0:3000*500) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
    theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Guandu")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f2 <- ggplot(data = cara1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 6000),breaks=0:6000*1000) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas", "Pastagem", "Rodovias", "Vegetação Nativa"))+
    theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Caratinga")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f3 <- ggplot(data = manhu1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 6000),breaks=0:6000*1000) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Manhuaçu")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f4 <- ggplot(data = pira1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 3000),breaks=0:3000*500) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 7, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Piracicaba")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f5 <- ggplot(data = piranga1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 16000),breaks=0:11000*2500) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 7, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Piranga")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f6 <- ggplot(data = maria1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 3000),breaks=0:3000*500) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 7, angle = 90), axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Santa Maria do Doce")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f7 <- ggplot(data = antonio1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 6000),breaks=0:6000*1000) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Santo Antonio")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")


f8 <- ggplot(data = jose1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área (km2)") +
  scale_y_continuous(limits = c(0, 6000),breaks=0:6000*1000) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("São José")+
  theme(plot.title = element_text(size=10))+
    theme(legend.position="none")


f9 <- ggplot(data = suacui1, aes(x=novo_uso, y=sum, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 16000),breaks=0:16000*2500) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento", "Areas Urbanas", "Pastagem", "Rodovias", "Vegetacao Nativa"),
                   labels=c("Afloramento rochoso", "Água", "Áreas abertas", "Áreas agrícolas", "Mineração", "Silvicultura", "Áreas Urbanas",  "Pastagem", "Rodovias", "Vegetação Nativa"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 7, angle = 90), axis.text.y = element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Suaçuí Grande")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

#salvando figuras (com dados brutos e nao %)
png("Figs/figura01.png", res = 300, width = 1400, height = 800)
grid.arrange(f5, f9, ncol =2)
dev.off()

png("Figs/figura02.png", res = 300, width = 1400, height = 800)
grid.arrange( f4, f6, ncol =2)
dev.off()
