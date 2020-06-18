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


#area em km2 (a area original é dada em m2) e inserir area total da UPH numa coluna

guandu.df$area_km <-guandu.df$area2/1e+6 #em km2
guandu.df$area_tot <- sum(guandu.df$area_km)

cara.df$area_km <- cara.df$area2/1e+6
cara.df$area_tot <- sum(cara.df$area_km)

manhu.df$area_km <- manhu.df$area2/1e+6
manhu.df$area_tot <- sum(manhu.df$area_km)

pira.df$area_km <- pira.df$area2/1e+6
pira.df$area_tot <- sum(pira.df$area_km)
#pira.df$area_kmb <- area(pira)/1e+6

piranga.df$area_km <- piranga.df$area2/1e+6
piranga.df$area_tot <- sum(piranga.df$area_km)

maria.df$area_km <- maria.df$area2/1e+6
maria.df$area_tot <- sum(maria.df$area_km)

antonio.df$area_km <- antonio.df$area2/1e+6
antonio.df$area_tot <- sum(antonio.df$area_km)

jose.df$area_km <- jose.df$area2/1e+6
jose.df$area_tot <- sum(jose.df$area_km)

suacui.df$area_km <- suacui.df$area2/1e+6
suacui.df$area_tot <- sum(suacui.df$area_km)


#summarise and make graphs ####
library(dplyr)
library(ggplot2)
library(gridExtra)

g0 <- filter(guandu.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_g <- sum(guandu.df$area_km)  #area total da UPH

g1 <- g0 %>%
  group_by(Uso) %>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_g)*100)

#to add levels that are missing in df (pq no total são 10 classes - tirando o aeroporto-, mas nem toda UPH tem todas as classes...)
#text clean para poder arrange em ordem alfabetica. otherwise ele nao entende os caracteres especiais...

g2 <- g1 %>%
add_row(Uso = "Áreas Agrícolas", sum = 0, per = 0)%>%
add_row(Uso = "Áreas de Mineração", sum = 0, per = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
add_row(Uso = "edificada", sum =sum((bind_rows((g2[7,]), (g2[9,]))$sum)), per=sum((bind_rows((g2[7,]), (g2[9,]))$per)), novo_uso = "Edificada")
#o ultimo add_row é pra add uma linha juntando rodovias+areas_urbanas

g2<-filter(g2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas")) #ara tirar essas linhas - preciso fazer isso por conta dos gráficos

#para Caratinga
area_tot_c <- sum(cara.df$area_km) #area total da UPH

cara1 <- cara.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_c)*100)

cara2 <-  cara1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
add_row(Uso = "edificada", sum =sum((bind_rows((cara1[7,]), (cara1[9,]))$sum)), per=sum((bind_rows((cara1[7,]), (cara1[9,]))$per)), novo_uso = "Edificada")

  cara2<- filter(cara2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para manhuacu
area_tot_mh <- sum(manhu.df$area_km) #area total da UPH

manhu1 <- manhu.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_mh)*100)

manhu2 <- manhu1 %>%
   add_row(Uso = "Áreas de Mineração", sum = 0, per=0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
  add_row(Uso = "edificada", sum =sum((bind_rows((manhu1[6,]), (manhu1[8,]))$sum)), per=sum((bind_rows((manhu1[6,]), (manhu1[8,]))$per)), novo_uso = "Edificada")

manhu2<- filter(manhu2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para piracicaba
pira0 <- filter(pira.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_pira <- sum(pira.df$area_km) #area total da UPH

pira1 <- pira0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_pira)*100)

pira2 <- pira1 %>%
add_row(Uso = "Áreas Agrícolas", sum = 0, per=0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
add_row(Uso = "edificada", sum =sum((bind_rows((pira2[7,]), (pira2[9,]))$sum)), per=sum((bind_rows((pira2[7,]), (pira2[9,]))$per)), novo_uso = "Edificada")

pira2<- filter(pira2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para Piranga
piranga0 <- filter(piranga.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_p <- sum(piranga.df$area_km) #area total da UPH

piranga1 <- piranga0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_p)*100)

piranga2 <-  piranga1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
add_row(Uso = "edificada", sum =sum((bind_rows((piranga1[7,]), (piranga1[9,]))$sum)), per=sum((bind_rows((piranga1[7,]), (piranga1[9,]))$per)), novo_uso = "Edificada")

piranga2<- filter(piranga2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para Sta Maria do Doce
area_tot_m <- sum(maria.df$area_km) #area total da UPH

maria1 <- maria.df %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_m)*100)

maria1 <- maria1 %>%
  add_row(Uso = "Rodovias", sum = 0, per = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

maria2 <-maria1%>%
  add_row(Uso = "edificada", sum =sum((bind_rows((maria1[7,]), (maria1[9,]))$sum)), per=sum((bind_rows((maria1[7,]), (maria1[9,]))$per)), novo_uso = "Edificada")

maria2<- filter(maria2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para Sto Antonio
antonio0 <- filter(antonio.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_a <- sum(antonio.df$area_km) #area total da UPH

antonio1 <- antonio0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_a)*100)

antonio2 <-  antonio1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)%>%
  add_row(Uso = "edificada", sum =sum((bind_rows((antonio1[6,]), (antonio1[8,]))$sum)), per=sum((bind_rows((antonio1[6,]), (antonio1[8,]))$per)), novo_uso = "Edificada")

antonio2<- filter(antonio2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para São Jose
jose0 <- filter(jose.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_j <- sum(jose.df$area_km) #area total da UPH

jose1 <- jose0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_j)*100)

jose1 <- jose1 %>%
  add_row(Uso = "Rodovias", sum = 0, per = 0)%>%
  add_row(Uso = "Áreas de Mineração", sum = 0, per = 0)%>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)

jose2<- jose1%>%
  add_row(Uso = "edificada", sum =sum((bind_rows((jose1[7,]), (jose1[9,]))$sum)), per=sum((bind_rows((jose1[7,]), (jose1[9,]))$per)), novo_uso = "Edificada")

jose2<- filter(jose2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#para Suasui Grande
suacui0 <- filter(suacui.df,!(Uso == "Aeroporto")) #para tirar o aeroporto!
area_tot_s <- sum(suacui.df$area_km) #area total da UPH

suacui1 <- suacui0 %>%
  group_by(Uso)%>%
  summarise (sum = sum(area_km))%>%
  mutate(per=(sum/area_tot_s)*100)

suacui1 <-  suacui1 %>%
  mutate(novo_uso= textclean::replace_non_ascii(Uso))%>%
  arrange(novo_uso)
suacui2 <- suacui1%>%
  add_row(Uso = "edificada", sum =sum((bind_rows((suacui1[7,]), (suacui1[9,]))$sum)), per=sum((bind_rows((suacui1[7,]), (suacui1[9,]))$per)), novo_uso = "Edificada")

suacui2<- filter(suacui2,!(Uso == "Rodovias" | Uso =="Áreas Urbanas"))

#barplots ####

f1 <- ggplot(data = g2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área ocupada (%)") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
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

f2 <- ggplot(data = cara2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
    theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(),axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Caratinga")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f3 <- ggplot(data = manhu2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(),axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Manhuaçu")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f4 <- ggplot(data = pira2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área ocupada (%)") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Piracicaba")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f5 <- ggplot(data = piranga2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(),axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Piranga")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f6 <- ggplot(data = maria2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_blank(), axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("Santa Maria do Doce")+
  theme(plot.title = element_text(size=10))+
  theme(legend.position="none")

f7 <- ggplot(data = antonio2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("Área ocupada (%)") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
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


f8 <- ggplot(data = jose2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento","Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
  theme_classic() +
  theme (axis.text = element_text(size = 7), axis.title=element_text(size=8),
         axis.text.x=element_text(size = 8, angle = 90),axis.text.y=element_blank(),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(), panel.border=element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0), ## to write x and y axis again, ja que removi da borda
        axis.line.y = element_line(color="black", size = 0))+
  ggtitle("São José")+
  theme(plot.title = element_text(size=10))+
    theme(legend.position="none")


f9 <- ggplot(data = suacui2, aes(x=novo_uso, y=per, width=.5)) + # width faz a barra ficar mais fina (ou grossa)
  geom_bar(stat = "identity")+
  xlab("") +
  ylab("") +
  scale_y_continuous(limits = c(0, 100),breaks=0:100*20) +
  scale_x_discrete(breaks=c("Afloramento Rochoso", "Agua", "Areas Abertas (umidas + Secas)","Areas Agricolas", "Areas de Mineracao", "Areas de Reflorestamento",  "Pastagem", "Vegetacao Nativa", "Edificada"),
                   labels=c("AR", "Água", "Ab", "Agri", "Min", "Silvi", "Past", "Veg Nat", "Ed"))+
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
png("Figs/figura01.png", res = 300, width = 2000, height = 1800)
grid.arrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, ncol =3)
dev.off()

png("Figs/figura02.png", res = 300, width = 1400, height = 800)
grid.arrange( f4, f6, ncol =2)
dev.off()
