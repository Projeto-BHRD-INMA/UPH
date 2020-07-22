####======= plotting uso do solo por UPH - dados da ANA ============
##================== Talita - 05/07/2020 =======================####

# loading pck ####
library(rgdal)
library(raster)
library(rgeos)
library(plyr)
library(ggplot2)
library(gridExtra)

# Loading shp file ####
guandu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_guandu_uso2", encoding = 'UTF-8')

caratinga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_caratinga_uso2", encoding = 'UTF-8')

manhuacu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_manhuacu_uso2", encoding = 'UTF-8')
manhu.df <- as.data.frame(manhuacu)

pira<- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piracicaba_uso2", encoding = 'UTF-8')
pira.df <- as.data.frame(pira)

piranga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piranga_uso2", encoding = 'UTF-8')

maria <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_Santa_Maria_do_Doce_uso2", encoding = 'UTF-8')

antonio <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_santo_antonio_uso2", encoding = 'UTF-8')

jose <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_sao_jose_uso2", encoding = 'UTF-8')

suacui <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_suasui_grande_uso2", encoding = 'UTF-8')

#passos para conseguir usar o ggplot. shps need to be transformed for presentation by ggplot2

guandu@data$id = rownames(guandu@data)
guandu.f = fortify(guandu, region="id")
guandu.df = join(guandu.f, guandu@data, by="id")

jose@data$id = rownames(jose@data)
jose.f = fortify(jose, region="id")
jose.df = join(jose.f, jose@data, by="id")

pira@data$id = rownames(pira@data)
pira.f = fortify(pira, region="id")
pira.df = join(pira.f, pira@data, by="id")

maria@data$id = rownames(maria@data)
maria.f = fortify(maria, region="id")
maria.df = join(maria.f, maria@data, by="id")

suacui@data$id = rownames(suacui@data)
suacui.f = fortify(suacui, region="id")
suacui.df = join(suacui.f, suacui@data, by="id")

caratinga@data$id = rownames(caratinga@data)
caratinga.f = fortify(caratinga, region="id")
caratinga.df = join(caratinga.f, caratinga@data, by="id")

head(suacui.df[, 1:11])

#### plots com ggplot2####
library(ggplot2)
library(gridExtra)
library(maptools)
library(RColorBrewer)

display.brewer.pal(n = 12, name = 'Paired') # para ver as cores
brewer.pal(n=12, name = "Paired") # para ver os codigos das cores
brewer.pal(n=8, name = "Dark2")
display.brewer.pal(n = 8, name = 'Dark2')

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

levels(guandu.df$Uso)

ggplot(guandu.df) +
  aes(long,lat,group=group,fill=Uso) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_manual(values=c("#666666", "#E6AB02", "#7570B3", "#E7298A", "#66A61E" , "#666666", "#A6761D","#666666", "#1B9E77"))+ ditch_the_axes +
  ggtitle("Guandu")+
  theme(plot.title = element_text(size=14))+
  theme(legend.text = element_text(size =12), legend.key.size = unit(1, "cm"))

ggsave("figs/Guandu1.png",width = 12, height = 12, dpi = 1000)

levels(maria.df$Uso)

ggplot(maria.df) +
  aes(long,lat,group=group,fill=Uso) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_manual(values=c("#A6761D","#7570B3", "#E7298A", "#333333", "#D95F02" , "#66A61E","#666666", "#E6AB02", "#1B9E77"))+ ditch_the_axes

+
  ggtitle("Santa Maria do Doce")+
  theme(plot.title = element_text(size=10))
  map2 <-map2 + labs(fill ="")


levels(jose.df$Uso)

 ggplot(jose.df) +
  aes(long,lat,group=group,fill=Uso) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_manual(values=c("#666666", "#E6AB02","#7570B3", "#E7298A", "#333333", "#66A61E","#666666", "#A6761D", "#1B9E77"))+ ditch_the_axes +
  ggtitle("São José")+
  theme(plot.title = element_text(size=10))


ggplot(caratinga.df) +
  aes(long,lat,group=group,fill=Uso) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_manual(values=c("#A6761D","#7570B3", "#E7298A", "#333333", "#D95F02" , "#66A61E","#666666", "#E6AB02", "#666666","#1B9E77"))+ ditch_the_axes

levels(caratinga.df$Uso)
levels(pira.df$Uso)

 ggplot(pira.df) +
  aes(long,lat,group=group,fill=Uso) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_manual(values=c("#666666", "#E6AB02","#7570B3", "#E7298A", "#D95F02", "#66A61E","#666666", "#A6761D", "#666666", "#1B9E77"))+ ditch_the_axes

