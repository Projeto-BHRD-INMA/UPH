## =========== rasterizar os shapefiles. resolução 1 km ======= ##
## ================== talita - julho 2020 ====================== ##

#pacotes
library(rgdal)
library(raster)
library(rgeos)

#loading files
#das UPHs

guandu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_guandu_uso2", encoding = 'UTF-8')

caratinga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_caratinga_uso2", encoding = 'UTF-8')

manhuacu <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_manhuacu_uso2", encoding = 'UTF-8')

pira<- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piracicaba_uso2", encoding = 'UTF-8')

piranga <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_piranga_uso2", encoding = 'UTF-8')

maria <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_Santa_Maria_do_Doce_uso2", encoding = 'UTF-8')

antonio <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_santo_antonio_uso2", encoding = 'UTF-8')

jose <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_sao_jose_uso2", encoding = 'UTF-8')

mask <- readOGR(dsn = "./Data/BHRD_limites", layer = "mask_bhrd_albers", encoding = 'UTF-8')

suacui <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_suasui_grande_uso2", encoding = 'UTF-8')

#raster referencia
ref <- raster::getData(name = "worldclim", var = "tmin", res = 0.5, lon = -40, lat = -20)

#reading raster referencia (raster do WorldClim que ja tem res de 1km)
#ref <- raster("~/Talita/wc2.1_30s_tmin_01.tif")

plot(ref) # dai dá pra ver q tem mtos layers
#temos que usar só um, entao usaremos ref$tmin1_34

crs(ref)

#reprojecting os shapefiles, melhor. todos tem q ter o mesmo crs
mask1 <- spTransform(mask, CRS("+proj=longlat +datum=WGS84"))

guandu1 <- spTransform(guandu, CRS("+proj=longlat +datum=WGS84"))
caratinga1 <- spTransform(caratinga, CRS("+proj=longlat +datum=WGS84"))
manhuacu1 <- spTransform(manhuacu, CRS("+proj=longlat +datum=WGS84"))
piranga1 <- spTransform(piranga, CRS("+proj=longlat +datum=WGS84"))
pira1 <- spTransform(pira, CRS("+proj=longlat +datum=WGS84"))
antonio1 <- spTransform(antonio, CRS("+proj=longlat +datum=WGS84"))
maria1 <- spTransform(maria, CRS("+proj=longlat +datum=WGS84"))
jose1 <- spTransform(jose, CRS("+proj=longlat +datum=WGS84"))
suacui1 <- spTransform(suacui, CRS("+proj=longlat +datum=WGS84"))

#croping o raster ref para as diferentes UPHs.
ref_g<-crop(ref$tmin1_34, mask1[13,]) #croping para guandu para guandu
ref_p <- crop(ref$tmin1_34, mask1[6,])#Para Piranga
ref_pira <- crop(ref$tmin1_34, mask1[7,])#Para Piracicaba
ref_sa <- crop(ref$tmin1_34, mask1[8,])#Para Sto Antonio
ref_sgde <- crop(ref$tmin1_34, mask1[9,])#Para Suacui Gde
ref_c <- crop(ref$tmin1_34, mask1[10,])#Para Caratinga
ref_m <- crop(ref$tmin1_34, mask1[11,])#Para Manhauçu
ref_sj <- crop(ref$tmin1_34, mask1[12,])#Para Sao Jose
ref_sm <- crop(ref$tmin1_34, mask1[14,])#Para Sta Maria do doce


#para rasterizar: rasterize(x, y, field = z), onde x é seu shapefile de polígonos, y é o raster de referência, z é o nome da variável do shapefile que vc quer que entre nos valores do raster

#rasterizando:
g.r <- rasterize(guandu1, ref_g, field = as.factor(guandu1$Uso))
pira.r<-rasterize(pira1, ref_pira, field = as.factor(pira1$Uso))
piranga.r<-rasterize(piranga1, ref_p, field = as.factor(piranga1$Uso))
caratinga.r<-rasterize(caratinga1, ref_c, field = as.factor(caratinga1$Uso))
manhuacu.r<-rasterize(manhuacu1, ref_m, field = as.factor(manhuacu1$Uso))
suacui.r<-rasterize(suacui1, ref_sgde, field = as.factor(suacui1$Uso))
antonio.r<-rasterize(antonio1, ref_sa, field = as.factor(antonio1$Uso))
maria.r<-rasterize(maria1, ref_sm, field = as.factor(maria1$Uso))
jose.r<-rasterize(jose1, ref_sj, field = as.factor(jose1$Uso))

plot(g.r)
g.r@data

plot(pira.r)
plot(caratinga.r)


#writing o raster
writeRaster(g.r, './Outputs/guandu_raster.tif', overwrite=TRUE)
writeRaster(pira.r, './Outputs/pira_raster.tif', overwrite=TRUE)
writeRaster(piranga.r, './Outputs/piranga_raster.tif', overwrite=TRUE)
writeRaster(caratinga.r, './Outputs/caratinga_raster.tif', overwrite=TRUE)
writeRaster(manhuacu.r, './Outputs/manhuacu_raster.tif', overwrite=TRUE)
writeRaster(suacui.r, './Outputs/suacui_gde_raster.tif', overwrite=TRUE)
writeRaster(maria.r, './Outputs/sta_maria_raster.tif', overwrite=TRUE)
writeRaster(antonio.r, './Outputs/sto_antonio_raster.tif', overwrite=TRUE)
writeRaster(jose.r, './Outputs/sao_jose_raster.tif', overwrite=TRUE)


showClass("RasterLayer")
getValues(g.r)

library(landscapemetrics)

check_landscape(g.r)
extract_lsm(g.r)
lsm_c_ai(g.r)
levels(guandu1$Uso)
