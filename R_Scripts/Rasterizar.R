## =========== rasterizar os shapefiles. resolução 1 km ======= ##
## ================== time - julho 2020 ====================== ##

#pacotes
library(rgdal)
library(raster)
library(rgeos)
library(rasterVis)
library(RColorBrewer)

myPal <- RColorBrewer::brewer.pal('Set3', n=9) #para fazer a figura...
myTheme <- rasterTheme(region = myPal)

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

suacui <- readOGR(dsn = "./Data/USO_ANA", layer = "uph_suasui_grande_uso2", encoding = 'UTF-8')


#rasterizando
#===para guandu====
nam <- unique(guandu$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
guandu$ID <- nam_df$ID[match(guandu$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(guandu)
res(r.raster) <- 100 # Define pixel size

g.r <- rasterize(guandu, r.raster, field = guandu$ID)

g.r <- ratify(g.r)
rat <- levels(g.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(g.r) <- rat
projection(g.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/guandu.png")
rasterVis::levelplot(g.r, par.settings = myTheme)
dev.off()

#====para pira====
nam <- unique(pira$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
pira$ID <- nam_df$ID[match(pira$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(pira)
res(r.raster) <- 100 # Define pixel size

pira.r <- rasterize(pira, r.raster, field = pira$ID)

pira.r <- ratify(pira.r)
rat <- levels(pira.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(pira.r) <- rat
projection(pira.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/pira.png")
rasterVis::levelplot(pira.r, par.settings = myTheme)
dev.off()

#====para piranga====
nam <- unique(piranga$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
piranga$ID <- nam_df$ID[match(piranga$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(piranga)
res(r.raster) <- 100 # Define pixel size

piranga.r <- rasterize(piranga, r.raster, field = piranga$ID)

piranga.r <- ratify(piranga.r)
rat <- levels(piranga.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(piranga.r) <- rat
projection(piranga.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/piranga.png")
rasterVis::levelplot(piranga.r, par.settings = myTheme)
dev.off()

#====para caratinga====
nam <- unique(caratinga$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
caratinga$ID <- nam_df$ID[match(caratinga$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(caratinga)
res(r.raster) <- 100 # Define pixel size

caratinga.r <- rasterize(caratinga, r.raster, field = caratinga$ID)

caratinga.r <- ratify(caratinga.r)
rat <- levels(caratinga.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(caratinga.r) <- rat
projection(caratinga.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/caratinga.png")
rasterVis::levelplot(caratinga.r, par.settings = myTheme)
dev.off()

#====para manhuaçu====
nam <- unique(manhuacu$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
manhuacu$ID <- nam_df$ID[match(manhuacu$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(manhuacu)
res(r.raster) <- 100 # Define pixel size

manhuacu.r <- rasterize(manhuacu, r.raster, field = manhuacu$ID)

manhuacu.r <- ratify(manhuacu.r)
rat <- levels(manhuacu.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(manhuacu.r) <- rat
projection(manhuacu.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/manhuacu.png")
rasterVis::levelplot(manhuacu.r, par.settings = myTheme)
dev.off()

#====para suacui====
nam <- unique(suacui$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
suacui$ID <- nam_df$ID[match(suacui$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(suacui)
res(r.raster) <- 100 # Define pixel size

suacui.r <- rasterize(suacui, r.raster, field = suacui$ID)

suacui.r <- ratify(suacui.r)
rat <- levels(suacui.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(suacui.r) <- rat
projection(suacui.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/suacui.png")
rasterVis::levelplot(suacui.r, par.settings = myTheme)
dev.off()

#====para Santa Maria do Doce====
nam <- unique(maria$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
maria$ID <- nam_df$ID[match(maria$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(maria)
res(r.raster) <- 100 # Define pixel size

maria.r <- rasterize(maria, r.raster, field = maria$ID)

maria.r <- ratify(maria.r)
rat <- levels(maria.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(maria.r) <- rat
projection(maria.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/maria.png")
rasterVis::levelplot(maria.r, par.settings = myTheme)
dev.off()

#====para São Jose====
nam <- unique(jose$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
jose$ID <- nam_df$ID[match(jose$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(jose)
res(r.raster) <- 100 # Define pixel size

jose.r <- rasterize(jose, r.raster, field =jose$ID)

jose.r <- ratify(jose.r)
rat <- levels(jose.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(jose.r) <- rat
projection(jose.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/jose.png")
rasterVis::levelplot(jose.r, par.settings = myTheme)
dev.off()


#====para Santo Antonio====
nam <- unique(antonio$Uso)
nam_df <- data.frame(ID = 1:length(nam), nam = nam)
antonio$ID <- nam_df$ID[match(antonio$Uso,nam_df$nam)]
r.raster <- raster()
extent(r.raster) <- extent(antonio)
res(r.raster) <- 100 # Define pixel size

antonio.r <- rasterize(antonio, r.raster, field = antonio$ID)

antonio.r<- ratify(antonio.r)
rat <- levels(antonio.r)[[1]]
rat$names <- nam_df$nam
rat$IDs <- nam_df$ID
levels(antonio.r) <- rat
projection(antonio.r)<- "+proj=utm +zone=23 +south +ellps=aust_SA +towgs84=-66.87,4.37,-38.52,0,0,0,0 +units=m +no_defs"

png("./Figs/raster_figs/antonio.png")
rasterVis::levelplot(antonio.r, par.settings = myTheme)
dev.off()

#writing os rasters
writeRaster(g.r, './Outputs/guandu_raster.tif', overwrite=TRUE)
writeRaster(pira.r, './Outputs/pira_raster.tif', overwrite=TRUE)
writeRaster(piranga.r, './Outputs/piranga_raster.tif', overwrite=TRUE)
writeRaster(caratinga.r, './Outputs/caratinga_raster.tif', overwrite=TRUE)
writeRaster(manhuacu.r, './Outputs/manhuacu_raster.tif', overwrite=TRUE)
writeRaster(suacui.r, './Outputs/suacui_gde_raster.tif', overwrite=TRUE)
writeRaster(maria.r, './Outputs/sta_maria_raster.tif', overwrite=TRUE)
writeRaster(antonio.r, './Outputs/sto_antonio_raster.tif', overwrite=TRUE)
writeRaster(jose.r, './Outputs/sao_jose_raster.tif', overwrite=TRUE)





