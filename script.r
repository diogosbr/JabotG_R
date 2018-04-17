#-----------------------------------------------#
#                                               #
# Script para extrair dados de shapes e rasters #
#                                               #
#-----------------------------------------------#

#Instalando os pacotes, se necessário
packages = c("maptools", "rgdal")
for (p in setdiff(packages, installed.packages()[, "Package"])) {
  install.packages(p, dependencies = T)
}

#Carregando os pacotes
require(maptools)
require(raster)

#Importando os shapes do IBGE
biomas = rgdal::readOGR("./biomas_5000/biomas_5000.shp")
clima = rgdal::readOGR("./ClimadoBrasil_5000/ClimadoBrasil_5000.shp")
solos = rgdal::readOGR("./solos_5000/solos_5000.shp")
relevo = rgdal::readOGR("./geomorfologia_5000/geomorfologia_5000.shp")
vegetacao_5000 = rgdal::readOGR("./vegetacao_5000/vegetacao_5000.shp")
vegetacao_radam = rgdal::readOGR("./vegetacao_radambrasil/vegetacao_radambrasil.shp")

#Lendo os pontos
pts  = read.table("DadosJabotG_IBGE_20171226.csv",
                  h = T,
                  sep = ";")

#Visualizando os 10 primeiros registros
head(pts, 10)
pts_1 = pts

#Verifica se tem NA nos dados
table(is.na(pts$longitude))
table(is.na(pts$latitude))

#plotando os pontos no mapa do mundo
maps::map()
points(pts_backup[, c("longitude", "latitude")],
       col = 'red',
       pch = 20,
       cex = 0.8)
maps::map.axes()

#plotando os pontos no mapa do Brasil
maps::map(xlim = c(-75,-33), ylim = c(-35, 10))
points(pts_backup[, c("longitude", "latitude")],
       col = 'red',
       pch = 20,
       cex = 0.8)
maps::map.axes()

#convertendo em um objeto 'spatial'
coordinates(pts) <- ~ longitude + latitude

#atribuinto projeções aos shapes e aos pontos
proj4string(pts) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(biomas) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(clima) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(solos) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(relevo) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(vegetacao_5000) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(vegetacao_radam) <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#extraindo dados dos shapes a partir dos pontos

#biomas
pts1$bioma = over(pts, biomas)[, 2]

#clima
pts1$clima_zona = over(pts, clima)[, 2]
pts1$clima_tp_umidade = over(pts, clima)[, 3]
pts1$clima_distr_umid = over(pts, clima)[, 4]
pts1$clima_temperatur = over(pts, clima)[, 5]
pts1$clima_desc_compl = over(pts, clima)[, 6]

#solo
pts1$solo_comp = over(pts, solos)[, 5]
pts1$solo_textur = over(pts, solos)[, 6]
pts1$solo_compo1 = over(pts, solos)[, 7]
pts1$solo_compo2 = over(pts, solos)[, 8]

gc()

#relevo
pts1$relevo_nom_unidad = over(pts, relevo)[, 4]
pts1$relevo_compar = over(pts, relevo)[, 5]
pts1$relevo_nom_regiao = over(pts, relevo)[, 6]
pts1$relevo_nom_domini = over(pts, relevo)[, 7]

#vegetação
pts1$vegetacao_5000_class = over(pts, vegetacao_5000)[, 5]
pts1$vegetacao_5000_veg_pr = over(pts, vegetacao_5000)[, 6]

gc()

#vegetação RADAM
pts1$vegetacao_radam_nmuveg = over(pts, vegetacao_radam)[, 8]
pts1$vegetacao_radam_nm_uantr = over(pts, vegetacao_radam)[, 10]
pts1$vegetacao_radam_nm_contat = over(pts, vegetacao_radam)[, 12]
pts1$vegetacao_radam_nm_pretet = over(pts, vegetacao_radam)[, 14]
pts1$vegetacao_legenda = over(pts, vegetacao_radam)[, 15]

#Verificando os seis primeiros registros
head(pts1)

#Salvando no diretório de trabalho um csv com as nomas informações extraídas dos shapes
write.table(pts1, "Data_NEW.csv", sep = ";", row.names = F)

# Incluindo informações de raster do Worldclim

#lendo os rasters
worldclim2 = raster::stack(list.files(
  "~/MEGA/Worldclim2",
  pattern = ".tif",
  full.names = T
))

#extraindo os dados
dados = raster::extract(worldclim2, cbind(pts1$long, pts1$lat))

#juntando a tabela com as informações dos shapes
pts2 = cbind(pts1, dados)

#Salvando no diretório de trabalho um csv com as nomas informações extraídas dos shapes e dos rasters
write.table(pts2,
            "dados_shapes_raster.csv",
            sep = ";",
            row.names = F)
