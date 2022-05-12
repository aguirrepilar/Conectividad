#Script realizado entre María del Pilar Aguirre & Leonor A. Valenzuela
#Fecha: Abril - Mayo 2022
#En el marco del convenio SIRAP-Ec & WCS Colombia.

#Paquetes especificos que hay que tener para instalar Makurhini

#crear un proyecto y corroborar el directorio de entrada y salida

getwd()
install.packages("devtools")
install.packages("remotes")
#Instalaci?n de Makurhini,paquete para an?lisis de conectividad: https://connectscape.github.io/Makurhini/
install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")
# Llamado de otras librerias
library(devtools)
library(remotes)
library(usethis)
library(sp)
library(Makurhini)
library(sf)
library(rgdal)
library(raster)
###################
##ProtConn por distancias
getwd()

### Corrida Marzo 23 20222 by Pilar Pc escritorio 15 min aprox ####
Protected_areas<-read_sf("Runap_SIRAP_CTM12.shp")
ecoregions<-read_sf("Ecoregiones_SIRAP_CTM12.shp")
?MK_ProtConnMult
MK_ProtConnMult <- MK_ProtConnMult(nodes = Protected_areas,
                        regions = ecoregions,
                        area_unit = "ha",
                        distance = list(type= "edge"),
                        distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                        probability = 0.5, transboundary = 70000, 
                        plot = TRUE, write = "D:/1.PILAR/1.WCS/SIRAP/R/Conectividad/MK_ProtConnMult",
                        parallel = 6, intern = TRUE)
MK_ProtConnMult
##
MK_dPCIIC <- MK_dPCIIC(nodes = Protected_areas, attribute = NULL,
                distance = list(type = "edge"),
                metric = "PC", probability = 0.5,
                distance_thresholds = c(1000, 5000, 10000,30000,50000,70000), 
                write = "D:/1.PILAR/1.WCS/SIRAP/R/Conectividad/MK_dPCIIC_",intern = TRUE)
MK_dPCIIC
#####Resistencia
Resistance_raster<-raster("resistencia.tif")
Resis_Actual<- MK_ProtConnMult(nodes = Protected_areas,
                                    regions = ecoregions,
                                    area_unit = "ha",
                                    distance = list(type="least-cost", 
                                                    resistance= Resistance_raster),
                                    distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                                    probability = 0.5, transboundary = 70000, 
                                    plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Resist/Res_Actual",
                                    parallel = 6, intern = TRUE)
Resis_Actual
Resis_aguac_Gob<-raster("Resistencia_Aguacate_Gob2030.tif")
Resis_Fut_Gob<- MK_ProtConnMult(nodes = Protected_areas,
                               regions = ecoregions,
                               area_unit = "ha",
                               distance = list(type="least-cost", 
                                               resistance= Resis_aguac_Gob),
                               distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                               probability = 0.5, transboundary = 70000, 
                               plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Resist/Res_Fut_Gob",
                               parallel = 6, intern = TRUE)
Resis_Fut_Gob
Resis_aguac_Bau<-raster("Resistencia_Aguacate_Bau2030.tif")
Resis_Fut_Bau<- MK_ProtConnMult(nodes = Protected_areas,
                               regions = ecoregions,
                               area_unit = "ha",
                               distance = list(type="least-cost", 
                                               resistance= Resis_aguac_Bau),
                               distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                               probability = 0.5, transboundary = 70000, 
                               plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Resist/Res_Fut_Bau",
                               parallel = 6, intern = TRUE)
Resis_Fut_Bau
#Nueva Resistencia Futuro sin aguacate###
getwd()
Resis_Gob<-raster("Resistencia_Gob2030.tif")
Resis_Fut_Gob2<- MK_ProtConnMult(nodes = Protected_areas,
                                regions = ecoregions,
                                area_unit = "ha",
                                distance = list(type="least-cost", 
                                                resistance= Resis_Gob),
                                distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                                probability = 0.5, transboundary = 70000, 
                                plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Resis_2/Res_Fut_Gob",
                                parallel = 6, intern = TRUE)
Resis_Fut_Gob2
Resis_Bau<-raster("Resistencia_Bau2030.tif")
Resis_Fut_Bau2<- MK_ProtConnMult(nodes = Protected_areas,
                                regions = ecoregions,
                                area_unit = "ha",
                                distance = list(type="least-cost", 
                                                resistance= Resis_Bau),
                                distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                                probability = 0.5, transboundary = 70000, 
                                plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Resis_2/Res_Fut_Bau",
                                parallel = 6, intern = TRUE)
Resis_Fut_Bau2
#corrida testECC una vez se tenga la capa de areas protegidas integrada con ECC###
Todas_Protected_areas<-read_sf("ECC_AP_SIRAP_CTM12_Union_Top.shp")
ecoregions<-read_sf("Ecoregiones_SIRAP_CTM12.shp")
?MK_ProtConnMult
getwd()
Todas_AP_MK_ProtConnMult <- MK_ProtConnMult(nodes = Todas_Protected_areas,
                                   regions = ecoregions,
                                   area_unit = "ha",
                                   distance = list(type= "edge"),
                                   distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                                   probability = 0.5, transboundary = 70000, 
                                   plot = TRUE, write = "D:/1.PILAR/1.WCS/R/Conectividad/Todas_AP/Todas_ProtConn_",
                                   parallel = 6, intern = TRUE)
Todas_AP_MK_ProtConnMult
##
MK_dPCIIC?
Todas_Protected_areas
Todas_AP_MK_dPC <- MK_dPCIIC(nodes = Todas_Protected_areas, attribute = NULL,
                       distance = list(type = "edge"),
                       metric = "PC", probability = 0.5,
                       distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                       write = "D:/1.PILAR/1.WCS/R/Conectividad/Todas_AP/Todas_dPCIIC_", intern = TRUE)
Todas_AP_MK_dPC

Todas_AP_MK_IIC <- MK_dPCIIC(nodes = Todas_Protected_areas, attribute = NULL,
                                distance = list(type = "edge"),
                                metric = "IIC", probability = 0.5,
                                distance_thresholds = c(1000, 5000, 10000,30000,50000,70000),
                                write = "D:/1.PILAR/1.WCS/R/Conectividad/Todas_AP/Todas_IIC_", intern = TRUE)
Todas_AP_MK_IIC
