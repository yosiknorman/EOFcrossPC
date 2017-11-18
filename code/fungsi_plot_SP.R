#!/usr/bin/Rscript

library(ncdf4)
library(raster)
library(sp)
library(maps)
library(ggplot2)
library(rgeos)
library(rgdal)

rm(list = ls())
f = dir("../output/",pattern = "EOF_X_PC")
nc <- nc_open(sprintf("../output/%s",f[1]))
lon1 <- ncvar_get(nc,"longitude")
lat1 <- ncvar_get(nc,"latitude")
lon = lon1[lon1 > 94 & lon1 < 106]
lat = lat1[lat1 > -7 & lat1 < 6]

# =============================  GENERATE CLIP GRIDDED MATRIX INSIDE POLYGON ==========================
# load("../data/kec2.Rda")
# ID <- getData("GADM", country="Indonesia", level=1)
# ID = kec[kec$Provinsi == "SUMATERA UTARA",]
# ID@data$
# nestates <- c("Aceh", "Sumatera Utara",  "Kepulauan Riau" , "Bengkulu"  ,
#               "Sumatera Selatan","Sumatera Barat","Lampung","Bangka Belitung","Riau","Jambi")
# nestates <- as.character(ID@data$Kabupaten)   # Tunggu data nya dulu
# state.sub <- ID[as.character(ID@data$NAME_1) %in% nestates, ]
# state.sub <- ID[as.character(ID@data$Kabupaten) %in% nestates, ]

shp_kab = readOGR("../data/SHP_Indonesia_kabupaten/INDONESIA_KAB.shp")
isum=which(shp_kab$Provinsi %in% c("Nangroe Aceh Darussallam","Sumatera Barat","Riau","Sumatera Utara"))
shp_sum = shp_kab[isum,]
i_sumut <- which(shp_kab$Provinsi == "Sumatera Utara")

shp_kab_sumut = shp_kab[i_sumut,]


load("../data/topograp.rda")
i_top_x = which(topograp$xnya >=lon[1] & topograp$xnya <= lon[length(lon)])
i_top_y = which(topograp$ynya >= lat[1] & topograp$ynya <= lat[length(lat)])
top_el = topograp$el[i_top_x,i_top_y]

topx = topograp$xnya[i_top_x]
topy = topograp$ynya[i_top_y]
# x11()
# plot(elevasi)
elevasi = raster(apply(t(top_el),2,FUN=rev))
extent(elevasi) <- c(xmin = lon[1],xmax= lon[length(lon)],ymin = lat[1],ymax = lat[length(lat)])
elevasi = crop(elevasi,shp_sum)
elevasi <- mask(elevasi, shp_sum)

fungsi_plot_SP =  function(f){
  nc <- nc_open(sprintf("../output/%s",f))
  pcp1 <- ncvar_get(nc,"pcp")
  lon1 <- ncvar_get(nc,"longitude")
  lat1 <- ncvar_get(nc,"latitude")
  lon = lon1[lon1 > 94 & lon1 < 106]
  lat = lat1[lat1 > -7 & lat1 < 6]
  pcp = pcp1[lon1 > 94 & lon1 < 106,lat1 > -7 & lat1 < 6,]
  
  m <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  komposit <- function(bulan){
    Ibul <- seq(bulan,dim(pcp)[3],by = 12)
    pcpkom <- apply(pcp[,,Ibul],c(1,2),FUN = mean)
    return(pcpkom)
  }
  
  monthly_pcp <- array(0,dim = c(length(lon),length(lat),length(m)))
  
  for(i in 1:length(m)){
    monthly_pcp[,,i] <- komposit(bulan = i)
  }
  

  source("colouralpha.R")
  
  # x11();image(dddr)
  plotbulanan <- function(bulan){
    # bulan=1
    dr1 <- raster(monthly_pcp[,,bulan])
    dddr = as.matrix(disaggregate(dr1, 25, method='bilinear'))
    dr1 <- raster(dddr)
    dr1@crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    extent(dr1) <- c(xmin = lon[1],xmax= lon[length(lon)],ymin = lat[1],ymax = lat[length(lat)])
    ff = crop(dr1,shp_kab_sumut)
    elevation.sub <- mask(ff, shp_kab_sumut)
    war = colorRampPalette(c("darkblue","white","darkred"))
    # x11(width = 10)
    plot(shp_kab_sumut,axes=T,main=sprintf("%s 2000-2017",m[bulan]),cex.axis = 2,cex.main = 3)
    
    image(elevasi,col=grey.colors(100,alpha = 0.5),add=T)
    image(elevation.sub,col = rdBalp,breaks=seq(-0.5,0.5,length=21),xaxt="n",yaxt="n",add=T);
    
    # image(elevation.sub,col = war(20),breaks=seq(-0.03,0.05,length=21),xaxt="n",yaxt="n");
    box()
    # plot(state.sub,add=TRUE)
    plot(shp_sum,lwd=0.1,lty=2,add=TRUE)
    plot(shp_kab_sumut,add=TRUE,lwd=0.2);grid()
    # map(shp_kab, fill=F, col="black", lwd=1, bg=NULL, xlim=c(90,150), ylim=c(-15, 15),
    #     mar=c(0,0,0,0),resolution = 0.0000000000001,add=T)
    
  }
  pdf(file = sprintf("../output/image/PEOF%s.pdf",f),width = 20,height = 23)
  
  par(mfrow = c(4,3))
  for(i in 1:12){
    plotbulanan(i)
  }
  dev.off()
}

# for(i in 1:length(f)){
#   fungsi_plot_SP(f[i])
#   
# }
fungsi_plot_SP(f[3])
  

# Notes : f1 and f2 breaks (-0.02 to 0.02)  , f3 (-0.5 to 0.5)

