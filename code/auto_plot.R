library(ncdf4)
library(raster)
library(sp)
library(maps)
library(ggplot2)
library(rgeos)

rm(list = ls())
nc <- nc_open("output/EOF_X_PC1.nc")
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

# =============================  GENERATE CLIP GRIDDED MATRIX INSIDE POLYGON ==========================
load("data/kec2.Rda")

image(lon,lat,pcp[,,1])
map("world", fill=F, col="black", lwd=1, bg=NULL, xlim=c(90,150), ylim=c(-15, 15),
            mar=c(0,0,0,0),resolution = 0.0000001,add=T)


i_w = which(!is.na(pcp[,,1]),arr.ind = T)
i_x = lon[i_w[,1]]
i_y = lat[i_w[,2]]

# xyw = data.frame(i_x,i_y,as.numeric(pcp[,,1]))
# colnames(xyw) = c("lon","lat","pcp")


ID <- getData("GADM", country="Indonesia", level=1)
# ID = kec[kec$Provinsi == "SUMATERA UTARA",]
# ID@data$

nestates <- c("Aceh", "Sumatera Utara",  "Kepulauan Riau" , "Bengkulu"  ,"Sumatera Selatan","Sumatera Barat","Lampung","Bangka Belitung","Riau","Jambi")
# nestates <- as.character(ID@data$Kabupaten)
state.sub <- ID[as.character(ID@data$Kabupaten) %in% nestates, ]

function()
dr1 <- raster(monthly_pcp[,,1])
dr1@crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
extent(dr1) <- c(xmin = lon[1],xmax= lon[length(lon)],ymin = lat[1],ymax = lat[length(lat)])



ff = crop(dr1,ID)
elevation.sub <- mask(ff, ID)

colorRampPalette(c("darkblue","white","darkred"))

x11()
plot(elevation.sub);plot(state.sub,add=TRUE)
map("world", fill=F, col="black", lwd=1, bg=NULL, xlim=c(90,150), ylim=c(-15, 15),
    mar=c(0,0,0,0),resolution = 0.0000001,add=T)



