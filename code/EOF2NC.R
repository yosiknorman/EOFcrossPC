library(R.matlab)
library(ncdf4)


rm(list = ls())
setwd("~/Desktop/Studi_Yosik/Friends/Budi/")
#------------------------ BACA DATA MAT ----------------------------#
f_EOF = list.files("data/",pattern = "eof")
f_PC = list.files("data/",pattern = "pc")


EOF1 = readMat(sprintf("data/%s",f_EOF[1]))$eof.rr1
EOF2 = readMat(sprintf("data/%s",f_EOF[2]))$eof.rr2
EOF3 = readMat(sprintf("data/%s",f_EOF[3]))$eof.rr3


PC1 = readMat(sprintf("data/%s",f_PC[1]))$pc1
PC2 = readMat(sprintf("data/%s",f_PC[2]))$pc2
PC3 = readMat(sprintf("data/%s",f_PC[3]))$pc3

Spatio_temporal_PCA = function(PC,EOF){
  hasil = array(0, dim=c(dim(EOF)[1],dim(EOF)[2],length(PC)))
  for(i in 1:length(PC)){
    hasil[,,i] = EOF*PC[i]
  }
  return(hasil)
}

ST_EOF1 = Spatio_temporal_PCA(PC = PC1,EOF = EOF1)
ST_EOF2 = Spatio_temporal_PCA(PC = PC2,EOF = EOF2)
ST_EOF3 = Spatio_temporal_PCA(PC = PC3,EOF = EOF3)

source("code/make_nc.R")
make_nc(ST_EOF = ST_EOF1,PC = PC1,MODE = 1)
make_nc(ST_EOF = ST_EOF2,PC = PC2,MODE = 2)  
make_nc(ST_EOF = ST_EOF3,PC = PC3,MODE = 3)
