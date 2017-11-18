#!/usr/bin/Rscript


data(volcano)
rdB <- colorRampPalette(c("darkblue","blue","white","red","darkred"))  
rdBalp<-paste(rdB(20), "90", sep="")


pdf("../sample/volcano.pdf")
image(volcano, col=rdBalp)
dev.off()