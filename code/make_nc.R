make_nc = function(ST_EOF,PC,MODE){
  Longvector = seq(90, 150, length = dim(ST_EOF)[1])
  Latvector = seq(-15, 15, length = dim(ST_EOF)[2])
  # Define data
  dataset = ST_EOF
  
  # Define the dimensions
  dimX = ncdim_def("longitude", "degrees", Longvector)
  dimY = ncdim_def("latitude", "degrees", Latvector)
  dimT = ncdim_def("time", "months", 1:length(PC))
  # dimT = ncdim_def('time', units="month", longname='BULANAN', calendar="standard", vals=data.time)
  
  # Define missing value
  mv = -9999
  
  # Define the data
  pcp = ncvar_def( "pcp", "units", list(dimX,dimY,dimT), mv, prec="double")
  
  # Create the NetCDF file
  # If you want a NetCDF4 file, explicitly add force_v4=T
  nc = nc_create(sprintf("output/EOF_X_PC%s.nc",MODE),  pcp)
  
  # Write data to the NetCDF file
  ncvar_put(nc, pcp, dataset)
  
  ncatt_put(nc,"longitude","axis","X") 
  ncatt_put(nc,"latitude","axis","Y")
  ncatt_put(nc,"time","axis","T")
  
  ncatt_put(nc,0,"title",sprintf("EOF %s presipitasi",MODE))
  ncatt_put(nc,0,"institution","BMKG")
  ncatt_put(nc,0,"source","BMKG")
  # ncatt_put(nc,0,"references",)
  history <- paste("Yosik Norman", date(), sep=", ")
  ncatt_put(nc,0,"history",history)
  ncatt_put(nc,0,"Conventions","COARDS")
  
  
  # Close your new file to finish writing
  nc_close(nc)
}
