library(terra)

create_aoi <- function(x, y){
  # starting point
  xx=-5900000; yy= -300000
  aoi <- ext(xx-(x/2), xx+(x/2), yy-(y/2),yy+(y/2))
  return(aoi)
}

# Starting point
plot(rast("./data/ABG1.tif"), axes=FALSE, legend=FALSE)
points(vect(data.frame(x=-5900000, y= -300000),geom=c("x", "y"),
            crs=crs(rast("./data/ABG1.tif"))),cex=2, pch=8)


# National 500,000 km2 (x=1000 x y=500) km
n_ext = as.polygons(create_aoi(x=1000000, y=500000))
n_ext$ext = "National"
n_ext$nbr = 1
polys(n_ext)

# crop ABG at national extent
nat_rst = terra::crop(terra::rast("./data/ABG1.tif"), n_ext)
nat_rst[nat_rst==0] = NA
plot(nat_rst, axes=FALSE, legend=FALSE)


# Regional x=400 x y=250 km

# create a buffer inside the national extent
# to prevent regional extent overlap outside

buff_ext = create_aoi(x=1000000, y=500000)
buff_ext = ext(c(xmin=buff_ext[1]+(400000/2),
                 xmax=buff_ext[2]-(400000/2),
                 ymin=buff_ext[3]+(250000/2),
                 ymax=buff_ext[4]-(250000/2)))

buff_polys = as.polygons(buff_ext)

polys(buff_polys)

# crop the national extent by the inside buffer
samp_reg = terra::crop(nat_rst,buff_ext)

set.seed(1234); n = 5
idx = sample(ncell(samp_reg),n)
reg_exts_po = as.data.frame(xyFromCell(samp_reg, idx))
points(vect(reg_exts_po,geom=c("x","y"),crs=crs(samp_reg)))

reg_exts = list()
for (i in 1:n) {
  reg_exts[[i]] = ext(c(xmin=reg_exts_po[i,"x"]-(400000/2),
                        xmax=reg_exts_po[i,"x"]+(400000/2),
                        ymin=reg_exts_po[i,"y"]-(250000/2),
                        ymax=reg_exts_po[i,"y"]+(250000/2)))
  
}

r_ext = do.call("rbind",lapply(reg_exts, function(i) as.polygons(i,crs=crs(samp_reg))))
r_ext$ext = "Regional"
r_ext$nbr = c(1:n)

polys(r_ext, border="red")


loc_exts = list()
loc_exts_po_df = data.frame(x=NULL, y=NULL)
for( i in r_ext$nbr){
  # Local x=100 x y=100 km
  
  # create a buffer inside the regional extent
  # to prevent regional extent overlap outside
  
  buff_ext = ext(r_ext[i])
  buff_ext = ext(c(xmin=buff_ext[1]+(100000/2),
                   xmax=buff_ext[2]-(100000/2),
                   ymin=buff_ext[3]+(100000/2),
                   ymax=buff_ext[4]-(100000/2)))
  
  # crop the regional extent by the inside buffer
  samp_reg = terra::crop(nat_rst,buff_ext)
  idx = sample(ncell(samp_reg),1)
  loc_exts_po = as.data.frame(xyFromCell(samp_reg, idx))
  loc_exts_po_df = rbind(loc_exts_po_df,loc_exts_po)
  
  loc_exts[[i]] = ext(c(xmin=loc_exts_po[1,"x"]-(100000/2),
                        xmax=loc_exts_po[1,"x"]+(100000/2),
                        ymin=loc_exts_po[1,"y"]-(100000/2),
                        ymax=loc_exts_po[1,"y"]+(100000/2)))
  
}

l_ext = do.call("rbind",lapply(loc_exts, function(i) as.polygons(i,crs=crs(samp_reg))))
l_ext$ext = "Local"
l_ext$nbr = c(1:n)

polys(l_ext,border="blue")

all_exts = rbind(n_ext, r_ext,l_ext)

writeVector(all_exts,"./data_vec/all_exts.shp")








