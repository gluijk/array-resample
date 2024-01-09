# Basic operations syntax comparison raster vs terra packages
# www.overfitting.net
# https://www.overfitting.net/2024/01/reescalado-de-imagenes-con-el-paquete.html

library(raster)  # read GeoTIFF, reprojection, crop and resample
library(terra)  # read GeoTIFF, reprojection, crop and resample


# READ raster
deepwaters=raster("geotiff_europe.tif.tif")  # read GeoTIFF file
deepwaters
plot(deepwaters)
abline(v=0)  # Greenwich meridian

deepwaters2=rast("geotiff_europe.tif")
deepwaters2
plot(deepwaters2)
abline(v=0)  # Greenwich meridian


# REPROJECT raster from Longitude Latitude (+proj=longlat)/WGS84
# to Lambert Conic Conformal (+proj=lcc)/WGS84
# by default crs="+proj=lcc +ellps=WGS84 +lat_1=33 +lat_2=45 +lon_0=0"
CRS="+proj=lcc +ellps=WGS84 +lat_1=33 +lat_2=45 +lon_0=0 +units=km"
deepwatersrp=projectRaster(deepwaters, crs=CRS)
deepwatersrp
plot(deepwatersrp)
abline(v=0)  # Greenwich meridian

CRS="+proj=lcc +ellps=WGS84 +lat_1=33 +lat_2=45 +lon_0=0 +units=km"
deepwatersrp2=project(x=deepwaters2, y=CRS, threads=TRUE)
deepwatersrp2
plot(deepwatersrp2)
abline(v=0)  # Greenwich meridian


# CROP raster to area of interest (save Canary Islands and The Netherlands)
cropdef=as(extent(-1850, 2200, 3450, 6050), 'SpatialPolygons')  # kms
crs(cropdef)=crs(deepwatersrp)
deepwaterscrop=crop(deepwatersrp, cropdef)
deepwaterscrop
plot(deepwaterscrop)
abline(v=0)  # Greenwich meridian

cropdef2=ext(-1850, 2200, 3450, 6050)
deepwaterscrop2=crop(x=deepwatersrp2, y=cropdef2)
deepwaterscrop2
plot(deepwaterscrop2)
abline(v=0)  # Greenwich meridian


# RESAMPLE raster to Full HD
DIMY=1080
DIMX=1920

deepwatersrs=raster(nrow=DIMY, ncol=DIMX, ext=extent(deepwaterscrop), crs=crs(deepwaterscrop))
deepwatersrs=resample(deepwaterscrop, deepwatersrs, method='bilinear')  # bilinear is faster than ngb!
plot(deepwatersrs)
abline(v=0)  # Greenwich meridian

deepwatersrs2=rast(nrows=DIMY, ncols=DIMX, extent=ext(deepwaterscrop2))
deepwatersrs2=resample(x=deepwaterscrop2, y=deepwatersrs2, method='bilinear', threads=TRUE)
plot(deepwatersrs2)
abline(v=0)  # Greenwich meridian



# Improvement raster vs terra packages:

# action    raster      terra       unit    terra/raster
# --------- ----------- ----------- ------- ------------
# REPROJECT 4.666385    1.858117    min     39.8%
# PLOT      103.1214    10.92895    s       10.6%
# CROP      4.796154    2.65933     s       55.4%
# RESAMPLE  25.8529     1.710264    s       6.6%