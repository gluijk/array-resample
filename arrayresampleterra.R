# Generic array resample function based on terra's resample()
# www.overfitting.net
# https://www.overfitting.net/2024/01/reescalado-de-imagenes-con-el-paquete.html

library(terra)
library(png)

# Generic array resample function
# works both for matrix (grayscale images) or 3-channel arrays (colour images)
arrayresample=function(img, DIMX, DIMY, method='bilinear') {
    require(terra)
    
    raster=rast(img)
    rasterrs=rast(nrows=DIMY, ncols=DIMX, extent=ext(raster))
    rasterrs=resample(raster, rasterrs, method=method)
    return (as.array(rasterrs))
}


# Resampling test images
images=c('circlesaliasing', 'mapoutline', 'mapsolid', 'portrait')
methods=c('near', 'bilinear', 'cubic', 'cubicspline', 'lanczos')
NIMAGES=length(images)
NMETHODS=length(methods)

DIMY=200
DIMX=200

imgout=array(0, c(DIMY*NIMAGES, DIMX*NMETHODS, 3))
for (i in 1:NIMAGES) {
    img=readPNG(paste0(images[i], ".png"))
    NCHAN=ifelse(length(dim(img))==2, 1, dim(img)[3])
    for (j in 1:NMETHODS) {
        imgrs=arrayresample(img, DIMX, DIMY, method=methods[j])
        print(paste0(images[i], " (", NCHAN, " chan) by ", methods[j],
                     ": min=", min(imgrs),", max=", max(imgrs)))
        
        imgrs[imgrs<0]=0  # clip saturated levels
        imgrs[imgrs>1]=1
        writePNG(imgrs, paste0(images[i], "_", methods[j], ".png"))
        
        # Create composite
        if (NCHAN==1) {  # grayscale image
            for (k in 1:3) imgout[(DIMY*(i-1)+1):(DIMY*i), (DIMX*(j-1)+1):(DIMX*j), k]=imgrs            
        } else {  # colour image
            imgout[(DIMY*(i-1)+1):(DIMY*i), (DIMX*(j-1)+1):(DIMX*j), ]=imgrs        
        }
    }
}

# Save two composite sizes
imgout2=arrayresample(imgout, ncol(imgout)*2, nrow(imgout)*2, method='near')
writePNG(imgout, "composite.png")
writePNG(imgout2, "composite2.png")


# Speed test - UPSAMPLE
DIMY=7547  # non-integer rescaling
DIMX=7547

# Speed test - DOWNSAMPLE
DIMY=327  # non-integer rescaling
DIMX=327

img=readPNG("testimages.png")
NCHAN=ifelse(length(dim(img))==2, 1, dim(img)[3])
for (j in 1:NMETHODS) {
    start.time <- Sys.time()
    for (k in 1:10) imgrs=arrayresample(img, DIMX, DIMY, method=methods[j])
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    print(paste0("testimages (", NCHAN, " chan) by ", methods[j],
                 ": min=", min(imgrs),", max=", max(imgrs)))
    imgrs[imgrs<0]=0  # clip saturated levels
    imgrs[imgrs>1]=1
    writePNG(imgrs, paste0("testimages_", methods[j], ".png"))
}



# Display grayscale images
img=imgout[,,1]  # R channel
image(t(img[nrow(img):1,]), useRaster=TRUE,
      col=c(gray.colors(256, start=0, end=1, gamma=0.5)),
      asp=nrow(img)/ncol(img), axes=FALSE)
