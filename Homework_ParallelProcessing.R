# Author: Patrick Sogno
# E-mail: patrick.sogno@stud-mail.uni-wuerzburg.de
# Date: 08 May 2019
# RStudio version: 1.1.456
# R version: 3.5.1

#--------------------------------------------------#
# Background: This code aims to give a rough idea
# of parallel processing in R and how to use it.
# It is mainly for personal use but if it
# can help you with your project, feel free to use
# what you see!
# If you have questions concerning the code or 
# found errors, please contact me via e-mail. :)
#--------------------------------------------------#
# I encourage you to take a look at this site for a very nice introduction & tutorial
# https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
#--------------------------------------------------#

## Packages:
library(doParallel)
library(foreach)
library(raster)
library(rgdal)
library(reshape2)

## Paths:
ipath <- getwd()
opath <- getwd()


## Cores:
numCores <- detectCores()
numCores

#--------------------------------------------------#
## Data preparation:
## Landklif Airborne true colour imagery was prepared for this exercise by clipping it to a smaller extent and segmenting it with Orfeo Toolbox
## Link to Landklif: https://www.biozentrum.uni-wuerzburg.de/en/zoo3/forschung/verbundprojekte/landklif/
## Link to Orfeo Toolbox: https://www.orfeo-toolbox.org/
#
#
#
#otbPath <- "D:/Programs/OTB-6.6.0-Win64/OTB-6.6.0-Win64/bin" # path to Orfeo Toolbox
#otbSeg <- list.files(otbPath, pattern = "otbcli_Segmentation") # otb segmentation algorithm
#
#
#Landklif <- brick("D:/Patrick/Documents/Dokumente/Landklif/Landklif1/Landklif1/P2/P2_merge_5637_3.jpg")
#Landklif
#extent(Landklif)
#e <- extent(4488127, 4488327, 5578500, 5578700)
#Landklif_crop <- crop(Landklif, e)
#Landklif_crop
#plotRGB(Landklif_crop, 1,2,3)
#
#cropfile <- paste0(opath, "/", "LandklifShow_crop.grd")
#writeRaster(Landklif_crop, filename = cropfile, overwrite = T)
#
## segment...
#segfile <- paste0(opath, "/", "LandklifShow_seg.sqlite")
#
#mode <- "vector" #otb cannot handle large input files in raster mode
#
## set segmentation function
#segfun <- "meanshift"
#
## formulate call
#otbCall <- paste0(otbPath, '/', otbSeg, 
#                  ' -in ', cropfile, 
#                  ' -mode ', mode, ' -mode.', mode, '.out ', 
#                  segfile, ' uint16', 
#                  ' -filter ', segfun)
#otbCall
#
## Call
#system(otbCall)
#
#--------------------------------------------------#


## Load prepared data:

seg <- readOGR(paste0(ipath, "/", "LandklifShow_seg.sqlite"))         # <- segmentation
Landklif_crop <- brick(paste0(ipath, "/", "LandklifShow_crop.gri"))   # <- cropped RGB imagery
plot(seg)
plot(Landklif_crop)
plotRGB(Landklif_crop, 1,2,3)
head(seg)

#--------------------------------------------------#

## Calculate vegetation index: <- (link: https://www.bluemarblegeo.com/knowledgebase/global-mapper-19/Raster_Calculator.htm)

NPCRI <- function(B, R) {
  (R - B)/(R + B)
}
NPCRI <- NPCRI(Landklif_crop[[3]], Landklif_crop[[1]])
plot(NPCRI)


#--------------------------------------------------#

## Classification based in NPCRI values:

seg$C_ID <- NA

## Perform calculation without parallel processing...
system.time({
  r <- foreach (i=1:length(seg)) %do% {
    tempmask <- raster::mask(NPCRI, seg[i,])
    tempC_ID <- names(which.max(table(raster::values(tempmask))))
    if (tempC_ID <= 0) {
      tempC_ID = 3
    } else {
      if (tempC_ID <= 0.2) {
        tempC_ID = 2
      } else{
        tempC_ID =1
      }
    }
  }
  
})

# ... elapsed time on my computer: ~48 sec

seg$C_ID <- unlist(r)
head(seg) # optional control

## Plot:
plot(seg[which(seg$C_ID == 3),], col = "greenyellow")
plot(seg[which(seg$C_ID == 2),], col = "yellow3", add = T)
plot(seg[which(seg$C_ID == 1),], col = "green4", add = T)


#--------------------------------------------------#


## Perform with parallel processing...

seg$C_ID <- NA

registerDoParallel(numCores-1) # parallel processing

system.time({
  
  r <- foreach (i=1:length(seg)) %dopar%{
    tempmask <- raster::mask(NPCRI, seg[i,])
    tempC_ID <- names(which.max(table(raster::values(tempmask))))
    if (tempC_ID <= 0) {
      tempC_ID = 3
    } else {
      if (tempC_ID <= 0.2) {
        tempC_ID = 2
      } else{
        tempC_ID =1
      }
    }
  }
  
})

stopImplicitCluster() # don't forget to clean up the cores after processing.

# ... elapsed time on my computer: ~ 18 sec.
class(r[[1]]) # -> notice that default return will be a list of numeric values

seg$C_ID <- unlist(r)
head(seg) # optional control


plot(seg[which(seg$C_ID == 3),], col = "greenyellow")
plot(seg[which(seg$C_ID == 2),], col = "yellow3", add = T)
plot(seg[which(seg$C_ID == 1),], col = "green4", add = T)


#-----------------------------------------------#

## Maybe it would be useful to have the pixel values stored in the segmented polygons:

seg$R <- NA
seg$G <- NA
seg$B <- NA


registerDoParallel(numCores-2) # parallel processing

system.time({
  
  r <- foreach (i=1:length(seg), .combine = rbind) %dopar%{
    tempmask <- raster::mask(Landklif_crop[[1]], seg[i,])
    names(which.max(table(raster::values(tempmask))))
  }
  
})

stopImplicitCluster() # don't forget to clean up the cores after processing.
# ... elapsed: 28.31 sec
class(r[1,]) # -> unlike before, with setting .combine=rbind, we defined the output to be a data frame with characters in each cell!
head(r)

seg$R <- as.vector(r)
head(seg)
seg$R <- as.numeric(seg$R)

plot(seg, col = seg$R)

#-----------------------------------------------#


# compared to non-parallel processing

system.time({
  
  r <- foreach (i=1:length(seg)) %do%{
    tempmask <- raster::mask(Landklif_crop[[2]], seg[i,])
    names(which.max(table(raster::values(tempmask))))
  }
  
})
# ... elapsed: 75.22 sec.
head(r)
seg$G <- unlist(r)
head(seg)
plot(seg, col = seg$G)

#-----------------------------------------------#

registerDoParallel(numCores-2) # parallel processing

system.time({
  
  r <- foreach (i=1:length(seg)) %dopar%{
    tempmask <- raster::mask(Landklif_crop[[3]], seg[i,])
    names(which.max(table(raster::values(tempmask))))
  }
  
})

stopImplicitCluster() # don't forget to clean up the cores after processing.
# ... elapsed: 28.10 sec

head(r)

seg$B <- unlist(r)
head(seg)

plot(seg, col = seg$B)

#-----------------------------------------------#

## Now it is easier to erase overlaps between classes:

plot(seg[which(seg$C_ID == 3),], col = "greenyellow")
seg3 <- seg[which(seg$C_ID == 3),] # class 3 has vegetation and street overlapping
segA <- seg3 - seg3[which(seg3$R <= 100),] # but streets have very low red reflection
plot(segA, col = "grey") # works nicely
seg3 <- seg3 - segA
plot(seg[which(seg$C_ID == 2),], col = "greenyellow", add = T) # plot nicely outlined classes, too.
plot(seg[which(seg$C_ID == 1),], col = "green2", add = T)
plot(seg3, col = "yellow4", add = T)
