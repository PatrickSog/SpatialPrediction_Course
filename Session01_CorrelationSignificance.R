# Author: Patrick Sogno
# E-mail: patrick.sogno@stud-mail.uni-wuerzburg.de
# Date: May 2019
# RStudio version: 1.1.456
# R version: 3.5.1

#--------------------------------------------------#
# Background: This code is based on a university
# lecture. It is mainly for personal use but if it
# can help you with your project, feel free to use
# what you see!
# If you have questions concerning the code or 
# found errors, please contact me via e-mail. :)
#--------------------------------------------------#

## Packages:
install.packages(c('measurements', 'sdm'))
library('measurements')
library('dplyr')
library('rgdal')
library('raster')
library('sdm')

## Paths:
ipath <- getwd() #set input path
opath <- getwd() #set output path

## Data names:
hwfile <- "weight-height.csv"
occfile <- "occurence.gpkg"
cambifile <- "campus_buildings.gpkg"

#--------------------------------------------------#

## Load and show data:
hw <- read.csv(paste0(ipath, "/", hwfile)) #don't forget the "/"!
head(hw)
summary(hw) #data is in "retard" units :(

hw2 <- data.frame(Gender=hw$Gender, Height=conv_unit(hw$Height, "inch", "cm"), Weight=conv_unit(hw$Weight, "lbs", "kg")) #convert to metric
head(hw2)
summary(hw2)

rm(hw) #clean up

## Easy data exploration:
dplyr::sample_n(hw2, 10) #random samples

summary(filter(hw2, Gender=="Female")) #filter by column name
summary(filter(hw2, Gender=="Male"))

boxplot(filter(hw2, Gender=="Female")$Height, filter(hw2, Gender=="Male")$Height, notch = T) #create boxplot
boxplot(filter(hw2, Gender=="Female")$Weight, filter(hw2, Gender=="Male")$Weight, notch = T)

shapiro.test(dplyr::sample_n(hw2, 5000)$Weight) #shapiro test to find out whether data is normally distributed
shapiro.test(dplyr::sample_n(hw2, 5000)$Height)

plot(density(hw2$Height)) #plot density curve
plot(density(hw2$Weight))

plot(density(filter(hw2, Gender=="Female")$Weight), col="red") #plot density curves together
lines(density(filter(hw2, Gender=="Male")$Weight), col="blue")
# works the same way with height...

# we see that the density distributions (almost) resemble Gaussian curves, now for another normality test:
shapiro.test(filter(hw2, Gender=="Female")$Weight)
shapiro.test(filter(hw2, Gender=="Male")$Weight)
shapiro.test(filter(hw2, Gender=="Female")$Height)
shapiro.test(filter(hw2, Gender=="Male")$Weight)
# -> the test tells us that the split dataset is not significantly different from a normal distribution
#    for the following investigations, we will have to use the subsets.

hw2.male <- filter(hw2, Gender=="Male")
hw2.fem <- filter(hw2, Gender=="Female")

## Linear regression
hw.lm <- lm(formula = Weight ~ Height, data = hw2.male)
summary(hw.lm)
# based on this, we might add new heights of other people and predict their weight or vice versa...
# For this we would need:
# 1. A new data frame with more heights/weights.
# 2. A prediction - can be done with predict().

#--------------------------------------------------#

## Spatial prediction

# read occurences
occ <- readOGR(paste0(ipath, "/", occfile))
class(occ)
summary(occ)
plot(occ)
# read buildings
bui <- readOGR(paste0(ipath, "/", cambifile))
plot(bui)

# plot
plot(bui)
plot(occ[occ$students == 1,], col= "blue", pch = 16, add= T)
plot(occ[occ$students == 0,], col= "red", pch = 16, add= T)
# -> we see a number of points around campus, blue points show where Eagles were, red points show were they have not been seen.
# -> Research question: What determines the presence of Eagles?

# Prepare data:
r <- raster(bui, ncols = 100, nrows = 100)
rr.0 <- rasterize(bui, r, progress = "text") # rasterize buildings
plot(rr.0)

rr.0.d <- distance(rr.0) # create distance to buildings raster
preds <- rr.0.d
plot(rr.0.d)

# Predict:
d <- sdmData(formula = students~layer, train = occ, predictors = preds) # produce data for spatial prediction
d

m1 <- sdm(students~., data = d, methods = c('glm', 'svm')) # fit species distribution model

p1 <- predict(m1, newdata = preds, filename = paste0(opath, "/", "sdm_preds_1.grd"), overwrite = T) # predict

plot(p1)
# -> we can see that the model only concentrates on distance to campus buildings, so let's enhance the training data!

# Pepare data (again):
rr <- rasterize(bui, r, progress = "text", field = "id") # new raster data incorporating building function
plot(rr)

rr.1 <- rr == 1     # create rasters with each raster layer representing a building function
rr.1[rr.1==0] <- NA
rr.2 <- rr == 2
rr.2[rr.2==0] <- NA
rr.3 <- rr == 3
rr.3[rr.3==0] <- NA
plot(rr.1)
plot(rr.2)
plot(rr.3)

rr.1.d <- distance(rr.1) # make distance rasters
plot(rr.1.d)
rr.2.d <- distance(rr.2)
plot(rr.2.d)
rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d, rr.2.d, rr.3.d) # stack distance bands

# Predict (again):
d <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ, predictors = preds)
m1 <- sdm(students~., data = d, methods = c('glm', 'svm'))
p1 <- predict(m1, newdata = preds, filename = paste0(opath, "/", "sdm_preds_2.grd"), overwrite = T)
plot(p1)              
# -> The result is a much better prediction than before. However, we still miss the temporal aspect...

# Prepare data, the 3rd:
occ.10h <- occ[occ$time == 10,] # split occurence values by time
occ.13h <- occ[occ$time == 13,]
occ.22h <- occ[occ$time == 22,]

plot(occ.10h)
plot(occ.13h)
plot(occ.22h)

# Predict... :

locc <- list(occ.10h, occ.13h, occ.22h)

ld <- lapply(locc, function(x) sdmData(formula = students~layer.1+layer.2+layer.3, train = x, predictors = preds))
lm <- lapply(ld, function(x) sdm(students~., data = x, methods = c('glm', 'svm')))
lp <- lapply(lm, function(x) predict(x, newdata=preds))
p.time <- stack(lp[[1]], lp[[2]], lp[[3]])

# Plot:
plotRGB(p.time, 1,3,5, stretch = "lin")
plot(bui, add = T)
