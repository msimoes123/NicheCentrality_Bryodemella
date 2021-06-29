#Niche centrality_ Bryodemella tubercullata - Biodiversity and Conservation 
setwd("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3")

#Organizing occ--------
library(rworldmap)
pts_A <- read.delim("Extant_.txt")
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
pts_a <- round_df(pts_A, 3) 
dt <- na.omit(unique( pts_a[ , 1:2 ] ))
dt$species <- 'brydo'
dt <- cbind(dt$species, dt$longitude, dt$latitude)
colnames(dt) <- c('species', 'long', 'lat')
write.csv(dt, 'extant.csv', row.names = F )

pts_I <- read.delim("Extinct_.txt")
pts_i <- round_df(pts_I, 3) 
dt <- na.omit(unique( pts_i[ , 1:2 ] ))
dt$species <- 'brydo'
dt <- cbind(dt$species, dt$longitude, dt$latitude)
colnames(dt) <- c('species', 'long', 'lat')
write.csv(dt, 'extinct.csv', row.names = F )


setwd('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\ellips')
a <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\June_Extant.csv')
i<- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\June_Extinct.csv')
a <- na.omit(unique(a))
i <- na.omit(unique(i))
b <- rbind(a,i) #all records
test <- na.omit(unique(b)) #to extract mahala values 
write.csv(test, 'C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\unique.csv', row.names = F)  
x <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\unique.csv')
test$status <- NULL
write.csv(test, 'C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\unique_runData.csv', row.names = F)
x_ <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\unique_runData.csv') 
#Check for Nas
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Bios_all\\bios_brydo\\bio1.asc")
g<- extract(r, x[,2:3])
total <- cbind(x, g)
write.csv(total, 'C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\Data1.csv', row.names = F)
x2 <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\Data2.csv')#to extract Mahala
x3 <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\Data3.csv')#To run



#Exploring options for M size----------
require(rgeos)
require(rgdal)
require(dplyr)
require(ggplot2)
t <- SpatialPoints(b[,c("longitude","latitude")])
x <-gDistance(t, t, byid = TRUE)
write.csv(x, 'DistanceOcc.csv', row.names = F)
x <- read.csv('DistanceOcc.csv') #matrix of distances
max(x)
hist( x[x > 0 & x < 150], main = 'Distances between Occurences of Brydomella', breaks=30) #5 and 10km and more frequent than any other


#Visualization---------
require(rworldmap)
require(raster)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(0, 120), ylim = c(40, 71), asp = 1)
points(b$long, b$lat, col = "blue", cex = .9)
points(b$longitude, b$latitude, col = "red", cex = .9)
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Bios_all\\bios_brydo\\bio1.asc")
d <- read.csv('total.csv')
t <- extract(r, b[,2:3])
x <- cbind(b, t)
c <- na.omit(x)
write.csv(c[,1:4], 'C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\total.csv', row.names = F)
b<-read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\total.csv')

#Crop variables----
#mask variables Worldclim
require(raster)
path <- ".\\Bios_all\\"
varaibles_list <-list.files(path = path, pattern = ".bil", full.names = TRUE) #variables 
variables <- stack(varaibles_list) #create a stack
require(rgdal)
shape <- readOGR(dsn = "C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp",layer = "brydo_cont") #SHAPEFILE YOU CREATED FOR PROJECTION AREA
var_mask <- mask(crop(variables, shape), shape)

## names for layers
rnames <- paste0("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Bios_all\\bios_brydo\\", names(variables), ".asc") # users select the format

## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii") # change format accordingly
})

# PCA------
library(kuenm)
require(kuenm)
var_folder <- "C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\bio_prec" # name of folder with variables to be combined in distinct sets
out_folder <- "C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\pc_prec_2" # name of folder that will contain the sets
in_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
out_format <- "ascii" # other options available are "GTiff" and "EHdr" = bil
npcs <- 6 # number of pcs you want as rasters, if not defined all pcs are returned as rasters

# PCA of variables for models
kuenm_rpca(variables  = var_folder, in.format = in_format, var.scale = TRUE, write.result = TRUE, 
           out.format = "ascii", out.dir = out_folder, n.pcs = npcs)

#M mask-------
#Based on the distance analysis we will create 2 buffers:5 and 10km 
#head(b)
#variables <- raster::stack(list.files("C:/Users/mari1/Desktop/Lara_ExtinctSp/pc_temp/Initial/", 
 #                                     pattern = "pc", full.names = TRUE))
#b_5 <- buffer_area(b, longitude = "long", latitude = "lat", buffer_distance = 50)
#b_10 <- buffer_area(b, longitude = "long", latitude = "lat", buffer_distance = 10)
#b_100 <- buffer_area(b, longitude = "long", latitude = "lat", buffer_distance = 100)
#plot(b_100)
#var_mask <- mask(crop(variables, b_100), b_100)
#rnames <- paste0("C:/Users/mari1/Desktop/Lara_ExtinctSp/Ms_variables/100_all/", names(variables), ".asc") # 
## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii", overwrite=T) # change format accordingly
})


#Ellispsoids-------------

if(!require(devtools)){
  install.packages("devtools")
}
if(!require(ellipsenm)){
  devtools::install_github("marlonecobos/ellipsenm")
}
require(ellipsenm)
require(raster)

# raster layers of environmental data (this ones are masked to the accessible area)
# users must prepare their layers accordingly if using other data
vars <- raster::stack(list.files("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\pc_temp\\Initial", pattern = "pc", full.names = TRUE))
crs(vars) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# preparing training and testing data
setwd("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Trouble_prec")
data_split <- split_data(x3, method = "random", longitude = "longitude", 
                         latitude = "latitude", train_proportion = 0.75, 
                         save = TRUE, name = "species")
# sets of variables (example)
#Here, I will test precipitation/ temperature/all 5 pcs/ all 6 pcs

sets <- list(set_1 = c('pc_1', 'pc_2', 'pc_3')) # change as needed
variable_sets <- prepare_sets(vars, sets)

# methods to create ellipsoids
methods <- c("covmat", "mve1")

# model calibration process
calib <- ellipsoid_calibration(data_split, species = "species", longitude = "longitude", 
                               latitude = "latitude", variables= variable_sets, methods = methods, level = 99, 
                               selection_criteria = "S_OR_P",
                               error = 5, iterations = 500, percentage = 50,
                               output_directory = "calibration_temp", overwrite=T)

#replicated was used default = "bootstrap"; prediction mahala and suitability; return_numeric - TRUE to get Mahal v
#Had to mantain only the columns with species long and lat for this calulation
#b <- b[,1:3]
#write.csv(b, 'total_Used.csv', row.names = F)
#b <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\total_Used.csv')

ell_model <- ellipsoid_model(data = x3, species = "species",
                             longitude = "longitude", latitude = "latitude",
                             replicate_type = "bootstrap", bootstrap_percentage = 75,
                             raster_layers = vars,  
                             method = "covmat",  level = 99,
                             replicates = 10, prediction = "both", 
                             return_numeric = TRUE, format = "GTiff", color_palette = viridis::magma, 
                             overwrite = TRUE, output_directory = "ellipsenm_model_prec")

#We will use the mean, so no need to calculate the median- takes too long
#rast <- raster::stack(list.files("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\ellipsenm_model_pcall5\\mahalanobis", pattern = ".tif", full.names = TRUE))
#crs(vars) <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 '
#r_median <- calc(rast, median)
#writeRaster(r_median, 'C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\ellipsenm_model_pcall5\\mahalanobis\\median', format="GTiff")
#extract values to all points
a <- read.csv('extant.csv')
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Bios_all\\bios_brydo\\bio1.asc")
g<- extract(r, a[,2:3])
x<-cbind(a,g)
a <-na.omit(x)
i$status <- 'extinct'
i <- read.csv('extinct.csv')
a <- a[,1:3]
a$status <- 'extant'
head(i)
t <- rbind(a,i)
write.csv(h, 'MahalanobisDistance.csv', row.names = F)
head(h)

#Extractiing Values of Mahala

#total <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\total_Used.csv')

require(raster)
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\ellips\\All_Pc5\\ellipsenm_model_all_pc5\\mean_mahalanobis_calibration_brydo.tif.tif")
pc5 <- extract(r, x3[,2:3])
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\ellips\\Precpitation\\ellipsenm_model_Prec\\mean_mahalanobis_calibration_brydo.tif.tif")
rP <- extract(r, x3[,2:3])
r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\Version3\\ellips\\Temperature\\ellipsenm_model_Temp\\mean_mahalanobis_calibration_brydo.tif.tif")
rT <- extract(r, x3[,2:3])

#r <- raster("C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\ellipsenm_model_pcprec\\mean_mahalanobis_calibration_brydo.tif.tif")
#pcP <- extract(r, t[,2:3])
h<- cbind(x3, pc5,rP,rT,x2[,4]) #all distances in a matrix
colnames(h) <- c('species', 'longitude', 'latitude', 'pc5', 'rP', 'rT', 'status')
write.csv(h, 'Results_V3.csv', row.names = F)
require(ggplot2)
i<- h[ which(h$status=='extinct'), ]
a<- h[ which(h$status=='extant'), ]
x<- i$pc6
#ggplot(a, aes(x=pc5)) +
 # geom_histogram(fill="white", color="black")+
  #labs(title="Mahalanobis Distance",x="Distance (D2) ", y = "Number of Occ records")+
  #theme_classic()


head(h)
p<-ggplot(h, aes(x=rP, color=status, fill=status)) +
  geom_histogram(alpha=0.8, position="identity", binwidth = 0.5)+
  labs(title="Principal components of Temperature Variables",x="Mahalanobis Distance (D2) ", y = "Number of Occ records")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0, 40)) +   theme_minimal() 
p

#binary suitability 
r <- raster('')
mop1 <- mop > 0

#Sign tets
x <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\stats\\MahalanobisDistance_extant.csv')
c <- read.csv('C:\\Users\\mari1\\Desktop\\Lara_ExtinctSp\\stats\\MahalanobisDistance_extinct .csv')

SIGN.test(x$pc5, c$pc5, 
          conf.level = 0.95)
install.packages('BSDA')
library(BSDA)
wilcox.test(x$pc5, c$pc5)
kruskal.test(x$pcT, c$pcT)
median.test(x$pcP, c$pcP)

mood.test(x$pc5, c$pc5, alternative = "greater")#extant median is greater than extinct median
mood.test(x$pcT, c$pcT, alternative = "greater")#extant median is greater than extinct median
mood.test(x$pcP, c$pcP, alternative = "greater")#extant median is greater than extinct median
p-value = 0.000654
p-value = 1.661e-05
p-value = 0.001403
median(x$pc5)
median(c$pc5)

mean(x$pc5)
t.test(c$pc5, mu = 5.016924, alternative = "less")


pt <-points(b) ##plot the points
spinosa = raster("G:/Multv_anal/Asc/ModelSpinosa.asc");  

##Calculating the distance - G-space (points - to every pixel) btw points______________________________________________________________________________________________________________________________________________________________________________________
SpinosaDistance=distanceFromPoints(spinosa, m4, xlim= c(-80, -30), ylim = c(-30, 0), filename='SpinosaDistance', asp=1, overwrite=TRUE);                         #create the distance of points 
Distance = raster("C:/Users/Marianna/Desktop/Multv_anal/Asc/SpinosaDistance.grd");                                                                               #create a object to open the file that i created with the distancefrom points
plot(Distance);                                                                                                                                                  #plot the environmental distance of each point to every pixel in the raster 
plot(Distance, xlim= c(-80, -30), ylim = c(-30, 0), main= "Distance between points to all cells in the Raster Layer-not cut", xlab='Longitud', ylab= "Latitud");      # cut the layer of distance to the same extent as model of spinosa
ModelDist = spinosa*Distance;                                                                                                                                      #getting the distance only within the raster of the modelling 
plot(ModelDist, main= "Distance Between points in the intire model G-space");
plot(ModelDist, main= "Distance Between points on the Logical ", xlim= c(-80, -30), ylim = c(-30, 0));                                                              #plot the enviromental distanvce of points within the model extent and map cut with respective limits
newmap R 
#get the map from source
plot(ModelDist, main= 'Distance between points within the G-space of the Potential Distribution', xlab= 'Longitude', ylab= 'Latitude', add=TRUE)                                             #plotting object
plot(newmap, xlim= c(-80, -30), ylim = c(-30, 0), asp=1, add=TRUE);                                                                                                #plot the 
points(m4, pch = 20, col="red");                                                                                                                                  #points of m4, plotting character (pch) type 19 , color grey 
