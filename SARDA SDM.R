



library(xlsx)
#### presence
spg1<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)


library(tidysdm)
### thining points
#############################################
set.seed(1234567)
spg <- thin_by_dist(spg1, dist_min = km2m(1))
nrow(spg)


class(spg1)

write.xlsx(spg, "presencefinaldata.xlsx")

#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################






#### absence

spg2<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)




library(tidysdm)
### thining points
#############################################
set.seed(1234567)
spgAB <- thin_by_dist(spg2, dist_min = km2m(1))
nrow(spgAB)



nrow(spgAB)


class(spgAB)

write.xlsx(spgAB, "absencefinaldata1KM.xlsx")



#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################



PRESENCE<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)


head(PRESENCE)

coordinates(PRESENCE) <- c('longitude','latitude')

class(PRESENCE)


mapview() + PRESENCE



ABSENCE<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)


head(ABSENCE)

coordinates(ABSENCE) <- c('longitude','latitude')

class(ABSENCE)



mapview() + PRESENCE+ABSENCE

mapview(PRESENCE, col.regions = "green", layer.name = "PRESENCE", alpha = 0.7, map.types = "CartoDB.Positron") +
  mapview(ABSENCE, col.regions = "red", layer.name = "ABSENCE", alpha = 0.7, map.types = "CartoDB.Positron")




#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################

############                               ABSENCE WITH 100 KM                            ####################################
#######################################################################################################################################


#### absence

spg2<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)




library(tidysdm)
### thining points
#############################################
set.seed(1234567)
spgAB <- thin_by_dist(spg2, dist_min = km2m(100))
nrow(spgAB)


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################




spg<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)

coordinates(spg) <- c('longitude','latitude')

class(spg)


library(raster)
library(usdm)


file_list <- list.files("C:\\Users\\DALLA\\Desktop\\work\\endemic odonata SDM\\sdm\\wC data\\", 
                      pattern = ".tif", full.names = T)

file_list <- list.files("D:\\wc2.1_30s_bio\\", 
                       pattern = ".tif", full.names = T)



file_list





bio <- stack(file_list)


plot(bio$wc2.1_30s_bio_1)
plot(spg1)

#################################

############################


library(sf)

##### i;portation dun vecteur


#(NAFR <- st_read("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/shapfile/SHP.shp"))


(NAFR <- st_read("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/shapfile/DZTU.shp"))
plot(NAFR)

library(dplyr)
library(tidyverse)



NAFR <- NAFR %>%
  select(geometry) 

plot(NAFR)

############################

library(terra)

###################################################


EVI <- raster("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/EVI_Mean_1977_2000.tif")
##EVI2 <- raster("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/EVI.tif")
LULC <- raster("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/LULC.tif")


plot(EVI)

library(raster)

# Replace class 0 with NA


custom_palette <- colorRampPalette(c('#1c0dff', '#05450a', '#086a10', '#54a708', '#78d203', 
                         '#009900', '#b6ff05', '#f9ffa4', '#a5a5a5'))



library(mapview)
mapview(LULC,col.regions=custom_palette(200),na.color = "transparent") + spg 

LULC <- reclassify(LULC, cbind(0, NA))

# Verify the new value range
summary(LULC)


custom_palette <- colorRampPalette(c('#05450a', '#086a10', '#54a708', '#78d203', 
                                     '#009900', '#b6ff05', '#f9ffa4', '#a5a5a5'))


mapview(LULC,col.regions=custom_palette(200),na.color = "transparent") + spg 


# Plot the raster to confirm changes
plot(biocM$LULC, main = "Land Use/Land Cover (Class 0 Excluded)")




plot(EVI)
plot(LULC)

##########################################
bioc <- crop(bio, extent(NAFR))
bioc <- mask(bioc, NAFR)


plot(bioc$wc2.1_30s_elev)

EVI <- crop(EVI, extent(NAFR))
EVI_masked <- mask(EVI, NAFR)

plot(EVI_masked)



EVI <- projectRaster(EVI_masked, bioc$wc2.1_30s_bio_1)

new_EVI <- crop(EVI,bioc$wc2.1_30s_bio_1)


plot(LULC)



LULC <- crop(LULC, extent(NAFR))
LULC_masked <- mask(LULC, NAFR)


LULC <- projectRaster(LULC_masked, bioc$wc2.1_30s_bio_1)

new_LULC <- crop(LULC,bioc$wc2.1_30s_bio_1)



compareRaster(bioc$wc2.1_30s_bio_1, new_LULC)


plot(new_LULC)
################################################################

cl <- colorRampPalette(c('#003300', 'green','yellow','orange','red','darkred'))


plot(new_EVI)


new_LULC <- resample(new_LULC, bioc$wc2.1_30s_bio_1, method='bilinear')
new_EVI <- resample(new_EVI, bioc$wc2.1_30s_bio_1, method='bilinear')



bioV<-stack(bioc,new_EVI, new_LULC)

plot(bioV$wc2.1_30s_bio_1)

cl <- colorRampPalette(c('#003300', 'green','yellow','orange','red','darkred'))



#------
library(mapview)

mapview(bioV$wc2.1_30s_elev,col.regions=cl(200),na.color = "transparent") + spg 

points(spg,col='black')




#------------

################ multicollinearity
library(usdm)


#bioV<-stack(bioc,EVI, slope,lulc,aridityI)

vif(bioV)

ex <- raster::extract(bioV,spg)
head(ex)



install.packages("PerformanceAnalytics")

library("PerformanceAnalytics")
chart.Correlation(ex, histogram=TRUE, pch=24)


head(ex)
colnames(ex) <- c("Elevation", "Enhanced Vegetation Index", "Annual mean temperature", 
                    "Mean temperature of wettest quarter", "Precipitation seasonality", "Land Use and land cover")




# Compute the correlation matrix with complete observations
cor_matrix <- cor(ex, use = "complete.obs")

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "pie", type = "upper", tl.col = "black", tl.srt = 45)

corrplot(cor_matrix, type="lower",tl.col = "black", tl.srt = 45)

corrplot(cor_matrix, type = "lower", tl.col = "black", tl.srt = 45, addCoef.col = "black")




library(virtualspecies)
removeCollinearity(ex, plot = TRUE, method = "pearson")



v <- vifstep(ex)

v <- vifcor(ex)



biocM <- exclude(bioV, v)
names(biocM)


vifstep(biocM)

removeCollinearity(biocM, plot = TRUE, method = "pearson")



#######################################
########### si tu veux choisir les variqbles a retenir



biocM<-stack(bioV$wc2.1_30s_elev, bioV$EVI, bioV$wc2.1_30s_bio_1, bioV$wc2.1_30s_bio_8,
             bioV$wc2.1_30s_bio_15, bioV$LULC)



ex <- raster::extract(biocM,spg)
head(ex)

v <- vifstep(ex)
v


v <- vifcor(ex)



biocM$LULC







#######################################
#--------------------
library(sdm)



d <- sdmData(Fsarda~., spg, predictors= biocM)

getmethodNames()

###################################

###################################

library(parallel)

m <- sdm(Fsarda~., d, methods=c('brt','rf'), replication=c('sub','boot'),
         test.p=30,n=3, parallelSetting=list(ncore=4,method='parallel'))


gui(m)


#p1 <- predict(m, bio,filename='pr.img', overwrite=TRUE)
p1 <- predict(m, newdata=biocM, filename='pr.grd', overwrite=TRUE)


#en1 <- ensemble(m, bio, filename='en.img',setting=list(method='weighted',stat='tss',opt=2))
en1 <- ensemble(m, biocM, filename='en.img', overwrite=TRUE, 
                setting=list(method='weighted',stat='tss',opt=2)) 



#p1 <- ensemble(m, p1, filename='ens.img',setting=list(method='weighted',stat='AUC'))

#########
cl <- colorRampPalette(c('#3E49BB','#3E49BB','yellow','red','darkred'))
#------

plot(en1, col=cl(200), xlab = "Longitude", ylab = "Latitude",
     main = "Habitat suitability")



library(mapview)



mapview(en1, col.regions = cl(200), na.color = "transparent")



###################################################################

######### exportation des fichiers raster



writeRaster(en1, filename = file.path("C:\\Users\\DALLA\\Desktop\\work\\Ssarda\\REVIEW\\results\\NEW1KM", en1), format = "GTiff")




getwd()



###################################################################################################





#-----------#-----------#-----------#-----------#-----------
#-----------#-----------#-----------#-----------#-----------

#---- thresholding to keep only high suitability values 


df <- as.data.frame(d)
df <- data.frame(species=df$Fsarda,coordinates(d))
xy <- as.matrix(df[,c('longitude','latitude')])
head(xy)
p <- raster::extract(en1,xy)
head(p)
nrow(df)
length(p)
ev <- evaluates(df$species,p)
ev@statistics


th <- ev@threshold_based$threshold[3]

pa1 <- raster(en1)

pa1[] <- ifelse(en1[] >= 0.22, 1, 0)
plot(pa1)

cl <- colorRampPalette(c('#3E49BB','#3E49BB','yellow','orange','red'))

############################################

mapview(pa1, na.color = "transparent")


writeRaster(pa1, filename = file.path("C:\\Users\\DALLA\\Desktop\\work\\Ssarda\\REVIEW", pa1), format = "GTiff")



plot(pa1, col=cl(200), xlab = "Longitude", ylab = "Latitude",
     main = "Binary map of habitat suitability (threshold>0.42)")




#-----------


ch <- en2 - en1
cl2 <- colorRampPalette(c('red','orange','yellow','gray','green','blue'))
plot(ch,col=cl2(200))
#----

#--------------- graphics
summary(m)
gui(m)
rcurve(m)

rcurve(m,method = 'brt',smooth = T) # only for models fitted using glm method & with smoothed curve
rcurve(m,method = 'rf',smooth = F) # only for models fitted using glm method & with smoothed curve

plot(getVarImp(m))

roc(m)
# the plots can be smoothes:
roc(m,smooth=T)


###################################################################
###################################################################

######### exportation des fichiers raster



writeRaster(en1, filename = file.path("C:\\Users\\DALLA\\Desktop\\work\\Ssarda", en1), format = "GTiff")


writeRaster(pa1, filename = file.path("C:\\Users\\DALLA\\Desktop\\work\\Ssarda\\results", pa1), format = "GTiff")


?getModelInfo
getModelInfo(m)

library(sdm)

getEvaluation(m,opt=1) # all models


e <-getEvaluation(m,stat=c('TSS','Kappa','AUC','Deviance')) # all models

write.xlsx(e, "Fsarda modelALL.xlsx")

getwd()


?getVarImp
vi <- getVarImp(m) # Mean variable importance for the method glm
vi


plot(vi,'auc')

plot(vi,'cor')

plot(vi, gg = F) # R standard plot is used instead of ggplot


######################niche ecologique

dataclim  


plot(bioV$wc2.1_30s_bio_1)


niche(x=bioV, h=en1, c('wc2.1_30s_bio_1','wc2.1_30s_bio_15'))

niche(x=bioV, h=en1, c('wc2.1_30s_bio_8','wc2.1_30s_elev'))

niche(x=bioV, h=en1, c('LULC','EVI'))

niche(x=bioV, h=en1, c('wc2.1_30s_bio_12','wc2.1_30s_bio_16'))+theme_bw()









bioV$wc2.1_5m_bio_16


gui(m)

####################################################################################################################################
##########################################################################################################################################################
############################################################################################################################################################################################################################



#######################################################################


###############################################################################################################
#####################################v##########################################################################
###############################################################################################################



library(sp)
# Get the extent of the raster
raster_extent <- extent(pa1)


library(dismo)


# Create random points within the raster extent
random_points <- randomPoints(pa1, n = 300, ext = raster_extent)

# Extract values at the random point locations
point_values <- extract(pa1, random_points)



# Filter points for values 0 and 1
points_0 <- random_points[point_values == 0, ]
points_1 <- random_points[point_values == 1, ]




ab<-as.data.frame(points_0)
ps<-as.data.frame(points_1)




coordinates(ab) <- c('x','y')

class(ab)


coordinates(ps) <- c('x','y')

class(ps)




library(mapview)

mapview(pa1,col.regions=cl(200)) + ps 

#########################################################################
#########################################################################
eleveation <- raster::extract(bioV$wc2.1_30s_elev,ps)

dfP<-as.data.frame(eleveation)

dfP$prec <- raster::extract(bioV$wc2.1_30s_bio_12,ps)
dfP$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,ps)
dfP$EVI <- raster::extract(bioV$EVI,ps)
dfP$MTWQ <- raster::extract(bioV$wc2.1_30s_bio_8,ps)
dfP$LULC <- raster::extract(bioV$LULC,ps)
dfP$Tann <- raster::extract(bioV$wc2.1_30s_bio_1,ps)
dfP$pWq <- raster::extract(bioV$wc2.1_30s_bio_16,ps) #Precipitation of Wettest Quarter
dfP$prec <- raster::extract(bioV$wc2.1_30s_bio_12,ps)



#########################################################################


eleveationAB <- raster::extract(bioV$wc2.1_30s_elev,ab)

dfA<-as.data.frame(eleveationAB)


dfA$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,ab)
dfA$EVIAB <- raster::extract(bioV$EVI,ab)
dfA$MTWQAB <- raster::extract(bioV$wc2.1_30s_bio_8,ab)
dfA$LULC <- raster::extract(bioV$LULC,ab)
dfA$Tann <- raster::extract(bioV$wc2.1_30s_bio_1,ab)
dfA$pWqAB <- raster::extract(bioV$wc2.1_30s_bio_16,ab) #Precipitation of Wettest Quarter
dfA$precAB <- raster::extract(bioV$wc2.1_30s_bio_12,ab)






library(xlsx)
write.xlsx(dfP,"presence.xlsx")
write.xlsx(dfA,"absence.xlsx")


getwd()


############################################################################################




spgOCC<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)


coordinates(spgOCC) <- c('longitude','latitude')

class(spgOCC)




#######################################################################






temp <- raster::extract(bioV$wc2.1_30s_bio_1,spgOCC)
df<-as.data.frame(temp)


df$eleveation <- raster::extract(bioV$wc2.1_30s_elev,spgOCC)
df$prec <- raster::extract(bioV$wc2.1_30s_bio_12,spgOCC)
df$EVI <- raster::extract(bioV$EVI,spgOCC)
df$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,spgOCC)
df$MTWQ <- raster::extract(bioV$wc2.1_30s_bio_8,spgOCC)
df$PWQ <- raster::extract(bioV$wc2.1_30s_bio_16,spgOCC)
df$lulc <- raster::extract(bioV$LULC,spgOCC)



getwd()
library(xlsx)
write.xlsx(df,"FsardapresencePAR.xlsx" )

clim<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1,
                row.names=1)





par(mfrow = c(2, 4))


# Create the histogram

# Calculate the mean precipitation
mean_precip <- mean(clim$prec, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
SD_precip <- sd(clim$prec, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present



# Add a vertical line for the mean precipitation
hist(clim$prec, col = "#006600", xlab = "Annual Precipitation", ylab = "Frequency")
abline(v = mean_precip, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type

###############################################


mean_temp <- mean(clim$temp, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
SD_temp <- sd(clim$temp, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present



# Add a vertical line for the mean precipitation
hist(clim$temp, col = "#006600", xlab = "Annual temperature", ylab = "Frequency")
abline(v = mean_temp, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type

###############################################

mean_elev <- mean(clim$eleveation, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
SD_elev <- sd(clim$eleveation, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present



# Add a vertical line for the mean precipitation
hist(clim$eleveation, col = "#006600", xlab = "Elevation", ylab = "Frequency")
abline(v = mean_elev, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type



###############################################


mean_EVI <- mean(clim$EVI, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
SD_EVI <- sd(clim$EVI, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present




# Add a vertical line for the mean precipitation
hist(clim$EVI, col = "#006600", xlab = "Enhanced vegetation index", ylab = "Frequency")
abline(v = mean_EVI, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type


###############################################

PrecipitationS <- mean(clim$PSeasonality, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
SD_PrecipitationS <- sd(clim$PSeasonality, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present




# Add a vertical line for the mean precipitation
hist(clim$PSeasonality, col = "#006600", xlab = "Precipitation Seasonality", ylab = "Frequency")
abline(v = PrecipitationS, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type



###############################################

mean_MTWQ <- mean(clim$MTWQ, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
sd_MTWQ <- sd(clim$MTWQ, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present



# Add a vertical line for the mean precipitation
hist(clim$MTWQ, col = "#006600", xlab = "Mean temperature of wettest quarter", ylab = "Frequency")
abline(v = mean_MTWQ, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type

###############################################

alulc <- mean(df$lulc, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
sd_lulc <- sd(clim$lulc, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present

# Add a vertical line for the mean precipitation
hist(df$lulc, col = "#006600", xlab = "Land use and land cover", ylab = "Frequency")
abline(v = alulc, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type

###############################################

PWQ <- mean(df$PWQ, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present
sd_PWQ <- sd(df$PWQ, na.rm = TRUE)  # Calculate mean, na.rm = TRUE removes NAs if present

# Add a vertical line for the mean precipitation
hist(df$PWQ, col = "#006600", xlab = "Precipitation of Wettest Quarter", ylab = "Frequency")
abline(v = PWQ, col = "red", lty = 5)  # 'v' for vertical line, col for color, lty for line type









########################################################################################
########################################################################################


#########################################################################
#########################################################################
eleveation <- raster::extract(bioV$wc2.1_30s_elev,spgOCC)

dfP<-as.data.frame(eleveation)

dfP$prec <- raster::extract(bioV$wc2.1_30s_bio_12,spgOCC)
dfP$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,spgOCC)
dfP$EVI <- raster::extract(bioV$EVI,spgOCC)
dfP$MTWQ <- raster::extract(bioV$wc2.1_30s_bio_8,spgOCC)
dfP$LULC <- raster::extract(bioV$LULC,spgOCC)
dfP$Tann <- raster::extract(bioV$wc2.1_30s_bio_1,spgOCC)
dfP$pWq <- raster::extract(bioV$wc2.1_30s_bio_16,spgOCC) #Precipitation of Wettest Quarter
dfP$prec <- raster::extract(bioV$wc2.1_30s_bio_12,spgOCC)




#########################################################################



eleveationAB <- raster::extract(bioV$wc2.1_30s_elev,ab)

dfA<-as.data.frame(eleveationAB)


dfA$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,ab)
dfA$EVIAB <- raster::extract(bioV$EVI,ab)
dfA$MTWQAB <- raster::extract(bioV$wc2.1_30s_bio_8,ab)
dfA$LULC <- raster::extract(bioV$LULC,ab)
dfA$Tann <- raster::extract(bioV$wc2.1_30s_bio_1,ab)
dfA$pWqAB <- raster::extract(bioV$wc2.1_30s_bio_16,ab) #Precipitation of Wettest Quarter
dfA$precAB <- raster::extract(bioV$wc2.1_30s_bio_12,ab)



getwd()


library(xlsx)
write.xlsx(dfP,"presence.xlsx")
write.xlsx(dfA,"absence.xlsx")

########################################################################################
########################################################################################







library(xlsx)
data<-read.xlsx(file = file.choose(), header = T, sheetIndex = 1)



# Define labels for each land cover class
lulc_labels <- c(
  "Evergreen needleleaf vegetation",
  "Evergreen broadleaf vegetation",
  "Deciduous needleleaf vegetation",
  "Deciduous broadleaf vegetation",
  "Annual broadleaf vegetation",
  "Annual herbaceous vegetation",
  "Non-vegetated land",
  "Urban and built-up areas"
)

# Convert LULC to factor with levels and labels
data$LULCF <- factor(
  round(data$LULC),  # Round to the nearest integer
  levels = 1:8,      # Define the levels from 1 to 8
  labels = lulc_labels  # Assign the corresponding labels
)



levels(data$LULCF)

data$LULCF <- relevel(data$LULCF, ref = "Annual herbaceous vegetation")

model <- glm(PA ~  LULCF, 
             data = data, family = "binomial")

summary(model)


levels(data$LULCF)


data$LULCF <- relevel(data$LULCF, ref = "Annual broadleaf vegetation")


modelF <- glm(PA ~  EVIAB+LULCF+MTWQAB+eleveationAB+Tann+PSeasonality, 
             data = data, family = "binomial")


summary(modelF)




library(ggstats)
ggcoef_table(model)




data$eleveationAB

model <- glm(PA ~  eleveationAB, 
             data = data, family = "binomial")


summary(model)

data

# Summary of the model


library(car)

# Calculate VIF for predictors in the model
vif_result <- vif(model)
print(vif_result)

library(performance)

coll<-check_collinearity(model)
plot(coll)


library(ggplot2)

################### 
# Plot for smod_code
A<-ggplot(data, aes(x = eleveationAB, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Elevation")+theme_pubr()

################### Plot for EVI
B<-ggplot(data, aes(x = LULC, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability (Presence/Absence)")+
  xlab("Land use and land cover")+
  theme_pubr()


###################

C<-ggplot(data, aes(x = Tann, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Annual temperature")+theme_pubr()

################### Plot for EVI
D<-ggplot(data, aes(x = EVIAB, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Enhanced vegetation index")+
  theme_pubr()

E<-ggplot(data, aes(x = MTWQAB, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Mean temperature of wettest quarter")+
  theme_pubr()

H<-ggplot(data, aes(x = PSeasonality, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Precipitation Seasonality")+
  theme_pubr()

G<-ggplot(data, aes(x = pWqAB, y = PA)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = T) +
  ylab("Probability of Suitability")+
  xlab("Precipitation of Wettest Month")+
  theme_pubr()


###############################################################################
###############################################################################
###############################################################################


library(ggpubr)
ggarrange(A,C,D,E,H,
          ncol = 3, nrow = 2)



summary(model)


library(broom)

# Obtenir un tableau structuré avec les coefficients
results_table <- tidy(model)
print(results_table)



write.xlsx(results_table,"GLMresults.xlsx")

getwd()

































library(dismo)


# Create random points within the raster extent
random_points <- randomPoints(en1, n = 2000, ext = raster_extent)

# Extract values at the random point locations
point_values <- extract(en1, random_points)





eleveation <- raster::extract(bioV$wc2.1_30s_elev,random_points)

dfP<-as.data.frame(eleveation)

dfP$prec <- raster::extract(bioV$wc2.1_30s_bio_12,random_points)
dfP$PSeasonality <- raster::extract(bioV$wc2.1_30s_bio_15,random_points)
dfP$EVI <- raster::extract(bioV$EVI,random_points)
dfP$MTWQ <- raster::extract(bioV$wc2.1_30s_bio_8,random_points)
dfP$LULC <- raster::extract(bioV$LULC,random_points)
dfP$Tann <- raster::extract(bioV$wc2.1_30s_bio_1,random_points)
dfP$PWQ <- raster::extract(bioV$wc2.1_30s_bio_16,random_points) #Precipitation of Wettest Quarter





dfP$suitability <- raster::extract(en1,random_points)






#############################################################

my_palette <- c('dared','orange','yellow','green','blue')  # Replace with your desired colors



# Your existing ggplot code
A<-ggplot(dfP, aes(x = Tann, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Annual Mean Temperature") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data




B<-ggplot(dfP, aes(x = eleveation, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Eleveation") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data


C<-ggplot(dfP, aes(x = PSeasonality, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Precipitation Seasonality") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data



#method = "glm", method.args = list(family = "poisson"), se = TRUE

#D<-ggplot(dfP, aes(x = LULC, y = suitability)) +
#  geom_point(aes(colour = suitability, size=suitability)) +
# geom_smooth(method = "loess", 
#             se = T) +
# ylab("Suitability") +
# xlab("Land use and land cover") +
# theme_bw() +
# scale_colour_gradientn(colours = terrain.colors(10))

D<-ggplot(DFFF, aes(x = LLUC, y = SUIT)) +
  geom_point(aes(colour = SUIT, size = SUIT)) +
  geom_smooth(method = "loess", se = TRUE) +
  ylab("Suitability") +
  xlab("Land use and land cover") +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  scale_x_continuous(breaks = 1:8, labels = 1:8, 
                     guide = guide_axis(angle = 0)) +
  theme(axis.text.x = element_text())



E<-ggplot(dfP, aes(x = MTWQ, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Mean Temperature of Wettest Quarter") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data




Fe<-ggplot(dfP, aes(x = PWQ, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Precipitation of Wettest Quarter") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data



g<-ggplot(dfP, aes(x = prec, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Annual Precipitation") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data


H<-ggplot(dfP, aes(x = EVI, y = suitability)) +
  geom_point(aes(colour = suitability, size=suitability)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Enhanced vegetation index") +
  theme_bw() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))  # Limit y-axis without removing data




# Define labels for each land cover class
lulc_labels <- c(
  "Evergreen needleleaf vegetation",
  "Evergreen broadleaf vegetation",
  "Deciduous needleleaf vegetation",
  "Deciduous broadleaf vegetation",
  "Annual broadleaf vegetation",
  "Annual herbaceous vegetation",
  "Non-vegetated land",
  "Urban and built-up areas"
)



lulc_labels <- c(
  "ENV",
  "EBV",
  "DNV",
  "DBV",
  "ABV",
  "AHV",
  "NVL",
  "Urban areas"
)


# Convert LULC to factor with levels and labels
dfP$LULCF <- factor(
  round(dfP$LULC),  # Round to the nearest integer
  levels = 1:8,      # Define the levels from 1 to 8
  labels = lulc_labels  # Assign the corresponding labels
)



D<-ggplot(dfP, aes(x = LULCF, y = suitability)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  geom_boxplot(outlier.shape = NA, fill = "grey90", colour = "black", alpha = 0.5) +
  ylab("Suitability") +
  xlab("Land use and land cover") +
  theme_pubr() +
  scale_colour_gradientn(
    colours = terrain.colors(10),
    limits = c(0, 0.3)  # Limit color scale to 0.3
  ) +
  scale_size_continuous(
    limits = c(0, 0.3)  # Limit size scale to 0.3
  ) +
  coord_cartesian(ylim = c(0, 0.4))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))






levels(data$LULCF)



library(ggpubr)


ggarrange(A,B,C,E,Fe,g,H,D,
          ncol = 4, nrow = 2)





biocM$LULC





(INTERSECTION <- st_read("C:/Users/DALLA/Desktop/work/Ssarda/REVIEW/data/shapfile/INTERSECTION.shp"))
plot(INTERSECTION)

library(dplyr)
library(tidyverse)



INTERSECTION <- INTERSECTION %>%
  select(geometry) 

plot(INTERSECTION)




FACLULC <- crop(FACLULC, extent(INTERSECTION))
FACLULC <- mask(FACLULC, INTERSECTION)

Cen1 <- crop(en1, extent(INTERSECTION))
Cen1 <- mask(Cen1, INTERSECTION)


FACLULC<-(biocM$LULC)



random_points <- randomPoints(FACLULC, n = 1000, ext = raster_extent)



en1 <- resample(Cen1, FACLULC, method="bilinear") # Align `en1` to `r_classified`




LLUC <- raster::extract(FACLULC,random_points)
DFFF$SUIT <- raster::extract(en1,random_points)

DFFF<-as.data.frame(LLUC)

head(DFFF)
plot(SUIT~LLUC)



ggplot(DFFF, aes(x = LLUC, y = SUIT)) +
  geom_point(aes(colour = SUIT, size=SUIT)) +
  geom_smooth(method = "loess", 
              se = T) +
  ylab("Suitability") +
  xlab("Land use and land cover") +
  theme_bw() +
  scale_colour_gradientn(colours = terrain.colors(10))






my_palette <- colorRampPalette(rev(c('darkblue','blue', 'green', 'yellow', 'orange', 'red', 'darkred')))

ggplot(dfP, aes(x = Tann, y = PSeasonality)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  xlab("Annual Temperature") +
  ylab("Precipitation Seasonality") +
  theme_bw() +
  scale_colour_gradientn(colours = my_palette(100))

ggplot(dfP, aes(x = MTWQ, y = eleveation)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  xlab("Mean temperature of wettest quarter") +
  ylab("Elevation") +
  theme_bw() +
  scale_colour_gradientn(colours = my_palette(100))

ggplot(dfP, aes(x = prec, y = PWQ)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  xlab("Annual Precipitation") +
  ylab("Precipitation of Wettest Quarter") +
  theme_bw() +
  scale_colour_gradientn(colours = my_palette(100))


ggplot(dfP, aes(x = prec, y = PWQ)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  xlab("Precipitation Seasonality") +
  ylab("Annual Temperature") +
  theme_bw() +
  scale_colour_gradientn(colours = my_palette(100))



# Define labels for each land cover class
lulc_labels <- c(
  "Evergreen needleleaf vegetation",
  "Evergreen broadleaf vegetation",
  "Deciduous needleleaf vegetation",
  "Deciduous broadleaf vegetation",
  "Annual broadleaf vegetation",
  "Annual herbaceous vegetation",
  "Non-vegetated land",
  "Urban and built-up areas"
)



lulc_labels <- c(
  "ENV",
  "EBV",
  "DNV",
  "DBV",
  "ABV",
  "AHV",
  "NVL",
  "Urban areas"
)

#dfP$LULC

# Convert LULC to factor with levels and labels
dfP$LULCF <- factor(
  round(dfP$LULC),  # Round to the nearest integer
  levels = 1:8,      # Define the levels from 1 to 8
  labels = lulc_labels  # Assign the corresponding labels
)

ggplot(dfP, aes(x = LULCF, y = EVI)) +
  geom_point(aes(colour = suitability, size = suitability)) +
  xlab("Land use and land cover") +
  ylab("Enhanced Vegetation Index") +
  theme_bw() +
  scale_colour_gradientn(colours = my_palette(100))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

