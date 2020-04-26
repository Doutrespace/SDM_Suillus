#setwd(choose.dir())
setwd("D:/QGIS_Poster/Species_Distrebution")

###install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("sp","SSDM","mapview","usdm","Rcpp","dismo","rgbif","sdm","raster","rlist","getSpatialData","sf","sp","list","dplyr","lubridate","rgdal","data.table","devtools","svDialogs","gdalUtils","Rcpp")
ipak(packages)

# Summary Species Data
out <- name_lookup(query='Suillus')
head(name_lookup(query='Suillus', return = 'data'))
occ_search(scientificName = "Suillus", fields=c('name','basisOfRecord','protocol'), limit = 20)

#open GUI
gui()

#Download Species Data
sp <- gbif("Suillus",download = T,geo=T,sp=F)
class(sp)
str(sp)

#assign lon lat
sp <- sp[,c('lon','lat')]
head(sp)
w <- which(is.na(sp$lon))
sp <- sp[-w,]
w <- which(is.na(sp$lat))
sp$species <- 1
head(sp)
coordinates(sp) <- ~ lon + lat
class(sp)
plot(sp)

#### Download Bioclim data for the current time

bio <- raster::getData('worldclim', var='bio', res=2.5)
plot(bio[[1]])
names(bio)

# CMIP5 Data
biof <- raster::getData('CMIP5', var='bio', res=2.5, year= 70, model = "AC",rcp=85)

plot(biof[[1]],col=bpy.colors(100))

names(biof) <- names(bio)
head(sp)

#remove collinear variables
spx <- extract(bio, sp)
head(spx)
class(spx)
spx <- data.frame(spx)
v <- vifstep(spx)
bio <- exclude(bio, v)
bio


#train the model
d <- sdmData(species~., train=sp, predictors= bio, bg=list(n=1000))

d

m <- sdm(species ~ . , d, methods=c('glm','brt','rf','svm','bioclim.dismo'),
         replication=c("boot"), n=2)

m
# open GUI
gui(m)

#Present Prediction
en <- ensemble(m, bio, filename = 'ens_current.img', setting=list(method='weighted',stat='TSS',opt=2 ))
# Future Prediction
enf <- ensemble(m, biof, filename = 'ens_future.img', setting=list(method='weighted',stat='TSS',opt=2))

plot(en)
plot(enf)

#Write Raster
writeRaster(en,"D:/QGIS_Poster/Species_Distrebution", overwrite= TRUE)
writeRaster(enf,"D:/QGIS_Poster/Species_Distrebution", overwrite= TRUE)

mapview(stack(en, enf))

mapview(stack(bio[[6]],biof[[18]]))

# prepare distinction map
ch <- enf - en
cl <- colorRampPalette(c('red','white','green','darkblue'))
plot(ch,col=cl(100))

th <- getEvaluation(m,stat='threshold')
mean(th[,2])

df <- data.frame(as.data.frame(d),coordinates(d))
head(df)

pr <- extract(en, df[,c('lon','lat')])

head(pr)
ev <- evaluates(df$species, pr)
ev@threshold_based

th <- 0.422682583

pa <- en

pa[] <- ifelse(pa[] >= th, 1, 0)

paf <- enf

paf[] <- ifelse(paf[] >= th, 1, 0)

plot(pa)
plot(paf)


pa.ch <- paf - pa
plot(pa.ch)


cl <- colorRampPalette(c('red','gray80','darkgreen'))
plot(pa.ch,col=cl(3))


e <- drawExtent()

enc <- crop(en, e)
plot(enc)
writeRaster(pa.ch,"D:/QGIS_Poster",overwrite= TRUE)
#extent(c()) # for specifying the coordintae if you have them (UL & LR)


