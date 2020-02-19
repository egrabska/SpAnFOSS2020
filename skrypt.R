library(raster)
library(RStoolbox)
library(sf)
library(rgdal)
library(ggplot2)
library(dplyr)
library(caTools)
library(reshape2)


setwd("D:/09_Dydaktyka/warsztaty_spanfoss/dane")
list.files()
list.files(pattern = "*.tif$")
img1 = stack("c20190418.tif")
img2 = stack("c20190423.tif")
img3 = stack("c20190425.tif")
plot(img1, col = gray(0:100 / 100))
plotRGB(img1, r =3, g=2, b=1, stretch = "lin")


#funckja pairs() - ró¿ne kombinacje
hist(img1[[1]])
pairs(img1)
pairs(img1[[c(3,10)]])
#pairs dla red i red-edge1 oraz dla red-edge1 i red-edge2
pairs(img1[[4:5]], main = "Red-edge1 versus red-edge2")
pairs(img1[[5:6]], main = "Red-edge2 versus red-edge3")


#analiza hotspostów na obrazie z po¿arem
img2 = stack("c20190423.tif")
plot(img2, col = gray(0:100 / 100))
plotRGB(img2, r =3, g=2, b=1, stretch = "lin")
plotRGB(img2, r =10, g=9, b=3, stretch = "lin")
plotRGB(img2, r =10, g=7, b=3, stretch = "lin")

#sygnatury spektralne-----
#wczytywanie danych referencyjnych:

ref1 = readOGR("D:/09_Dydaktyka/warsztaty_spanfoss/dane/reference.shp", pointDropZ = TRUE)
ref2 = shapefile("D:/09_Dydaktyka/warsztaty_spanfoss/dane/reference.shp")
ref3 = st_read("D:/09_Dydaktyka/warsztaty_spanfoss/dane/reference.shp")

#jakie s¹ klasy tych obiektów?
class(ref3)



ref_val = extract(img3, ref3, fun = "mean",na.rm=TRUE) %>% as.data.frame()
ref_val
names(ref_val) = c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2")
ref_val$class = ref3$klasa

#wizualizacja z wykorzytaniem pakietu ggplot2

pairs(ref_val)

ggplot(ref_val, aes(blue, nir1, color = class))+
  geom_point(size = 3)

#przekstza³cenie data frame 
ref_class_melt = melt(ref_val)
ref_class_melt

#wizualizacja krzywych spektralnych
ggplot(ref_class_melt, aes(variable, value, color = class, group = class))+
  geom_line(size = 1.5)+
  scale_colour_manual(values = c("darkgrey", "brown", "red", "darkgreen", "yellow", "green", "blue"))


#algebra map
diff = img3-img1
plotRGB(diff, r =10, g=7, b=3, stretch = "lin")

diff_swir = img3[[9]] - img1[[9]]
plot(diff_swir, col = gray(0:100 / 100))

NBR_t3 = (img3[[7]] - img3[[10]])/(img3[[7]] + img3[[10]])
NBR_t1 = (img1[[7]] - img1[[10]])/(img1[[7]] + img1[[10]])
plot(NBR_t3 - NBR_t1)

NDVI1 = (img1[[7]] - img1[[3]])/(img1[[7]] + img1[[3]])
NDVI3 = (img3[[7]] - img3[[3]])/(img3[[7]] + img3[[3]])
plot(NDVI3 - NDVI1)

NDMI1 = (img1[[7]] - img1[[9]])/(img1[[7]] + img1[[9]])
NDMI3 = (img3[[7]] - img3[[9]])/(img3[[7]] + img3[[9]])
plot(NDMI3 - NDMI1)

hist(NDMI3, breaks = 10)

#utworzenie funkcji do obliczania indeksu NBR
NBR = function(obraz){
  wynik = (obraz[[7]] - obraz[[10]])/(obraz[[7]] + obraz[[10]])
}


#zapisywanie wyników na dysku
writeRaster()


#CZÊŒÆ 2 -  analiza sk³adu gatunkowego i klasyfikacja
setwd("D:/09_Dydaktyka/warsztaty_spanfoss/dane")
list.files(pattern = "*.tif$")

lip_las = stack("las20190701.tif")
maj_las = stack("las20180512.tif")
paz_las = stack("las20181017.tif")

las3 = stack(lip_las, maj_las, paz_las)

gatunki = st_read("ref_gatunki.shp")

#analiza wartoœci spektralnych 

pts_gat = gatunki %>% 
  st_sample(rep(10, nrow(gatunki)), type = 'random') %>% 
  st_sf() %>%
  st_join(gatunki, join=st_intersects)

pts_values = extract(lipiec_las, pts_gat) %>% as.data.frame()
pts_values$species = pts_gat$species_cd

pairs(pts_values[-11])
names(pts_values) = c("blue", "green", "red", "re1", "re2", "re3", "nir1", "nir2", "swir1", "swir2", "gatunek")


#zwyk³y scatterplot
ggplot(pts_values, aes(swir1, re1, color = gatunek))+
  geom_point()+
  stat_ellipse(size=1.5)

#aby policzyæ œrednie wartoœci odbicia dla gatunków (mamy dla osobnych, losowych punktów) - musimy zamieniæ df uzywaj¹c melt
pts_melt = melt(pts_values)

#a nastêpnie policzyæ œredni¹ - grupuj¹c po gatunki i kanale
sr_gatunki = summarise(group_by(pts_melt, gatunek, variable), srednia = mean(value, na.rm = TRUE))
sr_gatunki


ggplot(sr_gatunki, aes(variable, srednia, color = gatunek, group = gatunek))+
  geom_point()+
  geom_line(size=1.5, alpha = 0.7)


#KLASYFIKACJA - podzia³ danych referencyjnych na trening i walidacjê
set.seed(123) #set.seed zapewnia zawsze ten sam randomowy podzia³ 
split = sample.split(gatunki$species_cd, 0.7)
training_set = subset(gatunki, split == TRUE)
validation_set = subset(gatunki, split == FALSE)

start_time = Sys.time()
klasyfikacja = superClass(all_las, training_set, validation_set, responseCol = "species_cd", mode = "classification", 
                          model = "svmLinear")
end_time = Sys.time()
end_time - start_time
klasyfikacja

#dok³adnoœæ
klasyfikacja

#zapisywanie rastrów
writeRaster(klasyfikacja$map, "klasyfikacjaSVMall.tif")


#--------------feature selection/reduction techniques------------------
#losowe punkty - tak jak wczeœniej (tylko mniej)
pts = gatunki %>% 
  st_sample(rep(2, nrow(gatunki)), type = 'random') %>% 
  st_sf() %>%
  st_join(gatunki, join=st_intersects)

pts_values = extract(las3, pts) %>% as.data.frame()

#podstawienie kolumny z klasami gatunków 
pts_values$species = pts$species_cd
pts_values = pts_values[complete.cases(pts_values),]


#MDA/MDG
library(randomForest)

rf = randomForest(species~., data=pts_values, importance=TRUE, method="cv", number=100)
imp = importance(rf)

imp
varImpPlot(rf)

library(VSURF)
#VSURF liczy siê trochê d³ugo
vsurf = VSURF(species~., pts_values)
summary(vsurf)
plot(vsurf)
inter = vsurf$varselect.interp


#RFE
library(caret)
start_time = Sys.time()

control = rfeControl(functions=rfFuncs, method="cv", number=10)
results = rfe(pts_values[,1:30], pts_values[,31], sizes=c(1:30), rfeControl=control)
end_time = Sys.time()
end_time - start_time

results
plot(results, type=c("g", "o"))
pred= predictors(results)






#Jakie s¹ najwa¿niejsze kana³y? zróbmy klasyfikacjê na najwazniejszych

imp10 = all_las[[pred]]
imp_vsurf = all_las[[inter]]

start_time = Sys.time()
klasyfikacja = superClass(imp_vsurf, training_set, validation_set, responseCol = "species_cd", mode = "classification", 
                          model = "svmLinear")
end_time = Sys.time()
end_time - start_time



