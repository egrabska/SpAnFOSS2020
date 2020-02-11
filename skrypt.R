#skrypt do warsztatów SpAnFOSS2020 z przetwarzania danych satelitarnych  Sentinel-2

pakiety = c("raster", "RStoolbox", "sf", "rgdal", "ggplot2", "kernlab")


require(raster)
library(RStoolbox)
library(sf)
library(rgdal)
library(ggplot2)


setwd("D:/09_Dydaktyka/warsztaty_spanfoss/L1C_T34UDD_A011645_20190530T094350/IMG_DATA")
bands = list.files()
bands

img10 = stack(bands[c(2,3,4,8)])
img20 = stack(bands[c(11,12)])
img10_crop = crop(img10, c(470000, 480000, 5820000, 5830000))
img20_crop = crop(img20, c(470000, 480000, 5820000, 5830000))

img20_crop

img20_res = resample(img20_crop, img10_crop, method="ngb")
img20_res

all_bands = stack(img10_crop, img20_res)
all_bands

#analiza "hotspot?w" - wykorzystanie kana??w kr?tko-falowej podczerwieni

par(mfrow=c(2,2))
plotRGB(all_bands, r= 6, g=4, b =3, stretch = "lin")
all_bands

#sygnatury spektralne
pts = readOGR("D:/09_Dydaktyka/warsztaty_spanfoss/points2.shp", pointDropZ = TRUE)

e = extract(all_bands, pts)
e_df = as.data.frame(e)
e_df$type = pts$typ

library(reshape2)
e_df_melt = melt(e_df)
e_df_melt

library(ggplot2)
ggplot(e_df_melt, aes(variable, value, color = type,group =type))+
  geom_point()+
  geom_line()

#indeksy 




