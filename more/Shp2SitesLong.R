library(rgdal)
pt <- readOGR("/Users/roli/Desktop/va.rar Folder", "va")
pt <- spTransform(pt, CRS("+init=epsg:4326"))

df <- cbind(coordinates(pt), as.character(pt$Comment))
write.csv2(df, "~/foo.csv")