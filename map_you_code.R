map.GB.LAT <- readOGR(dsn = "raw/LAT_shp", stringsAsFactors = TRUE)
map.data <- fortify(map.GB.LAT)
map.metadata <- data.frame(id = as.character(map.GB.LAT$OBJECTID-1),
                           CD = map.GB.LAT$LAD19CD,
                           Name = map.GB.LAT$LAD19NM)
write.csv(map.metadata, file = "linkage/Map.csv")
save(map.data, file = "cleaned/map.RData")