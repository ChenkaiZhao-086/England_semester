china_map <- rgdal::readOGR('~/Documents/论文1/空气污染数据/map/bou2_4p.shp')
x <- china_map@data # 读取行政信息，引入id信息，方便后面与chia_map1合并，-1为了与chia_map1一致同为从0开始
xs <- data.frame(x, id = seq(1:925)-1) 
china_map1 <- fortify(china_map) # 转化为数据框
china_map_data <- join(china_map1, xs, type = "full") # 合并两个数据框，joining by id
regn <- read.csv("~/Documents/论文1/空气污染数据/province.csv", fileEncoding = 'GBK') # 需要找到各省的经纬度，fileEncoding = 'GBK'用于解决mac读中文内容出现乱码的问题
regn <- as.data.frame(regn)
china_data <- join(china_map_data, regn, type='full') # 合并两个数据框，joining by NAME
poi <- read.csv("~/Documents//论文1/空气污染数据/poision.csv", fileEncoding = 'GBK') # 找到各城市经纬度
poi <- as.data.frame(poi)

#图例无字，无城市名
reg1 <- ggplot(china_data, aes(x = long, y = lat, group = group, fill = Region)) +
  geom_polygon(colour='ivory3') + 
  coord_map('polyconic') +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(), legend.position = c(0,0.3))