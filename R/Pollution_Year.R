####################################################################################
### pollution_period() 	
### ex) pollution_year(2016)
### Data Visualization map of air pollutant dataset in Seoul from 2010 to 2016
### 미세먼지(PM10), 초미세먼지(PM2.5), 오존(Ozone), 이산화질소(Nitrous Oxide), 일산화탄소(Carbon Monoxide), 아황산가스(SO2)			
####################################################################################

pollution_year <- function(year1) {
  
  
  # Loda Packages
  library(ggplot2)
  library(ggmap)
  library(raster)
  library(data.table)
  library(scales)
  library(showtext)
  library(gridExtra)
  
  
  # font load
  font_add("NanumGothic", "NanumGothic.ttf")
  showtext_auto()
  
  ##서울 지도 데이터 가져오기  
  seoul <- read.csv("https://www.dropbox.com/s/1vzkggtxjqcfqsr/seoul.csv?dl=1", header=T)
  
  
  ### 지도 데이터에 행정구역 이름 붙이기
  seoul <- data.table(seoul, '지역'=ifelse(seoul$id == 11110, '종로구',
                                         ifelse(seoul$id == 11140, '중구',
										 ifelse(seoul$id == 11170, '용산구',
                                         ifelse(seoul$id == 11200, '성동구',
                                         ifelse(seoul$id == 11215, '광진구',
                                         ifelse(seoul$id == 11230, '동대문구',
                                         ifelse(seoul$id == 11260, '중랑구',
                                         ifelse(seoul$id == 11290, '성북구',
                                         ifelse(seoul$id == 11305, '강북구',
                                         ifelse(seoul$id == 11320, '도봉구',
                                         ifelse(seoul$id == 11350, '노원구',
                                         ifelse(seoul$id == 11380, '은평구',
                                         ifelse(seoul$id == 11410, '서대문구',
                                         ifelse(seoul$id == 11440, '마포구',
                                         ifelse(seoul$id == 11470, '양천구',
                                         ifelse(seoul$id == 11500, '강서구',
                                         ifelse(seoul$id == 11530, '구로구',
								 	  	 ifelse(seoul$id == 11545, '금천구',
                                         ifelse(seoul$id == 11560, '영등포구',
                                         ifelse(seoul$id == 11590, '동작구',
                                         ifelse(seoul$id == 11620, '관악구',
                                         ifelse(seoul$id == 11650, '서초구',
                                         ifelse(seoul$id == 11680, '강남구',
                                         ifelse(seoul$id == 11710, '송파구',  
                                              '강동구')))))))))))))))))))))))))
  
  ### pollution_map 시각화
  
	  ## 년도별 데이터 넣기
	  if (year1 == 2010) {
		year2 <- "https://www.dropbox.com/s/r10jyuaxue7wuzn/2010.csv?dl=1"
	  }
	  
	  else if (year1 == 2011) {
		year2 <- "https://www.dropbox.com/s/2q046nt0gts8fle/2011.csv?dl=1"
		
	  }
	  
	  else if (year1 == 2012) {
		year2 <- "https://www.dropbox.com/s/h3yy887h9iyuu5n/2012.csv?dl=1"
		
	  }
	  
	  else if (year1 == 2013) {
		year2 <- "https://www.dropbox.com/s/caonh9w3gb6svam/2013.csv?dl=1"
		
	  }
	  
	  else if (year1 == 2014) {
		year2 <- "https://www.dropbox.com/s/pgnt07u7as8k0f5/2014.csv?dl=1"
		
	  }
	  
	  else if (year1 == 2015) {
		year2 <- "https://www.dropbox.com/s/zvfanfjutnacv9a/2015.csv?dl=1"
		
	  }
	  
	  else if (year1 == 2016) {
		year2 <- "https://www.dropbox.com/s/3lvsj2qv871c2h9/2016.csv?dl=1"
		
	  }
	  
	  
	  
	  
	  ##데이터 로드 및 평균 내기
	  
	  pol<-read.csv(year2, header=T)
	  pol[is.na(pol)]<-0
	  names(pol)[2] <- "지역"
	  pol1<-pol[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
	  polyear<-data.frame(substr(pol$날짜,1,4))
	  names(polyear)<-"날짜"
	  pol2<-cbind(polyear,pol1)
	  pol <- aggregate(pol2[,3:8],list(pol2$날짜,pol2$지역),mean)
	  names(pol)[2] <- "지역"
	  
	  ##pol 데이터에 id 붙이기
	  seoul1 <- unique(seoul[, c("지역", "id")])
	  pol <- merge(pol, seoul1, by='지역')
	  
	  ##지도와 데이터 merge
	  pol_seoul<- merge(seoul, pol, by='id')
	  
	  ## 오염 물질 데이터로 단계 구분 지도 만들기
	  names(pol_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
	  
	  
	  ## 행정 구역 이름을 지도에 넣기 위한 작업
	  cnames <- aggregate(cbind(long,lat) ~ 지역.x, data=pol_seoul, FUN=function(x) mean(range(x)))
	  
	  
	  ## 오염물질별 지도 시각화 작업 
	  
	  fine_dust <- pol_seoul$fine_dust
	  p<- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=fine_dust))+theme_void()
	  s <- scale_fill_distiller(name="PM-10\n단위(㎍/㎥)", palette="Reds", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  fine_dust_map <- p + s + g + coord_map() + 
		ggtitle('미세먼지') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  micro_dust <- pol_seoul$micro_dust
	  p <- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=micro_dust))+ theme_void()
	  s <- scale_fill_distiller(name="PM-25\n단위(㎍/㎥)", palette="YlOrRd", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  micro_dust_map <- p + s + g + coord_map() + 
		ggtitle('초미세먼지') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  
	  ozone <- pol_seoul$ozone
	  p <- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=ozone))+theme_void()
	  s <- scale_fill_distiller(name="03\n단위(ppm)", palette="PuBuGn", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  ozone_map <- p + s + g + coord_map() + 
		ggtitle('오존') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  nitro_dioxide <- pol_seoul$nitro_dioxide
	  p <- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide))+theme_void()
	  s <- scale_fill_distiller(name="NO2\n단위(ppm)", palette="BuPu", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  nitro_dioxide_map <-  p + s + g + coord_map() + 
		ggtitle('이산화질소') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  carbon_monoxide <- pol_seoul$carbon_monoxide
	  p <- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide))+theme_void()
	  s <- scale_fill_distiller(name="CO\n단위(ppm)", palette="YlGnBu", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  carbon_monoxide_map <-  p + s + g + coord_map() + 
		ggtitle('일산화탄소') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  
	  sulfurous_acid_gas <- pol_seoul$sulfurous_acid_gas
	  p <- ggplot() + geom_polygon(data=pol_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas))+theme_void()
	  s <- scale_fill_distiller(name="SO2\n단위(ppm)", palette="YlOrBr", breaks = pretty_breaks(n=5),direction = 1) 
	  g <- guides(fill = guide_legend(reverse = T))
	  
	  sulfurous_acid_gas_map <-p + s + g + coord_map() + 
		ggtitle('아황산가스') +
		theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), 
			  legend.title = element_text(family = "NanumGothic", face = "bold", size = 8)) +
		theme(panel.spacing=unit(1,"lines")) + geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")
	  
	  
  
  
  ## output of the total maps
  grid.arrange(fine_dust_map, micro_dust_map, ozone_map, nitro_dioxide_map, carbon_monoxide_map, sulfurous_acid_gas_map, ncol=3)
  
  }