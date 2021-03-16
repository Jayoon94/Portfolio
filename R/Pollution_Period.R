####################################################################################
### pollution_period() 	
### pollution_period("미세먼지")
### Data Visualization map of air pollutant dataset in Seoul from 2010 to 2016
### 미세먼지(PM10), 초미세먼지(PM2.5), 오존(Ozone), 이산화질소(Nitrous Oxide), 일산화탄소(Carbon Monoxide), 아황산가스(SO2)	
####################################################################################

pollution_period <- function(pollution1) {
  
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
year10 <- "https://www.dropbox.com/s/r10jyuaxue7wuzn/2010.csv?dl=1"
year11 <- "https://www.dropbox.com/s/2q046nt0gts8fle/2011.csv?dl=1"
year12 <- "https://www.dropbox.com/s/h3yy887h9iyuu5n/2012.csv?dl=1"
year13 <- "https://www.dropbox.com/s/caonh9w3gb6svam/2013.csv?dl=1"
year14 <- "https://www.dropbox.com/s/pgnt07u7as8k0f5/2014.csv?dl=1"
year15 <- "https://www.dropbox.com/s/zvfanfjutnacv9a/2015.csv?dl=1"
year16 <- "https://www.dropbox.com/s/3lvsj2qv871c2h9/2016.csv?dl=1"

## 데이터 로드 및 평균 내기

  pol10<-read.csv(year10, header=T)
  pol11<-read.csv(year11, header=T)
  pol12<-read.csv(year12, header=T)
  pol13<-read.csv(year13, header=T)
  pol14<-read.csv(year14, header=T)
  pol15<-read.csv(year15, header=T)
  pol16<-read.csv(year16, header=T)


  pol10[is.na(pol10)]<-0
  pol11[is.na(pol11)]<-0 
  pol12[is.na(pol12)]<-0
  pol13[is.na(pol13)]<-0
  pol14[is.na(pol14)]<-0
  pol15[is.na(pol15)]<-0
  pol16[is.na(pol16)]<-0			  


  names(pol10)[2] <- "지역"
  names(pol11)[2] <- "지역"
  names(pol12)[2] <- "지역"
  names(pol13)[2] <- "지역"
  names(pol14)[2] <- "지역"
  names(pol15)[2] <- "지역"
  names(pol16)[2] <- "지역"		  


  pol1.1<-pol10[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.2<-pol11[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.3<-pol12[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.4<-pol13[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.5<-pol14[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.6<-pol15[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]
  pol1.7<-pol16[, c("지역","미세먼지","초미세먼지","오존","이산화질소","일산화탄소","아황산가스")]


  polyear1<-data.frame(substr(pol10$날짜,1,4))
  polyear2<-data.frame(substr(pol11$날짜,1,4))
  polyear3<-data.frame(substr(pol12$날짜,1,4))
  polyear4<-data.frame(substr(pol13$날짜,1,4))
  polyear5<-data.frame(substr(pol14$날짜,1,4))
  polyear6<-data.frame(substr(pol15$날짜,1,4))
  polyear7<-data.frame(substr(pol16$날짜,1,4))


  names(polyear1)<-"날짜"
  names(polyear2)<-"날짜"
  names(polyear3)<-"날짜"
  names(polyear4)<-"날짜"
  names(polyear5)<-"날짜"
  names(polyear6)<-"날짜"
  names(polyear7)<-"날짜"


  pol2.1<-cbind(polyear1,pol1.1)
  pol2.2<-cbind(polyear2,pol1.2)
  pol2.3<-cbind(polyear3,pol1.3)
  pol2.4<-cbind(polyear4,pol1.4)
  pol2.5<-cbind(polyear5,pol1.5)
  pol2.6<-cbind(polyear6,pol1.6)
  pol2.7<-cbind(polyear7,pol1.7)


  pol10 <- aggregate(pol2.1[,3:8],list(pol2.1$날짜,pol2.1$지역),mean)
  pol11 <- aggregate(pol2.2[,3:8],list(pol2.2$날짜,pol2.2$지역),mean)
  pol12 <- aggregate(pol2.3[,3:8],list(pol2.3$날짜,pol2.3$지역),mean)
  pol13 <- aggregate(pol2.4[,3:8],list(pol2.4$날짜,pol2.4$지역),mean)
  pol14 <- aggregate(pol2.5[,3:8],list(pol2.5$날짜,pol2.5$지역),mean)
  pol15 <- aggregate(pol2.6[,3:8],list(pol2.6$날짜,pol2.6$지역),mean)
  pol16 <- aggregate(pol2.7[,3:8],list(pol2.7$날짜,pol2.7$지역),mean)



  names(pol10)[2] <- "지역"
  names(pol11)[2] <- "지역"
  names(pol12)[2] <- "지역"
  names(pol13)[2] <- "지역"
  names(pol14)[2] <- "지역"
  names(pol15)[2] <- "지역"
  names(pol16)[2] <- "지역"

  ##pol 데이터에 id 붙이기
  seoul1 <- unique(seoul[, c("지역", "id")])


  pol10 <- merge(pol10, seoul1, by='지역')
  pol11 <- merge(pol11, seoul1, by='지역')
  pol12 <- merge(pol12, seoul1, by='지역')
  pol13 <- merge(pol13, seoul1, by='지역')
  pol14 <- merge(pol14, seoul1, by='지역')
  pol15 <- merge(pol15, seoul1, by='지역')
  pol16 <- merge(pol16, seoul1, by='지역')


  ##지도와 데이터 merge
  pol10_seoul<- merge(seoul, pol10, by='id')
  pol11_seoul<- merge(seoul, pol11, by='id')
  pol12_seoul<- merge(seoul, pol12, by='id')
  pol13_seoul<- merge(seoul, pol13, by='id')
  pol14_seoul<- merge(seoul, pol14, by='id')
  pol15_seoul<- merge(seoul, pol15, by='id')
  pol16_seoul<- merge(seoul, pol16, by='id')

 ## 오염 물질 데이터로 단계 구분 지도 만들기
  names(pol10_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol11_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol12_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol13_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol14_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol15_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")
  names(pol16_seoul)[12:17] <- c("fine_dust","micro_dust","ozone","nitro_dioxide","carbon_monoxide","sulfurous_acid_gas")


  ## 행정 구역 이름을 지도에 넣기 위한 작업
  cnames <- aggregate(cbind(long,lat) ~ 지역.x, data=pol10_seoul, FUN=function(x) mean(range(x)))


	## 오염물질별 지도 시각화 작업
	  if (pollution1 == "미세먼지") {
		fine_dust <- "미세먼지"
		fine_dust0 <- pol10_seoul$fine_dust
		fine_dust1 <- pol11_seoul$fine_dust
		fine_dust2 <- pol12_seoul$fine_dust
		fine_dust3 <- pol13_seoul$fine_dust
		fine_dust4 <- pol13_seoul$fine_dust
		fine_dust5 <- pol14_seoul$fine_dust
		fine_dust6 <- pol15_seoul$fine_dust


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=fine_dust0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=fine_dust1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=fine_dust2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=fine_dust3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=fine_dust4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=fine_dust5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=fine_dust6))+theme_void()

		s <- scale_fill_distiller(name="PM-10\n단위(㎍/㎥)", palette="Reds", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('미세먼지 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 

		 }

	  else if (pollution1 == "초미세먼지") {
		micro_dust<- "초미세먼지"
		micro_dust0 <- pol10_seoul$micro_dust
		micro_dust1 <- pol11_seoul$micro_dust
		micro_dust2 <- pol12_seoul$micro_dust
		micro_dust3 <- pol13_seoul$micro_dust
		micro_dust4 <- pol14_seoul$micro_dust
		micro_dust5 <- pol15_seoul$micro_dust
		micro_dust6 <- pol16_seoul$micro_dust


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=micro_dust0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=micro_dust1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=micro_dust2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=micro_dust3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=micro_dust4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=micro_dust5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=micro_dust6))+theme_void()


		s <- scale_fill_distiller(name="PM-25\n단위(㎍/㎥)", palette="YlOrRd", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('초미세먼지 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 
	  }

	  else if (pollution1 == "오존") {
		ozone <- "오존"
		ozone0 <- pol10_seoul$ozone
		ozone1 <- pol11_seoul$ozone
		ozone2 <- pol12_seoul$ozone
		ozone3 <- pol13_seoul$ozone
		ozone4 <- pol14_seoul$ozone
		ozone5 <- pol15_seoul$ozone
		ozone6 <- pol16_seoul$ozone


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=ozone0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=ozone1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=ozone2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=ozone3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=ozone4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=ozone5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=ozone6))+theme_void()

		s <- scale_fill_distiller(name="03\n단위(ppm)", palette="PuBuGn", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('오존 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 
	  }

	  else if (pollution1 == "이산화질소") {
		nitro_dioxide <- "이산화질소"
		nitro_dioxide0 <- pol10_seoul$nitro_dioxide
		nitro_dioxide1 <- pol11_seoul$nitro_dioxide
		nitro_dioxide2 <- pol12_seoul$nitro_dioxide
		nitro_dioxide3 <- pol13_seoul$nitro_dioxide
		nitro_dioxide4 <- pol14_seoul$nitro_dioxide
		nitro_dioxide5 <- pol15_seoul$nitro_dioxide
		nitro_dioxide6 <- pol16_seoul$nitro_dioxide


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=nitro_dioxide6))+theme_void()

		s <- scale_fill_distiller(name="NO2\n단위(ppm)", palette="BuPu", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('이산화질소 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 
	  }

	  else if (pollution1 == "일산화탄소") {
		carbon_monoxide <- "일산화탄소"
		carbon_monoxide0 <- pol10_seoul$carbon_monoxide
		carbon_monoxide1 <- pol11_seoul$carbon_monoxide
		carbon_monoxide2 <- pol12_seoul$carbon_monoxide
		carbon_monoxide3 <- pol13_seoul$carbon_monoxide
		carbon_monoxide4 <- pol14_seoul$carbon_monoxide
		carbon_monoxide5 <- pol15_seoul$carbon_monoxide
		carbon_monoxide6 <- pol16_seoul$carbon_monoxide


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=carbon_monoxide6))+theme_void()


		s <- scale_fill_distiller(name="CO\n단위(ppm)", palette="YlGnBu", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('일산화탄소 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 
	  }

	  else if (pollution1 == "아황산가스") {
		sulfurous_acid_gas <- "아황산가스"
		sulfurous_acid_gas0 <- pol10_seoul$sulfurous_acid_gas
		sulfurous_acid_gas1 <- pol11_seoul$sulfurous_acid_gas
		sulfurous_acid_gas2 <- pol12_seoul$sulfurous_acid_gas
		sulfurous_acid_gas3 <- pol13_seoul$sulfurous_acid_gas
		sulfurous_acid_gas4 <- pol14_seoul$sulfurous_acid_gas
		sulfurous_acid_gas5 <- pol15_seoul$sulfurous_acid_gas
		sulfurous_acid_gas6 <- pol16_seoul$sulfurous_acid_gas


		p10<- ggplot() + geom_polygon(data=pol10_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas0))+theme_void()
		p11<- ggplot() + geom_polygon(data=pol11_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas1))+theme_void()
		p12<- ggplot() + geom_polygon(data=pol12_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas2))+theme_void()
		p13<- ggplot() + geom_polygon(data=pol13_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas3))+theme_void()
		p14<- ggplot() + geom_polygon(data=pol14_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas4))+theme_void()
		p15<- ggplot() + geom_polygon(data=pol15_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas5))+theme_void()
		p16<- ggplot() + geom_polygon(data=pol16_seoul, aes(x=long, y=lat, group=group, fill=sulfurous_acid_gas6))+theme_void()

		s <- scale_fill_distiller(name="SO2\n단위(ppm)", palette="YlOrBr", breaks = pretty_breaks(n=5),direction = 1) 
		g <- guides(fill = guide_legend(reverse = T))

		g2.0 <- ggtitle('아황산가스 2010년') 
		g2.1 <- ggtitle('2011년') 
		g2.2 <- ggtitle('2012년') 
		g2.3 <- ggtitle('2013년') 
		g2.4 <- ggtitle('2014년') 
		g2.5 <- ggtitle('2015년') 
		g2.6 <- ggtitle('2016년') 

		t <- theme(plot.title = element_text(family = "NanumGothic", hjust = 0.5, size = 20), legend.title = element_text(family = "NanumGothic", face = "bold", size = 8))
		t2 <- theme(panel.spacing=unit(1,"lines"))
		g3 <- geom_text(data=cnames,aes(long, lat, label=지역.x), size=3, fontface="bold")

		 pollution_map10 <- p10 + s + g + coord_map() + g2.0 + t + t2 + g3 
		 pollution_map11 <- p11 + s + g + coord_map() + g2.1 + t + t2 + g3 
		 pollution_map12 <- p12 + s + g + coord_map() + g2.2 + t + t2 + g3 
		 pollution_map13 <- p13 + s + g + coord_map() + g2.3 + t + t2 + g3 
		 pollution_map14 <- p14 + s + g + coord_map() + g2.4 + t + t2 + g3 
		 pollution_map15 <- p15 + s + g + coord_map() + g2.5 + t + t2 + g3 
		 pollution_map16 <- p16 + s + g + coord_map() + g2.6 + t + t2 + g3 
	  }





## output of the total maps
grid.arrange(pollution_map10, pollution_map11, pollution_map12,pollution_map13, pollution_map14, pollution_map15, pollution_map16, ncol=3)
}



