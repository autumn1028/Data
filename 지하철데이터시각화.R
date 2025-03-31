library(tidyverse)
library(ggplot2)
subway <- read_csv("서울시 지하철 호선별 역별 시간대별 승하차 인원 정보.csv", col_names = TRUE,
                   locale=locale(encoding = "euc-kr"))
glimpse(subway)
subway

지하철 <- filter(Line2_on) %>% select(ends_with('승차인원'))
View(지하철)
지하철승차 <- floor(apply(지하철,2,mean))
지하철승차
시간대 <- names(지하철)
print(지하철승차)
project1 <- ggplot(mapping = aes(x=시간대,y=지하철승차, group=1))+
  geom_point()+
  geom_line()+
  coord_cartesian()+
  theme(axis.text.x=element_text(angle=90, vjust=0.8))+
  ggtitle("2호선 시간대별 승차 평균 인원")+
  theme(plot.background = element_rect("#EBEBEB"))+
  theme(panel.background = element_rect("#EBEBEB"))
project1

Line2_on <- filter(subway, 호선명 == "2호선") %>% select(ends_with('승차인원'))
Line2 <- filter(subway, 호선명 == "2호선")
Line2_on 
Line2_on_m <- floor(apply(Line2_on,1,mean))

project2 <- ggplot(data = Line2_on, aes(x=Line2$지하철역, y=Line2_on_m))+
  geom_bar(stat = "identity", fill="green")+
  coord_cartesian()+
  theme(axis.text.x=element_text(angle=90, vjust=0.8))+
  ggtitle("2호선 역별 승차 평균 비교")+
  xlab('2호선 지하철역')+ylab('승차 인원수')+
  theme(plot.background = element_rect("#EBEBEB"))+
  theme(panel.background = element_rect("#EBEBEB"))
project2

강남 <- filter(subway, 지하철역=="강남") %>% select(ends_with('승차인원'))
View(강남)
강남승차 <- floor(apply(강남,2,mean))
강남승차
시간대 <- names(지하철)
project3 <- ggplot(mapping = aes(x=시간대,y=강남승차))+
  geom_point()+
  ggtitle("강남역 시간대별 승차 인원")+
  theme(axis.text.x=element_text(angle=90, vjust=0.8))+
  theme(title = element_text(color="black"))+
  theme(axis.title.x = element_text(color="black"))+
  theme(plot.background = element_rect("#EBEBEB"))+
  theme(panel.background = element_rect("#EBEBEB"))
project3

library(patchwork)

def_plot <- project1/project2/project3
def_plot+
  plot_annotation(title = "인포그래픽",
                  subtitle = "2호선 승차인원 중심",
                  caption = "데이터: 서울시 지하철 호선별 역별 시간대별 승하차 인원 정보",
                  theme = theme(plot.title = element_text(color = "#292929", size = 22, face = "bold"),
                                plot.subtitle = element_text(color = "#292929", size = 14),
                                plot.caption = element_text(color = "grey50", face = "bold.italic"),
                                plot.background = element_rect("#EBEBEB"),
                                panel.background = element_rect("#EBEBEB")))
