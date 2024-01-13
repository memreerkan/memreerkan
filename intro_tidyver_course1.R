install.packages("tidyverse")
install.packages("dplyr")
install.packages("gapminder")
library(tidyverse)
library(dplyr)
library(gapminder)
head(gapminder)

# gapminder %>% arrange(gdpPercap) ~ gdp ye gore siralama(artan sirada) yapar. 

# gapminder %>% arrange(desc(gdpPercap)) ~ gdp ye gore siralama(azalan sirada) yapar. 

# gapminder %>% filter(year == 2002, country == "China") ~ 2002 ve China filtreler

gapminder %>% 
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))
# 2007 yilindaki artan sirada gdpCapitalari cikardik.

# gapminder %>%
#   mutate(pop = pop / 1000000)  pop'u pop/1 million olarak degistirir.

gapminder %>% 
  mutate(gdp = gdpPercap * pop)
# yeni gdp kolonu ekledik.

gapminder %>%
  filter(year == 2007) %>%
  mutate(gdp = gdpPercap * pop) %>%
  arrange(desc(gdp))
# 2007 yi filtreleyip gdp ekleyip gdp lere gore azalan sirada siraladik.\


##ggplot2##
library(ggplot2)
gapminder_2007 <- gapminder %>% filter(year==2007)
gapminder_2007
# 2007 yili icin ayri dataframe olusturduk
ggplot(gapminder_2007, aes(x=gdpPercap, y=lifeExp)) + geom_point()
# 2007 data frame i icin x i gdppercap y si lifeexp olan noktali grafik olusturduk

ggplot(gapminder_2007, aes(x=gdpPercap, y=lifeExp)) + 
  geom_point()+ 
  scale_x_log10()
# x eksenini log10 sclaeine ayarladik verilerin bir sol tarafta sikismasini engellemek icin

ggplot(gapminder_2007,aes(x=gdpPercap, y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()

# bu kisimda boyut ve renk de ekleyerek 4 farkli kolonu tek grafikte incelemis olduk

ggplot(gapminder_2007, aes(x=gdpPercap, y=lifeExp)) + 
  geom_point()+
  scale_x_log10()+
  facet_wrap(~continent)
#faceting yaparak her kitayi ayiriyoruz.

ggplot(gapminder,aes(x=gdpPercap, y=lifeExp, color=continent, size=pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~year)
#yila gore

gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))
# tum data setin mean life expectancy sini aldik
gapminder %>%
  filter(year == 2007)%>%
  summarize(meanLifeExp = mean(lifeExp),totalPop = sum(pop))
#2007 yilindaki datanin mean life exp ini ve toplam pop sayisini aldik


# mean, sum, min, max, median gibi seyler de var.

gapminder %>%
  group_by(year)%>%
  summarize(meanLifeExp = mean(lifeExp),totalPop = sum(pop))
# filter yeri group_by yaparak tek bir yili degil de yillara gore gruplamis olduk

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(mean_LifeExp=mean(lifeExp),totalPop=sum(pop))
# 2007 yilindaki kitalar bazinda summary i aldik

gapminder %>%
  group_by(year,continent) %>%
  summarize(mean_LifeExp=mean(lifeExp),totalPop=sum(pop))
#hem yila hem kitaya gore grupladik

by_year <- gapminder %>%
  group_by(year)%>%
  summarize(meanLifeExp=mean(lifeExp),totalPop=sum(pop))
#gruplara gore ayirdigimiz summarize ettigimiz datayi yeni degiskene atadik
ggplot(by_year,aes(x=year,y=totalPop))+
  geom_point()
#yeni degiskeni kullanarak yillara gore total populasyonu cizdirdik.

ggplot(by_year,aes(x=year,y=totalPop))+
  geom_point()+
  expand_limits(y=0)
# y eksenini 0 dan baslatti

by_continent_year <- gapminder %>%
  group_by(year,continent) %>%
  summarize(mean_LifeExp=mean(lifeExp),totalPop=sum(pop))

ggplot(by_continent_year,aes(x=year,y=totalPop,color=continent))+
  geom_point()+
  expand_limits(y=0)
#renklere kitalari atayip yillara gore pop degisimini cizdirdik.

#dplyr fiilleri ve ggplot gorsellestirmeli ogreniyorsun sonra birlestiriyorsun



##Line Plot##

ggplot(by_continent_year,aes(x=year,y=mean_LifeExp,color=continent))+
  geom_line()+
  expand_limits(y=0)

##Bar Plot##

by_continent <- gapminder%>%
  filter(year==2007)%>%
  group_by(continent)%>%
  summarize(medianLifeExp=median(lifeExp))

ggplot(by_continent,aes(x=continent,y=medianLifeExp))+
  geom_col()

##Histogram##

ggplot(gapminder_2007,aes(x=lifeExp))+
  geom_histogram()
#aes sadece x ekenini alir y dagilimi verdigi icin. binwidht ayarlanabilir.

ggplot(gapminder_2007,aes(x=lifeExp))+
  geom_histogram(binwidth=5)
#burada kitalardan bagimsiz dagilim gosteriliyor. boxplotta kita bazli yapacagiz

##Box Plot##
ggplot(gapminder_2007,aes(x=continent,y=lifeExp))+
  geom_boxplot()+
  ggtitle("BENIM GRAFIK")
  











  

