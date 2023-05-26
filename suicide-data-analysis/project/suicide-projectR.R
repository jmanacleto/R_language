install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("countrycode")
install.packages("purrr")
install.packages("randomForest")
install.packages("viridis")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(purrr)
library(randomForest)


setwd("/cloud/project/dataset")
list.files(path = "/cloud/project/dataset")
suicides_data <- read.csv("/cloud/project/dataset/master.csv")


suicides_data_final <- filter(suicides_data,year<2016)

summary(suicides_data_final)

#no. of countries
summarise(suicides_data_final,n_distinct(country))

#quartiles
summary(suicides_data_final$gdp_per_capita....)

#categorising <- #quartiles: 1 - underdeveloped, 2 - developing, 3 - developed, 4 - highly developed
suicides_data_final$development_status <- ifelse(suicides_data_final$gdp_per_capita....>=251 & suicides_data_final$gdp_per_capita....<3436,1,ifelse(suicides_data_final$gdp_per_capita....>=3436 & suicides_data_final$gdp_per_capita.... < 9283,2,ifelse(suicides_data_final$gdp_per_capita....>=9283 & suicides_data_final$gdp_per_capita.... < 24796,3,4)))
suicides_data_final$development_status <- as.factor(suicides_data_final$development_status)
summary(suicides_data_final$development_status)

#total 31 years of data, some countries have very few years of data
rm <- group_by(suicides_data_final,country) %>% summarise(year_count = n_distinct(year))
#rm
#removing countries with less than 10 years of data
rm_country <- rm$country[rm$year_count<10]
rm_country
suicides_data_final<- suicides_data_final %>% filter(!(country %in% rm_country))

#renaming columns
suicides_data_final <- suicides_data_final %>% rename(gdp_for_year = gdp_for_year....,gdp_per_capita = gdp_per_capita....)

str(suicides_data_final)


suicides_data_final$sex = ifelse(suicides_data_final$sex == "male", "Male", "Female")
head(suicides_data_final$sex)


suicides_data_final$continent <- countrycode(suicides_data_final$country, "country.name", "continent")
suicides_data_final$continent <- as.factor(suicides_data_final$continent)
suicides_data_final$sex <- as.factor(suicides_data_final$sex)
suicides_data_final$age <- gsub(" years", "", suicides_data_final$age)
suicides_data_final$age <- factor(suicides_data_final$age, ordered = T, levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+"))
str(suicides_data_final)

#####Visualisation of data

###Global Trend over time
global_average <- mean(suicides_data_final$suicides.100k.pop)
year.wise.suicide.plot <- (suicides_data_final %>% 
                             group_by(year) %>% 
                             summarise(mean.suicides.per.100K.pop = mean(suicides.100k.pop)) %>% 
                             ggplot(aes(x = year, y = mean.suicides.per.100K.pop,group = 1)) + 
                             geom_line(col = 'red',size = 1) + 
                             geom_point(col = 'red',size = 2) +
                             geom_hline(yintercept = global_average, linetype = 2, size = 1) +
                             scale_x_continuous(breaks = seq(1985, 2015, 2))) + 
  labs(title = "Global Trend over time (1985-2015)", x = "Year", y = "Average Suicides (per 100k)") +
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90,hjust =1) )
year.wise.suicide.plot

###Median Suicides over time 
suicides_data_final %>% 
  group_by(year) %>% 
  summarise(count = n(), median_suicides = median(suicides.100k.pop)) %>% 
  ungroup() %>% 
  ggplot(aes(year, median_suicides)) +
  geom_point(aes(size = median_suicides, color = median_suicides)) +
  geom_line(aes(color = median_suicides,alpha = 0.2)) +
  geom_hline(aes(yintercept = max(median_suicides),alpha = 0.2), linetype = "dotted") +
  geom_hline(aes(yintercept = median(median_suicides),alpha = 0.2), linetype = "dotted") +
  geom_hline(aes(yintercept = min(median_suicides),alpha = 0.2), linetype = "dotted") +
  #geom_vline(aes(xintercept = max(medianSuicides), alpha = 0.2)) +
  scale_color_continuous(low = "purple", high = "green") +
  guides(alpha = FALSE) +
  labs(title = "Median Suicides across the years", subtitle = "Median Suicides overall is roughly 25", y = "Median Suicides", x = "Year")

### Top 10 countires with max suicides
max_suicides_plot <- (head(suicides_data_final %>% 
                             group_by(country) %>% 
                             summarise(total.suicides = sum(suicides_no)) %>% 
                             arrange(desc(total.suicides)),10) %>% 
                        ggplot(aes(x = country,y = total.suicides))+ 
                        geom_bar(col = 'skyblue',stat='identity',fill='skyblue')+
                        labs(title = "Total suicidies in top 10 Countries", x = "Countries", y = "Total Suicides") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size = 14), axis.title = element_text(size =16), plot.title = element_text(size = 20)))
max_suicides_plot
top10countries <- head(suicides_data_final %>% group_by(country) %>% summarise(total.suicides = sum(suicides_no))  %>% arrange(desc(total.suicides)),10)$country

library(viridis)
#World data has Data related to many countries which will be used for a global map visualization 
worldData <- map_data('world')
str(worldData)

#Custom function to look at the country names from the worldData dataset that have common strings present in them 
library(stringr)
countryCount <- function(countryName){
  worldData %>% 
    filter(str_detect(region, pattern = countryName)) %>% 
    group_by(region) %>% 
    summarize(count = n())
}

countryNames <- c("Korea", "Antigua", "Barbuda", "Saint", "Trinidad", "Verde")
lapply(countryNames, countryCount)


#Few countries labelled differently in the two datasets,hence using the function antijoin and then grouping them
suicides_data_final %>% 
  rename(region = country) %>% 
  mutate(region = as.character(region)) %>% 
  anti_join(worldData) %>% 
  group_by(region) %>% 
  summarize(count = n())

worldData <- worldData %>% 
  mutate(region = ifelse(str_detect(region, "Korea"),"Korea", region),
         region = ifelse(str_detect(region, "Barbuda"),"Antigua", region))

###Visualising the suicide data available on a worldwide map 

#Using full_join to visualise the suicide data of countries over the world map (worldData), after renaming country column to region
#Using mutate function to ensure that countries labelled differently are matched as much as possible 
#Using viridis package to generate colors for the world map.
suicides_data_final %>% 
  mutate(country = as.character(country), 
         country = ifelse(country == "Russian Federation", "Russia", country),
         country = ifelse(country == "United Kingdom", "UK", country),
         country = ifelse(country == "United States", "USA", country),
         country = ifelse(country == "Trinidad and Tobago", "Trinidad", country),
         country = ifelse(country == "Saint Kitts and Nevis", "Saint Kitts", country),
         country = ifelse(country == "Saint Vincent and Grenadines", "Saint Vincent", country),
         country = ifelse(country == "Cabo Verde", "Cape Verde", country),
         country = ifelse(country == "Antigua and Barbuda", "Antigua", country),
         country = ifelse(country == "Republic of Korea", "Korea", country)
  ) %>%
  group_by(country) %>% 
  rename(region = country) %>%
  summarize(count = n(), medianSuicides = median(suicides_no)) %>% 
  full_join(worldData, by = "region") %>% 
  ungroup() %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = medianSuicides)) +
  scale_fill_viridis(option = "B", direction = -1) +
  #scale_fill_gradientn(colors = heat.colors(10)) +
  labs(title = "Median suicides globally", subtitle = "Gray areas represent missing data", x = "", y = "")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )


###Global Suicides (by sex)
sex_plot <- (suicides_data_final %>% 
               group_by(sex) %>% 
               summarise(suicides_per_100k = mean(as.numeric(suicides.100k.pop)))%>%
               ggplot(aes(sex,suicides_per_100k, fill = sex)) +
               geom_bar(stat = "identity") + 
               labs(title = "Global Suicides (per 100k) by Sex", x=Sex, y = Suicides per 100k) + 
               scale_y_continuous(breaks = seq(0,25,2), minor_breaks = F)+
               theme(legend.position = "none", axis.text = element_text(size = 16),axis.title = element_text(size = 18), plot.title = element_text(size = 22) ))+
  geom_text(aes(label = round(suicides_per_100k,3)), size = 5.5,vjust = -0.25) 
sex_plot


### Global suicides different age categories

age_plot <- suicides_data_final %>% group_by(age) %>% summarise(suicides_100kpop = mean(suicides.100k.pop)) %>% ggplot(aes(x = age, y = suicides_100kpop, fill = age))+
  geom_bar(stat = 'identity') + labs(title = "Global Suicides (per 100k)", x = "Age", y = "Mean Suicides (per 100k)") + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), plot.title = element_text(size = 20), legend.position = "none")
age_plot

### Global suicides trend for different ages

age_trendplot <- suicides_data_final %>% group_by(year,age) %>% summarise(suicides_100kpop = mean(suicides.100k.pop)) %>% ggplot(aes(x = year,y =suicides_100kpop,col = age))+
  geom_line(size = 1) + geom_point(size = 2)+facet_wrap(age~., scales = "free_y", nrow = 3)+scale_x_continuous(breaks = seq(1985, 2015, 5)) + labs(title = "Trend over Time, by Age", x ="Year", y = "Mean Suicides (per 100k)") +
  theme(legend.position = "none", axis.text = element_text(size = 12), axis.title =element_text(size = 16), plot.title = element_text(size = 20), strip.text.x = element_text(size = 14))

age_trendplot

### Global suicides trend for different ages (by sex)
age_sex_trendplot <- suicides_data_final %>% group_by(year,age,sex) %>% summarise(suicides_100kpop = mean(suicides.100k.pop)) %>% ggplot(aes(x = year, y = suicides_100kpop,col = sex))+
  geom_line()+geom_point(size=0.4)+scale_x_continuous(breaks = seq(1985, 2015,2))+theme(axis.text.x = element_text(angle = 90,hjust = 1))+facet_grid(age~.)
age_sex_trendplot

###Age and Sex comparison

age_sex_plot <- suicides_data_final %>% group_by(age,sex) %>% summarise(suicides_100kpop = mean(suicides.100k.pop)) %>% ggplot(aes(x = sex, y = suicides_100kpop,fill = age))+
  geom_bar(stat = 'identity',position = 'dodge') +  labs(title = "Age disparity, by Sex", x = "Sex", y = "Mean Suicides (per 100k)") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), plot.title = element_text(size = 20), legend.text =element_text(size = 14), legend.title =element_text(size = 16))
age_sex_plot


#Trend over time (by continent)
continent_trend_plot <- (suicides_data_final%>%
                           group_by(year, continent)%>%
                           summarise(suicides_per_100k = mean(suicides_no*100000/population))%>%
                           ggplot(aes(x= year, y = suicides_per_100k, col = continent)) + 
                           facet_grid(continent ~ .,scales = "free_y") +
                           geom_line(size = 1) + 
                           geom_point(size = 2) +
                           labs(title = "Trend over time, by Continent", x = "Year", y = "Suicides (per 100k)") + 
                           theme(legend.position = "none",strip.text.y = element_text(size = 14), axis.text = element_text(size = 14), axis.title = element_text(size = 16), plot.title = element_text(size = 19)) + 
                           scale_x_continuous(breaks = seq(1985,2015,5), minor_breaks = F)
                         
)
continent_trend_plot


#Age Wise comparision per continent
gender_continent_plot <- (suicides_data_final%>%
                            group_by(continent, age)%>%
                            summarise(suicides_per_100k = mean(suicides_no*100000/population))%>%
                            ggplot(aes(x = continent, y = suicides_per_100k, fill = age)) + 
                            geom_bar(stat = "identity", position = "dodge") +
                            labs(title = "Age wise comparision per Continent", x = "Continent", y = "Suicides (per 100k)") +
                            theme(legend.title = element_text(size = 16), legend.text = element_text(size=13) ,axis.title = element_text(size = 16), axis.text = element_text(size = 13), plot.title = element_text(size = 20))
                          
                          
                          
)
gender_continent_plot