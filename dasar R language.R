paste("makarim","oke")
paste0("apa","aja","boleh")
a = 19


#statement
if(a == 10){
  print('benar')
}else{
  print('salah woy')
}


#perulangan
i = 0
while(i<10){
  print(i)
  i<-i+1
}
for(i in 1:10){
  print(i)
}


#fungsi
hw <- function(){
  print('hallo')
}
lingkaran <-function(r){
  result <- 3,14 * r*r 
}


#vector
names = c("karim","widy","achmad","yanto")


#matrix
n = matrix(1:12,3)


#factor
gender = factor(c("male","female","male","female"))


#array
a = array(1:24, c(3,4,2))
a[1,3,1]


#dataframe
users<-data.frame(
  names = names,
  jeniskelamin = c("maele", "female", "male","female"),
  usia = c("20","19","21","22")
)


#list
l = list(a, names, users)

#import dataset csv
dataset <- read.csv("D:\\Koding\\Python\\Corona\\2019_nCoV_data.csv")
summary(dataset)
head(dataset)
nrow(dataset)
ncol(dataset)

#simpan dataset csv
write.csv(dataset,"newdataset.csv")


#library deep layer
library(dplyr)
glimpse(dataset)


#mengambil kolom dari dataset
#cara1
select(dataset, Deaths)
select(dataset, Deaths, Country)
select(dataset, -c(Recovered))
#cara2
dataset_result <- filter(dataset, Country == 'China' & Confirmed > 20 )
dataset_result <- filter(dataset, Country != 'China' | Confirmed > 20 )


#membuat kolom baru
dataset2 <- mutate(dataset, ratarata = Confirmed / 2)
dataset4 <- transmute(dataset, ratarata = Confirmed / 2)

sapply(dataset, typeof)


dataset3 <- filter(dataset, Country == 'China')
dataset2 <- select(dataset2, c(Country, Confirmed))


#piping
dataset5 <- dataset %>% filter(Country == 'China') %>%
  mutate(ratarata = Confirmed / 2) %>%
  select(c(Country, Confirmed))


#summarization
dataset %>% group_by(Confirmed, Country) %>% summarise(total = sum(Confirmed), ratarata = mean(total), n_order = n())

dataset %>% group_by(Country = 'China') %>% summarise(total = sum(Confirmed), ratarata = mean(total), n_order = n())
data_a <- dataset %>% group_by(Country = 'China') %>% summarise(total = sum(Confirmed), ratarata = mean(total), n_order = n())
data_b <- dataset %>% group_by(Country = 'France') %>% summarise(total = sum(Confirmed), ratarata = mean(total), n_order = n())


#intersect untuk memilih baris-baris yang sama
intersect(data_a, data_b)


#Union semua data jika sama tidak dimunculkan
union(data_a, data_b)


#bindrow semua data ditumpuk
bind_rows(data_a, data_b)


#setdif untuk menampilkan di data a/b
setdiff(data_a, data_b)


#menggabungkan dataframe
data_c <- select(dataset, c(Confirmed, Country))


data_d <- select(dataset, c(Province.State))


#menggabungkan kolom
bind_cols(data_c, data_d)

inner_join(data_c, data_d)
full_join(data_c, data_d)
left_join(data_c, data_d)
right_join(data_c, data_d)


#visualisasi
library(ggplot2)

#scatterplot
ggplot(dataset, aes(x = Confirmed, y = Country)) + geom_point(colour = 'green')

#
ggplot(dataset, aes(x = Confirmed)) + geom_histogram(binwidth = 1000)

#
ggplot(dataset, aes(x = Confirmed, y = Country)) + geom_bar(stat = 'identity', width = 0.5, fill = 'blue')

#barplot
ggplot(dataset, aes(x = Confirmed, y = Country)) + geom_bar(stat = 'identity', width = 0.5, aes(fill=Deaths))

#piechart
Confirmed_percountry <- dataset %>% group_by(Country) %>% summarise(totalConfirmed = sum(Confirmed))
ggplot(dataset, aes(x = "", y = Confirmed, fill = Country)) + geom_bar(stat = 'identity', width = 1) + coord_polar("y", start = 0)

#linechart
ggplot(dataset, aes(x = Confirmed, y = Deaths)) + stat_summary(fun.y = sum, geom = 'line')

Confirmed_result <- dataset %>% group_by(Confirmed) %>% summarise(Confirmed = sum(Confirmed))

plot1 <- ggplot(dataset, aes(x = Confirmed, y = Country)) +
  #geom_point(color = '#42f5d1', size = 5, shape=17) +
  geom_point(aes(color = Country), size = 5, shape=17) +
  #geom_smooth( method = 'lm') +
  geom_smooth(method = 'lm', color = '#f5c242', linetype='dotdash')
  labs(title = 'scater confirmed',
       subtitle = 'based on dataset corona',
       caption = 'bahasa R')
  
plot2 <- plot1 +
  xlab('Dikonfirmasi') + ylab('Negara') +
  xlim(c(0, 15000)) +
  ylim(c('Indonesia, Japan')) +
  theme(
    plot.title = element_text(color = 'blue'),
    plot.subtitle = element_text(color = 'yellow', face='italic'),
    #legend.position = 'bottom'
    legend.position = c(1,0),
    legend.title = element_text(color = 'blue', size=12),
    legend.text = element_text(color = 'yellow')
  )

plot3 <- plot2

ggsave('contry.jpg')
