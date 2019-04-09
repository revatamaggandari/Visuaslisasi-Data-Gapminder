# Install Packages #
library(openxlsx)
library(plotly)
library(ggplot2)

### input data sheet 1 ###
gapminder <- read.xlsx("D:\\KULIAH\\SEMESTER VI\\DATVIS\\UTS\\gapminder.xlsx", sheet =1, startRow = 1, colNames = TRUE)
head(gapminder)

#membuat factor
country.vec <- gapminder[,1]

#membuat perulangan country sebanyak 47 kali (Jumlah tahun)
country_panel <- c()
for (i in 1:170)
{
  x = rep(country.vec[i], 47)
  country_panel <- append(country_panel, x)
}

#membuat perulangan tahun
years_panel <- rep(1970:2016, 170)

#mengambil data pada variabel gdp
gdp_panel <- c()
for (i in 1:170) 
{
  x = gapminder[i,]
  x = x[-c(1:3)]
  x = t(x)
  gdp_panel <- append(gdp_panel,x)
}

### Input data sheet 2 ###
gapminder1 <- read.xlsx("D:\\KULIAH\\SEMESTER VI\\DATVIS\\UTS\\gapminder.xlsx", sheet =2, startRow = 1, colNames = TRUE)

#mengambil data pada variabel populasi
pop_panel <- c()
for (i in 1:170) 
{
  x = gapminder1[i,]
  x = x[-c(1:3)]
  x = t(x)
  pop_panel <- append(pop_panel,x)
}

### Input data sheet 3 ###
gapminder2 <- read.xlsx("D:\\KULIAH\\SEMESTER VI\\DATVIS\\UTS\\gapminder.xlsx", sheet =3, startRow = 1, colNames = TRUE)

#mengambil data pada variabel life
life_panel <- c()
for (i in 1:170) 
{
  x = gapminder2[i,]
  x = x[-c(1:3)]
  x = t(x)
  life_panel <- append(life_panel,x)
}

### Input data sheet 4 ###
gapminder3 <- read.xlsx("D:\\KULIAH\\SEMESTER VI\\DATVIS\\UTS\\gapminder.xlsx", sheet =4, startRow = 1, colNames = TRUE)

chosen <- numeric(length = nrow(gapminder3))
choices <- paste(gapminder3$country, gapminder3$code_3)

for (i in seq(nrow(gapminder3))) {
  chosen[i] <- match(paste(gapminder$Country.Name[i], gapminder$Country[i]), choices)
  choices[chosen] <- 0
}

region <- gapminder3[chosen, "continent"]

region_panel <- c()
for (i in 1:170) {
  x = rep(region[i],47)
  region_panel <- append(region_panel,x)
}
#menggabungkan 5 panel menjadi 1 data frame
gapminder_frame <- data.frame(country_panel, years_panel, gdp_panel,pop_panel,life_panel, region_panel)

### Membuat ggplot ###
#membuat rumah
gap <- ggplot(gapminder_frame, aes(x = gdp_panel, y = life_panel))
#menambah point
gap <- ggplot(gapminder_frame, aes(x = log(gdp_panel), y = life_panel)) + geom_point()
#membuat layer
gap <- ggplot(gapminder_frame, aes(x = log(gdp_panel), y = life_panel)) + geom_point(aes(frame = years_panel))

#mempercantik tampilan memberi warna berdasarkan negara (country)
gap <- ggplot(gapminder_frame, aes(x = log(gdp_panel), y = life_panel, color = country_panel)) + geom_point(aes(frame = years_panel))
#Membuat size berdasarkan populasi
gap <- ggplot(gapminder_frame, aes(x = log(gdp_panel), y = life_panel, color = country_panel)) + geom_point(aes(size = pop_panel, shape =  region_panel, frame = years_panel))
#membuat animasi
ggplotly(gap)

#Menyimpan File
colnames(gapminder)<-c("Country","Years","GDP","Population","Life","Continent")
setwd("D:\\KULIAH\\SEMESTER VI\\DATVIS\\UTS")
write.csv(gapminder_frame, file="Visualisasi.Gapminder.csv", row.names = TRUE)
gapminder.save<-data.frame(gapminder_frame)
gapminder.save

