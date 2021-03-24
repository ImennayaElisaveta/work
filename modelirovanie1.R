# Именная Елизавета ПАЭ-123, вариант 8
# регион 64, Саратовская область (город Саратов)
# для региона 64 рассчитайте урожайность пшеницы в период с 2002 по 2012 год
# взяв для рассчета средние суммы активных температур за эти годы, с 18 ближайших метеостанций
# проверяем рабочую директорию
setwd("C:/Users/lisai/Documents"); 
getwd()

# устанавливаем пакеты
#install.packages("tidyverse")
#install.packages("rnoaa")
# открываем нужные нам пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)


# скачиваем список метеостанций
station_data = ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data=read.csv("station_data.csv")


# формируем список метеостанций
# создаем таблицу с именем региона и координатами его столицы
saratov = data.frame(id="SARATOV", latitude=51.54, longitude=46.00)

# выбираем метеостанции в Саратове,
# которые имеют необходимые данные за определнный период
saratov_around = meteo_nearby_stations(lat_lon_df = saratov,
                                       station_data = station_data,
                                       limit = 18,
                                       var=c("TAVG"),
                                       year_min = 2002, year_max = 2012)

# получили таблицу где указаны все индетификаторы метеостанций,
# получаем индентификатор метеостанции Саратова
saratov_id=saratov_around[["SARATOV"]][["id"]][1]
summary(saratov_id)

# для получения таблицы со всеми метеостанциями вокруг Саратова
# необходимо выбрать целиком первый объект из списка
saratov_table=saratov_around[[1]]
summary(saratov_table)

saratov_stations = saratov_table
#мы сформировали список необходимых станций, посмотрим, что он содержит
str(saratov_stations)


# список содержит 18 метеостанций расположенных рядом с Саратовом
# выведем индетификаторы отфильтрованных метеостанций
saratov_stations$id

## скачивание погодных данных для наших метеостанций
# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_saratov_data=meteo_tidy_ghcnd(stationid = saratov_id)
# посмотрим что мы скачали
summary(all_saratov_data)

# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные 18 метеостанций
all_saratov_meteodata = data.frame()
# создаем цикл для наших метеостанций
stations_names=saratov_stations$id
stations_names=stations_names[1:18] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2002-01-01",
                              date_max = "2012-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }

 one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_saratov_meteodata=rbind(all_saratov_meteodata, one_meteo)}

# записываем полученные результаты
write.csv(all_saratov_meteodata,"all_saratov_meteodata.csv")
# считываем данные all_saratov_meteodata.csv
#all_saratov_meteodata=read.csv("all_saratov_meteodata.csv")
# смотрим что получилось
str(all_saratov_meteodata)


# добавим год, месяц, день
all_saratov_meteodata = mutate(all_saratov_meteodata, year = year(date), month = month(date), day = day(date))
str(all_saratov_meteodata)

# превратим NA в 0 и где tavg<5
all_saratov_meteodata[is.na(all_saratov_meteodata$tavg),"tavg"] = 0
all_saratov_meteodata[all_saratov_meteodata$tavg<5, "tavg"] = 0
summary(all_saratov_meteodata)


# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata =all_saratov_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему
#числу дней в месяце,константа по табл. 1.
y = 1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300
# Коэффициент использования ФАР посевом
Qj = 1600
# калорийность урожая культуры
Lj = 2.2
# сумма частей основной и побочной продукции
Ej = 25
# стандартная влажность культуры
# Рассчитаем Fi по месяцаv
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
## Расчитываем урожай
Yield = (sum(sumT_month$Yi))
Yield
# получаем урожай в ц/га т.к если умножать на 10^6 то получим урожай в мг
# получили 20,6 ц/га







