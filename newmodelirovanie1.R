# ��� 123
#������� 8
#������� 1
#������� ��������� ������������� � ��� ������� 73 ����������� ����������� ������� � 2003 ����,
#���� ��� �������� ������� ����� �������� ���������� �� ���������� 9 ���, 
#� ������������ �� ���������� �� 70 �� 210 ��
setwd("C:/Users/Huawei/Documents"); 
getwd()

# ������������� ������
#install.packages("tidyverse")
#install.packages("rnoaa")
#install.packages ("lubridate")
# ��������� ������ ��� ������
library(tidyverse)
library(rnoaa)
library(lubridate)

# ��������� ������ ������������
station_data = ghcnd_stations()
write.csv(station_data,"station_data.csv")
station_data=read.csv("station_data.csv")


# ��������� ������ ������������
# ������� ������� � ������ ������� � ������������ ��� �������
ulyanovsk = data.frame(id = "ULYANOVSK", latitude = 54.328240,  longitude = 48.386570)
ulyanovsk


# �������� ������������ � ����������,
# ������� ����� ����������� ������ �� ����������� ������
ulyanovsk_around = meteo_nearby_stations(lat_lon_df = ulyanovsk,
                                         station_data = station_data,
                                         limit = 100,
                                         var=c("TAVG"),
                                         year_min = 1994, year_max = 2003)
ulyanovsk_around

# �������� ������� ��� ������� ��� �������������� ������������,
# �������� �������������� ������������ ��������
ulyanovsk_id=ulyanovsk_around[["ULYANOVSK"]][["id"]][1]
summary(ulyanovsk_id)

# ��� ��������� ������� �� ����� �������������� ������ ����������
# ���������� ������� ������� ������ ������ �� ������
ulyanovsk_table = ulyanovsk_around[[1]]
summary(ulyanovsk_table)

#������� �� �����������, ������� ������������� �� ���������� �� 70 �� 210

ulyanovsk_table = filter (ulyanovsk_table, distance > 69 & distance < 211 )
#ulyanovsk_stations = ulyanovsk_table[ulyanovsk_table$ulyanovsk.distance>70 & ulyanovsk_table$ulyanovsk.distance<210] 
ulyanovsk_stations = ulyanovsk_table

#������ ����������� �������
str(ulyanovsk_stations)
ulyanovsk_stations$id


#���������� �������� ������ ��� ��������� ������������
#��� ��������� ���� ������ � 1 ������������, ���� �� �������������, ����������� ����. �������
meteo_tidy_ghcnd
#all_ulyanovsk_data = meteo_tidy_ghcnd(stationid = ulyanovsk_id)


#�������� ������������� ������, ���� ����� ��������� ������ � ���������� ������������
all_i = data.frame()
# ������� ����, � ������� �� ����������� ������ ������ ��� ���� ������������
# c������� ������, ���� ������� ��� ������ � ������������
all_ulyanovsk_meteodata = data.frame()
# ������� ���� ��� ����� ������������
stations_names=ulyanovsk_stations$id
stations_names=stations_names[1:15] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "1994-01-01",
                              date_max = "2003-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_ulyanovsk_meteodata=rbind(all_ulyanovsk_meteodata, one_meteo)}

# ���������� ���������� ����������
write.csv(all_ulyanovsk_meteodata,"all_ulyanovsk_meteodata.csv")
# ��������� ������ all_ulyanovsk_meteodata.csv
#all_ulyanovsk_meteodata=read.csv("all_ulyanovsk_meteodata.csv")
# ������� ��� ����������
str(all_ulyanovsk_meteodata)

# ����������� ������ �� 1994 - 2003 ����
years_ulyanovsk_meteodata = filter(all_ulyanovsk_meteodata, year %in% c(1994:2003))

# ������� ���, �����, ����
all_ulyanovsk_meteodata = mutate(all_ulyanovsk_meteodata, year = year(date), month = month(date), day = day(date))
str(all_ulyanovsk_meteodata)

# ��������� NA � 0 � ��� tavg<5
all_ulyanovsk_meteodata[is.na(all_ulyanovsk_meteodata$tavg),"tavg"] = 0
all_ulyanovsk_meteodata[all_ulyanovsk_meteodata$tavg<5, "tavg"] = 0
summary(all_ulyanovsk_meteodata)

# ����������� ������������ �� id,������� � ����� � ������������ �������������
# �� ���� �������, ����� ����������� ������ �� ������� � ������ ������� �� �������
# ��� ���� ������������
group_meteodata =all_ulyanovsk_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## ���������� � ������� �� ������� ������ ##
### ���� ��������
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# ��������� �� ����.1. ������� ������
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# ��������� �� ����. 1. ������� ������
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# ��������� ����� ���� i-�� ������,
#�������� � ������ ��������� ��������, � ������

#����� ���� � ������,��������� �� ����. 1.
y = 1.0
# ����������� ��� ���������� ������ - �������, ��� ��� ���� �������� ������
Kf = 300
# ����������� ������������� ��� �������
Qj = 1600
# ������������ ������ ��������
Lj = 2.2
# ����� ������ �������� � �������� ���������
Ej = 25
# ����������� ��������� ��������
# ���������� Fi �� ������v
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#���������� Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
## ����������� ������
Yield = (sum(sumT_month$Yi))
Yield
# �������� ������ � �/�� �.� ���� �������� �� 10^6 �� ������� ������ � ��
# �������� �/��

