# ПАЭ 123
#Вариант 8
#Задание 2
#Именная Елизавета Александровна – создайте модель множественной линейной регрессии ночных потоков паров воды
#за период 2013 года 
#по данным измерений методом турбулентной пульсации
library("tidyverse") 
library("readr") 
# функция read_csv()
library("stringr") 
# фукция str_replace_all
библиотека("dplyr") 
# фукции: Filter (); arrange_ (); Select (); mutate (); summary (); group_by_n()
library("ggplot2") 
setwd("C:/Users/Huawei/Documents"); 
getwd()

library(tidyverse)
library(rnoaa)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
# манипуляция над данными и линейная регрессия
#Чтаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA',
# пустые и сгенерированные пороговые значения на NA, игнорируем строки с "["
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
                   comment=c("["))
eddypro = eddypro[-1, ]
# посмотрим на сами переменные и для этого воспользуеся функцией glimpse(),
# которая более наглядно представляет каждую отдельную переменную,
# жертвуя при этом предсталение строчек данных
glimpse(eddypro)
#Удаление первой строки и ненужного пустого столбца "roll"
eddypro = select(eddypro, -(roll))


#Изменение специальных символов в названии стобцов на допустимые для переменных названия
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]", "_emph_") %>%
  str_replace_all("[?]", "_quest_") %>%
  str_replace_all("[*]", "_star_") %>%
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
# посмотрим что у нас получилось
glimpse(eddypro)


# Отфильтруем данные, чтобы получить данные ночных потоков за весь исследуемый  период
tbl = filter(eddypro, daytime==FALSE)
# проверим где у нас содержатся численные значения
sapply(eddypro,is.numeric)
# теперь нам необходимо оатвить только интересующие нас данные,
# где содержатся численные значения
eddypro_numeric=eddypro[,sapply(eddypro,is.numeric)]
# избавимся от строк где есть хоть одно значение NA,с помощью функции
# drop_na
eddypro_numeric = drop_na(eddypro_numeric)


# переходим к корреляционному анализу
# посчитаем корреляцию для наших переменных
cor_edd=cor(eddypro_numeric, method="pearson")
# преобразуем матрицу в таблицу
cor_edd = cor(eddypro_numeric, method="pearson") %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_edd)[cor_edd$h2o_flux^2 >0.1] %>% na.exclude
vars
# Соберем все переменные из вектора с именнами переменных в одну формулу
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
#Создадание непересекающиеся выборки
row_numbers = 1:length(eddypro_numeric$h2o_flux)
#Обучающая
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
#Тестирующая
test = row_numbers[-teach]
#Обучающая
teaching_edd = eddypro_numeric[teach,]
#Тестирующая
testing_edd = eddypro_numeric[test,]


#МОДЕЛЬ 1

mod1 = lm(h2o_flux~un_h2o_flux , data = teaching_edd)
#Коэффициенты
coef(mod1)
#остатки
resid(mod1)
#Доверительный интервал
confint(mod1)
#P-значения по модели
summary(mod1)
#Дисперсионный анализ
anova(mod1)
##Графиик на нормальной веротяностной бумаге:
plot(mod1,2)

#Построем график зависимости СO2 от un_h2o_flux
plot(teaching_edd$h2o_flux~teaching_edd$un_h2o_flux)
abline(a=mod1$coefficients[1], b=mod1$coefficients[2], col="red")

# Построим график наблюдаемых значений от предсказанных значений
plot(mod1$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")

# Построим график остатков от набоюдаемых значений
plot(teaching_edd$h2o_flux,mod1$residuals)

# Для поиска коэффициентов для линии зададим модель, связывающую остатки и H2O
mo1=lm(mod1$residuals~teaching_edd$h2o_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")


#МОДЕЛЬ 2

mod2 = lm( h2o_flux~(.), data = teaching_edd)
#Коэффициенты
coef(mod2)
#Остатки
resid(mod2)
#Доверительный интервал
confint(mod2)
#P-значения по модели
summary(mod2)
#Дисперсионный анализ
anova(mod2)

#Графиик на нормальной веротяностной бумаге :
plot(mod2,2)

# Построим график наблюдаемых значений от предсказанных значений
plot(mod2$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="blue")

# Построим график остатков от набоюдаемых значений
plot(teaching_edd$h2o_flux,mod2$residuals)

# Для поиска коэффициентов для линии зададим модель, связывающую остатки и H2O
mo2=lm(mod2$residuals~teaching_edd$h2o_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="blue")

#МОДЕЛЬ 3

mod3 = lm ( h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H +qc_H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction +h2o_time_lag + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + water_vapor_density + e +es+
              + specific_humidity + VPD + Tdew + u_unrot +v_unrot + w_unrot + u_rot + v_rot + w_rot + max_speed + yaw + pitch + u_star_ + L + `_z_minus_d__div_L`
            + T_star_ + x_offset + x_30_perc_+ x_50_perc_ +x_70_perc_+x_90_perc_+ un_Tau + un_H + H_scf + un_LE+ LE_scf + un_co2_flux + w_spikes + co2_spikes + h2o_spikes
            + co2_var +co2_1, data = teaching_edd)

#Коэффициенты
coef(mod3)
#Остатки
resid(mod3)
#Доверительный интервал
confint(mod3)
#P-значения по модели
summary(mod3)
#Дисперсионный анализ
anova(mod3)
#Графиик на нормальной веротяностной бумаге :
plot(mod3,2)

# Построим график наблюдаемых значений от предсказанных значений
plot(mod3$fitted.values, teaching_edd$h2o_flux)
# Добавим линию у=х
abline(a=0, b=1, col="green")

# Построим график остатков от набоюдаемых значений
plot(teaching_edd$h2o_flux,mod3$residuals)

# Для поиска коэффициентов для линии зададим модель, связывающую остатки и Н2О
mo3=lm(mod3$residuals~teaching_edd$h2o_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")



##Проверка моделей
#Изпользуя МОДЕЛЬ 2, наложим её предсказанные значения

#Первый график
qplot(h2o_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod2, teaching_edd)))

#Второй график
qplot(h2o_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))

#Изпользуя МОДЕЛЬ 3, наложим её предсказанные значения

#Первый график
qplot(h2o_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod3, teaching_edd)))

#Второй график
qplot(h2o_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))

#Модель зависит от множества переменных, мы можем вывести много графиков зависимостей h2o_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке

#Примеры
qplot(DOY, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(DOY, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))
qplot(Tau, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(Tau, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))

