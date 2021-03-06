# ��� 123
#������� 8
#������� 2
#������� ��������� ������������� � �������� ������ ������������� �������� ��������� ������ ������� ����� ����
#�� ������ 2013 ���� 
#�� ������ ��������� ������� ������������ ���������
library("tidyverse") 
library("readr") 
# ������� read_csv()
library("stringr") 
# ������ str_replace_all
����������("dplyr") 
# ������: Filter (); arrange_ (); Select (); mutate (); summary (); group_by_n()
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
# ����������� ��� ������� � �������� ���������
#����� ������ �� �����, ���������� ������ ������, �������� ��������� 'NA',
# ������ � ��������������� ��������� �������� �� NA, ���������� ������ � "["
eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
                   comment=c("["))
eddypro = eddypro[-1, ]
# ��������� �� ���� ���������� � ��� ����� ������������ �������� glimpse(),
# ������� ����� �������� ������������ ������ ��������� ����������,
# ������� ��� ���� ������������ ������� ������
glimpse(eddypro)
#�������� ������ ������ � ��������� ������� ������� "roll"
eddypro = select(eddypro, -(roll))


#��������� ����������� �������� � �������� ������� �� ���������� ��� ���������� ��������
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
# ��������� ��� � ��� ����������
glimpse(eddypro)


# ����������� ������, ����� �������� ������ ������ ������� �� ���� �����������  ������
tbl = filter(eddypro, daytime==FALSE)
# �������� ��� � ��� ���������� ��������� ��������
sapply(eddypro,is.numeric)
# ������ ��� ���������� ������� ������ ������������ ��� ������,
# ��� ���������� ��������� ��������
eddypro_numeric=eddypro[,sapply(eddypro,is.numeric)]
# ��������� �� ����� ��� ���� ���� ���� �������� NA,� ������� �������
# drop_na
eddypro_numeric = drop_na(eddypro_numeric)


# ��������� � ��������������� �������
# ��������� ���������� ��� ����� ����������
cor_edd=cor(eddypro_numeric, method="pearson")
# ����������� ������� � �������
cor_edd = cor(eddypro_numeric, method="pearson") %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_edd)[cor_edd$h2o_flux^2 >0.1] %>% na.exclude
vars
# ������� ��� ���������� �� ������� � �������� ���������� � ���� �������
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
#���������� ���������������� �������
row_numbers = 1:length(eddypro_numeric$h2o_flux)
#���������
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
#�����������
test = row_numbers[-teach]
#���������
teaching_edd = eddypro_numeric[teach,]
#�����������
testing_edd = eddypro_numeric[test,]


#������ 1

mod1 = lm(h2o_flux~un_h2o_flux , data = teaching_edd)
#������������
coef(mod1)
#�������
resid(mod1)
#������������� ��������
confint(mod1)
#P-�������� �� ������
summary(mod1)
#������������� ������
anova(mod1)
##������� �� ���������� ������������� ������:
plot(mod1,2)

#�������� ������ ����������� �O2 �� un_h2o_flux
plot(teaching_edd$h2o_flux~teaching_edd$un_h2o_flux)
abline(a=mod1$coefficients[1], b=mod1$coefficients[2], col="red")

# �������� ������ ����������� �������� �� ������������� ��������
plot(mod1$fitted.values, teaching_edd$h2o_flux)
# ������� ����� �=�
abline(a=0, b=1, col="red")

# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$h2o_flux,mod1$residuals)

# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � H2O
mo1=lm(mod1$residuals~teaching_edd$h2o_flux)
abline(a=mo1$coefficients[1],b=mo1$coefficients[2],col="red")


#������ 2

mod2 = lm( h2o_flux~(.), data = teaching_edd)
#������������
coef(mod2)
#�������
resid(mod2)
#������������� ��������
confint(mod2)
#P-�������� �� ������
summary(mod2)
#������������� ������
anova(mod2)

#������� �� ���������� ������������� ������ :
plot(mod2,2)

# �������� ������ ����������� �������� �� ������������� ��������
plot(mod2$fitted.values, teaching_edd$h2o_flux)
# ������� ����� �=�
abline(a=0, b=1, col="blue")

# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$h2o_flux,mod2$residuals)

# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � H2O
mo2=lm(mod2$residuals~teaching_edd$h2o_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="blue")

#������ 3

mod3 = lm ( h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H +qc_H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux
            + h2o_flux + rand_err_h2o_flux + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
            + h2o_mole_fraction +h2o_time_lag + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + water_vapor_density + e +es+
              + specific_humidity + VPD + Tdew + u_unrot +v_unrot + w_unrot + u_rot + v_rot + w_rot + max_speed + yaw + pitch + u_star_ + L + `_z_minus_d__div_L`
            + T_star_ + x_offset + x_30_perc_+ x_50_perc_ +x_70_perc_+x_90_perc_+ un_Tau + un_H + H_scf + un_LE+ LE_scf + un_co2_flux + w_spikes + co2_spikes + h2o_spikes
            + co2_var +co2_1, data = teaching_edd)

#������������
coef(mod3)
#�������
resid(mod3)
#������������� ��������
confint(mod3)
#P-�������� �� ������
summary(mod3)
#������������� ������
anova(mod3)
#������� �� ���������� ������������� ������ :
plot(mod3,2)

# �������� ������ ����������� �������� �� ������������� ��������
plot(mod3$fitted.values, teaching_edd$h2o_flux)
# ������� ����� �=�
abline(a=0, b=1, col="green")

# �������� ������ �������� �� ����������� ��������
plot(teaching_edd$h2o_flux,mod3$residuals)

# ��� ������ ������������� ��� ����� ������� ������, ����������� ������� � �2�
mo3=lm(mod3$residuals~teaching_edd$h2o_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")



##�������� �������
#��������� ������ 2, ������� � ������������� ��������

#������ ������
qplot(h2o_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod2, teaching_edd)))

#������ ������
qplot(h2o_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))

#��������� ������ 3, ������� � ������������� ��������

#������ ������
qplot(h2o_flux , co2_flux, data = teaching_edd) + geom_line(aes(y = predict(mod3, teaching_edd)))

#������ ������
qplot(h2o_flux , co2_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))

#������ ������� �� ��������� ����������, �� ����� ������� ����� �������� ������������ h2o_flux �� ����������� � ������ ����������
#� ������ ������������� ����� ������ ������ ����� ��� �����, ��� ��� ����� ����� � ��� �� ����������� �������

#�������
qplot(DOY, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(DOY, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))
qplot(Tau, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod2, testing_edd)))
qplot(Tau, h2o_flux, data = testing_edd) + geom_line(aes(y = predict(mod3, testing_edd)))

