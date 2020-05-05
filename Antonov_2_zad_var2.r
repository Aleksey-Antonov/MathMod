# Антонов А.А., ПАЭ-123, вар. 2 
#создайте модель множественной линейной регрессии дневных потоков
# паров воды за весенний период 2013 года по данным измерений методом турбулентной пульсации
rm(list=ls())
library("tidyverse") 
library("readr")     
library("stringr")   
library("dplyr")     
library("ggplot2") 
library("tidyr")
library("stringr")
library("lubridate")

# Считываем файл 
#Пропускаем первую строку,заменяем все не числовые значения на NA, и игнорируем строчки с символом "["

getwd()
setwd("D:/PAE-123")
my_data = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#
#Подготовка к анализу
#
# Удаляем первую строку
my_data = my_data[-1, ]
# Удаляем ненужный пустой столбец "roll"
my_data = select(my_data, -(roll))
# Преобразуем в факторы (factor)столбцы типа char (символ)
my_data = my_data %>% mutate_if(is.character, factor)
#Заменим спец. символы в названии стобцов на допустимые для переменных имена
names(my_data) = names(my_data) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
glimpse(my_data)
# Удалим строки c NA
my_data = drop_na(my_data)
# Отфильтруем  данные только за весну
my_data = filter(my_data,DOY >= 60 & DOY < 152)
# Оставим данные  только за дневное время
my_data = filter(my_data, daytime ==TRUE)
my_data_numeric = my_data[,sapply(my_data, is.numeric)]
my_data_non_numeric = my_data[,!sapply(my_data, is.numeric)]

#  регрессионный анализ

row_numbers = 1:length(my_data_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(my_data_numeric$h2o_flux)*.7))
test = row_numbers[-teach]
# обучающая выборка
teaching_tbl = my_data_numeric[teach,]
#тестирующая выборка
testing_tbl = my_data_numeric[test,]
# 1я модель
mod1 = lm(h2o_flux~ (.) , data = teaching_tbl)
# информация о ней и дисперсионный анализ
summary(mod1) 
coef(mod1)
resid(mod1)
confint(mod1)
anova(mod1)

#Выведем график
plot(mod1)
# 2я модель
mod2 = lm(h2o_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H +  rand_err_H  + LE + qc_LE + rand_err_h2o_flux + rand_err_co2_flux + H_strg + h2o_molar_density + h2o_mole_fraction  + h2o_mixing_ratio + co2_molar_density 
          + co2_mole_fraction + co2_mixing_ratio + h2o_time_lag + sonic_temperature  + air_temperature + air_pressure + air_density + air_heat_capacity  + air_molar_volume 
          + water_vapor_density  + e + es + RH +  Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot + w_rot + wind_dir + yaw + pitch + TKE + L
          + bowen_ratio + x_peak + x_offset  + x_offset+ x_10. +x_30.+ x_50. +x_70. + un_Tau + H_scf + un_LE + un_co2_flux 
          + un_h2o_flux + h2o_spikes + h2o.1 , data = teaching_tbl)


# информация о ней и дисперсионный анализ
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
plot(mod2) 
anova(mod2)
anova(mod2, mod1)

#3я модель
mod3 = lm(h2o_flux~DOY + Tau + qc_Tau +  qc_H +  rand_err_H  + qc_LE + rand_err_h2o_flux + co2_flux + H_strg + h2o_molar_density + co2_molar_density 
          + co2_time_lag+ h2o_mixing_ratio + co2_molar_density + air_pressure + u_unrot + v_unrot + w_unrot + v_rot + yaw        + bowen_ratio + TKE + x_peak + un_h2o_flux, data = teaching_tbl )

# информация, дисперсионный анализ и графики
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

# Проведем корреляционный анализ переменных
cor_teaching_tbl = select(teaching_tbl, h2o_flux, DOY, Tau, qc_Tau, qc_H, rand_err_H, qc_LE, rand_err_h2o_flux, h2o_flux,
                          H_strg, co2_molar_density, h2o_molar_density, h2o_time_lag, air_pressure,
                         u_unrot, v_unrot, w_unrot, v_rot, yaw, bowen_ratio,  x_peak, un_h2o_flux)

#таблица коэффициентов корреляции
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#
# Графики по полученной модели
#по обучающей выборке
qplot(h2o_flux , h2o_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))

#по тестирующей
qplot(h2o_flux , h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

#Примеры графиков
qplot(DOY, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, h2o_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

