---
title: "Stepik"
author: "Anna"
date: "2022-11-07"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library (readxl)
library(tidyverse)
library (dplyr)
library (flextable)
library (stringi)

```


# Домашнее задание после курса «Введение в автоматизацию обработки данных на R»

```{r}

data <- read_excel ("data_excel.xlsx", sheet = "data")

summary (data)

```
```{r}

data <- data %>%
  mutate(across(-(1:5), ~ gsub(',', '.', .x))) %>%
  mutate(across(-(1:5), as.numeric))%>%
  mutate (`Группа крови` = `Группа крови` %>% na_if("NA"))%>%
  mutate(`Группа крови` = `Группа крови` %>% as.character() %>% replace_na ("Нет данных")%>% as.factor())%>%
  rename_with(function(x) x %>% stri_replace_all_regex(c("_E1", "_E2"), c("_Визит 1", "_Визит 2"), vectorize_all = FALSE))%>%
  glimpse()
 
 summary(data) 
 head (data)

```



##Средние значения и медиана показателей за два визита
```{r}
data %>%
  rowwise()%>%
  mutate(`Средняя по базофилам за два визита` = mean (c(`Базофилы_Визит 1`, `Базофилы_Визит 2`)))%>%
  mutate(`Медиана по базофилам за два визита` = median (c(`Базофилы_Визит 1`, `Базофилы_Визит 2`)))%>%
ungroup()%>%
select(`Базофилы_Визит 1`, `Базофилы_Визит 2`,`Средняя по базофилам за два визита`, `Медиана по базофилам за два визита`)%>%
  flextable()

```


```{r}
data %>%
  rowwise()%>%
  mutate(`Средняя по эозинофилам за два визита` = mean (c(`Эозинофилы_Визит 1`, `Эозинофилы_Визит 2`)))%>%
   mutate(`Медиана по эозинофилам за два визита` = median (c(`Эозинофилы_Визит 1`, `Эозинофилы_Визит 2`)))%>%
ungroup()%>%
select(`Эозинофилы_Визит 1`, `Эозинофилы_Визит 2`,`Средняя по эозинофилам за два визита`, `Медиана по эозинофилам за два визита`)%>%
  flextable()
 
```

```{r}
data %>%
  rowwise()%>%
  mutate(`Средняя по гемоглобину за два визита` = mean (c(`Гемоглобин_Визит 1`, `Гемоглобин_Визит 2`)))%>%
  mutate(`Медиана по гемоглобину за два визита` = median (c(`Гемоглобин_Визит 1`, `Гемоглобин_Визит 2`)))%>%
ungroup()%>%
select(`Гемоглобин_Визит 1`, `Гемоглобин_Визит 2`,`Средняя по гемоглобину за два визита`, `Медиана по гемоглобину за два визита`)%>%
  flextable()
 
```
```{r}
data %>%
  rowwise()%>%
  mutate(`Средняя по эритроцитам за два визита` = mean (c(`Эритроциты_Визит 1`, `Эритроциты_Визит 2`)))%>%
  mutate(`Медиана по эритроцитам за два визита` = median (c(`Эритроциты_Визит 1`, `Эритроциты_Визит 2`)))%>%
ungroup()%>%
select(`Эритроциты_Визит 1`, `Эритроциты_Визит 2`,`Средняя по эритроцитам за два визита`, `Медиана по эритроцитам за два визита`)%>%
  flextable()
 
```

## Cтатистическая таблица для количественной переменной
```{r}
data %>%
  select(`Группа`, where (is.numeric))%>%
  group_by(`Группа`)%>%
  summarise(across(where(is.numeric), function(x) mean(x, na.rm = TRUE)))
  
```

```{r}
statistics <- list(
      `Количество субъектов` = ~length(.x),
      `Количество (есть данные)` = ~sum(!is.na(.x)),
      `Нет данных` = ~sum(is.na(.x)),
      `Ср. знач.` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", mean(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
      `Станд. отклон.` = ~ifelse(sum(!is.na(.x)) < 3, "Н/П*", sd(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
      `95% ДИ для среднего` = ~sd(.x, na.rm = TRUE) %>% round(2) %>% as.character(),
      `мин. - макс.` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", paste0(min(.x, na.rm = TRUE) %>% round(2), " - ", max(.x, na.rm = TRUE) %>% round(2))),
      `Медиана` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", median(.x, na.rm = TRUE) %>% round(2) %>% as.character()),
      `Q1 - Q3` = ~ifelse(sum(!is.na(.x)) == 0, "Н/П*", paste0(quantile(.x, 0.25, na.rm = TRUE) %>% round(2), " - ", quantile(.x, 0.75, na.rm = TRUE) %>% round(2)))
)
data %>%
  select(`Группа`, where (is.numeric))%>%
  group_by(`Группа`)%>%
  summarise(across(where(is.numeric), statistics))%>%
     
  pivot_longer (!`Группа`,values_transform = as.numeric)%>%
  separate(name, into = c("Переменная", "Статистика"), sep = "_")%>%
  rename(`Значение`= value)%>%
  flextable()
```

## Cтатистическая таблица для категориальной переменной
```{r}
data %>%
  select(`Группа`, where (is.factor))%>%
  mutate(`Группа крови` = `Группа крови` %>% as.character() %>% replace_na ("Нет данных")%>% as.factor())%>%
  count(`Группа`, `Группа крови`)%>%
  group_by(`Группа`)%>%
  mutate(`Процент по группе`= (n / sum(n))%>% round(4)%>% `*`(100)%>% str_c("%"))%>%
  ungroup() %>%
  mutate(`Процент по выборке`= (n / sum(n))%>% round(4)%>% `*`(100)%>% str_c("%"))%>%
  flextable()
  
```

















