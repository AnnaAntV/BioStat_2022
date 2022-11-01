---
title: "Homework 2"
author: "Anna"
date: "2022-10-24"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


library(ggplot2)
library(ggpubr)
library (dplyr)
library (readr)
library(plotly)
library(corrplot)
library(corrr)
library(tidyr)
library(factoextra)
library(pheatmap)
library(cluster)
library(tidyverse)  
library(cluster)    

```

# Домашняя работа №1

 

 

## Задание 1

 
```{r}

insurance_cost <- read_csv ("~/data/insurance_cost.csv")

str(insurance_cost)
```


```{r}
summary(insurance_cost)
```

```{r}
head(insurance_cost)
```

## Задание 2

 
```{r}
plot_ly(data = insurance_cost ,
 x = ~ bmi,
 y = ~ charges,
  color = ~ smoker,
 marker = list(size = 7)) %>%
  layout(
    title = 'Отношение ИМТ и расходов страховой компании',
    yaxis = list(title = 'Расходы'),  
    xaxis = list(title = 'Индекс массы тела')
    
    ) 

```
## Задание 3

 
```{r}
insurance_cost %>% 
  ggplot(aes(x=bmi, y=charges, color = smoker, fill = smoker, group = smoker)) +     geom_point(size=2)+
   
  theme_minimal() +
  ggtitle('Отношение ИМТ и расходов страховой компании') + 
  labs(y = 'Расходы', x = 'Индекс массы тела') 
       
```
## Задание 4

 
```{r}

insurance_cost_clear <- insurance_cost %>% 
 select(is.integer | is.numeric) 

head(insurance_cost_clear)
```

```{r}
insurance_cost_cor <- cor(insurance_cost_clear)
insurance_cost_cor
```

```{r}
corrplot(insurance_cost_cor, method = 'pie')
```

```{r}
corrplot(insurance_cost_cor, method = 'ellipse', order = 'FPC', type = 'lower', diag = FALSE)
```

```{r}
insurance_cost_cor %>% 
  rplot(shape = 15, colors = c("blue", "red"))
```
## Задание 5

 

```{r}
insurance_cost$region_southeast <- ifelse (insurance_cost$region == 'northwest', "no", "yes")
insurance_cost$region_northwest <- ifelse (insurance_cost$region == 'northwest', "yes", "no")

insurance_cost_new <- insurance_cost %>% 
  mutate (female = as.numeric(sex == 'female'),
         smoker = as.numeric(smoker == 'yes'),
         region_southeast = as.numeric(region_southeast == 'yes'),
         region_northwest = as.numeric(region_northwest == 'yes')) %>%
select(age, female, bmi, children, smoker, charges, region_southeast, region_northwest)
    
head(insurance_cost_new)
  
```


```{r}

insurance_cost_new_clear <- insurance_cost_new %>% 
 select(is.integer | is.numeric) 

head(insurance_cost_new_clear)
```


## Задание 6

 
```{r}
insurance_cost_new_clear_scaled <- scale(insurance_cost_new_clear)
head(insurance_cost_new_clear_scaled)
```


```{r}
insurance_cost_new_clear_scaled_dist <- dist(insurance_cost_new_clear_scaled, method = "euclidean")
as.matrix(insurance_cost_new_clear_scaled_dist)[1:6,1:6]
insurance_cost_new_clear_scaled_hc <- hclust(d = insurance_cost_new_clear_scaled_dist, 
                        method = "ward.D2")
```        
```{r}
fviz_dend (insurance_cost_new_clear_scaled_hc, cex = 0.1)
```


## Задание 7

 
 

```{r}
fviz_dend (insurance_cost_new_clear_scaled_hc, k=5,
           cex = 0.1,
           k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
color_labels_by_k = TRUE,
rect = TRUE)          
```


```{r}
install.packages('igraph')
require("igraph")
fviz_dend(insurance_cost_new_clear_scaled_hc, k = 4, k_colors = "jco",type = "phylogenic", repel = TRUE)    
```




  
## Задание 8

 
```{r}
pheatmap(insurance_cost_new_clear_scaled)
```

## Задание 9

 

```{r}
install.packages("devtools", repo="http://cran.us.r-project.org")
library(devtools)
install_github("vqv/ggbiplot")
```



```{r}
library(FactoMineR)
library(ggbiplot)

```


```{r}
insurance_cost_new_full.pca <- prcomp(insurance_cost_new_clear, 
                        scale = T) 
summary(insurance_cost_new_full.pca)
```
```{r}
fviz_eig(insurance_cost_new_full.pca, 
         addlabels = T, 
         ylim = c(0, 40))
```
#### Анализ переменных по PCA
```{r}
fviz_pca_var(insurance_cost_new_full.pca, col.var = "contrib")
```

```{r}
fviz_pca_var(insurance_cost_new_full.pca, 
             select.var = list(contrib = 3), 
             col.var = "contrib")
```

```{r}
fviz_contrib(insurance_cost_new_full.pca, choice = "var", axes = 1, top = 24) 
fviz_contrib(insurance_cost_new_full.pca, choice = "var", axes = 2, top = 24) 
fviz_contrib(insurance_cost_new_full.pca, choice = "var", axes = 3, top = 24)
```


#### Анализ наблюдений по PCA


```{r}
ggbiplot(insurance_cost_new_full.pca, 
         scale=0, alpha = 0.1) + 
  theme_light ()
```

```{r}

ggbiplot(insurance_cost_new_full.pca, 
         scale=0, 
         groups = as.factor(insurance_cost$region_northwest), 
         ellipse = T,
         alpha = 0.2) +
  theme_light()
```
```{r}

ggbiplot(insurance_cost_new_full.pca, 
         scale=0, 
         groups = as.factor(insurance_cost$region_southeast), 
         ellipse = T,
         alpha = 0.2) +
  theme_light()
```

## Задание 10

 
```{r}
insurance_cost_new <- insurance_cost_new %>% 
  mutate(
      age_group = case_when(
      age < 31 ~ "18-30",
      age >= 31 & age < 51 ~ "31-50",
      age >= 51 ~ "60+"
    ))

```


```{r}

ggbiplot(insurance_cost_new_full.pca, 
         scale=0, 
         groups = as.factor(insurance_cost_new$age_group), 
         ellipse = T,
         alpha = 0.2) +
  theme_light()
```

## Задание 11

 

```{r}
insurance_cost_new <- insurance_cost_new %>% 
  mutate(
      charges_group = case_when(
      charges < 9000 ~ "< 9000",
      charges >= 9000 & charges < 33000 ~ "9000-33000",
      charges >= 33000 ~ "33000+"
    ))

```


```{r}

ggbiplot(insurance_cost_new_full.pca, 
         scale=0, 
         groups = as.factor(insurance_cost_new$charges_group), 
         ellipse = T,
         alpha = 0.1) +
  theme_light()
```




```{r}
insurance_cost_new <- insurance_cost_new %>% 
  mutate(
      children_group = case_when(
      children < 1 & smoker == '0' ~ "некурящие без детей",
      children >= 1 & smoker == '0' ~ "некурящие с детьми",
      children < 1 & smoker == '1' ~ "курящие без детей",
      children >= 1 & smoker == '1' ~ "курящие с детьми"
    ))

```


```{r}

ggbiplot(insurance_cost_new_full.pca, 
         scale=0, 
         groups = as.factor(insurance_cost_new$children_group), 
         ellipse = T,
         alpha = 0.1) +
  theme_light()
```





