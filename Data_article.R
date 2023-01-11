## Data Graphics ##

## Figure 1 ##

## Charge data ##

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/TEST/CARTAS Y DOCUMENTOS")

library(readxl)
library(tidyverse)

data <- read_excel("datos_articulo.xlsx")
data <- data[complete.cases(data), ]

str(data)

# As factor

data$condicion <- factor(data$condicion) 
data$tratamiento <- factor(data$tratamiento)

summary(data)

# Data analisis

datos <- data %>% 
  group_by(tratamiento) %>% 
  count(condicion)

names(datos) <- c("tratamiento", "condicion", "count")

datos$percent <- round((datos$count/90)*100,0)

# Graphics

library(ggthemes)

art <- ggplot(datos, aes(x = tratamiento, y = percent, color = condicion)) +
  geom_col(position = "dodge", fill = "white", size = 0.7 )+
  labs(x = "Tratamientos", y= "Porcentaje (%)") +
  theme_minimal()+
  labs(color = "Característica")+
  theme(axis.title.x = element_text(color = "black", size = 12), 
        axis.title.y = element_text(color = "black", size = 12))+
  geom_text(aes(label = paste0(percent, "%")), vjust= 1.5, position = position_dodge(0.9))+
  scale_color_manual(values = c("gree n4", "black"), labels = c("Enraizada", "Muerta"))
 
 
library(plotly)
ggplotly(art)


## Figure 2 ##

## Charge data ##

setwd("C:/Backup Sebastian/SEBASTIAN/Shebba/TEST/CARTAS Y DOCUMENTOS")

data2 <- read_excel("datos_articulo.xlsx", sheet = 2)

str(data2)

# As factor

data2$tratamiento <- factor(data2$tratamiento)

# Data analysis

datos2 <- data2 %>% 
  group_by(nraices) %>% 
  count() %>% 
  mutate_at("nraices", as.factor)

str(datos2)

  
# Graphics

library(ggthemes)

ggplot(datos2, aes(x = nraices, y = n )) +
  geom_segment( aes(xend = nraices, yend = 0)) +
  geom_point( size = 2, color ="darkgreen")+
  scale_y_continuous(limits = c(0,60), breaks=seq(0, 60, 10))+
  theme_minimal() +
  labs(x = "Número de raíces (u)", y= "Frecuencia Absoluta")+
  theme(axis.title.x = element_text(color = "black", size = 11), 
        axis.title.y = element_text(color = "black", size = 11))+
  annotate(geom = "text", x = c(1,2,3,4,5,6), y = c(52,53,30,25,5,4),
           label = c("49","50","27","22","2","1"), size = 3.5)


## Figure 3 ##

str(data2)

data2$nraices <- factor(data2$nraices)  
  
ggplot(data2, aes(x = tratamiento, color = nraices)) +
  geom_bar(stat = "count", position = "dodge", width = 0.3, fill = "white",size = 0.7) +
  labs(color = "Número de raíces")+
  labs(x = "Tratamiento", y= "Número de raíces (u)")+
theme_minimal()+
theme(axis.title.x = element_text(color = "black", size = 11), 
      axis.title.y = element_text(color = "black", size = 11))+
scale_color_hue(labels = c("1 raíz", "2 raíces", "3 raíces","4 raíces", "5 raíces", "6 raíces"))


## Figure 4 ##  

## Charge data ##  
  
data3 <- read_excel("datos_articulo.xlsx", sheet = 3)
  
str(data3)
  
# As factor
  
data3$tratamiento <- factor(data3$tratamiento)  
  
# Graphics

ggplot(data3, aes(x = tratamiento, y = lraices, color = tratamiento)) +
  stat_boxplot(geom = "errorbar", # Error bars
               width = 0.25) +
  geom_boxplot()+
  labs(color = "Dosis de AIB")+
  labs(x = "Tratamiento", y= "Longitud de raíces (cm)")+
  theme_minimal()+
  theme(axis.title.x = element_text(color = "black", size = 11), 
        axis.title.y = element_text(color = "black", size = 11))+
scale_color_hue(labels = c("Testigo", "1000 ppm", "1500 ppm", "2000 ppm", "Comercial"))

  
  
  
  
  


  