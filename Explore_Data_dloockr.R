#### Explore and Transformation data ####

# Load packages

install.packages("dlookr")
library(dlookr)
browseVignettes(package = "dlookr")

library(tidyverse)

# Load data

install.packages("nycflights13")
library(nycflights13)
dim(flights)

data("flights")

glimpse(flights)

# Diagnose

diagnose(flights)

# Missing Value(NA): Las variables con muchos valores perdidos, 
# es decir, aquellas con un valor missing_percentcercano a 100, deben excluirse del análisis.

# Unique value: Las variables con un valor único ( unique_count= 1) 
# se consideran excluidas del análisis de datos. 

# Select columns by name

diagnose(flights, year, month, day)

# Select all columns between year and day (include)

diagnose(flights, year:day)

# Select all columns except those from year to day (exclude)

diagnose(flights, -(year:day))

# Use dplyr

flights %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))

# Diagnose numeric variables

diagnose_numeric(flights)

# Diagnostico de variables que no pueden ser 0 o negativos

diagnose_numeric(flights) %>% 
  filter(minus > 0 | zero > 0) 

# Diagnose category variables

df <- diagnose_category(flights)

# Diagnostico de variables con mas N.A

diagnose_category(flights) %>% 
  filter(is.na(levels))

# Diagnostico de variables con ratio > 0.01

flights %>%
  diagnose_category(top = 500)  %>% # top: Cantidad de observaciones por variable
  filter(ratio <= 0.01)

# Diagnose outlaier

diagnose_outlier(flights)

# Diagnostico de variables con outlaiers

diagnose_outlier(flights) %>% 
  filter(outliers_cnt > 0) 

# Variable numerica con outliers mayor al 5%

diagnose_outlier(flights) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)

# En los casos en que la media de los valores atípicos sea grande en relación con el promedio general, 
# puede ser conveniente imputar o eliminar los valores atípicos.


# Plot outlier

flights %>%
  plot_outlier(diagnose_outlier(flights) %>% 
                 filter(outliers_ratio >= 0.5) %>% 
                 select(variables) %>% 
                 unlist())

### Data Analysis ###

library(ISLR)
str(Carseats)
data("Carseats")

# Agregando datos faltantes

carseats <- ISLR::Carseats

suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA

suppressWarnings(RNGversion("3.5.0"))
set.seed(456)
carseats[sample(seq(NROW(carseats)), 10), "Urban"] <- NA

### Estadistica descriptiva ###

describe(carseats)

# Los datos de distribución sesgados a la izquierda, es decir, las variables con un gran sesgo positivo,
# deben considerar las transformaciones logarítmicas o sqrt para seguir la distribución normal.

# Ordenar el sesgo

carseats %>%
  describe() %>%
  select(described_variables, skewness, mean, p25, p50, p75) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

# Group by

carseats %>%
  group_by(US) %>% 
  describe(Sales, Income) 

carseats %>%
  group_by(US, Urban) %>% 
  describe(Sales, Income) 

# Normalidad

normality(carseats)

carseats %>%
  normality() %>%
  filter(p_value <= 0.01) %>% 
  arrange(abs(p_value))

carseats %>%
  group_by(ShelveLoc, US) %>%
  normality(Income) %>% 
  arrange(desc(p_value))

carseats %>%
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  normality(log_income) %>%
  filter(p_value > 0.01)

# Plot

# Select columns by name

plot_normality(carseats, Sales, CompPrice)

carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  plot_normality(Income)

# Correlacion de datos bivariados

correlate(carseats)

# Select columns by name

correlate(carseats, Sales, CompPrice, Income)

carseats %>%
  correlate(Sales:Income) %>%
  filter(as.integer(var1) > as.integer(var2))

carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>%
  filter(abs(coef_corr) > 0.5)

# Plot

carseats %>% 
  correlate() %>%
  plot()

# Select columns by name

carseats %>% 
  correlate(Sales, Price) %>%
  plot()

carseats %>%
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>% 
  plot()


# Basado en la variable objetivo categorica

categ <- target_by(carseats, US) # similar a group_by

# If the variable of interest is a numerical variable

cat_num <- relate(categ, Sales)
cat_num

summary(cat_num)

# Plot

plot(cat_num)

# If the variable of interest is a categorical variable

cat_cat <- relate(categ, ShelveLoc)
cat_cat

summary(cat_cat)

plot(cat_cat)

# Basado en la variable objetivo numerica

# If the variable of interest is a numerical variable

num <- target_by(carseats, Sales)

# If the variable of interest is a numerical variable (Se vuelve una regresion lineal)

num_num <- relate(num, Price)
num_num

summary(num_num)

# Plot

plot(num_num)

# If the variable of interest is a categorical variable (Se vuelve un ANOVA)

num_cat <- relate(num, ShelveLoc)
num_cat

summary(num_cat)

# Plot

plot(num_cat)


# Transformacion de datos


# Imputación de valores faltantes

income <- imputate_na(carseats, Income, US, method = "rpart") # “rpart” : Recursive Partitioning and Regression Trees

# result of imputation

income

# summary of imputation

summary(income)

# viz of imputation

plot(income)

# Imput data categorical

library(mice)

urban <- imputate_na(carseats, Urban, US, method = "mice")

# result of imputation

urban

# summary of imputation

summary(urban)

# viz of imputation

plot(urban)

# The mean before and after the imputation of the Income variable

carseats %>%
  mutate(Income_imp = imputate_na(carseats, Income, US, method = "knn")) %>%
  group_by(US) %>%
  summarise(orig = mean(Income, na.rm = TRUE),
            imputation = mean(Income_imp))


# Imputación de valores atípicos

price <- imputate_outlier(carseats, Price, method = "capping") # “capping” : Imputate the upper outliers with 95 percentile, and Imputate the bottom outliers with 5 percentile.

# result of imputation

price

# summary of imputation

summary(price)

# viz of imputation

plot(price)

# The mean before and after the imputation of the Price variable

carseats %>%
  mutate(Price_imp = imputate_outlier(carseats, Price, method = "capping")) %>%
  group_by(US) %>%
  summarise(orig = mean(Price, na.rm = TRUE),
            imputation = mean(Price_imp, na.rm = TRUE))

# Transformacion de datos

carseats %>% 
  mutate(Income_minmax = transform(carseats$Income, method = "minmax"),
         Sales_minmax = transform(carseats$Sales, method = "minmax")) %>% 
  select(Income_minmax, Sales_minmax) %>% 
  boxplot()

# Resolución de datos de asimetría

# find_skewness()busca variables con datos sesgados. 
# Esta función encuentra datos sesgados por las condiciones de búsqueda y calcula la asimetría.

# find index of skewed variables

find_skewness(carseats)

# find names of skewed variables

find_skewness(carseats, index = FALSE)

# compute the skewness

find_skewness(carseats, value = TRUE)

# compute the skewness & filtering with threshold

find_skewness(carseats, value = TRUE, thres = 0.1)

# La asimetría de Advertisinges 0.637. 
# Esto significa que la distribución de datos está algo inclinada hacia la izquierda.

Advertising_log = transform(carseats$Advertising, method = "log")

# result of transformation

head(Advertising_log)

# summary of transformation

summary(Advertising_log)

# viz of transformation

plot(Advertising_log)

# Other transformation

Advertising_log <- transform(carseats$Advertising, method = "log+1")

# result of transformation

head(Advertising_log)

# summary of transformation

summary(Advertising_log)

# viz of transformation

plot(Advertising_log)


# Binning

# transforma una variable numérica en una variable categórica dividiéndola en intervalos

# Binning the carat variable. default type argument is "quantile"

bin <- binning(carseats$Income)

# Print bins class object

bin

# Summarize bins class object

summary(bin)

# Plot bins class object

plot(bin)

# Using labels argument

bin <- binning(carseats$Income, nbins = 4,
               labels = c("LQ1", "UQ1", "LQ3", "UQ3"))
bin

# Using another type argument

binning(carseats$Income, nbins = 5, type = "equal")

binning(carseats$Income, nbins = 5, type = "pretty")

binning(carseats$Income, nbins = 5, type = "kmeans")        

binning(carseats$Income, nbins = 5, type = "bclust")

# Extract the binned results

extract(bin)

# Using pipes & dplyr

library(dplyr)

carseats %>%
  mutate(Income_bin = binning(carseats$Income) %>% 
           extract()) %>%
  group_by(ShelveLoc, Income_bin) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  head(10)


# optimal binning using character

bin <- binning_by(carseats, "US", "Advertising")

# optimal binning using name

bin <- binning_by(carseats, US, Advertising)

bin

# summary optimal_bins class

summary(bin)

# performance table

attr(bin, "performance")

# visualize optimal_bins class

plot(bin)

# extract binned results

extract(bin)

### Informs ###

#Web

flights %>%
  diagnose_web_report(subtitle = "flights", output_dir = "./", 
                      output_file = "Diagn.html", theme = "blue")

# Page

flights %>%
  diagnose_paged_report(subtitle = "flights", output_dir = "./",
                        output_file = "Diagn.pdf", theme = "blue")

# Informe AED

# Web

heartfailure %>%
  eda_web_report(target = "death_event", subtitle = "heartfailure", 
                 output_dir = "./", output_file = "EDA.html", theme = "blue")

# Page

heartfailure %>%
  eda_paged_report(target = "death_event", subtitle = "heartfailure", 
                   output_dir = "./", output_file = "EDA.pdf", theme = "blue")

# Informe de Transformacion

# Web

heartfailure %>%
  transformation_web_report(target = "death_event", subtitle = "heartfailure",
                            output_dir = "./", output_file = "transformation.html", 
                            theme = "blue")
# Page

heartfailure %>%
  transformation_paged_report(target = "death_event", subtitle = "heartfailure",
                              output_dir = "./", output_file = "transformation.pdf", 
                              theme = "blue")

### Tablas DBMS ### Formato SQl


# Preparacion de datos

if (!require(DBI)) install.packages('DBI')
if (!require(RSQLite)) install.packages('RSQLite')
if (!require(dplyr)) install.packages('dplyr')
if (!require(dbplyr)) install.packages('dbplyr')

library(dbplyr)
library(dplyr)

carseats <- ISLR::Carseats
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA

# connect DBMS

con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# copy carseats to the DBMS with a table named TB_CARSEATS

copy_to(con_sqlite, carseats, name = "TB_CARSEATS", overwrite = TRUE)

# Diagonose table of the DBMS

# Diagnosis of all columns

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose()

# Positions values select columns, and In-memory mode

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose(1, 3, 8, in_database = FALSE)

# Diagnosticar la calidad de los datos de las variables categóricas en el DBMS

# Positions values select variables, and In-memory mode and collect size is 200

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_category(7, in_database = FALSE, collect_size = 200) 

# Diagnosticar la calidad de los datos de las variables numéricas en el DBMS

# Diagnosis of all numerical variables

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_numeric()

# Diagnosticar valores atípicos de variables numéricas en el DBMS

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_outlier()  %>%
  filter(outliers_ratio > 1)

# Trazar información de valores atípicos del diagnóstico de datos numéricos en el DBMS

# Visualization of numerical variables with a ratio of
# outliers greater than 1%

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  plot_outlier(con_sqlite %>% 
                 tbl("TB_CARSEATS") %>% 
                 diagnose_outlier() %>%
                 filter(outliers_ratio > 1) %>%
                 select(variables) %>%
                 pull())

# Informe de la información de diagnóstico de datos para la tabla de thr DBMS

# create html file. 

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_web_report()

# create pdf file. file name is Diagn.pdf

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  diagnose_paged_report(output_format = "pdf", output_file = "Diagn.pdf")

# Tabla EDA del DBMS

# extract only those with 'Urban' variable level is "Yes",
# and find 'Sales' statistics by 'ShelveLoc' and 'US'

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(Urban == "Yes") %>%
  group_by(ShelveLoc, US) %>%
  describe(Sales)

# Prueba de normalidad en columnas numéricas utilizando en el DBMS

# Test log(Income) variables by 'ShelveLoc' and 'US',
# and extract only p.value greater than 0.01.

# SQLite extension functions for log transformation

RSQLite::initExtension(con_sqlite)

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  mutate(log_income = log(Income)) %>%
  group_by(ShelveLoc, US) %>%
  normality(log_income) %>%
  filter(p_value > 0.01)

# Visualización de normalización de columna numérica en el DBMS

# extract only those with 'ShelveLoc' variable level is "Good",
# and plot 'Income' by 'US'

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(US) %>%
  plot_normality(Income)

# Calcular el coeficiente de correlación entre dos columnas de la tabla en DBMS

# extract only those with 'ShelveLoc' variable level is "Good",
# and compute the correlation coefficient of 'Sales' variable
# by 'Urban' and 'US' variables.
# And the correlation coefficient is negative and smaller than 0.5

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>%
  filter(coef_corr < 0) %>%
  filter(abs(coef_corr) > 0.5)

# Visualizar el gráfico de correlación de columnas numéricas en el DBM

# Extract only those with 'ShelveLoc' variable level is "Good",
# and visualize correlation plot of 'Sales' variable by 'Urban'
# and 'US' variables.

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  filter(ShelveLoc == "Good") %>%
  group_by(Urban, US) %>%
  correlate(Sales) %>% 
  plot()

# EDA basado en la variable objetivo

# If the target variable is a categorical variable

categ <- target_by(con_sqlite %>% tbl("TB_CARSEATS") , US)

# If the variable of interest is a numarical variable

cat_num <- relate(categ, Sales)
cat_num

summary(cat_num)

plot(cat_num)

# Reporte de la información de EDA para la tabla del DBMS

# create html file. file name is EDA_TB_CARSEATS.html

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  eda_web_report(US, output_file = "EDA_TB_CARSEATS.html")

## target variable is numerical variable
# reporting the EDA information, and collect size is 350

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  eda_web_report(Sales, collect_size = 350)

# create pdf file. file name is EDA2.pdf

con_sqlite %>% 
  tbl("TB_CARSEATS") %>% 
  eda_paged_report("Sales", output_file = "EDA2.pdf")