## Statistics Basic ##

### Correlations ###

## Charge packages

library(foreign)
install.packages("apaTables")
library(apaTables)
library(psych)
install.packages("corrr")
library(corrr)
library(PerformanceAnalytics)
library(tidyverse)

# Charge dataset

library(haven)

df <- read_spss("Data/base datos spss correlaciones.sav")

class(df)

data <- data.frame(df)

class(data)

# Complete N.A

data2 <- data[complete.cases(data), ]

# Correlation matrix

correlac <- cor(data2)

# Correlation table APA

apa.cor.table(data2, filename = "Table_correlation", table.number = 2, show.conf.interval = F, landscape = T)

# Correlation graphic

pairs.panels(data2, pch = 20, stars = T, main = "Correlation")

### T student ###

## Charge packages

library(haven)
library(tidyverse)
install.packages("apa")
library(apa)
install.packages("tidyBF")
install.packages("BayesFactor")
library(BayesFactor)

# Charge dataset

setwd("C:/Users/pc/Desktop/Diseños Experimentales/Scripts")

df <- read_sav("t_test_idepen.sav")
class(data)
data <- data.frame(df)

# T test

t.obj <- t.test(df$IIEF ~ df$group, var.equal = T)

t_apa(t.obj, es = "cohens_d", format = c("text"), print = T)

# Wilcox test (T test no parametrico)

wilcox.test(IIEF ~ group, data = df, exact = F)

# Description

df %>% 
  group_by(group) %>% 
  summarise(
    count = n(),
    M = mean(IIEF),
    SD = sd(IIEF),
    median = median(IIEF),
    IQR = IQR(IIEF))


# T test bayesiano #Cloud#

ttestBF(formula = IIEF ~ group, data = data)

# Graphics

install.packages("ggstatsplot")
library(ggstatsplot)

data_f <- data %>% 
  mutate_at("group", as.factor)
str(data_f)
  
# View in cloud

ggstatsplot::ggbetweenstats(
  data = data_f,
  x = group, # Variable de agrupación/independiente
  y = IIEF, # Variable dependiente
  xlab = "Grupo", # etiqueta para el eje X
  ylab = "IIFE", # etiqueta para el eje y
  type = "p",#eliges el tipo de prueba "p" (for parametric), "np" (for nonparametric), "r" (for robust), "bf" (for Bayes Factor).
  effsize.type = "d", #el tipo de estimador de efecto además de "d" está "g", "r" y la robusta que viene por defecto si eliges la robusta.
  conf.level = 0.99,
  plot.type = "violin", # type of plot también se puede "box" o "violin"
  outlier.tagging = TRUE, # Te indica cuales son outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  #outlier.label = IIEF, # La etiqueta que va a tener los outliers
  outlier.label.args = list(color = "red"), # la etiqueta de los outlairs en rojo
  messages = FALSE, # apagar los mensajes, ¿qué mensajes?, ni idea. 
  ggtheme = ggplot2::theme_gray(), # cambiar el fondo de la gráfica todos los temas en https://ggplot2.tidyverse.org/reference/ggtheme.html
  package = "yarrr", # El paquete asociado a la paleta de colores.
  palette = "info2", # Elegir la paletta dentro del paquete 
  title = "T Test",
  caption = "Source: Sebastian")


### ANOVA ###

## Charge packages

install.packages("apaTables")

library(apaTables)
library(tidyverse)
library(haven)
library(effectsize)
library(rstatix)
library(DescTools)
install.packages("report")
library(report)
install.packages("FSA")
library(FSA)
install.packages("rcompanion")
library(rcompanion)


# Charge dataset

data <- read_sav("Anova.sav")
str(data)
data$edad <- factor(data$edad)

# ANOVA parametrico

object <- aov(data$ASF ~ data$edad)
summary(object)
plot(object)
report(object)

# Post hoc

TukeyHSD(object)
plot(TukeyHSD(object))

# Descritivos

apa.1way.table(edad, ASF, data)

FSA::Summarize(ASF ~ edad,
               data = data,
               digits = 2)

lm_output <- lm(ASF ~ edad, data = data)
apa.aov.table(lm_output, filename = "Anova.doc", table.number = 4)


# Tamaño del efecto

eta <- 5809.37/(5809.37 + 56151.10)

ome <- effectsize::omega_squared(object)
ome
epsilon_squared(object)
cohens_f(object)

interpret_omega_squared(ome, rules = "field2013")


# ANOVA no parametrico

kruskal.test(ASF ~ edad, data = data)

# Posthoc

dunnTest(ASF ~ edad,
         data = data,
         method = "bh")

# ANOVA bayesiano

bf_oneway_anova(data = Anova, x = edad, y = ASF)

# Graphics
# View in cloud

install.packages("PMCMRplus")
library(PMCMRplus)
install.packages("rstantools")
library(rstantools)

ggstatsplot::ggbetweenstats(
  data = data,
  x = edad,
  y = ASF,
  plot.type = "violin", # type of plot también se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cuántos decimales?
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # método para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "rótulo del eje x", 
  ylab = "rótulo del eje y",
  title = "Anova de un factor", # Título del plot
  ggtheme = ggthemes::theme_economist(), # cambiar el fondo del gráfico
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # elegir el paquete asociado a la paleta de colores.
  palette = "Darjeeling1", # cambiar la paleta
  messages = FALSE)


## Histogram ##

library(ggplot2)

set.seed(1234)

df <- data.frame(
  sex = factor(rep(c("M", "H"), each = 200)),
  weigth = round(c(rnorm(200, mean = 55, sd = 5), 
                   rnorm(200, mean = 65, sd = 5))))

head(df)

# Histogram normal

ggplot(df, aes(x = weigth)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(weigth)),
             color = "blue", linetype = "dashed", size =1)


# Other example

ggplot(df, aes(x = weigth, fill = sex, color = sex)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)+
  geom_density(alpha = .2)

library(plyr)

mu <- ddply(df, "sex", summarise, grp.mean = mean(weigth))

ggplot(df, aes(x = weigth, fill = sex, color = sex)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)+
  geom_density(alpha = .2) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = sex),
             linetype = "dashed")


## Descriptive statistics ##

# Charge packages

install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(Hmisc)
install.packages("pastecs")
library(pastecs)
library(psych)

# Charge dataset

data(package = "palmerpenguins")
data <- penguins
head(data)
glimpse(data)

# Descriptive

summary(data)

# Mode1

require(Hmisc)
e1 <- describe(data) # Describe los cuantiles

# Mode 2

require(pastecs)
e2 <- stat.desc(data)
e3 <-  stat.desc(data[,-c(1:2,7)]) %>%
  round(2)
  
# Mode 3

require(psych)

psych::describe(data)
psych::describe.by(data, group = data$sex)

# Descriptivos por caracteristica

install.packages("doBy")
library(doBy)
summaryBy(bill_length_mm + bill_depth_mm ~ island + sex, data = data,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )


## Assumptions ##

# Charge dataset

data(package = "palmerpenguins")
data <- penguins
head(data)
glimpse(data)

df <- data.frame(data)

df <- df[complete.cases(df), ]

# Regression

mod <- lm(body_mass_g ~ flipper_length_mm, data = df)

# Linealidad

mean(mod$residuals) # si la media es cercana a 0 se cumple la linealidad

# graphics

library(car)
crPlots(mod)
plot(mod,1)

install.packages("ggfortify")
library(ggfortify)
autoplot(mod, 1)

# Normalidad

library(MASS)

# Distribution of studentized residuals

sres <- studres(mod)
shapiro.test(sres)

# Graphics

plot(mod, 2)
autoplot(mod, 2)

# Homocedasticidad

# Non - constant Variance Score Test

library(car)
ncvTest(mod)

# Breusch - Pagan Test

library(lmtest)
bptest(mod)

# Graphic

plot(mod, 3)
autoplot(mod, 3)

# Independence

durbinWatsonTest(mod) # si no siguen un patron espeficico tienen idenpendencia

# Graphic

plot(mod$residuals)
acf(mod$residuals)

install.packages("gvlma")
library(gvlma)
plot(gvlma(mod))

## Baremos ##

# Charge packages

library(palmerpenguins)
library(car)
install.packages("writexl")
library(writexl)
library(tidyverse)

# Charge dataset

data <- data.frame(penguins)

# Group by sex

machos <- data %>% 
  filter(sex == "male")

hembras <- data %>% 
  filter(sex == "female")

hembra2 <- data[data$sex == "female", ]

# Baremos table

score <- machos$body_mass_g
tosc <- sort(unique(machos$body_mass_g))
perc <- 100 * (cumsum(prop.table(table(score))))
sura <- 100 * (tosc/max(score))
zsco <- sort(unique(scale(score))) # Z score
tsco <- 50 + 10 * zsco # T score
baremos <- cbind(tosc, perc, tsco, zsco)
baremos <- data.frame(baremos)

# Save data in xlsx

write_xlsx(baremos, "baremos.xlsx")


### Regressions ###

# charge packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("see","jtools","olsrr","parameters","stats","ggplot2", "palmerpenguins", "plot3D" , "plot3Drgl","apaTables","gvlma","broomExtra","performance")
ipak(packages)

# Charge dataset

data <- data.frame(penguins)
data <- data[complete.cases(data), ]

scatter.smooth(x = data$flipper_length_mm,
               y = data$body_mass_g,
               main = "masa ~ aleta")

# Create regression model

mod <- lm(body_mass_g ~ flipper_length_mm, data = data)

# Verify assumptions

gvlma(mod)
summary(gvlma(mod))
plot(gvlma(mod))


# Model

summary(mod)
model_parameters(mod, bootstrap = T, iterations = 200)

# Ajuste del modelo

AIC(mod)
BIC(mod)

# Confidence intervals 

confint(mod)

# Prediction

data_1 <- data [-c(4:333), ] %>% 
  dplyr::select(body_mass_g, flipper_length_mm) 

pesoPred <- predict(mod, data_1)
actuals_preds <- data.frame(cbind(actuals=data_1$body_mass_g, predicteds=pesoPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)
scatter.smooth(x=data$flipper_length_mm, y=data$body_mass_g, main="masa ~ aleta")
sd(data$body_mass_g)

#visualizaciones

ggstatsplot::ggcoefstats(
  x = stats::lm(body_mass_g ~ flipper_length_mm, data=data),
  sort = "ascending", # sorting the terms of the model based on estimate values
  ggtheme = ggplot2::theme_gray(), # changing the default theme
  stats.label.color = c("#CC79A7"),
  title = "Chupito R",
  subtitle = "Suscríbete"
) +
  # further modification with the ggplot2 commands
  # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c(
    "aleta")) +
  ggplot2::labs(y = "etiqueta y") +
  ggplot2::theme(axis.title.y = ggplot2::element_text(size = 14, face = "bold"))

# Confiabilidad del modelo

lm1 <- lm(body_mass_g ~ flipper_length_mm, data = data)
lm2 <- lm(body_mass_g ~ bill_depth_mm, data = data)
plot_summs(lm1, lm2, scale = T) # mas alejado de 0 mejor prediccion

# other form

plot_summs(lm1, scale = T, plot.distributions = T, inner_ci_level = .9)

# Comparacion entre modelos

lm3 <- lm(body_mass_g ~ bill_length_mm, data = data)
compare_performance(lm1, lm2, lm3, rank = T)
plot(compare_performance(lm1, lm2, lm3, rank = T))

# Graphics

ggplot(data, aes(x=flipper_length_mm, y=body_mass_g, color=sex, shape=sex)) +
  geom_point()+ 
  geom_smooth(method=lm, aes(fill=sex))+
  geom_density_2d()

#función vwReg   #Felix Schönbrodt's blog

#https://rdrr.io/github/vinash85/avinash/src/R/vwReg.R

vwReg <- function(formula, data, title="", B=1000, shade=TRUE, shade.alpha=.1, spag=FALSE, spag.color="darkblue", mweight=TRUE, show.lm=FALSE, show.median = TRUE, median.col = "white", shape = 21, show.CI=FALSE, method=loess, bw=FALSE, slices=200, palette=colorRampPalette(c("#FFEDA0", "#DD0000"), bias=2)(20), ylim=NULL, quantize = "continuous",  add=FALSE, ...) {
  IV <- all.vars(formula)[2]
  DV <- all.vars(formula)[1]
  data <- na.omit(data[order(data[, IV]), c(IV, DV)])
  
  if (bw == TRUE) {
    palette <- colorRampPalette(c("#EEEEEE", "#999999", "#333333"), bias=2)(20)
  }
  
  print("Computing boostrapped smoothers ...")
  newx <- data.frame(seq(min(data[, IV]), max(data[, IV]), length=slices))
  colnames(newx) <- IV
  l0.boot <- matrix(NA, nrow=nrow(newx), ncol=B)
  
  l0 <- method(formula, data)
  for (i in 1:B) {
    data2 <- data[sample(nrow(data), replace=TRUE), ]
    data2 <- data2[order(data2[, IV]), ]
    if (class(l0)=="loess") {
      m1 <- method(formula, data2, control = loess.control(surface = "i", statistics="a", trace.hat="a"), ...)
    } else {
      m1 <- method(formula, data2, ...)
    }
    l0.boot[, i] <- predict(m1, newdata=newx)
  }
  
  # compute median and CI limits of bootstrap
  library(plyr)
  library(reshape2)
  CI.boot <- adply(l0.boot, 1, function(x) quantile(x, prob=c(.025, .5, .975, pnorm(c(-3, -2, -1, 0, 1, 2, 3))), na.rm=TRUE))[, -1]
  colnames(CI.boot)[1:10] <- c("LL", "M", "UL", paste0("SD", 1:7))
  CI.boot$x <- newx[, 1]
  CI.boot$width <- CI.boot$UL - CI.boot$LL
  
  # scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
  CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
  CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))
  
  
  # convert bootstrapped spaghettis to long format
  b2 <- melt(l0.boot)
  b2$x <- newx[,1]
  colnames(b2) <- c("index", "B", "value", "x")
  
  library(ggplot2)
  library(RColorBrewer)
  
  # Construct ggplot
  # All plot elements are constructed as a list, so they can be added to an existing ggplot
  
  # if add == FALSE: provide the basic ggplot object
  p0 <- ggplot(data, aes_string(x=IV, y=DV)) + theme_bw()
  
  # initialize elements with NULL (if they are defined, they are overwritten with something meaningful)
  gg.tiles <- gg.poly <- gg.spag <- gg.median <- gg.CI1 <- gg.CI2 <- gg.lm <- gg.points <- gg.title <- NULL
  
  if (shade == TRUE) {
    quantize <- match.arg(quantize, c("continuous", "SD"))
    if (quantize == "continuous") {
      print("Computing density estimates for each vertical cut ...")
      flush.console()
      
      if (is.null(ylim)) {
        min_value <- min(min(l0.boot, na.rm=TRUE), min(data[, DV], na.rm=TRUE))
        max_value <- max(max(l0.boot, na.rm=TRUE), max(data[, DV], na.rm=TRUE))
        ylim <- c(min_value, max_value)
      }
      
      # vertical cross-sectional density estimate
      d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
        res <- data.frame(density(df$value, na.rm=TRUE, n=slices, from=ylim[1], to=ylim[2])[c("x", "y")])
        #res <- data.frame(density(df$value, na.rm=TRUE, n=slices)[c("x", "y")])
        colnames(res) <- c("y", "dens")
        return(res)
      }, .progress="text")
      
      maxdens <- max(d2$dens)
      mindens <- min(d2$dens)
      d2$dens.scaled <- (d2$dens - mindens)/maxdens  
      
      ## Tile approach
      d2$alpha.factor <- d2$dens.scaled^shade.alpha
      gg.tiles <-  list(geom_tile(data=d2, aes(x=x, y=y, fill=dens.scaled, alpha=alpha.factor)), scale_fill_gradientn("dens.scaled", colours=palette), scale_alpha_continuous(range=c(0.001, 1)))
    }
    if (quantize == "SD") {
      ## Polygon approach
      
      SDs <- melt(CI.boot[, c("x", paste0("SD", 1:7))], id.vars="x")
      count <- 0
      d3 <- data.frame()
      col <- c(1,2,3,3,2,1)
      for (i in 1:6) {
        seg1 <- SDs[SDs$variable == paste0("SD", i), ]
        seg2 <- SDs[SDs$variable == paste0("SD", i+1), ]
        seg <- rbind(seg1, seg2[nrow(seg2):1, ])
        seg$group <- count
        seg$col <- col[i]
        count <- count + 1
        d3 <- rbind(d3, seg)
      }
      
      gg.poly <-  list(geom_polygon(data=d3, aes(x=x, y=value, color=NULL, fill=col, group=group)), scale_fill_gradientn("dens.scaled", colours=palette, values=seq(-1, 3, 1)))
    }
  }
  
  print("Build ggplot figure ...")
  flush.console()
  
  
  if (spag==TRUE) {
    gg.spag <-  geom_path(data=b2, aes(x=x, y=value, group=B), size=0.7, alpha=10/B, color=spag.color)
  }
  
  if (show.median == TRUE) {
    if (mweight == TRUE) {
      gg.median <-  geom_path(data=CI.boot, aes(x=x, y=M, alpha=w3^3), size=.6, linejoin="mitre", color=median.col)
    } else {
      gg.median <-  geom_path(data=CI.boot, aes(x=x, y=M), size = 0.6, linejoin="mitre", color=median.col)
    }
  }
  
  # Confidence limits
  if (show.CI == TRUE) {
    gg.CI1 <- geom_path(data=CI.boot, aes(x=x, y=UL), size=1, color="red")
    gg.CI2 <- geom_path(data=CI.boot, aes(x=x, y=LL), size=1, color="red")
  }
  
  # plain linear regression line
  if (show.lm==TRUE) {gg.lm <- geom_smooth(method="lm", color="darkgreen", se=FALSE)}
  
  gg.points <- geom_point(data=data, aes_string(x=IV, y=DV), size=1, shape=shape, fill="white", color="black")       
  
  if (title != "") {
    gg.title <- theme(title=title)
  }
  
  
  gg.elements <- list(gg.tiles, gg.poly, gg.spag, gg.median, gg.CI1, gg.CI2, gg.lm, gg.points, gg.title, theme(legend.position="none"))
  
  if (add == FALSE) {
    return(p0 + gg.elements)
  } else {
    return(gg.elements)
  }
}

vwReg(body_mass_g ~ flipper_length_mm,data)
vwReg(body_mass_g ~ flipper_length_mm,data,family="symmetric",show.CI=TRUE)
vwReg(body_mass_g ~ flipper_length_mm,data,spag = 0.3,show.median = TRUE)
display.brewer.all()
vwReg(body_mass_g ~ flipper_length_mm,data,palette = brewer.pal(9,"YlGnBu"))
vwReg(body_mass_g ~ flipper_length_mm,data,quantize = "SD")
vwReg(body_mass_g ~ flipper_length_mm,data,shade.alpha = 0,slices = 400, family="symmetric",
      palette = colorRampPalette(c("black","green","yellow","red"),bias = 5)
      (20))


