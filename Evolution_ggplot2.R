### Evolucion GGplot ###

library(tidyverse)

# Charge dataset

df_students <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

df_world_tile <- readr::read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>% 
  mutate(
    ## Namibias two-digit country code is handled as `NA` - let us fix that
    alpha.2 = if_else(name == "Namibia", "NA", alpha.2),
    ## We are going to split "Americas" into "North America" and "Sout America"
    region = if_else(region == "Americas", sub.region, region),
    region = if_else(region %in% c("Northern America", "Central America", "Caribbean"), 
                     "North America", region),
    region = if_else(region == "Southern America", "South America", region),
    ## to join both data sets, we need a id column
    country_code = alpha.3
  )

df_ratios <- df_students %>% 
  ## Let's keep only the most recent data per country
  group_by(country, indicator) %>% 
  filter(year == max(year)) %>% 
  ungroup() %>% 
  # Create `NA`s for countries which do not have any data 2012-2018
  complete(indicator, nesting(country, country_code)) %>% 
  ## Let's focus on primary education and keep only countries (coded by letters)
  filter(
    indicator == "Primary Education",
    str_detect(country_code, "[A-Z]")
  ) %>% 
  ## merge with world tile map data
  full_join(df_world_tile) %>%
  filter(
    !is.na(region),
    !is.na(indicator)
  ) %>% 
  group_by(region) %>% 
  mutate(student_ratio_region = median(student_ratio, na.rm = T)) %>% 
  ungroup()

# Graphics

ggplot(df_ratios, aes(x = region, y = student_ratio)) +
  geom_boxplot()

df_sorted <-
  df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region))

ggplot(df_sorted, aes(x = region, y = student_ratio)) +
  geom_boxplot()+

# Coord Flip

ggplot(df_sorted, aes(x = region, y = student_ratio)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90))

# Modificar grafico

install.packages("ggsci")
library(ggsci)
install.packages("systemfonts")
library(systemfonts)

g <-
  ggplot(df_sorted, aes(x = region, y = student_ratio, color = region)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 90), expand = c(0.02, 0.02)) +
  scale_color_uchicago() +
  labs(x = NULL, y = "Student to teacher ratio") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    axis.text.x = element_text(family = "Roboto Mono", size = 12),
    panel.grid = element_blank()
  )

# Boxplot

g + geom_boxplot()

g +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15)

# Distribucion

set.seed(2019)

g + geom_jitter(size = 2, alpha = 0.25, width = 0.2)

g +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)

# Media mundial

world_avg <-
  df_ratios %>%
  summarize(avg = mean(student_ratio, na.rm = TRUE)) %>%
  pull(avg)

g +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2)

g +
  geom_segment(
    aes(x = region, xend = region,
        y = world_avg, yend = student_ratio_region),
    size = 0.8
  ) +
  geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5)

# Texto

(g_text <-
    g +
    geom_segment(
      aes(x = region, xend = region,
          y = world_avg, yend = student_ratio_region),
      size = 0.8
    ) +
    geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
    stat_summary(fun = mean, geom = "point", size = 5) +
    geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
    annotate(
      "text", x = 6.3, y = 35, family = "Poppins", size = 2.8, color = "gray20", lineheight = .9,
      label = glue::glue("Worldwide average:\n{round(world_avg, 1)} students per teacher")
    ) +
    annotate(
      "text", x = 3.5, y = 10, family = "Poppins", size = 2.8, color = "gray20",
      label = "Continental average"
    ) +
    annotate(
      "text", x = 1.7, y = 11, family = "Poppins", size = 2.8, color = "gray20",
      label = "Countries per continent"
    ) +
    annotate(
      "text", x = 1.9, y = 64, family = "Poppins", size = 2.8, color = "gray20", lineheight = .9,
      label = "The Central African Republic has by far\nthe most students per teacher")
)

# Arrows

arrows <-
  tibble(
    x1 = c(6.2, 3.5, 1.7, 1.7, 1.9),
    x2 = c(5.6, 4, 1.9, 2.9, 1.1),
    y1 = c(35, 10, 11, 11, 73),
    y2 = c(world_avg, 19.4, 14.16, 12, 83.4)
  )

g_text +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = -0.3
  )

# Ajustar arrows

arrows <-
  tibble(
    x1 = c(6.1, 3.62, 1.8, 1.8, 1.8),
    x2 = c(5.6, 4, 2.18, 2.76, 0.9),
    y1 = c(world_avg + 6, 10.5, 9, 9, 77),
    y2 = c(world_avg + 0.1, 18.4, 14.16, 12, 83.45)
  )

(g_arrows <-
    g_text +
    geom_curve(
      data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
      arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
      color = "gray20", curvature = -0.3
    )
)

# Ajust y axis

(g_final <-
    g_arrows +
    scale_y_continuous(
      limits = c(1, NA), expand = c(0.02, 0.02),
      breaks = c(1, seq(20, 80, by = 20))
    ) +
    labs(caption = "Data: UNESCO Institute for Statistics") +
    theme(plot.caption = element_text(size = 9, color = "gray50"))
)

# Mosaicos

map_regions <-
  df_ratios %>%
  mutate(region = fct_reorder(region, -student_ratio_region)) %>%
  ggplot(aes(x = x, y = y, fill = region, color = region)) +
  geom_tile(color = "white") +
  scale_y_reverse() +
  scale_fill_uchicago(guide = "none") +
  coord_equal() +
  theme_light() +
  theme(
    line = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent",
                                   color = "transparent"),
    panel.border = element_rect(color = "transparent"),
    strip.background = element_rect(color = "gray20"),
    axis.text = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  labs(x = NULL, y = NULL)

g_final +
  annotation_custom(ggplotGrob(map_regions), xmin = 2.5, xmax = 7.5, ymin = 52, ymax = 82)


## Raincloud ##

# Charge dataset

library(tidyverse)
library(ggforce)
install.packages("ggdist")
library(ggdist)
install.packages("gghalves")
library(gghalves)
install.packages("ggbeeswarm")
library(ggbeeswarm)
data("iris")

# Violin-boxplot-combination

theme_set(theme_light(base_size = 16))

g1 <- 
  ggplot(iris, aes(Species, Sepal.Width)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)

g1

# Better: add raw data

g1 + geom_point(alpha = .7, position = position_jitter(seed = 1))

g1 + geom_point(alpha = .7, width = 0.1, position = position_jitter(seed = 1))

g1 + ggforce::geom_sina(method = "counts", alpha = .5) #puntos dentro del violin

g1 + ggbeeswarm::geom_quasirandom(width = .3, alpha = .5, varwidth = TRUE, size = 2) #puntos quasirandom

g1 + ggbeeswarm::geom_beeswarm(width = .3, alpha = .5, cex = 1.2, size = 2)

# with linerange + dotplot

ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = .1)

# with boxplot + dotplot

ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = .1)
  #gghalves::geom_half_dotplot(stackdir = "down")

# with boxplot + jitter (on top)

ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .7, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  geom_jitter(width = .05, alpha = .3)

# with boxplot + jitter (side by side)

ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5)

# with boxplot + barcode (side by side)

ggplot(iris, aes(Species, Sepal.Width)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = 0, shape = 95, size = 15, alpha = .3)


# Palmerpenguinssssss

library(palmerpenguins)

# Charge dataset

data <- penguins

df_peng_stats <- 
  data %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  group_by(species) %>% 
  mutate(
    n = n(),
    median = median(bill_ratio),
    max = max(bill_ratio)
  ) %>% 
  ungroup() %>% 
  mutate(species_num = as.numeric(fct_rev(species))) 

# Raincloud

n_fun <- function(x){
  return(data.frame(y = median(x) - 1.25, 
                    label = paste0("n = ",length(x))))
}


p1 <- ggplot(df_peng_stats, aes(x = species, y = bill_ratio, color = species)) + 
  geom_violin(fill = "grey90") + 
  geom_boxplot(width = .2, outlier.shape = NA, coef = 0)+
  stat_summary(
    geom = "text",
    fun.data = n_fun,
    family = "Oswald",
    size = 5
  )

ggplot(df_peng_stats, aes(x = species, y = bill_ratio, color = species)) + 
  ggdist::stat_halfeye(adjust = .5, width = .7, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  geom_jitter(width = .05, alpha = .3)+
  stat_summary(
    geom = "text",
    fun.data = n_fun,
    family = "Oswald",
    size = 5)+
  theme_minimal()+
  coord_cartesian(clip = "off", expand = FALSE) +
  guides(color = F)

p2 <- 
  ggplot(df_peng_stats, aes(bill_ratio, species_num, color = species)) +
  stat_summary(
    geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) +
  geom_point(
    aes(y = species_num - .15), 
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = species_num,
      color = species,
      fill = after_scale(colorspace::lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    aes(x = median, label = format(round(median, 2), nsmall = 2)),
    stat = "unique",
    color = "white",
    family = "Open Sans",
    fontface = "bold",
    size = 3.4,
    nudge_y = .15
  ) +
  geom_text(
    aes(x = max, label = glue::glue("n = {n}")),
    stat = "unique",
    family = "Open Sans",
    fontface = "bold",
    size = 3.5,
    hjust = 0,
    nudge_x = .01,
    nudge_y = .02
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_continuous(
    limits = c(1.6, 3.8),
    breaks = seq(1.6, 3.8, by = .2)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie")
  ) +
  scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  labs(
    x = "Bill ratio",
    y = NULL,
    subtitle = "B. Raincloud plot showing the distribution of bill ratios, estimated as bill length divided by bill depth.",
    caption = "Data: Gorman, Williams & Fraser (2014) *PLoS ONE* &bull; Illustration: Allison Horst"
  ) +
  theme(
    panel.grid.major.x = element_line(size = .35),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.ticks.length = unit(0, "lines"),
    plot.title.position = 'plot',
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.margin = margin(10, 25, 10, 25)
  )
p2


ggplot(df_peng_stats, aes(bill_ratio, species_num, color = species)) +
  stat_summary(
    #geom = "linerange",
    fun.min = function(x) -Inf,
    fun.max = function(x) median(x, na.rm = TRUE),
    linetype = "dotted",
    orientation = "y",
    size = .7
  ) +
  #geom_point(
    #aes(y = species_num - .15), 
    #shape = "|",
    #size = 5,
    #alpha = .33
  #) +
  ggdist::stat_halfeye(
    aes(
      y = species_num,
      color = species,
      fill = after_scale(colorspace::lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    aes(x = median, label = format(round(median, 2), nsmall = 2)),
    stat = "unique",
    color = "white",
    family = "Open Sans",
    fontface = "bold",
    size = 3.4,
    nudge_y = .15
  ) +
  geom_text(
    aes(x = max, label = glue::glue("n = {n}")),
    stat = "unique",
    family = "Open Sans",
    fontface = "bold",
    size = 3.5,
    hjust = 0,
    nudge_x = .01,
    nudge_y = .02
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_x_continuous(
    limits = c(1.6, 3.8),
    breaks = seq(1.6, 3.8, by = .2)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie")
  ) +
  scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
  labs(
    x = "Bill ratio",
    y = NULL,
    subtitle = "B. Raincloud plot showing the distribution of bill ratios, estimated as bill length divided by bill depth.",
    caption = "Data: Gorman, Williams & Fraser (2014) *PLoS ONE* &bull; Illustration: Allison Horst"
  ) +
  theme(
    panel.grid.major.x = element_line(size = .35),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.ticks.length = unit(0, "lines"),
    plot.title.position = 'plot',
    plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
    plot.margin = margin(10, 25, 10, 25)
  )

#### Trelliscope ####

# Charge packages

install.packages("trelliscopejs")

library(tidyverse)
library(plotly)
library(trelliscopejs)

# Charge data

data(mpg)

# GGplot

mpg %>%
  ggplot(aes(displ, hwy)) +
  geom_point(size = 4) +
  geom_smooth(se = FALSE, span = 1) +
  facet_trelliscope(
    ~ manufacturer,
    ncol      = 4,
    nrow      = 3
  )

# Plotly

mpg %>%
  ggplot(aes(displ, hwy)) +
  geom_point() +
  geom_smooth(se = FALSE, span = 1) +
  facet_trelliscope(
    ~ manufacturer,
    ncol      = 4,
    nrow      = 3,
    as_plotly = TRUE
  )

#### ggstatplot ####

library(ggstatsplot)

#  Comparaciones entre grupos # ggbetweenstats #

# Example 1

data(iris)

set.seed(123)

ggbetweenstats(
  data  = iris,
  x     = Species,
  y     = Sepal.Length,
  title = "Distribution of sepal length across Iris species"
)

# Example 2

set.seed(123)
View(movies_long)
str(movies_long)

grouped_ggbetweenstats(
  data             = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
  x                = mpaa, # Variable de agrupación/independiente
  y                = length, # Variable dependiente
  type             = "p",
  grouping.var     = genre,
  outlier.tagging  = TRUE, # Te indica cuales son outliers
  outlier.label    = title, # La etiqueta que va a tener los outliers
  outlier.coef     = 2, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  ggsignif.args    = list(textsize = 4, tip_length = 0.01), # argumentos esteticos adicionales
  p.adjust.method  = "bonferroni", # método para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  annotation.args  = list(title = "Differences in movie length by mpaa ratings for different genres")
)

# Comparaciones dentro del grupo # ggwithinstats #

# Example 1

set.seed(123)
install.packages("WRS2")
library(WRS2) ## for data
install.packages("afex")
library(afex) ## to run anova
data(WineTasting)

ggwithinstats(
  data    = WineTasting,
  x       = Wine,
  y       = Taste,
  title   = "Wine tasting"
)

# Example 2

set.seed(123)

View(bugs_long)

grouped_ggwithinstats(
  data            = dplyr::filter(bugs_long, region %in% c("Europe", "North America"), condition %in% c("LDLF", "LDHF")),
  x               = condition,
  y               = desire,
  type            = "np",
  xlab            = "Condition",
  ylab            = "Desire to kill an artrhopod",
  grouping.var    = region,
  outlier.tagging = TRUE,
  outlier.label   = education
)

# Distribucion de datos # gghistostats #

# Example 1

# Data analisis

view(msleep)

summary(msleep)

set.seed(123)

t.test(msleep$awake, mu = 12)

# Graphic

gghistostats(
  data       = ggplot2::msleep,
  x          = awake,
  title      = "Amount of time spent awake",
  test.value = 12,
  binwidth   = 1
)

# Example 2

set.seed(123)

grouped_gghistostats(
  data              = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
  x                 = budget,
  test.value        = 50,
  type              = "nonparametric",
  xlab              = "Movies budget (in million US$)",
  grouping.var      = genre,
  normal.curve      = TRUE,
  normal.curve.args = list(color = "red", size = 1),
  ggtheme           = ggthemes::theme_tufte(),
  ## modify the defaults from `{ggstatsplot}` for each plot
  plotgrid.args     = list(nrow = 1),
  annotation.args   = list(title = "Movies budgets for different genres")
)

# Distribución sobre variable numérica etiquetada # ggdotplotstats #

# Example 1

install.packages("gapminder")
library(gapminder)

set.seed(123)

ggdotplotstats(
  data       = dplyr::filter(gapminder::gapminder, continent == "Asia"),
  y          = country,
  x          = lifeExp,
  test.value = 55,
  type       = "robust",
  title      = "Distribution of life expectancy in Asian continent",
  xlab       = "Life expectancy"
)


# Example 2

set.seed(123)

grouped_ggdotplotstats(
  data            = dplyr::filter(ggplot2::mpg, cyl %in% c("4", "6")),
  x               = cty,
  y               = manufacturer,
  type            = "bayes",
  xlab            = "city miles per gallon",
  ylab            = "car manufacturer",
  grouping.var    = cyl,
  test.value      = 15.5,
  point.args      = list(color = "red", size = 5, shape = 13),
  annotation.args = list(title = "Fuel economy data")
)

# Scatterplot #

install.packages("ggside")
library(ggside)

# Example 1

data("msleep")

ggscatterstats(
  data  = ggplot2::msleep,
  x     = sleep_rem,
  y     = awake,
  xlab  = "REM sleep (in hours)",
  ylab  = "Amount of time spent awake (in hours)",
  title = "Understanding mammalian sleep"
)

# Example 2

set.seed(123)
library(ggstatsplot)
library(dplyr, warn.conflicts = FALSE)


mtcars_new <- as_tibble(mtcars, rownames = "car")

if (require("ggside")) {
  ggscatterstats(
    data = mtcars_new,
    x = wt,
    y = mpg,
    label.var = car,
    label.expression = wt < 4 & mpg < 20
  ) + # making further customization with `{ggplot2}` functions
    geom_rug(sides = "b")
}

# Example 3

data(movies_long)

set.seed(123)

which(is.na(movies_long$rating))

grouped_ggscatterstats(
  data             = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
  x                = rating,
  y                = length,
  grouping.var     = genre,
  label.var        = title,
  label.expression = length > 200,
  xlab             = "IMDB rating",
  ggtheme          = ggplot2::theme_grey(),
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(2, 9, 1), limits = (c(2, 9)))),
  plotgrid.args    = list(nrow = 1),
  annotation.args  = list(title = "Relationship between movie length and IMDB ratings")
)



# Correlacion # 

install.packages("ggcorrplot")
set.seed(123)

# Example 1

## as a default this function outputs a correlation matrix plot
ggcorrmat(
  data     = ggplot2::msleep,
  colors   = c("#B2182B", "white", "#4D4D4D"),
  title    = "Correlalogram for mammals sleep dataset",
  subtitle = "sleep units: hours; weight units: kilograms"
)

str(msleep)

# Example 2

set.seed(123)

grouped_ggcorrmat(
  data         = dplyr::filter(movies_long, genre %in% c("Action", "Comedy")),
  type         = "robust",
  colors       = c("#cbac43", "white", "#550000"),
  grouping.var = genre,
  matrix.type  = "lower"
)

# Pieplot


# Example 1

set.seed(123)

ggpiestats(
  data         = mtcars,
  x            = am,
  y            = cyl,
  package      = "wesanderson",
  palette      = "Royal1",
  title        = "Dataset: Motor Trend Car Road Tests",
  legend.title = "Transmission"
)

View(mtcars)

# Example 2

set.seed(123)

grouped_ggpiestats(
  data         = mtcars,
  x            = cyl,
  grouping.var = am,
  label.repel  = TRUE,
  package      = "ggsci",
  palette      = "default_ucscgb"
)

# Barplot

# Example 1

set.seed(123)

ggbarstats(
  data             = movies_long,
  x                = mpaa,
  y                = genre,
  title            = "MPAA Ratings by Genre",
  xlab             = "movie genre",
  legend.title     = "MPAA rating",
  ggplot.component = list(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))),
  palette          = "Set2"
)

# Example 2

## setup

set.seed(123)

grouped_ggbarstats(
  data         = mtcars,
  x            = am,
  y            = cyl,
  grouping.var = vs,
  package      = "wesanderson",
  palette      = "Darjeeling2" # ,
  # ggtheme      = ggthemes::theme_tufte(base_size = 12)
)

# Puntos y Bigotes

set.seed(123)

## model
mod <- stats::lm(formula = mpg ~ am * cyl, data = mtcars)

ggcoefstats(mod)

# Boxplot

## loading the needed libraries
set.seed(123)
library(ggplot2)

## using `{ggstatsplot}` to get expression with statistical results

extract_subtitle <- function(p) extract_stats(p)$subtitle_data$expression[[1L]]

stats_results <- ggbetweenstats(morley, Expt, Speed) %>% extract_subtitle()

## creating a custom plot of our choosing
ggplot(morley, aes(x = as.factor(Expt), y = Speed)) +
  geom_boxplot() +
  labs(
    title = "Michelson-Morley experiments",
    subtitle = stats_results,
    x = "Speed of light",
    y = "Experiment number"
  )


