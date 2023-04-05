### Data Manipulate ###

library(tidyverse)
library(gapminder)

data("gapminder")

# Select

year_country_gdp <- gapminder %>% select(year,country,gdpPercap)

# Filter

year_country_gdp_euro <- gapminder %>%
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)

year_country_gdp_afr <- gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp, country, year)

# Group by

str(gapminder)

gdp_bycontinents <- gapminder %>%
  group_by(continent) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

life_bycontinents <- gapminder %>% 
  group_by(country) %>% 
  summarise(mean_life = mean(lifeExp)) %>% 
  arrange(-mean_life)

gdp_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap))

gdp_pop_bycontinents_byyear <- gapminder %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop))

# Count

gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)

gapminder %>%
  group_by(continent) %>%
  summarize(se_pop = sd(lifeExp)/sqrt(n()))


gapminder %>%
  group_by(continent) %>%
  summarize(
    mean_le = mean(lifeExp),
    min_le = min(lifeExp),
    max_le = max(lifeExp),
    se_le = sd(lifeExp)/sqrt(n()))

# Mutate

gdp_pop_bycontinents_byyear <- gapminder %>%
  mutate(gdp_billion=gdpPercap*pop/10^9) %>%
  group_by(continent,year) %>%
  summarize(mean_gdpPercap=mean(gdpPercap),
            sd_gdpPercap=sd(gdpPercap),
            mean_pop=mean(pop),
            sd_pop=sd(pop),
            mean_gdp_billion=mean(gdp_billion),
            sd_gdp_billion=sd(gdp_billion))

gdp_pop_bycontinents_byyear_above25 <- gapminder %>%
  mutate(gdp_billion = ifelse(lifeExp > 25, gdpPercap * pop / 10^9, NA)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop),
            mean_gdp_billion = mean(gdp_billion),
            sd_gdp_billion = sd(gdp_billion))

gdp_future_bycontinents_byyear_high_lifeExp <- gapminder %>%
  mutate(gdp_futureExpectation = ifelse(lifeExp > 40, gdpPercap * 1.5, gdpPercap)) %>%
  group_by(continent, year) %>%
  summarize(mean_gdpPercap = mean(gdpPercap),
            mean_gdpPercap_expected = mean(gdp_futureExpectation))


lifeExp_2countries_bycontinents <- gapminder %>%
  filter(year==2002) %>%
  group_by(continent) %>%
  sample_n(2) %>%
  summarize(mean_lifeExp=mean(lifeExp)) %>%
  arrange(desc(mean_lifeExp))