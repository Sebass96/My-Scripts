### CHAT GPT ####

# Install packages #

library(devtools)

devtools::install_github("samterfa/openai")

# Set up #

Sys.setenv(openai_organization_id = {"org-gQqbnIF2df879od1fyG9ieZb"})
Sys.setenv(openai_secret_key = {"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"})

# Examples #

library(openai)
library(purrr)

# Example 1 #

list_models() %>% 
  pluck('data') %>% 
  map_dfr(compact)

# Example 2 #

create_completion(
  model = 'davinci', 
  max_tokens = 30,
  temperature = .5,
  top_p = 1,
  n = 1,
  stream = F, 
  prompt = 'Once upon a time') %>% 
  pluck('choices') %>% 
  map_chr(~ .x$text)

# Example 3 #

create_image(
  prompt = "A rollerskating zebra", 
  n = 1, 
  response_format = "url")

create_image(
  prompt = "A reading pig", 
  n = 1, 
  response_format = "url")

# Write code #

install.packages("gptstudio")

library(gptstudio)

ggplot(mtcars, aes(x=wt, y=mpg)) + 
 geom_point(color="blue")

ggplot(iris, aes(x=Species, y=Petal.Length, color=Species)) + 
geom_boxplot() +
  xlab("Especies")
