#### Gtsummary ####

# Load pacakages

install.packages("gtsummary")

# Data

library(gtsummary)
data("trial")

tbl_summary(trial) %>%
  as_flex_table()

# Markdown

system.file(package = "gtsummary") %>%
  file.path("rmarkdown_example/gtsummary_rmarkdown_html.Rmd") %>%
  file.edit()

# build gtsummary table
tbl <- tbl_summary(trial)

# using the {gt} package
as_gt(tbl) %>% gt::as_latex()

# using the {huxtable} package
as_hux_table(tbl) %>% huxtable::to_latex()

# using the {kableExtra} package
as_kable_extra(tbl, format = "latex")

# using the knitr::kable function
as_kable(tbl, format = "latex")


# Web Image

install.packages("webshot2")

tbl_summary(trial) %>% # build gtsummary table
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "my_table_image.png"
  )

# Max potencial 

trial %>% 
  select(trt, age, grade, response) %>% 
  tbl_summary(by = trt, 
              missing = "no") %>% 
  add_p() %>% 
  add_overall() %>% 
  add_n() %>% 
  bold_labels()







