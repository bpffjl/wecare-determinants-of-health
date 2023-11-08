
# Start timer -------------------------------------------------------------

tictoc::tic()

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)

# Parameters --------------------------------------------------------------

project <- 'project name'
plot_type <-  'plotly'
# could select from 'highcharter', 'ggplot2', 'plotly'
month_start <- '2021-12-01'
month_end <- '2021-12-01'

# Determine file name -----------------------------------------------------

file_name <- glue::glue("{str_replace_all(str_to_lower(project),' ','-')}-{in_month}-{today()}.html")

file_name

# Render data extraction --------------------------------------------------

rmarkdown::render(input = here::here("scripts",
                                     "data-extraction.Rmd"),
                  params = list(
                    project = project,
                    plot_type = plot_type,
                    dw_uid = dw_uid,
                    dw_pw = dw_pw,
                    month_start =month_start,
                    month_end =month_end
                  ),
                  output_file =
                    here::here("output",
                               'html',
                               file_name))


# End timer ---------------------------------------------------------------

tictoc::toc()

