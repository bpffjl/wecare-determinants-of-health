
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)

# Parameters --------------------------------------------------------------


in_file <- "WECARE DataPull 1.xlsx"
in_start_date <- "2023-05-01"

# Render CPI data collection tool -----------------------------------------


file_name = glue::glue("wecare-sdoh-questions-{in_start_date}-{lubridate::today()}.html")


rmarkdown::render(input = here::here("scripts",
                                     "report.Rmd"),
                  params = list(
                    in_file = in_file,
                    in_start_date=in_start_date
                  ),
                  output_file = 
                    here::here("output",
                               "html",
                               file_name))

# Upload Months Data Pull report from Rite Track
# Rename to "Year.month WECARE Data Pull.xlsx" into WECARE Urgent Need Department Report folder and "data folder"
# Change Year on "run markdown data collections report" and month as 2 digit month
# RUn/Cntrl Shift enter 
