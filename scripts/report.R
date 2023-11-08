
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(purrr)

# Parameters --------------------------------------------------------------


data_file <-  "WECARE-Assessments-Month-09-01-2023.xlsx"
data_month <- "2023-09-01"

# Render CPI data collection tool -----------------------------------------


file_name = glue::glue("monthly-wecare-update-{data_month}-{lubridate::today()}.pptx")


rmarkdown::render(input = here::here("scripts",
                                     "report.Rmd"),
                  params = list(
                    in_file = data_file,
                    in_start_date=data_month
                  ),
                  output_file = 
                    here::here("output",
                               "html",
                               file_name))

# Upload Months Data Pull report from Rite Track
# Rename to "Year.month WECARE Data Pull.xlsx" into WECARE Urgent Need Department Report folder and "data folder"
# Change Year on "run markdown data collections report" and month as 2 digit month
# RUn/Cntrl Shift enter 
