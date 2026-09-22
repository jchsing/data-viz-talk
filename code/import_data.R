library(here)
library(readr)
library(tidyverse)
library(arrow)
library(lubridate)


## import data
sr_data <- read_csv(here("data", "example_sr_data.csv"))
screen_data <- read_parquet(here("data", "example_screen_data.parquet"))
jerk_data <- read_parquet(here("data", "example_jerk_data.parquet"))
    

## wrangle data a bit
screen_df <- 
    screen_data |> 
    filter(!is.na(state)) |> 
    filter(!day_window == 9)

sr_df <- 
    sr_data |> 
    mutate(
        study_date = mdy(study_date),
        time_to_bed   = mdy_hms(time_to_bed),
        sleep_onset   = mdy_hms(sleep_onset),
        sleep_offset  = mdy_hms(sleep_offset),
        time_out_bed  = mdy_hms(time_out_bed)
    ) |> 
    mutate(
        window_start = as.POSIXct(study_date - days(1)) + hours(16),
        hours_time_to_bed = as.numeric(
            difftime(time_to_bed, window_start, units = "hours")
        ),
        
        hours_sleep_onset = as.numeric(
            difftime(sleep_onset, window_start, units = "hours")
        ),
        
        hours_sleep_offset = as.numeric(
            difftime(sleep_offset, window_start, units = "hours")
        ),
        
        hours_time_out_bed = as.numeric(
            difftime(time_out_bed, window_start, units = "hours")
        ) 
    ) 

jerk_df <- 
    jerk_data |> 
    filter(!day_window == 9)


