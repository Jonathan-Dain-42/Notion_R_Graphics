#' +++++++++++++++++++++++++++++++++++++++
#' 
#' Goal to make a life progress bar graphic for notion. 
#' 
#' 
#' +++++++++++++++++++++++++++++++++++++++


library(tidyverse)
library(ggplot2)
library(shadowtext)


bg_color   <- "#e0e1d5" # The pale background
main_color <- "#46634f" # The dark sage green
font_family <- "sans"   # A generic sans-serif font


life_tibble <- tibble(
progress_bar=c("Life","Year","Quarter","Month","Week","PhD"),
height=100
)

# functions to get progress
current_date <- Sys.Date()
current_date
current_year <- as.numeric(format(current_date, "%Y"))
current_year
current_month <- as.numeric(format(current_date, "%m"))
current_month
current_day <- as.numeric(format(current_date, "%d"))
current_day

# Life progress
get_life_progress <- function(current_date) {
  born <- as.Date("1997-08-02")
  death <- as.Date("2077-08-02")
  total_time <- death - born
  days_since_birth <- current_date-born
  percent_lived <- as.numeric(days_since_birth)/as.numeric(total_time)*100
  return(percent_lived)
}

get_life_progress(current_date = Sys.Date())

# Year Progress:
get_Year_progress <- function(current_date) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  total_time <- as.numeric(365)
  days_since_start_of_year <- current_date-as.Date(paste0(current_year,"-01-01"))
  percent_year <- as.numeric(days_since_start_of_year)/as.numeric(total_time)*100
  return(percent_year)
}

get_Year_progress(Sys.Date())

# Quarter Progress:
get_quarter_progress <- function(current_date) {
  quarter(current_date)
  q_start <- floor_date(x = current_date,unit = "quarter")
  q_end <- ceiling_date(x = current_date,unit = "quarter")
  days_in_quarter <- as.numeric(q_end-q_start)
  days_since_q_start <- as.numeric(current_date-q_start)
  percent_quarter <- as.numeric(days_since_q_start)/as.numeric(days_in_quarter)*100
  return(percent_quarter)
}


# Month Progress:
get_month_progress <- function(current_date) {
  total_time <- days_in_month(x = Sys.Date()) |> as.numeric()
  day(current_date)
  percent_month <- as.numeric(day(current_date))/as.numeric(total_time)*100
  return(percent_month)
}

get_month_progress(current_date = Sys.Date())

# Week Progress:
get_week_progress <- function(current_date) {
  wday(current_date)
  percent_week <- as.numeric(wday(current_date))/as.numeric(7)*100
  return(percent_week)
}

get_week_progress(current_date = as.Date("2025-08-16"))

# PhD Progress:
get_precent_done <- function(current_date) {
  start_date <- as.Date("2022-09-01")
  grad_date <- as.Date("2027-05-01")
  total_time <- grad_date - start_date
  days_since <- current_date-start_date
  percent_done <- as.numeric(days_since)/as.numeric(total_time)*100
  return(percent_done)
}

get_precent_done(current_date = Sys.Date())



life_tibble |> 
  mutate(progress=case_when(
    progress_bar=="Life"~get_life_progress(current_date = current_date),
    progress_bar=="PhD"~get_precent_done(current_date = current_date),
    progress_bar=="Year"~get_Year_progress(current_date = current_date),
    progress_bar=="Month"~get_month_progress(current_date = current_date),
    progress_bar=="Week"~get_week_progress(current_date = current_date),
    progress_bar=="Quarter"~get_quarter_progress(current_date = current_date)
  )) |> 
  ggplot()+
  # add the tiles
  geom_bar(stat='identity',aes(y=progress_bar,x=height),fill=bg_color,color="black",lwd=1)+
  # fill the tiles
  geom_bar(stat="identity",aes(x=progress,y=progress_bar),fill=main_color)+
  # add percentages
  geom_text(aes(x=progress+8,y=progress_bar,label = paste(round(progress,digits = 2),sep = "","%")),size = 8)+
  # Add the labels on side
  geom_text(aes(x=-1,y=progress_bar,label=progress_bar),color=main_color,size=8,fontface="bold",hjust="right")+
  # remove theme
  theme_void(base_size = 25)+
  # adjust the background color
  theme(plot.background = element_rect(fill = bg_color, color = NA))+
  # adjust the text elements
  theme(plot.title = element_text(hjust = 0.5,colour = "black"),
        plot.subtitle = element_text(hjust = 0.5,color="black"))+
  expand_limits(x = -8)+
  # add labels
  labs(title = "Memento mori",
       subtitle = "'Where you mind goes, your body will follow'")->B

ggsave(filename = "Progress_plot.png",plot = B,device = "png",width = 12,height = 7,units = "in",path = "Figures/",dpi=600)

