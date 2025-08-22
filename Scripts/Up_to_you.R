# Install necessary packages if you don't have them
# install.packages("ggplot2")
# install.packages("dplyr")

# Load the libraries
library(ggplot2)
library(dplyr)
library(shadowtext)

# --- 1. Define Colors and Plot Constants ---
bg_color   <- "#e0e1d5" # The pale background
main_color <- "#46634f" # The dark sage green
font_family <- "sans"   # A generic sans-serif font
tile_width <- 0.85      # Store tile dimensions in variables
tile_height <- 0.7

# # --- 2. Prepare the Data (with Day-of-Month Logic) ---
# # Define the target year for the chart
# target_year <- 2025


# Get the current date from the system
current_date <- Sys.Date()
current_date
current_year <- as.numeric(format(current_date, "%Y"))
current_year
current_month <- as.numeric(format(current_date, "%m"))
current_month
current_day <- as.numeric(format(current_date, "%d"))
current_day

# Function to get the number of days in the current month for the target year
get_days_in_month <- function(year, month) {
  dt <- as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d")
  next_month <- seq(dt, by = "month", length = 2)[2]
  last_day <- as.numeric(format(next_month - 1, "%d"))
  return(last_day)
}

# Create the main data frame for the 12 month tiles
months_df <- data.frame(
  month_num = 1:12,
  month_abb = toupper(month.abb)
) %>%
  mutate(
    month_letters = sapply(strsplit(month_abb, ""), paste, collapse = "\n"),
    category = case_when(
      # target_year < current_year ~ "done",
      # target_year > current_year ~ "up_to_you",
      month_num>current_month~"up_to_you",
      month_num==current_month~"current_month",
      .default = "done"
      
    )
  )

# --- 3. Create a separate data frame for the partial fill rectangle ---
current_month_fill_df <- data.frame() # Start with an empty df

# Only create the rectangle if we are in the target year (2025)
if (current_year == current_year) {
  days_in_month <- get_days_in_month(current_year, current_month)
  percentage_complete <- current_day / days_in_month
  
  # Calculate the coordinates for the TOP-TO-BOTTOM fill rectangle
  current_month_fill_df <- data.frame(
    xmin = current_month - tile_width / 2,
    xmax = current_month + tile_width / 2,
    ymin = tile_height / 2 - (tile_height * percentage_complete),
    ymax = tile_height / 2
  )
}

# --- 4. Create the Plot ---
ggplot(months_df, aes(x = month_num, y = 0)) +
  
  # Draw the 12 base tiles (filled or outlined)
  geom_tile(aes(fill = category, color = category), width = tile_width, height = tile_height, size = 1.2)+
  
  # **NEW**: Add the partial fill rectangle for the current month (TOP-TO-BOTTOM)
  geom_rect(
    data = current_month_fill_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = main_color,
    inherit.aes = FALSE # IMPORTANT
  )+
  
  # Add text for "done" months
  geom_text(
    data = . %>% filter(category == "done"),
    aes(label = month_letters),
    color = bg_color, fontface = "bold", family = font_family, size = 8, lineheight = 0.8)+
  
  
  # Add text for "up to you" months
  geom_text(
    data = . %>% filter(category == "up_to_you"),
    aes(label = month_letters),
    color = main_color, fontface = "bold", family = font_family, size = 8, lineheight = 0.8
  )+
  # Add text for current month
  geom_shadowtext(
    data = . %>% filter(category == "current_month"),
    aes(label = month_letters),
    color = bg_color, fontface = "bold", family = font_family, size = 8, lineheight = 0.8
  )+
  
  # Add Annotations
  annotate("text", x = 6.5, y = 0.6, label = as.character(current_year), family = font_family, size = 15, color = main_color) +
  annotate("text", x = 4, y = -0.55, label = "this is done", family = font_family, size = 12, color = main_color) +
  annotate("text", x = 10, y = -0.55, label = "this is up to you", family = font_family, size = 12, color = main_color) +
  
  # --- 5. Final Theming and Styling ---
  scale_fill_manual(values = c("done" = main_color, "up_to_you" = bg_color,"current_month"=bg_color), guide = "none") +
  scale_color_manual(values = c("done" = main_color, "up_to_you" = main_color,"current_month"=main_color), guide = "none") +
  coord_cartesian(xlim = c(0.5, 12.5), ylim = c(-1, 1)) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color, color = NA))->A


## Save the plot
ggsave(filename = "Up_to_you_plot.png",plot = A,device = "png",width = 12,height = 7,units = "in",path = "Figures/",dpi=600)
