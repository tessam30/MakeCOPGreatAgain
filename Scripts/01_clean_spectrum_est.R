# PROJECT: Pull in COP23 Estimates from Nashiva
# PURPOSE: Munge and Analysis of Spectrum estimates
# AUTHOR: Tim Essam | SI
# REF ID:   c6791216
# LICENSE: MIT
# DATE: 2023-03-06
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(readxl)
    
    
  # SI specific paths/functions  
    load_secrets()
    file_list <- list.files("Data", pattern = "spectrum", full.names = T)
    #benin <- "Data/Benin updated spectrum feb2023.xlsx"
    
  # Functions  
  

# LOAD DATA ============================================================================  
    
  munge_spectrum <- function(file_name){
    
    # Extract out the fields that will be used as crosswalk
    df_header <- read_excel(file_name, skip = 3, n_max = 3) %>% 
      mutate(`...49` = NA_character_, 
             `...50` = NA_character_) %>% 
      janitor::clean_names() %>% 
      pivot_longer(estimated_hiv_prevalence_percent:x50,
                   names_to = "indicator",
                   values_to = "category") %>% 
      mutate(cw_number = seq(3, max(row_number()+2), 1)) %>% 
      fill(category, .direction = "down") %>% 
      mutate(indic = ifelse(str_detect(indicator, "x[:digit:]", negate = T), indicator, NA_character_)) %>% 
      fill(indic, .direction = "down") %>% 
      select(-c(1, 2, indicator))
    
    # Extract out body of the data
    df_body <- read_excel(file_name, skip = 7) %>%
      janitor::clean_names() %>% 
      rename(country = x1, 
             year = x2) %>% 
      pivot_longer(estimate_3:high_50,
                   names_to = "est",
                   values_to = "value") %>% 
      separate(est, into = c("est", "cw_number"), sep = "_") %>% 
      mutate(cw_number = as.numeric(cw_number))
  
  df_body_join <- df_body %>% 
    left_join(., df_header, by = c("cw_number")) %>% 
    mutate(category = gsub("\\)|\\(", "", category)) %>% 
    select(-cw_number)
  
  return(df_body_join)
  }
    
   df_spectrum <-  map(file_list, ~munge_spectrum(.x)) %>% 
     list_rbind()
  
# VIZ ============================================================================
  
  # Viz function
  plot_epi_curve <- function(df, cntry) {
  df1 <- df %>% 
    filter(str_detect(category, "All ages"),  
           indic %in% c("estimated_aids_deaths", "estimated_new_hiv_infections"), 
           est == "estimate", 
           country == {{cntry}}) %>% 
    spread(indic, value) %>% 
    rename(number_total_deaths_hiv_pop = estimated_aids_deaths,
           number_new_hiv_infections = estimated_new_hiv_infections) %>% 
    mutate(epi_gap = number_new_hiv_infections - number_total_deaths_hiv_pop)
  
  ou <- df1 %>% distinct(country) %>% pull()

  df1 %>% 
    ggplot(aes(x = year)) +
    geom_area(aes(y = number_new_hiv_infections), fill = "#C6D5E9", alpha = 0.95) +
    geom_area(aes(y = -number_total_deaths_hiv_pop), fill = "#F1CED2",  alpha = 0.95) +
    geom_line(aes(y = number_new_hiv_infections), color = denim, linewidth = 1) +
    geom_line(aes(y = -number_total_deaths_hiv_pop), color = old_rose, linewidth = 1) +
    geom_line(aes(y = epi_gap), color = "white", size = 0.25) +
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = number_new_hiv_infections, fill = denim), shape = 21, color = "white", size = 3)+
    geom_point(data = . %>% filter(year == max(year)), 
               aes(y = -number_total_deaths_hiv_pop, fill = old_rose), shape = 21, color = "white", size = 3) + 
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = number_new_hiv_infections, color = denim, 
                  label = label_number_si(accuracy = 0.1)(abs(number_new_hiv_infections))),
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    geom_text(data = . %>% filter(year == max(year)), 
              aes(y = -number_total_deaths_hiv_pop, color = old_rose, 
                 label = label_number_si(accuracy = 0.1)(abs(number_total_deaths_hiv_pop))), 
              hjust = -0.3, size = 12/.pt,
              family = "Source Sans Pro SemiBold") +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(label = ~ label_number_si()(abs(.))) +
    scale_x_continuous(breaks = seq(1990, 2024, 5)) +
    geom_hline(yintercept = 0, color = grey80k) +
    si_style_ygrid(text_scale = 1.15) +
    labs(x = NULL, y = NULL,
         title = glue("{ou}: Epidemic Controls Curve"),
         caption =  glue("Source: Spectrum 2023 Draft Estimates | SI analytics: US Agency for International Development")) +
    coord_cartesian(expand = T, clip = "off") +
    theme(plot.title = element_markdown()) 
  }

# Create plots ============================================================================

  ou_list <- df_spectrum %>% distinct(country) %>% pull() 
  plot_epi_curve(df_spectrum, cntry = "Benin")

  map(ou_list, ~plot_epi_curve(df_spectrum, .x))
  