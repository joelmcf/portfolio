#-------------------------------------------------------------------------------
# Project: COVID-19 Weekly Surveillance Report
# Purpose: Create graphics summarizing COVID-19 data for local
# Author: Joel McFarland
# Inputs: 
#   - COVID data from NYT github
#   - 2019 ACS county-level population estimates
#   - Vaccine data from IDPH
#
# Outputs
#   - New COVID-19 cases time series, faceted by district county
#   - New COVID-19-related deaths time series, faceted by district county
#   - COVID-19 vaccinations administered time series, faceted by district county
#   - New COVID-19 cases per 100,000 residents time series, single plot
#   - GIF of new weekly cases by county, including DuPage and Cook
#
# Instructions for faceted GIF: https://towardsdatascience.com/how-to-combine-animated-plots-in-r-734c6c952315
#-------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(gganimate)
library(magick)
library(zoo)
library(lubridate)
library(scales)

substr(a$data,1,nchar(a$data)-3)

# Load datasets

# County-level population data for denominators
countypop <- read_csv("co-est2019-alldata.csv") %>%
    filter(STNAME == "Illinois" & COUNTY != "000") %>%
    mutate(CTYNAME = substr(CTYNAME,1,nchar(CTYNAME)-7))


# County-level COVID-19 data from NYT
covid <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) 

covid2 <- covid %>%
    filter(state == "Illinois") %>%
    left_join(countypop, by = c("county" = "ctyname")) %>%
    filter(date >= as.Date('2020-03-01') & date <= Sys.Date()) %>% 
    arrange(county, date) %>%
    group_by(county) %>%
    mutate(daily_cases = cases - lag(cases, 1),
           daily_cases_7d_avg = rollmean(daily_cases, k=7, na.pad=TRUE, align="right"),
           daily_cases_100k = (daily_cases/popestimate2019)*100000,
           daily_cases_100k_7d_avg = rollmean(daily_cases_100k, k=7, na.pad=TRUE, align="right"),
           daily_deaths = deaths - lag(deaths, 1),
           daily_deaths_7d_avg = rollmean(daily_deaths, k=7, na.pad=TRUE, align="right"),
           daily_deaths_100k = (daily_deaths/popestimate2019)*100000,
           daily_deaths_100k_7d_avg = rollmean(daily_deaths_100k, k=7, na.pad=TRUE, align="right")) %>%
    filter(county %in% c("DeKalb", "Kane", "Kendall", "LaSalle", "Will"))

# County-level vaccine data from IDPH
dekalb_vaccine <- read.csv("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=dekalb", skip = 1)
kane_vaccine <- read.csv("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=kane", skip = 1)
kendall_vaccine <- read.csv("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=kendall", skip = 1)
lasalle_vaccine <- read.csv("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=lasalle", skip = 1)
will_vaccine <- read.csv("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=will", skip = 1)

vaccines <- rbind(dekalb_vaccine, kane_vaccine, kendall_vaccine, lasalle_vaccine, will_vaccine) %>%
    mutate(Report_Date = as.character(Report_Date)) %>%
    mutate(date = mdy(substr(Report_Date, 1, nchar(Report_Date)-12)),
           PctVaccinatedPopulation = round(PctVaccinatedPopulation*100, 1))

# Create figures

cases_plot <- ggplot(covid2, aes(date, daily_cases)) + 
    geom_col(fill = "#ccc2b6") + 
    geom_line(aes(y = rollmean(daily_cases, 7, fill = TRUE )), colour = "#c41230") +
    labs(y="# of New Cases", x= "Date") +
    theme_classic()+
    theme(axis.title.x = element_blank()) +
    facet_wrap(vars(county), nrow = 2, scales='free') + 
    scale_x_date(labels = date_format("%b"))

deaths_plot <- ggplot(covid2, aes(date, daily_deaths)) + 
    geom_col(fill = "#ccc2b6") + 
    geom_line(aes(y = rollmean(daily_deaths, 7, fill = TRUE )), colour = "#c41230") +
    labs(y="# of Deaths", x= "Date") +
    theme_classic()+ 
    theme(axis.title.x = element_blank()) +
    facet_wrap(vars(county), nrow = 2, scales='free') + 
    scale_x_date(labels = date_format("%b"))

vaccines_plot <- ggplot(vaccines, aes(date, AdministeredCountChange)) +
    geom_col(fill = "#ccc2b6") + 
    geom_line(aes(y = AdministeredCountRollAvg), colour = "#c41230") + 
    labs(y = "# of Administered Doses", x = "Date") +
    theme_classic() +
    theme(axis.title.x = element_blank()) + 
    scale_x_date(breaks = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")), 
                 labels = date_format("%b")) + 
    facet_wrap(vars(CountyName), nrow = 2, scales='free')


cases_singleplot <-  ggplot(covid2, aes(x=date, y=daily_cases_100k, 
                                        group=county, color=county)) +
    geom_line(aes(y=rollmean(daily_cases_100k, 7, fill = county, group=county, 
                             color=county))) +
    labs(y="# of New Cases", x= "Date") +
    theme_classic() + 
    labs(title = "New COVID-19 cases per 100,000 residents", 
         subtitle = "7-Day Rolling Average") +
    theme(axis.title.x = element_blank()) + 
    scale_x_date(labels = date_format("%b"))

cases_plot
deaths_plot
vaccines_plot
cases_singleplot


# Export figures

ggsave("cases_plot.png", cases_plot)
ggsave("deaths_plot.png", deaths_plot)
ggsave("vaccines_plot.png", vaccines_plot)
ggsave("cases_singleplot.png", cases_singleplot)


# ------------------------------------------------------------------------------

# Filter shapefiles for GIF

il_state <- map_data("state") %>%
    filter(region == "illinois")
il_counties <- map_data("county") %>%
    filter(region == "illinois" & 
               subregion %in% c("cook", "de kalb", "du page", "kane", "kendall", "la salle", "will")) %>%
    mutate(subregion = case_when(
        subregion == "cook" ~ "Cook",
        subregion == "de kalb" ~ "DeKalb",
        subregion == "du page" ~ "DuPage",
        subregion == "kane" ~ "Kane",
        subregion == "kendall" ~ "Kendall",
        subregion == "la salle" ~ "LaSalle",
        subregion == "will" ~ "Will"
    ))

# Merge shapefile with NYT COVID-19 data

combined <- inner_join(covid2, il_counties, by = c("county" = "subregion"))

il_base <- ggplot(data = il_state, mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")
il_base


# Create base map for GIF

finalmap <- il_base +
    geom_polygon(data = combined, aes(fill = daily_cases_100k_7d_avg), color = "white") +
    geom_polygon(color = "black", fill = NA) +
    theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
    ) +
    ggtitle("7-Day Average of New Cases") + 
    scale_fill_gradient(low = "#CCC2B6", high = "#c41230", 
                        breaks = c(5, 50, 100, 200),  
                        name = "New Daily Cases per 100,000") + 
    transition_time(date) + 
    labs(title = "Date: {frame_time}")


# Animate GIF

a1 <- animate(finalmap, renderer = av_renderer())
a2 <- animate(finalmap, nframes = 372, dur = 40)


# Save GIF

anim_save("a.mp4", a)

# Second GIF

trend_final <- cases_singleplot +
    transition_reveal(date)

b1 <- animate(trend_final, renderer = av_renderer())
b2 <- animate(trend_final, nframes = 372, dur = 40)

anim_save("b.mp4", b)
