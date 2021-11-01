# Load libraries
pacman::p_load(tidyverse, here, sysfonts, ggforce, cowplot, lubridate, ggext)

#Aesthetics
showtext::showtext_auto()
font1 <- "Roboto Condensed"
font2 <- "Roboto Condensed Light"
sysfonts::font_add_google(font1)
sysfonts::font_add_google(font2)
# hrbrthemes::import_roboto_condensed()
# extrafont::loadfonts()

# Get palette
scales::show_col(rainbow(20))
palette <- c(rainbow(9)[8],rainbow(9)[2])
palette2 <- c(rainbow(9)[2],rainbow(9)[8])
scales::show_col(palette)

menColour <- palette[1]
womenColour <- "#cc00ff" #palette[2]

foreground <- hrbrthemes::theme_ft_rc()$text$colour
background <- hrbrthemes::theme_ft_rc()$plot.background$fill

# Set Data Directories
dataDir <-  here::here("2021", "2021-10-26", "data")
outputDir <-  here::here("2021", "2021-10-26","output")

# Load Data
ultraRankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

countryToContinentMapping <- readr::read_csv('https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/r/country-and-continent-codes-list-csv.csv')

# Use IOC country codes to get to a country name we can use
# From https://www.olympiandatabase.com/index.php?id=1670&L=1
iocCountries <- readr::read_csv(file.path(dataDir, "iocCountries.csv"))

iocCountriesTidy <- iocCountries %>%
  # Fix some country names to fix Continent Mapping
  mutate(
         iocCountry = replace(iocCountry, iocCode == "BIH", "Bosnia and Herzegovina"), # Bosnia-Herzegovina in Continent Mapping
         iocCountry = replace(iocCountry, iocCode == "BRU", "Brunei Darussalam"), # Brunei
         iocCountry = replace(iocCountry, iocCode == "KGZ", "Kyrgyz Republic"), # Kyrgyzstan
         iocCountry = replace(iocCountry, iocCode == "MAC", "Macao"), #Macau
         iocCountry = replace(iocCountry, iocCode == "MKD", "Macedonia"), # North Macedonia
         iocCountry = replace(iocCountry, iocCode == "RUS", "Russian Federation"), # Russia x 2
         iocCountry = replace(iocCountry, iocCode == "SVK", "Slovakia (Slovak Republic)"), # Slovakia SVK
         iocCountry = replace(iocCountry, iocCode == "SYR", "Syrian Arab Republic")) #Syria

countryToContinentMappingTidy <- countryToContinentMapping %>%
  mutate(countryName = sub(" *\\,.*", "", Country_Name),
         # Fix some country names to fix too
         countryName = replace(countryName, Three_Letter_Country_Code == "GBR", "Great Britain"), # United Kingdom of Great Britain & Northern Ireland
         countryName = replace(countryName, Three_Letter_Country_Code == "KOR", "South Korea"), # Korea
         countryName = replace(countryName, Three_Letter_Country_Code == "PRK", "North Korea") # Korea
  ) %>%
  select(countryName, Continent_Name, Three_Letter_Country_Code)

# Tidy Race Data
racesTidy <- races %>%
  filter(!is.na(country) & !is.na(distance) & distance > 0) %>% 
  select(race_year_id, event, city, country, distance, elevation_gain, date)

ultraRankingsTidy <- ultraRankings %>% 
  filter(!is.na(time_in_seconds) & !is.na(gender)) %>%
  select(c(-time, age)) %>%
  mutate(nationality = toupper(nationality),
         nationality = replace(nationality, nationality == "SGP", "SIN"),
         nationality = replace(nationality, nationality == "LBN", "LIB"),
         nationality = replace(nationality, nationality == "SPA", "ESP"))

maxDistance = max(racesTidy$distance)
racesWithRunners <- ultraRankingsTidy %>% 
  left_join(., racesTidy, by="race_year_id") %>%
  left_join(., iocCountriesTidy, by=c("nationality" = "iocCode")) %>%
  left_join(., countryToContinentMappingTidy, by=c("iocCountry" = "countryName")) %>%
  mutate(time_in_hours = time_in_seconds/3600,
         speed = distance / time_in_hours,
         # distanceGroup = cut(distance,c(0, 50, 160, 170, 171, 172, 173, 174, 175, maxDistance), include.lowest= TRUE, right = FALSE,
         #                     labels = c('< 50 km', '50-159 km', '160-169 km', '170 km', '171 km', '172 km', '173 km', '174 km', '175 km+')
         #                     )
         ) %>%
  filter(speed < 20) # Note there are some outliers speed that need to be removed

# Get 10 Most Improved Runners per Continent
mostImproved3 <-racesWithRunners %>% 
  group_by(Continent_Name, Three_Letter_Country_Code, gender, runner) %>%
  summarise(
    firstDate = min(date), # lowest time
    highestSpeedDate = date[which(speed==max(speed))], # time for highest speed
    firstSpeed = speed[which(date==min(date))], #speed for earliest date
    highestSpeed = max(speed), # highest speed
    improvement = highestSpeed - firstSpeed) %>%
  group_by(Continent_Name) %>%
  top_n(10, improvement) %>%
  mutate(rank = order(order(improvement, decreasing=TRUE))) %>%
  ungroup() %>%
  mutate(period = lubridate::time_length(difftime(highestSpeedDate, firstDate), "years"))

#Plots
p2 <- ggplot(data=mostImproved3) +
    geom_link(aes(colour=gender, alpha = stat(index), #size = stat(index), Use this for Headlight Effect
                  x= firstDate, xend = highestSpeedDate, y = firstSpeed,  yend = highestSpeed),
              size=0.3
    ) +
    # Starting circle
    geom_point(aes(x=firstDate, y = firstSpeed, colour=gender), size=1, shape = 21, alpha=1,
               fill = hrbrthemes::theme_ft_rc()$plot.background$fill, stroke = 0.3
    ) +
    # Ending circle
    geom_point(aes(x=highestSpeedDate, y = highestSpeed, colour=gender), size=1, shape = 21, alpha=1,
               fill = hrbrthemes::theme_ft_rc()$plot.background$fill, stroke = 0.3
    ) +
  
  # Break down by Continent
  facet_grid(. ~ Continent_Name) +
  # Style
  hrbrthemes::theme_ft_rc() +
  scale_y_continuous(labels = function(x) paste(x, "mph")) +
  scale_colour_manual(values = palette) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(hjust = 0.5), # Centre y axis title 
        panel.spacing.x = unit(1, "lines"), # Create horizontal spacing between facets
        plot.title.position = "plot",
        plot.margin = margin(0.6, 0.6, 0.4, 0.6, "cm")
  ) +
  # Allow styling of headings
  theme(
    plot.title=ggtext::element_markdown(size = 50, margin = margin(0, 0, 0.18, 0, "cm")),
    plot.subtitle=ggtext::element_markdown(size = 18, margin = margin(0, 0, 0.4 , 0, "cm")),
  ) +
  # Headings
  labs(title = "Ultra Trail Running - Most improved runners by Continent",
       subtitle = paste0("10 most improved runners in each continent, with <span style='color:", 
                         womenColour, "'>**4 women**</span> and <span style='color:", 
                         menColour, "'>**96 men**</span>  overall, based on the difference between their first and fastest runs."),
       caption = "V.0.1, 31.10.2021  |  Visualisation by @ChrisWoodsSays  |  Data from International Trail Running Association",
       y = "Run Speed") 

# Get top runner details for hand crafting labels
topWomenAndMen <- mostImproved3 %>% filter %>% 
  mutate(newRank = if_else(gender == "W", rank/100.0, rank/1.0)) %>% 
  group_by(Continent_Name) %>% 
  top_n(1, -newRank)
  
# Add Labels
ggdraw(p2) +
  draw_label(label = "Abraham Bobby", x=0.168, y=0.426, color=menColour, size=12, fontfamily = font2) +
  draw_label(label = "Sergey Ionov", x=0.323, y=0.415, color=menColour, size=12, fontfamily = font2) +
  draw_label(label = "Romuald Gerard", x=0.465, y=0.560, color=menColour, size=12, fontfamily = font2) +
  draw_label(label = "Denise Bourassa", x=0.586, y=0.235, color=womenColour, size=12, fontfamily = font2) +
  draw_label(label = "Katherine Macmillan", x=0.755, y=0.435, color=womenColour, size=12, fontfamily = font2) +
  draw_label(label = "Andrea Lopez", x=0.862, y=0.325, color=womenColour, size=12, fontfamily = font2)

ggsave(file.path(outputDir, "TrailRunnersMostImproved.png"), height = 9, width = 16, units = "cm")
