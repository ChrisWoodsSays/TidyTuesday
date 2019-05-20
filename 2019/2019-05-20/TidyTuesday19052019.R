library(tidyverse)
library(gganimate)
library(countrycode)
setwd("~/Documents/GitHub/TidyTuesday")

# Get Nobel Prize Winners
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

# Determine to 10 Countries
topCountries <- nobel_winners %>%
    group_by(birth_country) %>%
    summarise(n = n()) %>%
    na.omit() %>%
    top_n(10)

# Tidy Winners
# - Take just country in brackets where there is such
# - Change UK country names to UK
# - Get ISO country code
winnersTidy <-
    nobel_winners %>%
    mutate(birth_country = gsub(".*\\((.*)\\).*", "\\1", birth_country),
           birth_country = gsub("Scotland|Northern Ireland", "United Kingdom", birth_country),
           birthCountryCode = countrycode(birth_country, 'country.name', 'iso3c')
    ) %>%
    select(prize_year, category,birth_date,birth_country,birthCountryCode) %>%
    filter(complete.cases(.))

# Count Prizes by Country, Category and Year
counts <- winnersTidy %>%
    filter(birth_country %in% topCountries$birth_country) %>%
    group_by(birthCountryCode, category, prize_year) %>%
    summarise(prizes = n()) %>%
    mutate(cumPrizes=cumsum(prizes),
           birthCountryName = countrycode(birthCountryCode, 'iso3c', 'country.name'))

# Plot Animated Chart
g = ggplot(counts, aes(x = birthCountryName, y = category, 
                       colour = birthCountryName)) + 
    geom_point(aes(size = cumPrizes), alpha=0.6) + 
    scale_size_continuous(range = c(2, 40)) +
    transition_reveal(prize_year) + 
    labs(title = 'Top 10 Nobel Prize Winning Countries', 
         y = 'Prize Category') + 
    theme_minimal() + 
    theme(
        plot.title = element_text(size=22),
        axis.title = element_blank()) +
    scale_color_brewer(palette = "RdYlBu") +
    theme(legend.position = "none") +
    theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))
animate(g, fps = 10, width = 750, height = 450)
anim_save("nobelprizes.gif")