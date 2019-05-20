library(tidyverse)
library(gganimate)
install.packages("gridgraphics")
#install.packages("ggimage")
library(ggimage)
#install.packages("countrycode")
library(countrycode)

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")


str(nobel_winners$category)

# Get Population
# From http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv
populationRaw <- read.csv("/Users/Chris/Documents/GitHub/9 Dev Data Products/Week3Assignment/World Bank Data/Population/API_SP.POP.TOTL_DS2_en_csv_v2_10576638.csv", 
                          skip=4, header = TRUE, sep = ",")
population <- populationRaw %>%
    select(Country.Code, X2017) %>%
    rename(Population = X2017) %>%
    filter(complete.cases(.)) %>%
    mutate(Country.Code.Char = as.character(Country.Code)) %>%
    select(-Country.Code)

topCountries <- nobel_winners %>%
    group_by(birth_country) %>%
    summarise(n = n()) %>%
    na.omit() %>%
    top_n(10)

winners2 <-
    nobel_winners %>%
    mutate(birth_country = gsub(".*\\((.*)\\).*", "\\1", birth_country),
           birth_country = gsub("Scotland|Northern Ireland", "United Kingdom", birth_country),
           birthCountryCode = countrycode(birth_country, 'country.name', 'iso3c'),
           death_country = gsub(".*\\((.*)\\).*", "\\1", death_country),
           death_country = gsub("Scotland|Northern Ireland", "United Kingdom", death_country),
           death_country = ifelse(is.na(death_country),birth_country, death_country),
           deathCountryCode = countrycode(death_country, 'country.name', 'iso3c'),
    ) %>%
    select(prize_year, category,birth_date,birth_country,death_date,death_country, birthCountryCode, deathCountryCode) %>%
    filter(complete.cases(.))

countryFinalHome <-
    winners2 %>%
    group_by(deathCountryCode, prize_year) %>%
    summarise(finalHomeCount = n())

countryData <-
    winners2 %>%
    group_by(birthCountryCode) %>%
    summarise(prizes = n()) %>%
    inner_join(countryFinalHome, by = c("birthCountryCode" = "deathCountryCode")) 

# counts <- nobel_winners %>%
#     #filter(birth_country %in% topCountries$birth_country) %>%
#     group_by(birth_country, prize_year) %>%
#     summarise() %>%
# 
#     mutate(cumsum = cumsum(n),
#            countryCode = countrycode(birth_country, 'country.name', 'iso3c', nomatch = 'BAD'))



counts <- winners2 %>%
    filter(birth_country %in% topCountries$birth_country) %>%
    group_by(birthCountryCode, category, prize_year) %>%
    #filter(prize_year > 1970) %>%
    summarise(prizes = n()) %>%
    inner_join(population, by = c("birthCountryCode" = "Country.Code.Char")) %>%
    select(-"Country.Code") %>%
    mutate(cumPrizes=cumsum(prizes),
        cumPrizesByPop=cumsum(prizes)/(population/1000000),
           birthCountryName = countrycode(birthCountryCode, 'iso3c', 'country.name'))


# Grid Plot
g = ggplot(counts, aes(x = birthCountryName, y = category, 
                       colour = birthCountryName)) + 
    geom_point(aes(size = cumPrizes), alpha=0.6) + 
    scale_size_continuous(range = c(2, 40)) +
    #geom_image(aes(image="https://www.r-project.org/logo/Rlogo.png"), size=.05) +
    #geom_text(aes(x = cumHomeCount, y=cumPrizes, label = birthCountryName), hjust = 0.2) + 
    transition_reveal(prize_year) + 
    #coord_cartesian(clip = 'off') + 
    labs(title = 'Top 10 Nobel Prize Winning Countries', 
         y = 'Prize Category') + 
    theme_minimal() + 
    theme(axis.title = element_blank()) +
    scale_color_brewer(palette = "RdYlBu") +
    theme(legend.position = "none") +
    theme(plot.margin = margin(5.5, 20, 5.5, 5.5))
animate(g, fps = 10, width = 750, height = 450)
anim_save("nobelprizes.gif")


counts <- winners2 %>%
    filter(birth_country %in% topCountries$birth_country) %>%
    group_by(birthCountryCode, prize_year) %>%
    #filter(prize_year > 1970) %>%
    summarise(prizes = n()) %>%
    left_join(countryFinalHome, by = c("birthCountryCode" = "deathCountryCode","prize_year")) %>%
    mutate(finalHomeCount = ifelse(is.na(finalHomeCount),0,finalHomeCount)) %>%
    inner_join(population, by = c("birthCountryCode" = "Country.Code.Char")) %>%
    select(-"Country.Code") %>%
    mutate(cumPrizes=cumsum(prizes)/(population/1000000),
           cumHomeCount=cumsum(finalHomeCount)/(population/1000000),
           birthCountryName = countrycode(birthCountryCode, 'iso3c', 'country.name'))

# Animation
g = ggplot(counts, aes(x = cumHomeCount, y = cumPrizes, group = birthCountryName, 
                   colour = birthCountryName)) + 
    geom_point(aes(size = population/1000000), alpha=0.3) + 
    scale_size_continuous(range = c(2, 80)) +
    #geom_image(aes(image="https://www.r-project.org/logo/Rlogo.png"), size=.05) +
    geom_text(aes(x = cumHomeCount, y=cumPrizes, label = birthCountryName), hjust = 0.2) + 
    transition_reveal(prize_year) + 
    #coord_cartesian(clip = 'off') + 
    labs(title = 'Nobel Prize Winning Countries', 
         x = 'Total Prizes (at Death) per Million Pop.', 
         y = 'Total Prizes (Nationality) per Million Pop.') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(5.5, 50, 5.5, 5.5))
g
gganimate(g, "nobelprizes.gif")
animate(g, fps = 10, width = 750, height = 450)
anim_save("nobelprizes.gif")



d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
)





ggplot(counts, aes(x = prize_year, y = cumsum, group = birth_country, colour = birth_country)) + 
    geom_line() + 
    #geom_segment(aes(xend = 1900, yend = cumsum), linetype = 2) + 
    geom_point(size = 3) + 
    geom_text(aes(x = prize_year+5, label = birth_country), hjust = 0) + 
    transition_reveal(prize_year) + 
    coord_cartesian(clip = 'off') + 
    labs(title = 'Nobel Prize Winning Countries', y = 'Total Prizes') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(5.5, 40, 5.5, 5.5))


countrycode(topCountries$birth_country, 'country.name', 'iso3c', nomatch = 'BAD')

install.packages("ggimage")
library(ggimage)
set.seed(2017-02-21)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
)
ggplot(d, aes(x, y)) + geom_image(aes(image=image))
