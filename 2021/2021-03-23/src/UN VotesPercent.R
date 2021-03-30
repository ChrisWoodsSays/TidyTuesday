# Load packages
#install.packages("pacman")
pacman::p_load(tidyverse, lubridate, stringi, 
               unvotes, hrbrthemes, wesanderson)

hrbrthemes::import_roboto_condensed()
# Get palette
Darjeeling1 <- wes_palettes$Darjeeling1[c(3,1,5)]

# Information on each roll call vote of the United Nations General Assembly.
# un_roll_calls

# Data on the issue of each roll call of the United Nations General Assembly
# un_roll_call_issues

# United Nations General Assembly voting data by country and roll call
# un_votes

# Ppower Rankings
# https://www.usnews.com/news/best-countries/power-rankings
powerCountries <- c("US", "RU", "CN", "DD", "DE", "GB", "FR", "JP", "IL", "KR", "SA")

votes <- un_votes %>%
  # Get details of vote including date
  left_join(un_roll_calls, by = "rcid") %>% 
  # Get issue for each one
  left_join(un_roll_call_issues, by = "rcid") %>% select(-short_name) %>%
  filter(country_code %in% powerCountries) %>%
  # Rename German Democratic Republic to GDR
  mutate(country = if_else(country_code == "DD", "GDR", country),
  # Shorten Nuclear Title
        issue = if_else(issue == "Nuclear weapons and nuclear material", "Nuclear weapons and material", as.character(issue))) %>%
  mutate(year = lubridate::year(date)) 

# Get just UK votes
ukVotesByIssue <- votes %>% 
  filter(country_code == "GB") %>%
  group_by(rcid, issue, vote) %>%
  rename(ukVote = vote) %>%
  summarise() 

# And pair them with others
ukVotesPairedWithOthers <- ukVotesByIssue %>% 
  left_join(votes %>% rename(countryVote = vote), by = c("issue", "rcid")) %>%
  select(rcid, issue, ukVote, country_code, country, countryVote, year) %>%
  filter(country_code != "GB")

# Compare and group
ukVotesVariance <- ukVotesPairedWithOthers %>%
  mutate(comparison = case_when(
    ukVote == "yes" & (countryVote == "no" | countryVote == "abstain") ~ "UK+",
    (ukVote == "no" | ukVote == "abstain") & countryVote == "yes" ~ "UK-",
    ukVote == countryVote ~ "Same",
    TRUE ~ "Other"
  )) %>%
  # Remove ones without an issue
  filter(!is.na(issue)) %>%
  group_by(country, country_code, 
           issue, year, 
           comparison) %>%
  summarise(count = n())

startYear <- min(votes$year)
endYear <- max(votes$year)

# Filter down to the ones we want to visualise
data <- ukVotesVariance %>% filter(comparison %in% c("UK+", "UK-", "Same") & country_code!='CN*')
extras <- rbind(data.frame(country='South Korea', country_code='KR', issue='Palestinian conflict', year=2013, comparison='UK+', count=0),
                data.frame(country='Germany', country_code='DE', issue='Palestinian conflict', year=2013, comparison='UK+', count=0),
                data.frame(country='Germany', country_code='DE', issue='Colonialism', year=2013, comparison='UK+', count=0),
                data.frame(country='Germany', country_code='DE', issue='Economic development', year=2004, comparison='UK+', count=0))
data <- rbind(data, extras)

data <- data %>% group_by(year) %>% mutate(proportion = 100 * count / sum(count))

overlapFactor = 2/5
UKNegLimit = max(subset(data, comparison == "UK-")$proportion)
UKSameLimit = max(subset(data, comparison == "Same")$proportion)
UKPosLimit = max(subset(data, comparison == "UK+")$proportion)
innerLine <- UKNegLimit
middleLine <- innerLine + UKSameLimit * overlapFactor/ 2
outerLine <- innerLine + UKSameLimit * overlapFactor
outerLimit <- outerLine + UKPosLimit

alpha = 1

g <- ggplot() +
  # Add area where UK votes the same way
  geom_ribbon(data = data %>% filter(comparison == "Same"), aes(x = year,
                                                                ymin = middleLine - proportion / 2, 
                                                                ymax = middleLine + proportion  / 2,
                                                                fill = comparison), alpha = alpha/1, stat="identity", colour = NA) +
  # Votes more positively
  geom_ribbon(data = data %>% filter(comparison == "UK+"), aes(x = year, ymin = outerLine, ymax = outerLine + proportion, 
                                                               fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  # Votes less positively    
  geom_ribbon(data = data %>% filter(comparison == "UK-"), aes(x = year, ymin = innerLine, ymax = innerLine - proportion, 
                                                               fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  theme_ft_rc() +
  coord_polar(direction=1) +
  scale_x_continuous(breaks=seq(startYear, endYear, by=round((endYear - startYear)/5)), expand=c(0,0), lim=c(startYear, endYear)) +
  scale_y_continuous(limits=c(0,outerLimit),breaks=seq(0, 30, 10)) +
  #ylim(0, outerLimit) +
  #expand_limits(y = c(1, outerLimit) +
  scale_fill_manual(values = Darjeeling1) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(hjust = 0.5),
        strip.text.y = element_text(hjust = 0.5, size = 10),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(0, "lines"), # Remove horizontal spacing between facets
        panel.spacing.y = unit(0, "lines"), # Remove vertical pacing between facets
        plot.title.position = "plot"
  ) +
  labs(title = "UN Votes - How the UK votes differently to others",
       subtitle = "Compared to US News 'Power Countries', 1946 - 2019",
       caption = "V.0.5, 28.3.2021  |  Visualisation by @ChrisWoodsSays  |  Data: https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-03-23, www.usnews.com/news/best-countries/power-rankings") +
  
  facet_grid(vars(issue), vars(country),
             labeller = label_wrap_gen(width = 18, multi_line = TRUE))

# Create non radial legend with a sample of data
dataLegend <- data  %>% filter(country_code == "RU" & issue == "Human rights") 
dataLegend$comparison = factor(dataLegend$comparison, levels=c('UK+','Same','UK-'))
legend <- ggplot() +
  geom_ribbon(data = dataLegend %>% filter(comparison == "Same"), aes(x = year, ymin = 20 - count/2, ymax = 20 + count/2, fill = comparison), alpha = alpha/1, stat="identity", colour = NA) +
  geom_ribbon(data = dataLegend %>% filter(comparison == "UK+"),
              aes(x = year, ymin = 18, ymax = 18 + count * 1.5, fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  geom_ribbon(data = dataLegend %>% filter(comparison == "UK-"),
              aes(x = year, ymin = 20 - count/1.5, ymax = 20, fill = comparison), alpha = alpha, stat="identity", colour = NA) +
  theme_ft_rc() +
  coord_polar(direction=1) +
  scale_fill_manual(values = Darjeeling1) +
  theme(legend.position = "none",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(6, "lines"), # Increase horizontal spacing between facets
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        strip.text.x = element_blank()) +
  facet_wrap(vars(comparison))

# Shift years to left as facets move when an extra country is added or iamge size changed
xShift = - 0.050
yShift = - 0.0190
# Add facets, text and legend together and plot
plot <- cowplot::ggdraw(g) +
  cowplot::draw_label(startYear, x = 0.140 + xShift, y = 0.856 + yShift, colour = "#929299", size = 6) + # 1946
  cowplot::draw_label(startYear + 15, x = 0.163 + xShift, y = 0.815 + yShift, colour = "#929299", size = 6) + # 1961
  cowplot::draw_label(startYear + 30, x = 0.162 + xShift, y = 0.738 + yShift, colour = "#929299", size = 6) + # 1976
  cowplot::draw_label(startYear + 45, x = 0.098 + xShift, y = 0.742 + yShift, colour = "#929299", size = 6) + # 1991
  cowplot::draw_label(startYear + 60, x = 0.087 + xShift, y = 0.82 + yShift, colour = "#929299", size = 6) + # 2006
  cowplot::draw_plot(legend, .5, .89, .55, .08) +
  cowplot::draw_label("UK votes Yes\nvs No or abstain", x = 0.595, y = 0.935, colour = "#929299", size = 8, hjust = 1) +
  cowplot::draw_label("UK votes\nthe same", x = 0.747, y = 0.935, colour = "#929299", size = 8, hjust = 1) +
  cowplot::draw_label("UK votes No or abstains\nvs Yes", x = 0.908, y = 0.937, colour = "#929299", size = 8, hjust = 1) +
  cowplot::draw_label("As a proportion of annual votes", x = 0.908, y = 0.89, colour = "#929299", size = 8, hjust = 1)

plot
ggsave(here::here("2021", "2021-03-23", "output", "HowTheUKVotesAtTheUNProportion.png"), plot, device = "png", width = 29.65, height = 21,
       units = "cm")

