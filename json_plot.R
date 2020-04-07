# Script to plot graph from JSON file
# ICH Code Club 08.04.20 - What's a JSON file?
# Samuel R. Neal
# Data source: http://api.nobelprize.org/v1/laureate.json

### Libraries
library("plyr") #for mapvalues()
library("tidyverse")
library("jsonlite")
library("ISOcodes")

library("ggrepel") ### for labelling

### Read JSON file into R
laureates <- jsonlite::fromJSON("laureates.json")

### Examine our new object
laureates <- dplyr::as_tibble(laureates) #convert to tibble
names(laureates)
laureates

### Prepare data for plotting graph
# Convert bornCountryCode to factor
laureates <- laureates %>% mutate(bornCountryCode = as.factor(bornCountryCode))
levels(laureates$bornCountryCode)

# Convert bornCountryCode (alpha-2) to country name
str(ISOcodes::ISO_3166_1)
laureates <- laureates %>% mutate(bornCountryCode =
                                    plyr::mapvalues(bornCountryCode,
                                                    ISO_3166_1$Alpha_2,
                                                    ISO_3166_1$Name))
# Plot graph
plot <- laureates %>%
  ggplot(aes(x = bornCountryCode)) +
  geom_bar() +
  NULL
plot

# Prettify plot
pretty_plot <- laureates %>%
  ggplot(aes(x = forcats::fct_infreq(bornCountryCode),
             fill = bornCountryCode)) +
  geom_bar() +
  scale_y_continuous(expand = c(0,0), limits = c(0,300)) +
  labs(x="Country of Birth", y="Number of Nobel Laureates",
       title = "Number of Nobel Laureates by Country of Birth",
       caption = "Source: http://api.nobelprize.org/v1/laureate.json") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 7),
        legend.position = "none") +
  NULL
pretty_plot

ggsave("laureate_plot.png", width = 8, height = 5)



