library(tidyverse)
library(readxl)
logbook <- read_excel("logbook/Anaesthetics_Logbook.xlsx", col_names = TRUE)


## This selects out just the ETT from the other options (LMA/MASK/Sedation)
tubes <- logbook %>%
  filter(Airway == "GA_ETT_SV" | Airway =="GA_ETT_IPPV") %>%
  filter(`Airway-Code` =="P")

## This turns the strings of S and U into a factor
tubes$`If P - Successful (S) or Unsuccessful (U)` <- as.factor(tubes$`If P - Successful (S) or Unsuccessful (U)`)


## This will create our new data frame to look at success rate over time
## We filter the variables we care about
## Insert a count of the number of attempts 'attempt'
## Count a success as 1, and a failure as 0 'success'
## Counts the cumulative sum of successes ever 'success_count'
## Counts the percentage of success out of total attempts 'success_percentage'
tube_success <- tubes %>%
  select(Date, `Airway-Code`, `If P - Successful (S) or Unsuccessful (U)`, Notes) %>%
  mutate(attempt = seq.int(nrow(tubes))) %>%
  mutate(success = as.logical(as.numeric(`If P - Successful (S) or Unsuccessful (U)`)-1)) %>%
  mutate(success = as.numeric(!success)) %>%
  mutate(success_count = cumsum(success)) %>%
  mutate(success_percentage = (success_count/attempt)*100)

spinal <- logbook %>%
  filter(`Regional Anaesthetic` == "Spinal") %>%
  filter(`Regional-Anaesthetic-Code` == "P")

spinal$`Regional Success` <- as.factor(spinal$`Regional Success`)
spinal_success <- spinal %>%
  select(Date, `Regional Success`) %>%
  mutate(attempt = seq.int(nrow(spinal))) %>%
  mutate(success = as.logical(as.numeric(`Regional Success`)-1)) %>%
  mutate(success = as.numeric(!success)) %>%
  mutate(success_count = cumsum(success)) %>%
  mutate(success_percentage = (success_count/attempt)*100)
spinal_plot <- ggplot(spinal_success, aes(attempt, success_percentage, alpha = attempt, size = 5)) +
  geom_point(colour = "#0074D9") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Successful Spinal Anaesthetic", x = "Attempt Number", y = "Percentage of Spinals Placed Successfully") +
  ylim(0,100)

spinal_plot


## This will create the plot of successful ET Tube Placements

tube_plot <- ggplot(tube_success, aes(attempt, success_percentage, alpha = attempt, size = 5)) +
  geom_point(colour = "#0074D9") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Successful ET Tube Placements", x = "Attempt Number", y = "Percentage of Tubes Placed Successfully") 

tube_plot

