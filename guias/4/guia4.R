require(palmerpenguins)
require(tidyverse)
d <- penguins
# 1.1
filter(d, island == "Biscoe" & bill_length_mm >= 48)

# 1.2
filter(d, species == "Adelie" & sex == "male" & year != 2008)

# 1.3
select(d, -c(year, sex, body_mass_g))

# 1.4
penguins %>%
  filter(sex == "male" & flipper_length_mm > 200) %>%
  select(ends_with("mm"))

# 1.5
penguins %>%
  filter(island == "Dream") %>%
  select(species, starts_with("bill"))

# 1.8
penguins %>%
  rename_with(toupper, starts_with("bill"))  

# 1.9
penguins %>%
  rename_with(toupper, -starts_with("bill"))  

# 1.10
penguins %>%
  mutate(character = species, species = NULL, 
         body_mass_kg = body_mass_g / 1000, 
         island = tolower(island))

penguins <- drop_na(penguins)
# 1.11
penguins %>%
  filter(species == "Chinstrap") %>%
  group_by(island) %>%
  summarise(min_flipper_length_mm = min(flipper_length_mm),
            max_flipper_length_mm = max(flipper_length_mm))

# 1.12
penguins %>%
  group_by(species, year) %>%
  summarise(bill_depth_mean = mean(bill_depth_mm), bill_length_mean = mean(bill_length_mm))

# 1.13
penguins %>%
  group_by(species) %>%
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>%
  select(species, bill_ratio) %>%
  summarise(bill_ratio_mean = mean(bill_ratio))

# 1.14
penguins %>%
  rename(masa_corporal_g = body_mass_g)

# 1.15
penguins %>%
  group_by(species) %>%
  summarise(body_mass_median = median(body_mass_g))

# 1.16
penguins %>%
  filter(island != "Biscoe") %>%
  select()












