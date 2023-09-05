library(dplyr)
library(wbstats)
library(readxl)
library(tidyr)
library(countrycode)

countries <- wb_countries()

# Access shares
access.electricity <- wb_data(indicator = c("EG.ELC.ACCS.ZS"))
access.electricity.world <- wb_data(indicator = c("EG.ELC.ACCS.ZS"), country = "WLD")
access.electricity.regions <- wb_data(indicator = c("EG.ELC.ACCS.ZS"), country = "regions_only")
access.all <-rbind(access.electricity, access.electricity.regions, access.electricity.world)
access.all.ok <- select(access.all, -iso2c, -country) %>%
  #pivot_longer(cols = 3:5, names_to = "indicator", values_to = "value") %>%
  rename(value = EG.ELC.ACCS.ZS) %>%
  filter(!is.na(value), date > 1999) %>%
  mutate(value = round(value, 1)) %>%
  #filter(date %% 2 == 0) %>%
  select(iso3c, date, value)
write.csv(access.all.ok, "../output/accessperc.csv", row.names = FALSE)

access.rururb <- wb_data(indicator = c('EG.ELC.ACCS.RU.ZS', "EG.ELC.ACCS.UR.ZS"), country = "regions_only")
access.rururb.ok <- select(access.rururb, iso3c, date, EG.ELC.ACCS.RU.ZS, EG.ELC.ACCS.UR.ZS) %>%
  filter(date > 1999 & date < 2021)
pop.rururb <- wb_data(indicator = c('SP.RUR.TOTL', 'SP.URB.TOTL'), country = "regions_only")
pop.rururb.ok <- select(pop.rururb, iso3c, date, SP.RUR.TOTL, SP.URB.TOTL) %>%
  filter(date > 1999 & date < 2021)
access.pop <- left_join(access.rururb.ok, pop.rururb.ok, by = c("iso3c", "date")) %>%
  mutate(pop_without_urb = round(SP.URB.TOTL * (100 - EG.ELC.ACCS.UR.ZS)/100)) %>%
  mutate(pop_without_rur = round(SP.RUR.TOTL * (100 - EG.ELC.ACCS.RU.ZS)/100)) %>%
  select(iso3c, date, pop_without_urb, pop_without_rur)
## Group Europe and central asia and North America together
access.pop.def <- mutate(access.pop, iso3c = if_else(iso3c %in% c("NAC", "ECS"), "RESTOFWORLD", iso3c)) %>%
  group_by(iso3c, date) %>%
  summarise(pop_without_urb = sum(pop_without_urb), pop_without_rur = sum(pop_without_rur))
write.csv(access.pop.def, "../output/accessabsolute.csv", row.names = FALSE)

# Tiers
tiers <- read_excel("../input/Figure3.xlsx")
tiers <- select(tiers, -Num, -Country, -Year)
colnames(tiers) <- c("iso3c", "rururb", "tier0", "tier1", "tier2", "tier3", "tier4", "tier5")
tiers.long <- pivot_longer(tiers, cols = 3:8, names_to = "tier", values_to = "percent") %>%
  group_by(iso3c, rururb) %>%
  mutate(cumperc = cumsum(percent) - percent) %>%
  filter(rururb != "Nationwide") %>%
  mutate(rururb = recode(rururb, Urban = "urban", Rural = "rural"))
pop.shares <- wb_data(indicator = c('SP.RUR.TOTL.ZS', 'SP.URB.TOTL.IN.ZS'))
pop.shares.ok <- filter(pop.shares, date == 2020) %>%
  select(iso3c, poprural = SP.RUR.TOTL.ZS, popurban = SP.URB.TOTL.IN.ZS)
tiers.ok <- left_join(tiers.long, pop.shares.ok, by = "iso3c")
write.csv(tiers.ok, "../output/tiers.csv", row.names = FALSE)

# Figure 4. Access vs emissions
emis.raw <- read.csv('../input/historical_emissions_ciat.csv', na.strings = "N/A")
access.electricity.regions <- wb_data(indicator = c("EG.ELC.ACCS.ZS"), country = "regions_only")
access.electricity.world <- wb_data(indicator = c("EG.ELC.ACCS.ZS"), country = "world")

access.electricity.ok <- rbind(access.electricity.regions, access.electricity.world) %>%
  select(iso3c, access = EG.ELC.ACCS.ZS, year = date) %>%
  filter(year > 1999, year < 2021)

pop.regions <- wb_data(indicator = c("SP.POP.TOTL"), country = "regions_only")
pop.world <- wb_data(indicator = c("SP.POP.TOTL"), country = "world")
pop.regions.ok <- rbind(pop.regions, pop.world) %>%
  select(iso3c, pop = SP.POP.TOTL, year = date)

country.regions <- select(countries, iso3c, region_iso3c)

emis <- select(emis.raw, -Data.source, -Sector, -Gas, -Unit) %>%
  pivot_longer(cols = 2:31, names_to = "year", values_to = "emissions") %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  mutate(iso3c = if_else(Country == "World", "WLD", iso3c)) %>%
  left_join(country.regions, by = "iso3c") %>%
  mutate(region_iso3c = if_else(Country == "World", "WLD", region_iso3c)) %>%
  filter(!is.na(region_iso3c)) %>% 
  filter(!is.na(emissions))

emis.regions <- group_by(emis, region_iso3c, year) %>%
  summarise(emissions = sum(emissions)) %>%
  filter(year > 1999) %>%
  rename(iso3c = region_iso3c)

access.pop.emis <- left_join(emis.regions, pop.regions.ok,  access.electricity.ok, by = c("iso3c", "year")) %>%
  left_join(access.electricity.ok, by = c("iso3c", "year")) %>%
  mutate(emissions_percap = emissions*1000000/pop)

access.pop.emis.ok <- select(access.pop.emis, iso3c, year, access, emissions_percap) %>%
  mutate(access = round(access, 2)) %>%
  mutate(emissions_percap = round(emissions_percap, 2))
write.csv(access.pop.emis.ok, "../output/access_emissions.csv", row.names = FALSE)


# Figure 5: Sources of electricity
offgrid <- read.csv("../input/offgrid.csv", na.strings = "..")
ongrid <- read.csv("../input/ongrid.csv", na.strings = "..")
sources.all <- rbind(offgrid, ongrid)
sources.all.ok <- mutate(sources.all, Country.area = if_else(Country.area == "T<fc>rkiye", "Turkey", Country.area)) %>%
  mutate(iso3c = countrycode(Country.area, origin="country.name", destination="iso3c")) %>%
  filter(!is.na(Electricity.generation..GWh.)) %>%
  group_by(iso3c, Technology, Year) %>%
  summarise(generation = sum(Electricity.generation..GWh.)) %>%
  mutate(source = recode(Technology,
                         `Coal and peat` = "coal",
                         `Natural gas` = "gas",
                         `Fossil fuels n.e.s.` = "oil",
                         `Oil` = "oil",
                         `Other non-renewable energy` = "oil",
                         `Nuclear` = "nuclear",
                         `Geothermal energy` = "otherrenewable",
                         `Renewable municipal waste` = "otherrenewable",
                         `Marine energy` = "otherrenewable",
                         `Renewable hydropower` = "hydro",
                         `Pumped storage` = "hydro",
                         `Mixed Hydro Plants` = "hydro",
                         `Solid biofuels` = "bioenergy",
                         `Biogas` = "bioenergy",
                         `Liquid biofuels` = "bioenergy",
                         `Solar photovoltaic` = "solar",
                         `Solar thermal energy` = "solar",
                         `Onshore wind energy` = "wind",
                         `Offshore wind energy` = "wind"
  )) %>%
  group_by(iso3c, Year, source) %>%
  summarise(generation = sum(generation)) %>%
  rename(year = Year)

sources.world <- group_by(sources.all.ok, year, source) %>%
  summarise(generation = sum(generation)) %>%
  mutate(iso3c = "WLD") %>%
  relocate(iso3c, .before = year)

sources.all.def <- rbind(sources.all.ok, sources.world) %>%
  filter(!is.na(iso3c)) %>%
  pivot_wider(names_from = source, values_from = generation)
sources.all.def[is.na(sources.all.def)] <- 0

write.csv(sources.all.ok, "../output/electricitysources.csv", row.names = FALSE)
