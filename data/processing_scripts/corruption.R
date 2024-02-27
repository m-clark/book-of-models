# taken straight off of Andrew Heiss' marginalia blog post.
# https://www.andrewheiss.com/blog/2022/05/20/marginalia/#what-are-marginal-effects

wdi_raw <- WDI::WDI(country = "all", 
               indicator = c(population = "SP.POP.TOTL",
                             gdp_percapita = "NY.GDP.PCAP.KD"), 
               start = 2000, end = 2020, extra = TRUE)

# Clean up the World Bank data
wdi_2020 <- wdi_raw |> 
  filter(region != "Aggregates") |> 
  filter(year == 2020) |> 
  mutate(log_gdp_percapita = log(gdp_percapita)) |> 
  select(-region, -status, -year, -country, -lastupdated, -lending)

  # Get data from V-Dem and clean it up
vdem_2020 <- vdemdata::vdem %>% 
  select(country_name, country_text_id, year, region = e_regionpol_6C,
         disclose_donations_ord = v2eldonate_ord, 
         public_sector_corruption = v2x_pubcorr,
         polyarchy = v2x_polyarchy, civil_liberties = v2x_civlib) %>% 
  filter(year == 2020) %>% 
  mutate(disclose_donations = disclose_donations_ord >= 3,
         disclose_donations = ifelse(is.na(disclose_donations), FALSE, disclose_donations)) %>% 
  # Scale these up so it's easier to talk about 1-unit changes
  mutate(across(c(public_sector_corruption, polyarchy, civil_liberties), ~ . * 100)) |> 
  mutate(region = factor(region, 
                         labels = c("Eastern Europe and Central Asia",
                                    "Latin America and the Caribbean",
                                    "Middle East and North Africa",
                                    "Sub-Saharan Africa",
                                    "Western Europe and North America",
                                    "Asia and Pacific")))

# Combine World Bank and V-Dem data into a single dataset
corruption <- vdem_2020 |>
  left_join(wdi_2020, by = c("country_text_id" = "iso3c")) |>
  drop_na(gdp_percapita)

glimpse(corruption)
table(corruption$region)

write_csv(corruption, "data/corruption.csv")


#####
# Electoral democracy index, or polyarchy (v2x_polyarchy in V-Dem): a continuous variable measured from 0–1 with higher values representing greater achievement of democratic ideals
# Civil liberties index (v2x_civlib in V-Dem): a continuous variable measured from 0–1 with higher values representing better respect for human rights and civil liberties
# Log GDP per capita (NY.GDP.PCAP.KD at the World Bank): GDP per capita in constant 2015 USD


###
