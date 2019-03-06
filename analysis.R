# income analysis
# 2019-03-05

# load ---------

# load packages
library(tidyverse)
library(readxl)
library(scales)
library(sf)
library(USAboundaries)
library(kydistgeo)

# load data 
dist_loc <- read_excel("data/PROFILE.xlsx")

teacher <- read_excel("data/Average Certified Salaries (1989 -2019).xlsx",
                      skip = 3, n_max = 177)

acs <- read_csv("data/ACS_17_5YR_S1901/ACS_17_5YR_S1901_with_ann.csv")

fte <- read_excel("data/Total Certified Staff (FTE) (1996-2019).xlsx",
                  skip = 4, n_max = 177)

# clean --------

# clean district location df
districts <- dist_loc %>%
  select(SCH_CD, CNTYNAME, DIST_NAME, LONGITUDE, LATITUDE,
         MEMBERSHIP) %>%
  rename(sch_id = SCH_CD, county = CNTYNAME, dist_name = DIST_NAME,
         enroll = MEMBERSHIP,
         long = LONGITUDE, lat = LATITUDE) %>%
  mutate(county = str_to_title(county), 
         county = str_replace_all(county, "Mccracken", "McCracken"),
         county = str_replace_all(county, "Mclean", "McLean"),
         county = str_replace_all(county, "Mccreary", "McCreary"),
         enroll = as.numeric(str_replace_all(enroll, ",", ""))) %>%
  # only include districts
  filter(nchar(sch_id) ==3) %>%
  # filter out state total
  filter(sch_id != "999")

# clean acs data 
acs_clean <- acs %>%
  select(`GEO.display-label`, HC01_EST_VC13, HC01_MOE_VC13) %>%
  rename(county = `GEO.display-label`,
         mhi = HC01_EST_VC13,
         moe = HC01_MOE_VC13) %>%
  mutate(county = str_replace_all(county, " County, Kentucky", ""))

teacher_clean <- teacher %>%
  rename(sch_id = `Dist No`,
         salary19 = `2018-19`) %>%
  select(sch_id, salary19)

# clean fte data
fte_clean <- fte %>%
  select(DISTNO, `2018-19`) %>%
  rename(sch_id = DISTNO,
         fte19 = `2018-19`)

# join data -----------

ky_comp <- districts %>%
  left_join(teacher_clean, by = "sch_id") %>%
  left_join(acs_clean) %>%
  left_join(fte_clean) %>%
  mutate(diff = salary19 - mhi, 
         ratio = salary19/mhi, 
         ratio_bin = cut(ratio, breaks = c(0,.9,1.1,1.5,2,3),
                         labels = c("Less than 90% of MHI",
                                    "Similar to MHI (+/- 10%)",
                                    "Slightly more than MHI (10-50%)",
                                    "Significantly more than MHI (50-200%)",
                                    "More than double MHI")))

# plot -----------

ggplot(ky_comp, aes(x = mhi, y = salary19, size = enroll)) +
  geom_point(alpha= .6)+
  geom_abline() +
  scale_size_area(max_size = 8) +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar, limits = c(20000,80000))

counties_spec <- us_counties(resolution = "low", states="Kentucky")

county_data <- counties_spec %>%
  left_join(acs_clean, by = c("name" = "county"))

# plot county MHI w/ points of district avg salary
ggplot() + 
  scale_fill_gradientn(colors = c("grey60","grey70",
                                  "royalblue3", "royalblue4",
                                  "royalblue4"),
                       limits = c(10000,100000),
                       labels = dollar)+
  scale_color_gradientn(colors = c("grey60", "grey70",
                                   "royalblue3", "royalblue4",
                                   "royalblue4"),
                        limits = c(10000,100000), 
                        guide = FALSE) +
  geom_sf(data=county_data, aes(fill = mhi),
          show.legend = T, color="gray50", lwd=0.4) +
  scale_size_area(guide = FALSE, max_size = 12) +
  geom_point(data = ky_comp %>% filter(!is.na(salary19)),
             aes(x = as.numeric(long), y = as.numeric(lat), 
                                size = fte19, color = salary19))+
  coord_sf(datum = NA)+
  theme_void() +
  labs(fill = "Teacher Salary/County MHI", 
       title = "School District Average Certified Salary and County Median Household Income",
       caption = "County shading reflects Median Household Income; points represent average certified salary in school districts and point area is scaled to represent 2018-19 certified FTE.") +
  theme(legend.position = c(0.15,.7),
        plot.title = element_text(hjust = 0.11),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggsave("figs/salary_mhi_map.png", height = 6, width = 11, units = "in")

# district map, shaded into bins
ggplot(ky_dist_geo %>%
         left_join(ky_comp, by = "sch_id")) +
  geom_sf(color = "grey80", fill = "grey90")+
  geom_sf( aes(fill = ratio_bin), alpha = .7) +
  coord_sf(datum = NA)+
  scale_fill_manual(values = c("lightcoral", "grey66",
                                "seagreen3", "springgreen4",
                                "gold3")) +
  theme_void() +
  labs(fill = "District Avgerage Salary\nas Ratio of County MHI") +
  theme(legend.position = c(0.12,.8),
        plot.title = element_text(hjust = 0.11),
        plot.caption = element_text(hjust = 0.9, size = 8))

ggsave("figs/ratio_map.png", height = 6, width = 11, units = "in")
