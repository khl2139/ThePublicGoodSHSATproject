library(tidyverse)
library(gt)
library(DescTools)
library(ggmap)
library(ggthemes)
library(plotly)

#reading in data
MS_data <- readxl::read_xlsx("point_data.xlsx")

#making categorical variables factors
MS_data <- MS_data %>%
  mutate(G_T = as.factor(G_T),
         HS_Program = as.factor(HS_Program),
         Citywide = as.factor(Citywide))

# schools with the top percent of the pool tested
perc_tested <- 
  MS_data %>%
  arrange(desc(perc_tested)) %>%
  head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt() %>%
#  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with the Highest Percentage of\nStudents Taking the SHSAT"
  ) %>%
  data_color(columns = perc_tested,
             colors = scales::col_numeric(
               palette = c(
                 "red", "orange", "green"
               ), domain = c(0, 100))
             ) %>%
  data_color(columns = perc_tested_offered,
             colors = scales::col_numeric(
               palette = c(
                 "red", "orange", "green"
               ), domain = c(0, 100)
             )) %>%
  data_color(columns = G_T,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  data_color(columns = HS_Program,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  data_color(columns = Citywide,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(perc_tested, "TopPercTested.png", expand = 10,
#       path = "Tables")


# top schools of percent of testers with offers (in color)
perc_offered_TOP_color <-
  MS_data %>%
  arrange(desc(perc_tested_offered)) %>%
  head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt() %>%
  #  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with the Highest Percentage of\nStudents Taking the SHSAT"
  ) %>%
  data_color(columns = perc_tested,
             colors = scales::col_numeric(
               palette = c(
                 "red", "orange", "green"
               ), domain = c(0, 100))
  ) %>%
  data_color(columns = perc_tested_offered,
             colors = scales::col_numeric(
               palette = c(
                 "red", "orange", "green"
               ), domain = c(0, 100)
             )) %>%
  data_color(columns = G_T,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  data_color(columns = HS_Program,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  data_color(columns = Citywide,
             colors = scales::col_factor(
               palette = c(SetAlpha("red", .7), SetAlpha("green", .7)),
               domain = NULL, alpha = TRUE)) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
gtsave(perc_offered_TOP_color, "TopPercOfferedColor.png", expand = 10,
       path = "Tables")

MS_data %>%
  arrange(desc(perc_tested_offered)) %>%
  writexl::write_xlsx("top_10_offered.xlsx")

MS_data %>%
  arrange(desc(perc_tested)) %>%
  writexl::write_xlsx("top_10_tested.xlsx")

# top schools of percent of testers with offers
perc_offered_TOP <- 
  MS_data %>%
  arrange(desc(perc_tested_offered)) %>%
  head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with the Highest Percentage of\nStudents Offered Spot in a Specialized School"
  ) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(perc_offered_TOP, "TopPercOffered.png", expand = 10,
#       path = "Desktop/TPG/Middle School/Tables/")

# lowest schools for percent of pool tested (does not include NAs)

perc_tested_LOW <-
  MS_data %>%
  arrange(perc_tested) %>%
  head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with the Lowest Percentage of\nStudents Taking the SHSAT"
  ) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(perc_tested_LOW, "LowPercTested.png", expand = 10,
#       path = "Desktop/TPG/Middle School/Tables")

# low offered

perc_offered_LOW <- 
  MS_data %>%
  arrange(perc_tested_offered) %>%
  head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with the Lowest Percentage of\nStudents Offered Spot in a Specialized School"
  ) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(perc_offered_LOW, "LowPercOffered.png", expand = 10,
#       path = "Desktop/TPG/Middle School/Tables/")

# all schools with 5 or fewer students tested

tested_NA <- 
  MS_data %>%
  filter(is.na(perc_tested)) %>%
 # head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with Five or Fewer Students Taking the SHSAT"
  ) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(tested_NA, "Tested_NA.png", expand = 10,
#       path = "Desktop/TPG/Middle School/Tables")

# all schools with 5 or fewer students with offers

offered_NA <- 
  MS_data %>%
  filter(is.na(perc_tested_offered)) %>%
  # head(10) %>%
  select(Dist, Borough, MS, total_students, testers_count, perc_tested, 
         total_offers, perc_tested_offered, G_T, HS_Program, Citywide) %>%
  gt(groupname_col = "Borough") %>%
  tab_header(
    title = "Middle Schools with Five or Fewer Students Offered Spot in a Specialized School"
  ) %>%
  cols_label(
    Dist = "District",
    MS = "Middle School",
    total_students = "Total students in pool",
    testers_count = "# of students tested",
    perc_tested = "Percent of students tested",
    total_offers = "Total offers",
    perc_tested_offered = "Percent of test takers with offers",
    G_T = "Gifted and Talented",
    HS_Program = "High School program",
  )
#gtsave(offered_NA, "Offered_NA.png", expand = 10,
#       path = "Desktop/TPG/Middle School/Tables")


#creating a map visualization

map_top <- MS_data %>%
  arrange(desc(perc_tested)) %>%
  head(10) %>%
  mutate(stat = "Top 10 in percent of students tested")

map_top_temp <- MS_data %>%
  arrange(desc(perc_tested_offered)) %>%
  head(10) %>%
  mutate(stat = "Top 10 in percent of test takers with offers")

map_top_temp2 <- MS_data %>%
  filter(is.na(perc_tested)) %>%
  mutate(stat = "5 or fewer students tested")

map_top_temp3 <- MS_data %>%
  filter(is.na(perc_tested_offered)) %>%
  mutate(stat = "5 or fewer students with offers")

map_top_all <- bind_rows(map_top, map_top_temp, map_top_temp2, map_top_temp3)
map_top <- bind_rows(map_top, map_top_temp)

map_top_geos <- geocode(map_top$MS)

map_top <- bind_cols(map_top, map_top_geos)

writexl::write_xlsx(map_top, "map_top_all.xlsx")

temp <- readxl::read_xlsx("map_top_all.xlsx")
#write_csv(temp, "map_top_all.csv")


temp <- temp %>% pivot_wider(id_cols = c(1:16, 18:19), names_from = stat, values_from = stat) %>%
  mutate(Tested = `Top 10 in percent of students tested`,
         Offered = `Top 10 in percent of test takers with offers`)

temp <- temp %>% mutate(stat =  case_when(
  is.na(Tested) ~ "Top 10: Percent of students tested",
  is.na(Offered) ~ "Top 10: Percent of test takers with offers",
  TRUE ~ "Top 10: Both"))

NYC_map <- get_map("New York City",  zoom = 11, source = "stamen",
                   maptype = "toner-background")

NYC_gg <- ggmap(NYC_map) + theme_map()

NYC_gg + geom_point(data = temp, aes(lon, lat, color = stat)) +
  ggtitle("Top 10 Schools in Percent of SHSAT Test Takers\nand Percent of Test Takers Offered Spots in Selective HS") +
  labs(color = "Legend") +
  theme(text = element_text(family = "serif", size = 12),
        plot.title = element_text(hjust = 0.5))

#ggsave("top 10 map.png")

# Getting percentiles that were used to color the data in the google drive

MS_data %>% arrange(desc(perc_tested_offered))

quantile(MS_data$perc_tested_offered, na.rm = TRUE)
quantile(MS_data$perc_tested, na.rm = TRUE)


MS_data %>% dplyr::group_by(fof_testers) %>% dplyr::count(fof_testers)
