library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# read the file
cholera_data_2011 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=2)
cholera_data_2012 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=3)
cholera_data_2013 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=4)
cholera_data_2014 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=5)
cholera_data_2015 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=6)
cholera_data_2016 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=7)
cholera_data_2017 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=8)
cholera_data_2018 =read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=9)


# clean the data
Regions = c("ADAMAOUA",
            "CENTRE",
            "EST",
            "EXTREME NORD",
            "LITTORAL",
            "NORD",
            "NORD OUEST",
            "OUEST",
            "SUD",
            "SUD OUEST")


cholera_2011 = cholera_data_2011 %>%
    rename(
        "regions" = CHOLERA,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2012 = cholera_data_2012 %>%
    rename(
        "regions" = CHOLERA,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)

cholera_2013 = cholera_data_2013 %>%
    rename(
        "regions" = Choléra,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2014 = cholera_data_2014 %>%
    rename(
        "regions" = Choléra,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2015 = cholera_data_2015 %>%
    rename(
        "regions" = Choléra,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2016 = cholera_data_2016 %>%
    rename(
        "regions" = Choléra,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2017 = cholera_data_2017 %>%
    rename(
        "regions" = Cholera,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)


cholera_2018 = cholera_data_2018 %>%
    rename(
        "regions" = Choléra,
        "district" = ...2,
        "population" = ...3
    ) %>%
    filter(
        regions %in% Regions
    ) %>%
    select(1:211)






# reshape data
# Generate a vector of new column names based on the pattern
new_column_names <- character(0)  # Initialize an empty character vector

# Specify the number of weeks you have (adjust the number accordingly)
num_weeks <- 52  # or however many weeks you have

for (i in 1:num_weeks) {
    # Generate the column names for the current week
    week_str <- sprintf("week%02d", i)
    week_columns <- c(paste(week_str, "_case", sep = ""), 
                      paste(week_str, "_death", sep = ""), 
                      paste(week_str, "_attackRate", sep = ""), 
                      paste(week_str, "_deathRate", sep = ""))
    
    # Append the generated column names to the new_column_names vector
    new_column_names <- c(new_column_names, week_columns)
}

# Rename the columns in the data frame
colnames(cholera_2011)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2012)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2013)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2014)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2015)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2016)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2017)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names
colnames(cholera_2018)[4:(4 + (num_weeks * 4) - 1)] <- new_column_names

# add year
cholera_2011$year = "2011"
cholera_2012$year = "2012"
cholera_2013$year = "2013"
cholera_2014$year = "2014"
cholera_2015$year = "2015"
cholera_2016$year = "2016"
cholera_2017$year = "2017"
cholera_2018$year = "2018"




# merge all the data
cholera = rbind(
    cholera_2011,
    cholera_2012,
    cholera_2013,
    cholera_2014,
    cholera_2015,
    cholera_2016,
    cholera_2017,
    cholera_2018
) %>%
    pivot_longer(
        cols = starts_with("week"),  # Select columns starting with "week"
        names_to = "week",
        names_prefix = "week",
        values_to = "value"
    ) %>%
    separate(week, into = c("week", "measure"), sep = "_") %>%
    pivot_wider(
        names_from = measure,
        values_from = value
    ) %>%
    mutate(
        case = as.numeric(case),
        death = as.numeric(death),
        attackRate = as.numeric(attackRate),
        deathRate = as.numeric(deathRate),
        year = factor(year)
    )

sorted_district <- sort(unique(cholera$district))

cholera <- cholera %>%
    filter(
        # filter list of district that start with number
        !district %in% sorted_district[1:48]
    )



# Case and Death for each year and week
data1 = cholera %>%
    group_by(week, year) %>%
    summarize(
        Total_cases = sum(case, na.rm = T),
        Total_death = sum(death, na.rm = T),
        Average_deathRate = mean(deathRate, na.rm = T),
        Average_attackRate = mean(attackRate, na.rm = T)
        )

g = ggplot(data = data1, aes(x=week, y= Total_cases))+
    geom_col()+
    theme_bw()+
    facet_wrap(. ~ year, ncol=1)+
    labs(
        title = "Total case in each week"
    )

ggsave("Figures/box_col_week_totlaCase.jpg",
       g, 
       height=14,width=8,scale=1)


g = ggplot(data = data1, aes(x=week, y= Total_death))+
    geom_col()+
    theme_bw()+
    facet_wrap(. ~ year, ncol=1)+
    labs(
        title = "Total death in each week"
    )

ggsave("Figures/box_col_week_totalDeath.jpg",
       g, 
       height=14,width=8,scale=1)

g = ggplot(data = data1, aes(x=week, y= Average_deathRate))+
    geom_col()+
    theme_bw()+
    facet_wrap(. ~ year, ncol=1)+
    labs(
        title = "Average death rate in each week"
    )

ggsave("Figures/box_col_week_Average_deathRate.jpg",
       g, 
       height=14,width=8,scale=1)

g = ggplot(data = data1, aes(x=week, y= Average_attackRate))+
    geom_col()+
    theme_bw()+
    facet_wrap(. ~ year, ncol=1)+
    labs(
        title = "Average attack rate in each week"
    )

ggsave("Figures/box_col_week_Average_attackRate.jpg",
       g, 
       height=14,width=8,scale=1)



# Calculate the average case count by region
average_cases_by_region <- cholera %>%
    ungroup()%>%
    group_by(regions) %>%
    summarise(avg_cases = mean(case, na.rm = TRUE),
              avg_death = mean(death, na.rm = TRUE),
              avg_attackRate = mean(attackRate, na.rm = TRUE),
              avg_deathRate = mean(deathRate, na.rm = TRUE))
    mutate(
        NAME_1 = str_to_title(average_cases_by_region$regions)
        )

names(cholera)

average_cases_by_district <- cholera %>%
    group_by(district) %>%
    summarise(avg_cases = mean(case, na.rm = TRUE),
              avg_death = mean(death, na.rm = TRUE),
              avg_attackRate = mean(attackRate, na.rm = TRUE),
              avg_deathRate = mean(deathRate, na.rm = TRUE)) %>%
    filter(
        !is.na(district)
    )



library(raster)
library(ggplot2)
library(stringr)

Cameroon <- getData("GADM", country="Cameroon", level=0)
Cameroon1 <- getData("GADM", country="Cameroon", level=1)
Cameroon2 <- getData("GADM", country="Cameroon", level=2)

# Assuming you've already loaded the "Cameroon2" dataset using getData
summary(Cameroon1M)
# Create a plot using ggplot
Cameroon1M <- merge(Cameroon1,
                   average_cases_by_region,
                   by.x = "NAME_1",
                   by.y = "NAME_1",
                   all.x = TRUE)

cameroon_df <- fortify(Cameroon1M)
#########################################



# install.packages("sf")
library(sf)
# install.packages("dplyr")
library(dplyr)
#install.packages("giscoR")
library(giscoR)

year_ref <- 2016

nuts2_IT <- gisco_get_nuts(
    year = year_ref,
    resolution = 20, 
    nuts_level = 2,
    country = "Cameroon")
    dplyr::select(NUTS_ID, NAME_LATN)

plot(st_geometry(nuts2_IT)) 

