library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# read the file
cholera_data_2000_2010 = read_excel("RawData/Cholera_Cameroon_2000-2018.xlsx", sheet=1)

# clean the data
cholera_2000_10 = cholera_data_2000_2010 %>%
    select(
        "regions" = ProvinceRegion,
        "district" = District,
        "year" = Year,
        "week" = WeekNum,
        "case" = Cases,
        "death" = Deaths
        ) %>%
    mutate(
        regions = toupper(regions),
        district = toupper(district),
        regions = gsub("-", " ", regions),
        district = gsub("-", " ", district),
        case = as.numeric(case),
        year = as.character(year),
        population = NA,
        attackRate = NA,
        deathRate = NA
    ) %>%
    filter(
        !is.na(regions),
        !is.na(district),
        regions != "-",
        district != "-",
        week != "-"
    )

names(cholera_1118)
# read the other sheets
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
        year = as.character(year)
    ) %>%
    rbind(cholera_2000_10)
    
# clean district
sorted_district <- sort(unique(cholera$district))

cholera <- cholera %>%
    filter(
        # filter list of district that start with number
        !district %in% sorted_district[1:49]
    )


# Assuming your dataframe is named "combined_cholera"
write.csv(cholera,
          "processedData/cholera.csv",
          row.names = FALSE)

