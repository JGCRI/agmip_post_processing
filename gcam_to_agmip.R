
# USER-CONFIGURABLE ASSUMPTIONS AND FILE PATHS
BUILD_MAIN_TEMPLATE <- FALSE
BUILD_DIET_TEMPLATE <- FALSE
BUILD_MITIGDECOMP_TEMPLATE <- TRUE

# This script assumes that the main runs and mitig-decomp runs are on PIC and the diet runs are in a local database
FILEPATH_TO_DIET_DB <- "/Users/d3p747/Documents/ObjECTS/stash/master/output/database_basexdb/"

# Set the scenario names for the mitigation, no-mitigation, and diet scenarios
NO_MITIG_SCENARIOS <- c("SSP1_NoMt_NoCC", "SSP2_NoMt_NoCC", "SSP3_NoMt_NoCC", "SSP1_NoMt_CC26", "SSP2_NoMt_CC26",
                        "SSP3_NoMt_CC26", "SSP1_NoMt_CC85", "SSP2_NoMt_CC85", "SSP3_NoMt_CC85")
MITIG_SCENARIOS <- c("SSP1_2p6_NoCC", "SSP1_2p6_CC26", "SSP2_2p6_NoCC", "SSP2_2p6_CC26", "SSP3_2p6_NoCC",
                     "SSP3_2p6_CC26")
MAIN_SCENARIOS <- c(NO_MITIG_SCENARIOS, MITIG_SCENARIOS)
DIET_SCENARIOS <- c("SSP2_NoMt_NoCC", "SSP2_NoMt_NoCC_Diet_FlexA_WLD", "SSP2_NoMt_NoCC_Diet_FlexA_CHN",
                    "SSP2_NoMt_NoCC_Diet_FlexA_EUR", "SSP2_NoMt_NoCC_Diet_FlexA_LAM", "SSP2_NoMt_NoCC_Diet_FlexA_USA")
MITIG_DECOMP_SCENARIOS <- c("SSP1_Baseline", "ModelB_SSP1_Mit_Full", "ModelB_SSP1_Mit_wNonCO2", "ModelB_SSP1_Mit_wBio",
                            "ModelB_SSP1_Mit_wAff", "SSP2_Baseline", "ModelB_SSP2_Mit_Full", "ModelB_SSP2_Mit_wNonCO2",
                            "ModelB_SSP2_Mit_wBio", "ModelB_SSP2_Mit_wAff", "SSP3_Baseline", "ModelB_SSP3_Mit_Full",
                            "ModelB_SSP3_Mit_wNonCO2", "ModelB_SSP3_Mit_wBio", "ModelB_SSP3_Mit_wAff")

# Load necessary libraries to run the script
 #to get the rgcam package:
 #in the R console, key in:
 #install.packages('devtools')
 #devtools::install_github('JGCRI/rgcam')

library(rgcam)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Read in the exogenous mapping files, and pre-process
commodity_map <- read.csv("commodity_map.csv", stringsAsFactors = FALSE, na.strings = "") %>%
  gather(drop, Item, -GCAM_commodity, -traded_commodity, -regional_commodity) %>%
  drop_na(Item) %>%
  select(-drop) %>%
  gather(drop, GCAM_commodity, -Item) %>%
  select(-drop)
demand_map <- read.csv("demand_map.csv", stringsAsFactors = FALSE, na.strings = "") %>%
  gather(drop, Variable, -sector) %>%
  drop_na(Variable) %>%
  select(-drop)
land_map <- read.csv("land_map.csv", stringsAsFactors = FALSE, na.strings = "") %>%
  gather(drop, Item, -GCAM_LUT) %>%
  drop_na() %>%
  select(-drop)
region_map <- read.csv("region_map.csv", stringsAsFactors = FALSE, na.strings = "") %>%
  gather(drop, Region, -region) %>%
  drop_na() %>%
  select(-drop)
YECC_scenario_map <- read.csv("YECC_scenario_map.csv", stringsAsFactors = FALSE, na.strings = "")
food_waste_regions <- read.csv("food_waste_regions.csv", stringsAsFactors = FALSE)
food_waste_commodities <- read.csv("food_waste_commodities.csv", stringsAsFactors = FALSE)
food_waste_fractions <- read.csv("food_waste_fractions.csv", stringsAsFactors = FALSE) %>%
  gather(food_waste_region, value, -commodity) %>%
  left_join(food_waste_regions, by = "food_waste_region") %>%
  left_join(food_waste_commodities, by = "commodity") %>%
  select(region, GCAM_commodity, waste_frac = value)

# Indicate which years to report for the template
agmip_years <- seq(2010, 2050, 10)

# Useful unit conversions
conv_thouskm2_kha <- 100 # convert from thousand km2 (GCAM land area unit) to thousand hectares
conv_bio_EJ_to_kt <- 57100 # convert biomass volumes reported in EJ to kt, assuming 17.5 GJ per tonne
conv_bio_EJ_to_Mt <- 57.1 # convert biomass reported in EJ to Mt (similar to other GCAM crops) assuming 17.5 GJ/t
conv_Mt_to_kt <-1000 # convert Mt (model unit) to kt (reporting unit)
conv_kg_t <- 1000
conv_1975_2010_USD <- 3.228
conv_1990_2010_USD <- 1.516
conv_kgm2_tha <- 10
conv_Pcal_to_Gcal <- 1e6
conv_year_to_days <- 365.25
conv_C_to_CO2 <- 44/12
conv_CH4_to_CO2e <- 28
conv_N2O_to_CO2e <- 298

# Column names of the output template
NAMES_TEMPLATE <- c("Model", "Scenario", "Region", "Item", "Variable",
                    "Year", "Unit", "Value")

# Indicate which items to report for each variable (loosely based on the "Variables" sheet of the excel workbook:
# Reporting_template_AGMIP_2019_v0.xlsx)
LAND_ITEMS <- c("CRP", "GRS", "ONV", "FOR", "NLD", "AGR", "ECP")
HARVESTED_AREA_ITEMS <- c("RIC", "WHT", "CGR", "OSD", "SGC", "VFN", "VFN|VEG", "VFN|FRU", "VFN|NUT",
                          "PFB", "ECP", "OCR", "CRP", "AGR")
PROD_ITEMS <- c("RIC", "WHT", "CGR", "OSD", "SGC", "VFN", "VFN|VEG", "VFN|FRU", "VFN|NUT",
                "PFB", "ECP", "OCR", "CRP", "RUM", "NRM", "NRM|PRK", "NRM|PTM", "DRY", "LSP", "FSH", "AGR", "GRS")
YEXO_ITEMS_EXCLUDE <- "ECP"
PRICE_ITEMS <- PROD_ITEMS[PROD_ITEMS != "GRS"]
TRADE_ITEMS <- PRICE_ITEMS
FRTN_ITEMS <- HARVESTED_AREA_ITEMS
WATR_ITEMS <- HARVESTED_AREA_ITEMS

# useful functions
repeat_add_columns <- function(x, y) {
  x %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD)
}

# DATA QUERYING
# All data queried  will be placed into a single "project" file, whether queried remotely or locally, and whether for
# the main study or the diet study
agmip_data.proj <- loadProject("agmip_data.proj")

# QUERYING FROM PIC - MAIN AGMIP SCENARIOS
# First, open up the appropriate connection from constance, making note of the server in the session (constance01 or
# constance03). To open the connection, run the following line from /people/d3p747/.
# ./basex-server-helper.sh ./agmip-2020-dbs/

if(BUILD_MAIN_TEMPLATE){
  conn <- remoteDBConn("database_batchnoMitigxdb", "test", "test", "constance01")
  for(NoMt_scenario in NO_MITIG_SCENARIOS){
    agmip_data.proj <- addScenario(conn, agmip_data.proj, NoMt_scenario, "BatchQueries_agmip.xml", clobber = FALSE)
  }

  conn <- remoteDBConn("database_batchMitigxdb", "test", "test", "constance01")
  for(Mt_scenario in MITIG_SCENARIOS){
    agmip_data.proj <- addScenario(conn, agmip_data.proj, Mt_scenario, "BatchQueries_agmip.xml", clobber = FALSE)
  }
}

# FROM A LOCAL DATABASE - DIET SCENARIOS
if(BUILD_DIET_TEMPLATE){
#  for(diet_scenario in DIET_SCENARIOS){
#    agmip_data.proj <- addScenario(FILEPATH_TO_DIET_DB, agmip_data.proj,
#                                   diet_scenario, "BatchQueries_agmip.xml", clobber = FALSE)
#  }
  conn <- remoteDBConn("database_agmip_diet", "test", "test", "constance01")
  for(diet_scenario in DIET_SCENARIOS){
    agmip_data.proj <- addScenario(conn, agmip_data.proj,
                                   diet_scenario, "BatchQueries_agmip.xml", clobber = FALSE)
  }
}

if(BUILD_MITIGDECOMP_TEMPLATE){
  # These scenarios are written to separate databases
  for(MtD_scenario in MITIG_DECOMP_SCENARIOS){
    conn <- remoteDBConn(paste0("database_MitigDecomp_", MtD_scenario), "test", "test", "constance01")
    agmip_data.proj <- addScenario(conn, agmip_data.proj, c(), "BatchQueries_agmip.xml", clobber = FALSE)
  }
}

# DATA PROCESSING
# Subset each table to only the relevant years, and re-name some columns for consistent capitalization with the final template
for(i in 1:length(agmip_data.proj)){
  agmip_data.proj[[i]] <- lapply(agmip_data.proj[[i]], subset, year %in% agmip_years)
  agmip_data.proj[[i]] <- lapply(agmip_data.proj[[i]], rename, Year = year)
  agmip_data.proj[[i]] <- lapply(agmip_data.proj[[i]], rename, Scenario = scenario)
}

# 1. Population
pop <- list()
for(i in names(agmip_data.proj)){
  pop[[i]] <- agmip_data.proj[[i]]$`Population`
}

POPT <- do.call(bind_rows, pop) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Year) %>%
  summarise(Value = sum(value) * 1e-3) %>%
  ungroup() %>%
  mutate(Unit = "million",
         Variable = "POPT",
         Model = "GCAM",
         Item = "TOT") %>%
  select(NAMES_TEMPLATE)

# 2. GDP
gdp <- list()
for(i in names(agmip_data.proj)){
  gdp[[i]] <- agmip_data.proj[[i]]$`GDP MER`
}

GDPT <- do.call(bind_rows, gdp) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Year) %>%
  summarise(Value = sum(value) * 1e-3 * 1.377) %>%
  ungroup() %>%
  mutate(Unit = "bn USD 2005 MER",
         Variable = "GDPT",
         Model = "GCAM",
         Item = "TOT") %>%
  select(NAMES_TEMPLATE)

# 3. LAND, AREA, ARIR, ARRF
land <- list()
for(i in names(agmip_data.proj)){
  land[[i]] <- agmip_data.proj[[i]]$`land_cover`
}
land_df <- do.call(bind_rows, land) %>%
  select(-Units) %>%
  separate(landleaf, into = c("GCAM_LUT", "basin", "IRR_RFD", "MGMT_level"),
           remove = FALSE, fill = "right")

# join in the AgMIP regions and land use type categories
# using left_join to expand the data from GCAM in order to meet all AgMIP reporting categories

LAND <- left_join(land_df, land_map, by = c("GCAM_LUT")) %>%
  filter(Item %in% LAND_ITEMS) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_thouskm2_kha) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "LAND",
         Unit = "1000 ha") %>%
  select(NAMES_TEMPLATE)

# harvested area - multiply land area by harvests per year
harvested_area <- list()
for(i in names(agmip_data.proj)){
  harvested_area[[i]] <- agmip_data.proj[[i]]$`harvests per year`
}
harvested_area_df <- do.call(bind_rows, harvested_area) %>%
  select(-Units) %>%
  left_join(land_df, by = c("Scenario", "region", technology = "landleaf", "Year")) %>%
  mutate(value = value.x * value.y) %>%
  select(-sector, -subsector, -value.x, -value.y)

AREA <- left_join(harvested_area_df, land_map, by = c("GCAM_LUT")) %>%
  filter(Item %in% HARVESTED_AREA_ITEMS) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_thouskm2_kha) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "AREA",
         Unit = "1000 ha") %>%
  select(NAMES_TEMPLATE)

ARRF <- left_join(harvested_area_df, land_map, by = c("GCAM_LUT")) %>%
  filter(Item %in% HARVESTED_AREA_ITEMS,
         IRR_RFD == "RFD") %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_thouskm2_kha) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "ARRF",
         Unit = "1000 ha") %>%
  select(NAMES_TEMPLATE)

ARIR <- left_join(harvested_area_df, land_map, by = c("GCAM_LUT")) %>%
  filter(Item %in% HARVESTED_AREA_ITEMS,
         IRR_RFD == "IRR") %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_thouskm2_kha) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "ARIR",
         Unit = "1000 ha") %>%
  select(NAMES_TEMPLATE)

# 4. PROD: production
# 4a. AG_PROD: crop production
ag_prod <- list()
for(i in names(agmip_data.proj)){
  ag_prod[[i]] <- agmip_data.proj[[i]]$`ag production by tech`
}

ag_prod_df <- do.call(bind_rows, ag_prod) %>%
  separate(technology, into = c("GCAM_LUT", "basin", "IRR_RFD", "MGMT_level"),
           remove = FALSE, fill = "right") %>%
  mutate(value = if_else(sector == "biomass",
                         value * conv_bio_EJ_to_kt,
                         value * conv_Mt_to_kt))

AG_PROD <- left_join(ag_prod_df, select(commodity_map, GCAM_commodity, Item), by = c(sector = "GCAM_commodity")) %>%
  filter(Item %in% PROD_ITEMS) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "PROD",
         Unit = "1000 t")

# These irrigated and rainfed production tables aren't written out; they are generated to calculate yields (YIRF and YIIR)
AG_PROD_IR <- left_join(ag_prod_df, select(commodity_map, GCAM_commodity, Item), by = c(sector = "GCAM_commodity")) %>%
  filter(Item %in% PROD_ITEMS,
         IRR_RFD == "IRR") %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup()

AG_PROD_RF <- left_join(ag_prod_df, select(commodity_map, GCAM_commodity, Item), by = c(sector = "GCAM_commodity")) %>%
  filter(Item %in% PROD_ITEMS,
         IRR_RFD == "RFD") %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup()

# Note - bioenergy crop data isn't written out in the base years. Fill this out.
AG_PROD <- complete(AG_PROD, nesting(Model, Scenario, Region, Item, Variable, Unit),
                    Year = agmip_years,
                    fill = list(Value = 0))
AG_PROD_IR <- complete(AG_PROD_IR, nesting(Scenario, Region, Item),
                       Year = agmip_years,
                    fill = list(Value = 0))
AG_PROD_RF <- complete(AG_PROD_RF, nesting(Scenario, Region, Item),
                       Year = agmip_years,
                       fill = list(Value = 0))

# 4b. AN_PROD: animal commodity production
an_prod <- list()
for(i in names(agmip_data.proj)){
  an_prod[[i]] <- agmip_data.proj[[i]]$`meat and dairy production by type`
}

an_prod_df <- do.call(bind_rows, an_prod) %>%
  mutate(value = value * conv_Mt_to_kt)

AN_PROD <- an_prod_df %>%
  left_join(select(commodity_map, GCAM_commodity, Item), by = c(sector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "PROD",
         Unit = "1000 t")

# Because the Item "AGR" includes both ag and animal commodities, need to aggregate this one more time
PROD <- bind_rows(AG_PROD, AN_PROD) %>%
  group_by(Model, Scenario, Region, Item, Variable, Year, Unit) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  select(NAMES_TEMPLATE)


# 5. XPRP: producer prices, computed using base-year weights
# 5a. AG_XPRP: crop commodity prices
ag_prices <- list()
for(i in names(agmip_data.proj)){
  ag_prices[[i]] <- agmip_data.proj[[i]]$`ag commodity prices`
}
ag_prices_df <- do.call(bind_rows, ag_prices) %>%
  select(-Units)

# Production weights are pulled from the base year. left_join to price data to expand to all agmip reporting years
# For bioenergy crops, use 2020 as the weight year, resetting it to the final base year prior to completing the table
AG_XPRP <- filter(ag_prod_df, Year == min(Year) |
                 (sector == "biomass" & Year == 2020)) %>%
  mutate(Year = min(Year)) %>%
  select(-Units, -Year) %>%
  left_join(ag_prices_df,
            by = c("Scenario", "region", "sector"),
            suffix = c(".prod", ".price")) %>%
  mutate(value.prod = if_else(sector == "biomass", value.prod * conv_bio_EJ_to_Mt, value.prod),
         value.revenue = value.prod * value.price) %>%
  left_join(select(commodity_map, GCAM_commodity, Item),
            by = c(sector = "GCAM_commodity")) %>%
  filter(Item %in% PRICE_ITEMS) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(value.revenue = sum(value.revenue),
            value.prod = sum(value.prod)) %>%
  ungroup()

# 5b. AN_XPRP: animal commodity prices
an_prices <- list()
for(i in names(agmip_data.proj)){
  an_prices[[i]] <- agmip_data.proj[[i]]$`meat and dairy prices`
}
an_prices_df <- do.call(bind_rows, an_prices)

AN_XPRP <- filter(an_prod_df, Year == min(Year)) %>%
  select(-Units, -Year) %>%
  left_join(an_prices_df,
            by = c("Scenario", "region", "sector"),
            suffix = c(".prod", ".price")) %>%
  mutate(value.revenue = value.prod * value.price) %>%
  left_join(select(commodity_map, GCAM_commodity, Item),
            by = c(sector = "GCAM_commodity")) %>%
  filter(Item %in% PRICE_ITEMS) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(value.revenue = sum(value.revenue),
            value.prod = sum(value.prod)) %>%
  ungroup()

# Bind the two tables. Group and summarise again to get the AGR commodity value
XPRP <- bind_rows(AG_XPRP, AN_XPRP) %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(value.revenue = sum(value.revenue),
            value.prod = sum(value.prod)) %>%
  ungroup() %>%
  mutate(Value = value.revenue / value.prod * conv_kg_t * conv_1975_2010_USD,
         Model = "GCAM",
         Variable = "XPRP",
         Unit = "USD/t") %>%
  select(NAMES_TEMPLATE)

# 6. YILD, YIRF, YIIR: yields, with rainfed and irrigated crop production disaggregated in separate tables
YILD <- left_join(AREA, AG_PROD, by = c("Model", "Scenario", "Region", "Item", "Year"),
                  suffix = c(".area", ".prod")) %>%
  mutate(Variable = "YILD",
         Unit = "t/ha",
         Value = Value.prod / Value.area) %>%
  replace_na(list(Value = 0)) %>%
  select(NAMES_TEMPLATE)

YIIR <- left_join(ARIR, AG_PROD_IR, by = c("Scenario", "Region", "Item", "Year"),
                  suffix = c(".area", ".prod")) %>%
  mutate(Variable = "YIIR",
         Unit = "t/ha",
         Value = Value.prod / Value.area) %>%
  replace_na(list(Value = 0)) %>%
  select(NAMES_TEMPLATE)

YIRF <- left_join(ARRF, AG_PROD_RF, by = c("Scenario", "Region", "Item", "Year"),
                  suffix = c(".area", ".prod")) %>%
  mutate(Variable = "YIRF",
         Unit = "t/ha",
         Value = Value.prod / Value.area) %>%
  replace_na(list(Value = 0)) %>%
  select(NAMES_TEMPLATE)

# 7. YEXO: Exogenous yield assumptions over time multiplied by base-year production weights, aggregated
yexo <- list()
for(i in names(agmip_data.proj)){
  yexo[[i]] <- agmip_data.proj[[i]]$`ag tech yield`
}
yexo_df <- do.call(bind_rows, yexo) %>%
  mutate(value = if_else(sector == "biomass",
                         value * conv_bio_EJ_to_Mt,
                         value))

YEXO <- filter(harvested_area_df, Year == min(Year)) %>%
  select(-Year) %>%
  left_join(yexo_df,
            by = c("Scenario", "region", "technology"),
            suffix = c(".area", ".yield")) %>%
  mutate(value.prod = value.area * value.yield) %>%
  left_join(commodity_map, by = c(sector = "GCAM_commodity")) %>%
  drop_na(Item) %>%
  filter(!Item == YEXO_ITEMS_EXCLUDE) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(value.prod = sum(value.prod),
            value.area = sum(value.area)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "YEXO",
         Unit = "t/ha",
         Value = value.prod / value.area * conv_kgm2_tha) %>%
  select(NAMES_TEMPLATE)

# 8. DEMAND: FOOD, FEED, OTHU (commodity disposition)
# 8a. AG_DEMAND: crop uses
ag_use <- list()
for(i in names(agmip_data.proj)){
  ag_use[[i]] <- agmip_data.proj[[i]]$`demand balances by crop commodity`
}
ag_use_df <- do.call(bind_rows, ag_use)

AG_DEMAND <- ag_use_df %>%
  inner_join(demand_map, by = "sector") %>%
  left_join(commodity_map, by = c(input = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Variable, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Unit = "1000 t")

# 8b. AN_DEMAND: animal commodity uses
an_use <- list()
for(i in names(agmip_data.proj)){
  an_use[[i]] <- agmip_data.proj[[i]]$`demand balances by animal commodity`
}
an_use_df <- do.call(bind_rows, an_use)

AN_DEMAND <- an_use_df %>%
  inner_join(demand_map, by = "sector") %>%
  left_join(commodity_map, by = c(input = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Variable, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Unit = "1000 t")

# bind the two, and re-aggregate to get the correct "AGR" commodity reporting
DEMAND <- bind_rows(AG_DEMAND, AN_DEMAND) %>%
  group_by(Model, Scenario, Region, Variable, Item, Unit, Year) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  select(NAMES_TEMPLATE)

# 9. TRADE
#9a. IMPO: imports
imports <- list()
for(i in names(agmip_data.proj)){
  imports[[i]] <- agmip_data.proj[[i]]$`imports`
}
imports_df <- do.call(bind_rows, imports)

# use inner_join because the imports query picks up commodities not considered in AgMIP
# complete the table because categories that are zero in all years aren't necessarily written out
IMPO <- inner_join(imports_df, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region), Item = TRADE_ITEMS, Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "IMPO",
         Unit = "1000 t") %>%
  select(NAMES_TEMPLATE)

#9b. EXPO: exports
exports <- list()
for(i in names(agmip_data.proj)){
  exports[[i]] <- agmip_data.proj[[i]]$`exports`
}
exports_df <- do.call(bind_rows, exports)

# Concatenate the subsector and sector where the sector is "Exports_Meat". This will return e.g. "Exports_Meat Beef"
# For the "traded" crop commodities, the region name should be replaced with the region name prepended to the subsector name
# Using inner_join again for any energy/industrial commodities that aren't considered in AgMIP
EXPO <- exports_df %>%
  mutate(sector = if_else(sector == "Exports_Meat", paste(sector, subsector), sector),
         region = if_else(grepl("traded", sector),
                          substr(subsector, 1, nchar(subsector) - (nchar(sector) + 1)),
                          region)) %>%
  inner_join(commodity_map, by = c(sector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region), Item = TRADE_ITEMS, Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "EXPO",
         Unit = "1000 t") %>%
  select(NAMES_TEMPLATE)

#9c. NETT: net trade (EXPO - IMPO)
NETT <- left_join(EXPO, IMPO,
                  by = NAMES_TEMPLATE[!NAMES_TEMPLATE %in% c("Variable", "Value")],
                  suffix = c(".exports", ".imports")) %>%
  mutate(Variable = "NETT",
         Value = Value.exports - Value.imports) %>%
  select(NAMES_TEMPLATE)

# 10. FRTN: Fertilizer N inputs by crop type
fertilizer <- list()
for(i in names(agmip_data.proj)){
  fertilizer[[i]] <- agmip_data.proj[[i]]$`fertilizer consumption by crop type`
}
fertilizer_df <- do.call(bind_rows, fertilizer)

FRTN <- left_join(fertilizer_df, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  drop_na(Item) %>%    #NAs show up for "Exports_fertilizer" sector
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region),
           Item = FRTN_ITEMS,
           Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "FRTN",
         Unit = "1000 t") %>%
  select(NAMES_TEMPLATE)

# 11. WATR: Irrigation water inputs by crop type
water <- list()
for(i in names(agmip_data.proj)){
  water[[i]] <- agmip_data.proj[[i]]$`irrigation water withdrawals by crop type`
}
water_df <- do.call(bind_rows, water)

WATR <- left_join(water_df, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region),
           Item = WATR_ITEMS,
           Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "WATR",
         Unit = "km3") %>%
  select(NAMES_TEMPLATE)

# 12. CALO, CALI: Calorie availability, calorie intake
calories <- list()
for(i in names(agmip_data.proj)){
  calories[[i]] <- agmip_data.proj[[i]]$`food consumption (Pcal)`
}
calories_df <- do.call(bind_rows, calories)

CALO <- left_join(calories_df, commodity_map, by = c(subsector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  left_join(select(POPT, -Item, -Unit),
            by = c("Scenario", "Region", "Year"),
            suffix = c(".calo", ".pop")) %>%
  mutate(Value = Value.calo * conv_Pcal_to_Gcal / Value.pop / conv_year_to_days) %>%
  mutate(Model = "GCAM",
         Variable = "CALO",
         Unit = "kcal/cap/d") %>%
  select(NAMES_TEMPLATE)

CALI <- left_join(calories_df, commodity_map, by = c(subsector = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  left_join(food_waste_fractions, by = c("region", subsector = "GCAM_commodity")) %>%
  mutate(value = value * (1 - waste_frac)) %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  left_join(select(POPT, -Item, -Unit),
            by = c("Scenario", "Region", "Year"),
            suffix = c(".cali", ".pop")) %>%
  mutate(Value = Value.cali * conv_Pcal_to_Gcal / Value.pop / conv_year_to_days) %>%
  mutate(Model = "GCAM",
         Variable = "CALI",
         Unit = "kcal/cap/d") %>%
  select(NAMES_TEMPLATE)

# 13. EMIS, ECO2, ECH4, EN2O: Emissions
emissions <- list()
for(i in names(agmip_data.proj)){
  emissions[[i]] <- agmip_data.proj[[i]]$`emissions by sector`
}
emissions_df <- do.call(bind_rows, emissions)

luc_emissions <- list()
for(i in names(agmip_data.proj)){
  luc_emissions[[i]] <- agmip_data.proj[[i]]$`LUC emissions by region`
}
luc_emissions_df <- do.call(bind_rows, luc_emissions)

# 13a. ECO2: Total CO2
ECO2 <- filter(emissions_df, ghg == "CO2") %>%
  bind_rows(luc_emissions_df) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Year) %>%
  summarise(Value = sum(value) * conv_C_to_CO2) %>%
  mutate(Model = "GCAM",
         Variable = "ECO2",
         Item = "TOT",
         Unit = "MtCO2e") %>%
  select(NAMES_TEMPLATE)

# 13b. ECH4: CH4 by crop type
ECH4 <- filter(emissions_df, ghg %in% c("CH4_AGR", "CH4_AWB")) %>%
  left_join(region_map, by = "region") %>%
  inner_join(commodity_map, by = c(sector = "GCAM_commodity")) %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_CH4_to_CO2e) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region),
           Item = unique(Item),
           Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "ECH4",
         Unit = "MtCO2e") %>%
  select(NAMES_TEMPLATE)

# 13c. EN2O: N2O by crop type
EN2O <- filter(emissions_df, ghg %in% c("N2O_AGR", "N2O_AWB")) %>%
  left_join(region_map, by = "region") %>%
  inner_join(commodity_map, by = c(sector = "GCAM_commodity")) %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_N2O_to_CO2e) %>%
  ungroup() %>%
  complete(nesting(Scenario, Region),
           Item = unique(Item),
           Year = agmip_years,
           fill = list(Value = 0)) %>%
  mutate(Model = "GCAM",
         Variable = "EN2O",
         Unit = "MtCO2e") %>%
  select(NAMES_TEMPLATE)

# 13d. EMIS: total CO2e emissions (just using the totals, given incomplete coverage by item across the other queries)
EMIS <- filter(emissions_df, ghg %in% c("CO2", "CH4", "N2O", "CH4_AGR", "CH4_AWB", "N2O_AGR", "N2O_AWB")) %>%
  bind_rows(luc_emissions_df) %>%
  mutate(ghg = if_else(is.na(ghg), "CO2", ghg),
    Value = if_else(grepl("N2O", ghg), value * conv_N2O_to_CO2e,
                         if_else(grepl("CH4", ghg), value * conv_CH4_to_CO2e,
                                 value * conv_C_to_CO2))) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Year) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Item = "TOT",
         Variable = "EMIS",
         Unit = "MtCO2e") %>%
  select(NAMES_TEMPLATE)

# 14. FEEF: feed conversion efficiency
feed_inputs <- list()
for(i in names(agmip_data.proj)){
  feed_inputs[[i]] <- agmip_data.proj[[i]]$`feed consumption by animal commodity`
}
FEED_INPUTS <- do.call(bind_rows, feed_inputs) %>%
  left_join(region_map, by = "region") %>%
  left_join(commodity_map, by = c(sector = "GCAM_commodity")) %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value) * conv_Mt_to_kt) %>%
  ungroup()

FEEF <- left_join(AN_PROD, FEED_INPUTS, by = c("Scenario", "Region", "Item", "Year"),
                  suffix = c(".prod", ".feed")) %>%
  mutate(Value = Value.prod / Value.feed,
         Variable = "FEEF",
         Unit = "kg prt/kg prt") %>%
  select(NAMES_TEMPLATE)

# 15. CTAX
CO2prices <- list()
for(i in names(agmip_data.proj)){
  if(!is.null(agmip_data.proj[[i]]$`CO2 prices`)){
    CO2prices[[i]] <- agmip_data.proj[[i]]$`CO2 prices`
  }
}
CO2prices_df <- do.call(bind_rows, CO2prices)

if(nrow(CO2prices_df) > 0){
  CTAX <- CO2prices_df %>%
    filter(!grepl("_LUC", market)) %>%
    select(Scenario, Year, value) %>%
    distinct() %>%
    complete(nesting(Year),
             Scenario = unique(POPT$Scenario),
             fill = list(value = 0)) %>%
    repeat_add_columns(unique(region_map["Region"])) %>%
    mutate(Value = value * conv_1990_2010_USD / conv_C_to_CO2,
           Unit = "USD/tCO2e",
           Model = "GCAM",
           Variable = "CTAX",
           Item = "TOT") %>%
    select(NAMES_TEMPLATE)
}

# 16. YECC: climate change shifter on crop yield
# This is calculated as (YEXOcc / YEXOnocc) - 1
YECC_numerator <- subset(YEXO, Scenario %in% YECC_scenario_map$CC_Scenario) %>%
  left_join(YECC_scenario_map, by = c(Scenario = "CC_Scenario"))
YECC_denominator <- subset(YEXO, Scenario %in% YECC_scenario_map$NoCC_Scenario) %>%
  rename(NoCC_Scenario = Scenario)
YECC <- left_join(YECC_numerator, YECC_denominator,
                  by = c("Model", "NoCC_Scenario", "Region", "Item", "Variable", "Year", "Unit"),
                  suffix = c(".num", ".denom")) %>%
  mutate(Value = (Value.num / Value.denom - 1) * 100,
         Variable = "YECC",
         Unit = "%") %>%
  select(NAMES_TEMPLATE)

# Feed shares
ag_feed_shares <- subset(ag_use_df, sector %in% c("FeedCrops", "FodderHerb_Residue", "Pasture_FodderGrass")) %>%
  group_by(Scenario, region, sector, Year) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  select(Scenario, region, sector, input, Year, share)

FEED_INPUTS_BYCROP <- do.call(bind_rows, feed_inputs) %>%
  rename(feed_class = input) %>%
  left_join(ag_feed_shares, by = c("Scenario", "region", feed_class = "sector", "Year")) %>%
  mutate(input = if_else(feed_class == "Scavenging_Other", "Scavenging_Other_Rsrc", input),
         share = if_else(feed_class == "Scavenging_Other", 1, share),
         value = value * share) %>%
  select(Scenario, region, sector, input, Year, value)

# FRUM: Feed use by ruminant meat animals
FRUM <- left_join(FEED_INPUTS_BYCROP, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  filter(Item == "RUM") %>%
  select(-Item) %>%
  left_join(commodity_map, by = c(input = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "FRUM",
         Unit = "1000 t")

# FDRY: Feed use by dairy animals
FDRY <- left_join(FEED_INPUTS_BYCROP, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  filter(Item == "DRY") %>%
  select(-Item) %>%
  left_join(commodity_map, by = c(input = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "FDRY",
         Unit = "1000 t")

# FNRM: feed use by non-ruminant animals (pork, poultry)
FNRM <- left_join(FEED_INPUTS_BYCROP, commodity_map, by = c(sector = "GCAM_commodity")) %>%
  filter(Item == "NRM") %>%
  select(-Item) %>%
  left_join(commodity_map, by = c(input = "GCAM_commodity")) %>%
  left_join(region_map, by = "region") %>%
  group_by(Scenario, Region, Item, Year) %>%
  summarise(Value = sum(value)) %>%
  ungroup() %>%
  mutate(Model = "GCAM",
         Variable = "FNRM",
         Unit = "1000 t")

# FINAL STEP: bind all individual components to form the template
datestamp <- gsub("-", "_", Sys.Date())
FINAL_TEMPLATE <- bind_rows(POPT, GDPT,
                            LAND, AREA, ARRF, ARIR,
                            PROD,
                            XPRP,
                            YILD, YIIR, YIRF, YEXO,
                            DEMAND,
                            IMPO, EXPO, NETT,
                            FRTN, WATR,
                            CALO, CALI,
                            EMIS, ECO2, ECH4, EN2O,
                            FEEF, FRUM, FDRY, FNRM)
if(nrow(CO2prices_df) > 0) FINAL_TEMPLATE <- bind_rows(FINAL_TEMPLATE, CTAX)

if(BUILD_MAIN_TEMPLATE){
  FINAL_MAIN_TEMPLATE <- subset(FINAL_TEMPLATE, Scenario %in% MAIN_SCENARIOS)
  write.csv(FINAL_MAIN_TEMPLATE, paste0("GCAM_AgMIP_", datestamp, ".csv"), row.names = FALSE)
}

if(BUILD_DIET_TEMPLATE){
  FINAL_DIET_TEMPLATE <- subset(FINAL_TEMPLATE, Scenario %in% DIET_SCENARIOS)
  write.csv(FINAL_DIET_TEMPLATE, paste0("GCAM_AgMIP_Diet_", datestamp, ".csv"), row.names = FALSE)
}

if(BUILD_MITIGDECOMP_TEMPLATE){
  FINAL_MITIGDECOMP_TEMPLATE <- subset(FINAL_TEMPLATE, Scenario %in% MITIG_DECOMP_SCENARIOS)
  write.csv(FINAL_MITIGDECOMP_TEMPLATE, paste0("GCAM_AgMIP_MitigDecomp_", datestamp, ".csv"), row.names = FALSE)
}
