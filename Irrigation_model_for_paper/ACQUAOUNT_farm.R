# ==============================================================================
# ACQUAOUNT-farm irrigation model
# ==============================================================================
# Description: Multi-layer soil water balance model for irrigation scheduling
# Based on FAO-56 methodology with dual crop coefficient approach
# ==============================================================================

# Clear workspace
rm(list = ls())

# ==============================================================================
# LOAD LIBRARIES
# ==============================================================================

cat("Model started...\n")

library(tictoc)
library(readxl)
library(units)
library(lubridate)
library(dplyr)
library(randomForest)
library(outliers)
library(soilDB)
library(aqp)
library(ranger)
library(parallel)
library(jsonlite)


# ==============================================================================
# SETUP DIRECTORIES AND CONFIGURATION
# ==============================================================================
wd <- setwd("SET WORKING DIRECTORY")
#wd <- getwd()
in_dir <- paste0(wd, "/Input_data/")
out_dir <- paste0(wd, "/Output_data/")
json_data <- fromJSON(paste0(wd, "/config.input.json"))

source(paste0(wd, "/Functions/ACQUAOUNT_farm_functions.r"))


# ==============================================================================
# CROP PARAMETERS
# ==============================================================================

# Import crop table
crop_table <- read.csv(
  paste0(in_dir, "crop_data/crop_table.csv"),
  sep = if (any(grepl(";", readLines(paste0(in_dir, "crop_data/crop_table.csv"))[1]))) ";" else ","
)

# Crop selection and season dates
crop_list <- list(json_data$crop_name)
season_start <- json_data$season_start  # Planting/budbreak date
season_end <- json_data$season_end      # Harvesting date

# Plant spacing configuration (m)
plant_spacing <- json_data$plant_spacing  # Distance between plants
line_spacing <- json_data$line_spacing    # Distance between lines
Area_crop <- json_data$Area_crop          # Cultivated area (m²)
n_plants <- Area_crop / (plant_spacing * line_spacing)

# Import corrected Kc from previous run
kc_corr <- as.numeric(unlist(read.table(
  paste0(in_dir, json_data$kc_correction_filepath), 
  quote = "\"", 
  comment.char = ""
)))


# ==============================================================================
# SOIL PARAMETERS
# ==============================================================================

# Soil texture (%)
P_sand <- json_data$P_sand
P_clay <- json_data$P_clay
P_silt <- json_data$P_silt
P_om <- json_data$P_om  # Organic matter

# Hydraulic properties
Ks <- json_data$Ks  # Saturated hydraulic conductivity (cm/h)
Pb <- json_data$Pb  # Bulk density (g/cm³)

# Water retention properties
soil_SAT_user <- json_data$soil_SAT_user / 100  # Saturation
theta_fc_user <- json_data$theta_fc_user / 100  # Field capacity
theta_wp_user <- json_data$theta_wp_user / 100  # Wilting point


# ==============================================================================
# CLIMATE DATA
# ==============================================================================

# Climate zone configuration
climate_zone <- json_data$climate_zone
lat <- json_data$lat
elev <- json_data$elev

# Import historical sensor data (PAST)
temp_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/temperature_past.csv"))))
precipitation_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/precipitation_past.csv"))))
sw_rad_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/solar_rad_past.csv"))))
RH_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/humidity_past.csv"))))
wind_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/wind_past.csv"))))

date_past <- as.character(unlist(read.csv(paste0(in_dir, "climate_data/date_past.csv"))))
date_past <- parse_date_time(date_past, orders = c("ymd", "dmy", "mdy", "ydm"))
hh_past <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/hour_past.csv"))))

duration_past <- length(temp_past)

# Import weather forecast data (FUTURE)
temp_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/temperature_fut.csv"))))
precipitation_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/precipitation_fut.csv"))))
sw_rad_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/solar_rad_fut.csv"))))
RH_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/humidity_fut.csv"))))
wind_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/wind_fut.csv"))))

date_fut <- as.character(unlist(read.csv(paste0(in_dir, "climate_data/date_fut.csv"))))
date_fut <- parse_date_time(date_fut, orders = c("ymd", "dmy", "mdy", "ydm"))
hh_fut <- as.numeric(unlist(read.csv(paste0(in_dir, "climate_data/hour_fut.csv"))))

duration_fut <- length(temp_fut)


# ==============================================================================
# IRRIGATION SYSTEM PARAMETERS
# ==============================================================================

# Irrigation method: "sprinkler", "drip", or "subterranean"
irr_method <- json_data$irr_method

if (irr_method == "sprinkler") {
  
  n_emitter <- json_data$n_emitter
  s_rate <- json_data$s_rate  # Sprinkler flowrate (l/h)
  Flow_rate <- n_emitter * s_rate / 1000  # System flowrate (m³/h)
  
} else if (irr_method == "drip") {
  
  n_drip_per_line <- json_data$n_drip_per_line
  d_spacing <- json_data$d_spacing  # Dripper spacing (m)
  d_rate <- json_data$d_rate        # Dripper flowrate (l/h)
  w_length <- json_data$w_length    # Wetted length (m)
  n_emitter <- w_length / d_spacing
  Flow_rate <- n_emitter * d_rate / 1000  # System flowrate (m³/h)
  
} else if (irr_method == "subterranean") {
  
  n_drip_per_line <- json_data$n_drip_per_line
  d_depth <- json_data$d_depth      # Depth of drippers (m)
  d_spacing <- json_data$d_spacing  # Dripper spacing (m)
  d_rate <- json_data$d_rate        # Dripper flowrate (l/h)
  w_length <- json_data$w_length    # Wetted length (m)
  n_emitter <- w_length / d_spacing
  Flow_rate <- n_emitter * d_rate / 1000  # System flowrate (m³/h)
  
}

# Irrigation deficit array (user-defined or default from crop_table)
irr_def_user <- c(NA, NA, NA)

# Historical irrigation volumes (m³)
irr_given_vol_original <- as.numeric(unlist(read.csv(
  paste0(in_dir, "irrigation_data/water_flow_past.csv")
)))


# ==============================================================================
# SOIL MOISTURE DATA
# ==============================================================================

moisture_sensor_raw <- read.csv(paste0(in_dir, "soil_moisture_data/SM_past.csv"))
sensor_spacing <- json_data$sensor_spacing  # Spacing between measurement points (m)

extremes_previous_run <- as.numeric(unlist(read.table(
  paste0(in_dir, "soil_moisture_data/SM_extremes_previous_run.csv"),
  sep = "",
  header = FALSE
))) / 100

# Identify moisture extremes from sensor data
moisture_sensor_raw <- moisture_sensor_raw / 100
soil_SAT_sensor <- max(c(max(moisture_sensor_raw), extremes_previous_run[1]))
soil_WP_sensor <- min(c(min(moisture_sensor_raw), extremes_previous_run[2]))

# Sensor configuration
sensor_levels <- ncol(moisture_sensor_raw)
sensor_level <- rep(sensor_spacing, sensor_levels)
level <- cumsum(sensor_level)  # Depth of each sensor (m)


# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Calculate soil water retention properties using Pedotransfer Functions
WRP <- PTFs_function(P_sand, P_silt, P_clay, P_om, Pb, theta_fc_user, theta_wp_user, soil_SAT_user)

theta_fc <- WRP$FC
theta_wp <- WRP$WP
soil_SAT <- WRP$SAT

crop_AWC <- theta_fc - theta_wp  # Available water capacity

# Convert to mm
soil_SAT_mm <- soil_SAT * sensor_spacing * 1000
theta_fc_mm <- theta_fc * sensor_spacing * 1000
theta_wp_mm <- theta_wp * sensor_spacing * 1000


# ------------------------------------------------------------------------------
# Calibrate soil moisture data to match PTF estimates
# ------------------------------------------------------------------------------

m <- (theta_fc - theta_wp) / (theta_fc_user - soil_WP_sensor)  # Slope
q <- theta_wp - m * soil_WP_sensor  # Intercept

moisture_sensor <- m * moisture_sensor_raw + q
moisture_sensor[moisture_sensor > soil_SAT] <- soil_SAT


# ------------------------------------------------------------------------------
# Aggregate and combine all input data
# ------------------------------------------------------------------------------

dataset_model <- data_aggregation_function(
  moisture_sensor, date_past, hh_past, temp_past, precipitation_past, 
  sw_rad_past, RH_past, wind_past, irr_given_vol_original,
  date_fut, hh_fut, temp_fut, precipitation_fut, sw_rad_fut, RH_fut, wind_fut,
  duration_fut, sensor_levels
)

# Extract variables from combined dataset
date <- dataset_model$date
hh <- dataset_model$hh
temp <- dataset_model$temp
precipitation <- dataset_model$prec
RH <- dataset_model$RH
wind <- dataset_model$wind
sw_rad <- dataset_model$SW
moisture_sensor <- dataset_model[, 1:3]
irr_given_vol <- dataset_model$irrigation


# ------------------------------------------------------------------------------
# Convert dates to Day of Year (DOY)
# ------------------------------------------------------------------------------

season_start <- as.Date(season_start, format = "%d/%m/%Y")
season_end <- as.Date(season_end, format = "%d/%m/%Y")
season_start <- yday(season_start)
season_end <- yday(season_end)


# ==============================================================================
# RUN MODEL
# ==============================================================================

# ------------------------------------------------------------------------------
# Calculate reference evapotranspiration (ET0)
# ------------------------------------------------------------------------------

Evapotranspiration <- FAO_ET0_function(
  lat, temp, sw_rad, wind, RH, elev, date, hh, 
  "°C", "W/m2", "m/s", "%", "m"
)

ET0 <- Evapotranspiration$ET0
OWET <- Evapotranspiration$OWET


# ------------------------------------------------------------------------------
# Initialize output arrays
# ------------------------------------------------------------------------------

Irr_req <- as.numeric(array(0, dim = length(ET0)))
ETa_rf <- as.numeric(array(0, dim = length(ET0)))
ETa_full <- as.numeric(array(0, dim = length(ET0)))
Dr <- as.numeric(array(0, dim = length(ET0)))
Irr_vol <- as.numeric(array(0, dim = length(ET0)))
Irr_vol_def <- as.numeric(array(0, dim = length(ET0)))
moisture_mean_array <- as.numeric(array(0, dim = length(ET0)))
Irr_vol_percolated <- as.numeric(array(0, dim = length(ET0)))

Date_irr <- data.frame(
  Today = date[duration_past],
  Early = NA,
  Late = NA,
  Limit = NA, 
  row.names = "Date"
)

Irr_volume <- as.numeric(array(0, dim = length(ET0)))
Irr_hours <- as.numeric(array(0, dim = length(ET0)))


# ------------------------------------------------------------------------------
# Date formatting
# ------------------------------------------------------------------------------

j <- as.numeric(format(date, "%j"))  # Julian day
year <- as.numeric(format(date, "%Y"))
year_list <- unique(year)
l <- c(1:length(date))

# Identify last day of each year (365 or 366)
end <- array(0, length(year_list))
end[length(year_list)] <- j[length(j)]
for (p in 1:(length(l) - 1)) {
  if (j[p] > j[p + 1]) {
    end[round(l[p] / 365)] <- j[p]
  }
}

sensor_level_names <- gsub("moisture_sensor_raw", "", colnames(moisture_sensor_raw))


# ------------------------------------------------------------------------------
# Create summary table
# ------------------------------------------------------------------------------

variables_st <- c(
  "Date", "Hour", "precipitation", "P_eff", "ponding", "DP",
  paste0("moisture_", sensor_level_names), "ET0", 
  paste0("Deficit meteo ", sensor_level_names), "SAT_daily", "theta_fc", 
  "TAW", "RAW", "DFadj", "kc", "ke", "kc_correction", "ETc", "ks", "Dr", 
  "ETa", "Irr_req", paste0("Deficit final ", sensor_level_names), "Irr_vol", 
  "Irr_deficit", "Irr_vol_percolated", paste0("moisture_new_", sensor_level_names)
)

summary_table <- data.frame(matrix(NA, nrow = length(ET0), ncol = length(variables_st)))
colnames(summary_table) <- variables_st


# ==============================================================================
# SOIL WATER BALANCE - MAIN LOOP
# ==============================================================================

for (crop in crop_list) {
  
  i <- which(crop == crop_list)
  r_crop <- which(crop_table$Crop == crop & crop_table$Clim == climate_zone)
  type <- crop_table[r_crop, which(colnames(crop_table) == "Type")]
  
  # Crop height
  height <- as.numeric(crop_table[r_crop, which(colnames(crop_table) == "Height")])
  
  # Length of growing period
  if (season_start < season_end) {
    LGP <- season_end - season_start + 1
  } else if (season_start > season_end) {
    LGP <- season_end - season_start + 366
  }
  
  
  # --------------------------------------------------------------------------
  # Basal crop coefficients (Kcb)
  # --------------------------------------------------------------------------
  
  kcb_in <- crop_table[r_crop, which(colnames(crop_table) == "Kc_in")]
  kcb_mid <- crop_table[r_crop, which(colnames(crop_table) == "Kc_mid")]
  kcb_end <- crop_table[r_crop, which(colnames(crop_table) == "Kc_end")]
  
  # Adjust Kcb for climate conditions
  wind_mean <- mean(wind, na.rm = TRUE)
  RH_min_mean <- mean(tapply(RH, rep(1:14, each = 24), min), na.rm = TRUE)
  
  kcb_in_adj <- ifelse(
    kcb_in > 0.45,
    kcb_in + (0.04 * (wind_mean - 2) - 0.004 * (RH_min_mean - 45)) * (height / 3)^0.3,
    kcb_in
  )
  
  kcb_mid_adj <- kcb_mid + (0.04 * (wind_mean - 2) - 0.004 * (RH_min_mean - 45)) * (height / 3)^0.3
  
  kcb_end_adj <- ifelse(
    kcb_end > 0.45,
    kcb_end + (0.04 * (wind_mean - 2) - 0.004 * (RH_min_mean - 45)) * (height / 3)^0.3,
    kcb_end
  )
  
  
  # --------------------------------------------------------------------------
  # Growth stage durations (days)
  # --------------------------------------------------------------------------
  
  L1 <- round(LGP * crop_table[r_crop, which(colnames(crop_table) == "lgp_f1")])
  L2 <- round(LGP * crop_table[r_crop, which(colnames(crop_table) == "lgp_f2")])
  L3 <- round(LGP * crop_table[r_crop, which(colnames(crop_table) == "lgp_f3")])
  L4 <- LGP - (L1 + L2 + L3)
  
  L12 <- L1 + L2
  L123 <- L1 + L2 + L3
  
  L2_den <- L2
  ifelse(L2_den == 0, NA, L2_den)
  L4_den <- L4
  ifelse(L4_den == 0, NA, L4_den)
  
  s <- season_start
  h <- season_end
  
  
  # --------------------------------------------------------------------------
  # Rooting depth
  # --------------------------------------------------------------------------
  
  RD <- as.numeric(crop_table[r_crop, which(colnames(crop_table) %in% c("RD1", "RD2"))])
  
  L12_den <- L12
  ifelse(L12_den == 0, NA, L12_den)
  
  
  # --------------------------------------------------------------------------
  # Depletion factor and irrigation deficit
  # --------------------------------------------------------------------------
  
  DF <- crop_table[r_crop, which(colnames(crop_table) == "DF")]
  
  if (is.na(sum(irr_def_user)) == TRUE) {
    irr_def_array <- as.numeric(crop_table[r_crop, which(colnames(crop_table) %in% 
                                                           c("Irr_def_1", "Irr_def_2", "Irr_def_3"))])
  } else {
    irr_def_array <- irr_def_user
  }
  
  irr_threshold_array <- as.numeric(crop_table[r_crop, which(colnames(crop_table) %in% 
                                                               c("Early_irr", "Late_irr", "Limit_irr"))])
  
  
  # --------------------------------------------------------------------------
  # Define lower measurement level for deep percolation
  # --------------------------------------------------------------------------
  
  r <- ifelse(
    level[length(level)] > RD[2],
    which(level > RD[2] + 0.3),
    level[length(level)]
  )
  r <- as.array(r)
  moisture_LOW <- as.numeric(moisture_sensor[, which(level == r[1])])
  
  
  # --------------------------------------------------------------------------
  # Initialize layer variables
  # --------------------------------------------------------------------------
  
  ponding_in <- 0
  kc_correction <- 0
  deficit_meteo <- rep(0, sensor_levels)
  excess <- rep(0, sensor_levels)
  ETa_layer <- rep(0, sensor_levels)
  deficit_final <- rep(0, sensor_levels)
  moisture_new <- rep(0, sensor_levels)
  
  
  # ==========================================================================
  # HOURLY TIME STEP LOOP
  # ==========================================================================
  
  for (a in 1:length(l)) {
    
    day <- j[a]
    
    
    # ------------------------------------------------------------------------
    # Define wetted area for localized irrigation
    # ------------------------------------------------------------------------
    
    if (irr_method == "drip" | irr_method == "subterranean") {
      
      if (type == 1) {
        Area_irr <- Area_crop * 0.9
      } else if (type == 2) {
        Area_irr <- Area_crop * 0.3
      }
      
    } else if (irr_method == "sprinkler" | irr_method == "surface") {
      
      Area_irr <- Area_crop
      radius <- NA
      
    }
    
    
    # Convert irrigation volume from m³ to mm
    irr_given_mm <- irr_given_vol[a] / Area_irr * 1000
    
    
    # ------------------------------------------------------------------------
    # Rooting depth increment
    # ------------------------------------------------------------------------
    
    if (any((s < h & (yday(c(date_past, date_fut)) < s | yday(c(date_past, date_fut)) > h)) | 
            (s > h & (yday(c(date_past, date_fut)) < s & yday(c(date_past, date_fut)) > h)))) {
      daily_rooting_depth <- RD[1]
    } else {
      daily_rooting_depth <- RD_increment(s, h, day, L12, RD, L12_den, type)
    }
    
    # Identify sensor levels within root zone
    if (daily_rooting_depth >= max(level)) {
      level_rz <- length(level)
    } else {
      level_rz <- which.min(abs(level - daily_rooting_depth))
    }
    
    
    # ------------------------------------------------------------------------
    # Root distribution per layer (Fan et al., 2016)
    # ------------------------------------------------------------------------
    
    perc_root_distr_cum <- 1 / (1 + (level / 0.15)^-1.117) + 
      (1 - 1 / (1 + (daily_rooting_depth / 0.15)^-1.117)) * level / daily_rooting_depth
    perc_root_distr_cum[perc_root_distr_cum > 1] <- 1
    perc_root_distr <- c(perc_root_distr_cum[1], diff(perc_root_distr_cum))
    
    
    # ------------------------------------------------------------------------
    # Mean soil moisture in root zone
    # ------------------------------------------------------------------------
    
    moisture_mean <- mean(as.numeric(moisture_sensor[a, ]))
    moisture_a <- as.numeric(moisture_sensor[a, ])
    moisture_mean_array[a] <- moisture_mean
    
    
    # ------------------------------------------------------------------------
    # Effective precipitation (Ali & Mubarak, 2017)
    # ------------------------------------------------------------------------
    
    prec_eff <- P_eff(sensor_spacing, soil_SAT, moisture_a, ET0, a, precipitation, irr_given_mm)
    prec_eff <- prec_eff + ponding_in
    
    ponding_gross <- ifelse(
      (precipitation[a] + irr_given_mm) > prec_eff,
      precipitation[a] + irr_given_mm - prec_eff,
      0
    )
    
    if (irr_method == "subterranean") {
      ponding_net <- ponding_gross
    } else {
      ponding_net <- ifelse(OWET[a] <= ponding_gross, ponding_gross - OWET[a], 0)
    }
    
    ponding_in <- ponding_net
    
    
    # ------------------------------------------------------------------------
    # STEP 1: Soil moisture increment due to precipitation
    # ------------------------------------------------------------------------
    
    excess <- rep(0, sensor_levels)
    deficit_meteo[1] <- max((soil_SAT - moisture_a[1]) * sensor_spacing * 1000 - prec_eff, 0)
    excess[1] <- ifelse(
      deficit_meteo[1] < (soil_SAT_mm - theta_fc_mm),
      min(Ks * 10, (soil_SAT_mm - theta_fc_mm) - deficit_meteo[1]),
      0
    )
    
    for (x in 2:sensor_levels) {
      deficit_meteo[x] <- max((soil_SAT - moisture_a[x]) * sensor_spacing * 1000 - excess[x - 1], 0)
      excess[x] <- ifelse(
        deficit_meteo[x] < (soil_SAT_mm - theta_fc_mm),
        min(Ks * 10, (soil_SAT_mm - theta_fc_mm) - deficit_meteo[x]),
        0
      )
    }
    
    deficit_meteo_tot <- sum(deficit_meteo)
    deficit_meteo_rz <- sum(deficit_meteo[1:level_rz])
    
    
    # ------------------------------------------------------------------------
    # STEP 2: Daily crop coefficient calculation
    # ------------------------------------------------------------------------
    
    kcb <- crop_coeff(s, h, day, L1, L12, L123, L2_den, L4_den, LGP, kcb_in, kcb_mid, kcb_end)
    
    # Ke calculation
    kcmin <- 0.15
    kcmax <- max((1.2 + (0.04 * (wind_mean - 2) - 0.004 * (RH_min_mean - 45)) * (h / 3)^0.3), kcb + 0.05)
    
    ze <- level[1]
    TEW <- 1000 * (theta_fc - 0.5 * theta_wp) * ze
    dr_lev1 <- deficit_meteo[1] - (soil_SAT - theta_fc) * level[1] * 1000
    REW <- TEW * DF
    kr <- max(0, min((TEW - dr_lev1) / (TEW - REW), 1))
    
    if (irr_method == "sprinkler") {
      fw <- 1
    } else {
      fw <- 0.3
    }
    
    fc <- max(((kcb - kcmin) / (kcmax - kcmin))^(1 + 0.5 * height), 0, na.rm = TRUE)
    few <- min(1 - fc, fw)
    
    ke <- min(kr * (kcmax - kcb), few * kcmax)
    kc <- (kcb + ke)
    
    
    # ------------------------------------------------------------------------
    # STEP 3: Total and readily available water
    # ------------------------------------------------------------------------
    
    ET0_day <- sum(ET0[which(j == day)])
    if (ET0_day > 4) {
      DFadj <- DF + 0.1 * (4 - ET0_day)
    } else {
      DFadj <- DF
    }
    
    TAW <- crop_AWC * 1000 * level[level_rz]
    RAW <- crop_AWC * 1000 * level[level_rz] * DFadj
    
    SAT_daily <- (soil_SAT) * 1000 * level[level_rz]
    SAT_daily_limit <- (soil_SAT - theta_wp) * 1000 * level[level_rz]
    
    
    # ------------------------------------------------------------------------
    # STEP 4: Water stress coefficient
    # ------------------------------------------------------------------------
    
    Dr[a] <- ifelse(
      deficit_meteo_rz > (soil_SAT - theta_fc) * level[level_rz] * 1000,
      min(deficit_meteo_rz - (soil_SAT - theta_fc) * level[level_rz] * 1000, TAW),
      0
    )
    
    ks <- ws_coeff(TAW, Dr, a, RAW)
    
    
    # ------------------------------------------------------------------------
    # STEP 5: Kc correction based on soil moisture
    # ------------------------------------------------------------------------
    
    kc_correction <- kc_corr_function(
      a, kc_corr, kc_correction, irr_given_vol, moisture_LOW, 
      duration_past, moisture_sensor, sensor_levels
    )
    
    
    # ------------------------------------------------------------------------
    # STEP 6: Actual evapotranspiration
    # ------------------------------------------------------------------------
    
    ETc <- ET0[a] * kc * kc_correction
    ETa <- ET0[a] * (kcb * ks + ke) * kc_correction
    
    layers_in_rz <- sum(daily_rooting_depth > level)
    
    if (layers_in_rz > 0) {
      ETa_layer[1] <- ET0[a] * (kcb * ks * perc_root_distr[1] + ke) * kc_correction
      ETa_layer[2:sensor_levels] <- ET0[a] * kcb * ks * perc_root_distr[2:sensor_levels] * kc_correction
    } else {
      ETa_layer[1:sensor_levels] <- ETa * perc_root_distr
    }
    
    
    # ------------------------------------------------------------------------
    # STEP 7: Final water deficit after ET
    # ------------------------------------------------------------------------
    
    for (x in 1:sensor_levels) {
      if (deficit_meteo[x] < 0) {
        deficit_final[x] <- 0
      } else if (deficit_meteo[x] >= 0 & deficit_meteo[x] <= SAT_daily_limit) {
        deficit_final[x] <- deficit_meteo[x] + ETa_layer[x] + excess[x]
      } else if (deficit_meteo[x] > SAT_daily_limit) {
        deficit_final[x] <- SAT_daily_limit
      }
    }
    
    deficit_final_tot <- sum(deficit_final)
    
    
    # ------------------------------------------------------------------------
    # STEP 8: ET components and irrigation requirements
    # ------------------------------------------------------------------------
    
    ETa_rf[a] <- ETa
    ETa_full[a] <- ETc
    Irr_req[a] <- irrigation_function(Dr, a)
    
    
    # ------------------------------------------------------------------------
    # STEP 9: Irrigation volume calculation
    # ------------------------------------------------------------------------
    
    Irr_vol[a] <- Irr_vol_function(irr_method, Irr_req, a, Area_irr)
    
    Irr_vol_def[a] <- Irr_def_function(
      day, s, L12, L123, irr_def_array, h, Irr_vol, a, irr_def, RAW, Dr, irr_threshold_array
    )
    
    
    # ------------------------------------------------------------------------
    # STEP 10: Irrigation timing suggestions
    # ------------------------------------------------------------------------
    
    Date_irr <- irr_suggestion_function(RAW, irr_threshold_array, a, duration_past, Dr, date)
    
    
    # ------------------------------------------------------------------------
    # STEP 11: Deep percolation calculation
    # ------------------------------------------------------------------------
    
    if (a <= duration_past) {
      DP <- max(min(Ks * 10, (moisture_a[sensor_levels] - theta_fc) * sensor_spacing * 1000), 0)
    } else {
      DP <- max(min(Ks * 10, excess[sensor_levels]), 0)
    }
    
    Irr_vol_percolated[a] <- DP /1000 * (Area_irr)  
    
    
    # ------------------------------------------------------------------------
    # Update soil moisture for next time step (future data only)
    # ------------------------------------------------------------------------
    
    if (a >= duration_past) {
      if (a < length(ET0)) {
        moisture_new <- (soil_SAT_mm - deficit_final) / 1000 / sensor_spacing
        moisture_new <- ifelse(
          moisture_new > soil_SAT,
          soil_SAT,
          ifelse(moisture_new < theta_wp, theta_wp, moisture_new)
        )
        moisture_sensor[a + 1, ] <- moisture_new
        moisture_LOW[a + 1] <- moisture_new[sensor_levels]
      }
    }
    
    
    # ------------------------------------------------------------------------
    # Update summary table
    # ------------------------------------------------------------------------
    
    summary_table[a, which(colnames(summary_table) == "Date")] <- as.character(date[a])
    summary_table[a, which(colnames(summary_table) == "Hour")] <- hh[a]
    summary_table[a, which(colnames(summary_table) == "moisture_LOW")] <- moisture_LOW[a]
    summary_table[a, which(colnames(summary_table) == "ET0")] <- ET0[a]
    summary_table[a, which(colnames(summary_table) == "precipitation")] <- precipitation[a]
    summary_table[a, which(colnames(summary_table) == "P_eff")] <- prec_eff
    summary_table[a, which(colnames(summary_table) == "ponding")] <- ponding_net
    summary_table[a, which(colnames(summary_table) == "DP")] <- DP
    summary_table[a, paste0("moisture_", sensor_level_names)] <- moisture_a
    summary_table[a, paste0("Deficit meteo ", sensor_level_names)] <- deficit_meteo
    summary_table[a, which(colnames(summary_table) == "SAT_daily")] <- SAT_daily
    summary_table[a, which(colnames(summary_table) == "theta_fc")] <- theta_fc * level[3] * 1000
    summary_table[a, which(colnames(summary_table) == "TAW")] <- TAW
    summary_table[a, which(colnames(summary_table) == "RAW")] <- RAW
    summary_table[a, which(colnames(summary_table) == "DFadj")] <- DFadj
    summary_table[a, which(colnames(summary_table) == "kc")] <- kc
    summary_table[a, which(colnames(summary_table) == "ke")] <- ke
    summary_table[a, which(colnames(summary_table) == "ks")] <- ks
    summary_table[a, which(colnames(summary_table) == "Dr")] <- Dr[a]
    summary_table[a, which(colnames(summary_table) == "kc_correction")] <- kc_correction
    summary_table[a, which(colnames(summary_table) == "ETc")] <- ETc
    summary_table[a, which(colnames(summary_table) == "ETa")] <- ETa
    summary_table[a, paste0("Deficit final ", sensor_level_names)] <- deficit_final
    summary_table[a, which(colnames(summary_table) == "Irr_req")] <- Irr_req[a]
    summary_table[a, which(colnames(summary_table) == "Irr_vol")] <- Irr_vol[a]
    summary_table[a, which(colnames(summary_table) == "Irr_deficit")] <- Irr_vol_def[a]
    summary_table[a, which(colnames(summary_table) == "irr_given_mm")] <- irr_given_mm
    summary_table[a, which(colnames(summary_table) == "Irr_vol_percolated")] <- Irr_vol_percolated[a]
    summary_table[a, which(colnames(summary_table) == "moisture_new")] <- ifelse(a <= duration_past, NA, moisture_new)
    summary_table[a, paste0("moisture_new_", sensor_level_names)] <- if (a <= duration_past) {
      rep(NA, length(gsub("_sensor_raw", "_new", colnames(moisture_sensor_raw))))
    } else {
      moisture_new
    }
    
  }  # End of hourly time step loop
  
}  # End of crop loop


# ==============================================================================
# RESULTS EXPORT
# ==============================================================================

# ------------------------------------------------------------------------------
# Handle pre-season period (set irrigation to NA before sowing)
# ------------------------------------------------------------------------------

if ((s < h & (yday(date_past[1]) < s | yday(date_past[1]) > h)) | 
    (s > h & (yday(date_past[1]) < s & yday(date_past[1]) > h))) {
  summary_table$Irr_vol <- NA
  summary_table$Irr_deficit <- NA
  Date_irr[1, -1] <- NA
}


# ------------------------------------------------------------------------------
# Aggregate results for export
# ------------------------------------------------------------------------------

moisture_mean <- rowMeans(cbind(
  summary_table$moisture_15cm,
  summary_table$moisture_30cm,
  summary_table$moisture_45cm
))

hourly_variables_final <- data.frame(
  as.POSIXct(paste(summary_table$Date, summary_table$Hour, sep = "T"), format = "%Y-%m-%dT%H"),
  summary_table$Irr_vol,
  summary_table$Irr_deficit,
  (moisture_mean - q) / m * 100  # Reconvert to uncalibrated soil moisture
)

colnames(hourly_variables_final) <- list("TIME", "Irr_vol_m3", "Irr_deficit_m3", "Soil_moisture")


# ------------------------------------------------------------------------------
# Aggregate hourly data to daily values
# ------------------------------------------------------------------------------

summary_table$Date <- paste(summary_table$Date, summary_table$Hour, sep = "T")
summary_table$Date <- as.POSIXct(summary_table$Date, format = "%Y-%m-%dT%H")

daily_variables_final <- hourly_variables_final %>%
  mutate(TIME = floor_date(TIME, "day")) %>%
  group_by(TIME) %>%
  summarize(
    Irr_vol_m3 = ifelse(all(is.na(Irr_vol_m3)), NA, max(Irr_vol_m3, na.rm = TRUE)),
    Irr_deficit_m3 = ifelse(all(is.na(Irr_deficit_m3)), NA, max(Irr_deficit_m3, na.rm = TRUE)),
    Soil_moisture = mean(Soil_moisture, na.rm = TRUE)
  )

colnames(daily_variables_final) <- list("Date", "Irr_vol_m3", "Irr_deficit_m3", "Soil_moisture")


# ------------------------------------------------------------------------------
# Write output files
# ------------------------------------------------------------------------------

write.table(
  daily_variables_final,
  paste0(out_dir, "daily_variables_final.csv"),
  row.names = FALSE,
  col.names = TRUE,
  sep = ";"
)

write.table(
  hourly_variables_final,
  paste0(out_dir, "hourly_variables_final.csv"),
  row.names = FALSE,
  col.names = TRUE,
  sep = ";"
)

write.table(
  Date_irr,
  paste0(out_dir, "Irrigation_dates.csv"),
  row.names = FALSE,
  col.names = TRUE,
  sep = ";"
)

write.table(
  c(soil_SAT_sensor * 100, soil_WP_sensor * 100),
  paste0(in_dir, "soil_moisture_data/SM_extremes_previous_run.csv"),
  row.names = FALSE,
  col.names = FALSE
)

write.table(
  kc_correction,
  paste0(in_dir, "crop_data/kc_correction.csv"),
  row.names = FALSE,
  col.names = FALSE
)

write.table(
  summary_table,
  paste0(out_dir, "summary_table.csv"),
  row.names = FALSE,
  col.names = TRUE,
  sep = ";"
)



cat("Model completed successfully.\n")
