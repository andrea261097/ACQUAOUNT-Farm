# ==============================================================================
# IRRIGATION MODEL - FUNCTIONS
# ==============================================================================
# Description: Support functions for soil water balance model
# Based on FAO-56 methodology with dual crop coefficient approach
# ==============================================================================


# ==============================================================================
# PEDOTRANSFER FUNCTIONS
# ==============================================================================

PTFs_function <- function(P_sand, P_silt, P_clay, P_om, Pb, theta_fc_user, theta_wp_user, soil_SAT_user) {
  
  # --------------------------------------------------------------------------
  # Method: RAWLS et al. 2003
  # --------------------------------------------------------------------------
  
  x <- -0.837531 + 0.430183 * P_om
  y <- -1.40744 + 0.0661969 * P_clay
  z <- -1.51866 + 0.0393284 * P_sand
  
  theta_fcA <- (29.7528 + 10.3544 * (0.0461615 + 0.290955 * x - 0.0496845 * x^2 + 
                                       0.00704802 * x^3 + 0.269101 * y - 0.176528 * x * y + 0.0543138 * x^2 * y + 
                                       0.1982 * y^2 - 0.060699 * y^3 - 0.320249 * z - 0.0111693 * x^2 * z + 
                                       0.14104 * y * z + 0.0657345 * x * y * z - 0.102026 * y^2 * z - 0.04012 * z^2 + 
                                       0.160838 * x * z^2 - 0.121392 * y * z^2 - 0.0616676 * z^3)) / 100
  
  theta_wpA <- (14.2568 + 7.36318 * (0.06865 + 0.108713 * x - 0.0157225 * x^2 + 
                                       0.00102805 * x^3 + 0.886569 * y - 0.223581 * x * y + 0.0126379 * x^2 * y - 
                                       0.017059 * y^2 + 0.0135266 * x * y^2 - 0.0334434 * y^3 - 0.0535182 * z - 
                                       0.0354271 * x * z - 0.00261313 * x^2 * z - 0.154563 * y * z - 0.0160219 * x * y * z - 
                                       0.0400606 * y^2 * z - 0.104875 * z^2 + 0.0159857 * x * z^2 - 0.0671656 * y * z^2 - 
                                       0.0260699 * z^3)) / 100
  
  
  # --------------------------------------------------------------------------
  # Method: Zacharias et al., 2006
  # --------------------------------------------------------------------------
  
  if (P_sand < 66.5) {
    theta_sat <- 0.788 + 0.001 * P_clay - 0.263 * Pb
    ln_alpha <- -0.648 + 0.023 * P_sand + 0.044 * P_clay - 3.168 * Pb
    n <- 1.392 - 0.418 * P_sand^-0.024 + 1.212 * P_clay^-0.704
  } else {
    theta_sat <- 0.890 - 0.001 * P_clay - 0.276 * Pb
    ln_alpha <- -4.197 + 0.013 * P_sand + 0.076 * P_clay - 0.276 * Pb
    n <- -2.562 + 7 * 10^-9 * P_sand^4.004 + 3.750 * P_clay^-0.016
  }
  
  alpha <- exp(ln_alpha)
  m <- 1 - 1 / n
  
  theta_fcB <- theta_sat / (1 + (alpha * 33)^n)^m
  theta_wpB <- theta_sat / (1 + (alpha * 1500)^n)^m
  soil_AWCB <- theta_sat
  
  
  
  # --------------------------------------------------------------------------
  # Method: ROSETTA (Zhang and Schaap., 2017)
  # --------------------------------------------------------------------------
  
  
  vars <- c('sand', 'silt', 'clay', 'bd')
  suelos <- data.frame(#"soil"="Tunisia",
    #"hzdept_r"=0,
    #"hzdepb_r"=100,
    "sand"= P_sand,
    "silt"= P_silt,
    "clay"= P_clay,
    "bd"= Pb,
    "Corg" = P_om)
  r <- ROSETTA(suelos, vars = vars, v = '3')
  # flatten to data.frame
  
  vg <- KSSL_VG_model(VG_params = r, phi_min = 10^-3, phi_max=10^6)
  
  # extract VWC at specific matric potentials (kPa)
  d <- data.frame(
    #soil = r$soil,
    sat = vg$VG_function(0),
    fc = vg$VG_function(33),
    pwp = vg$VG_function(1500)
  )
  # flatten to data.frame
  theta_fcC=d$fc
  theta_wpC=d$pwp
  soil_AWCC=d$sat
  
  
 
  # --------------------------------------------------------------------------
  # Method: EUPTFv2, Szabo et al 2021
  # --------------------------------------------------------------------------
  
  soil <- data.frame("DEPTH_M"=30,
                     "USSAND"=P_sand,
                     "USSILT"=P_silt,
                     "USCLAY"=P_clay,
                     "OC"=P_om,
                     "BD"=Pb)
  # SATURATION -------------
  load(paste(wd,"/Functions/Pedotransfer_functions/THS_PTF03.rdata",sep=""))
  soil_AWCD <- predict(THS_PTF03,
                       data=soil,
                       type = "response",
                       num.threads = detectCores()-1)
  soil_AWCD <- soil_AWCD$prediction
  
  # FIELD CAPACITY ---------
  load(paste(wd,"/Functions/Pedotransfer_functions/FC_PTF02.rdata",sep=""))
  theta_fcD <- predict(FC_PTF02,
                       data=soil,
                       type = "response",
                       num.threads = detectCores()-1)
  theta_fcD <- theta_fcD$predictions
  
  # WILTING POINT ---------
  load(paste(wd,"/Functions/Pedotransfer_functions/WP_PTF02.rdata",sep=""))
  theta_wpD <- predict(WP_PTF02,
                       data=soil,
                       type = "response",
                       num.threads = detectCores()-1)
  theta_wpD <- theta_wpD$predictions
  
  

  
  # --------------------------------------------------------------------------
  # Ensemble mean of PTFs
  # --------------------------------------------------------------------------
  
  theta_fcE=mean(c(theta_fcA,theta_fcB,theta_fcC,theta_fcD))
  theta_wpE=mean(c(theta_wpA,theta_wpB,theta_wpC,theta_wpD))
  soil_AWCE=mean(c(soil_AWCB,soil_AWCC,soil_AWCD))
  
  PTF <- data.frame(
    "SAT" = soil_AWCE,
    "FC" = theta_fcE,
    "WP" = theta_wpE
  )
  
  
  # --------------------------------------------------------------------------
  # Calculate error with user-provided values
  # --------------------------------------------------------------------------
  
  err_fc <- abs(theta_fc_user - theta_fcE) / theta_fcE
  err_wp <- abs(theta_wp_user - theta_wpE) / theta_wpE
  err_AWC <- abs(soil_SAT_user - soil_AWCE) / soil_AWCE
  
  
  # --------------------------------------------------------------------------
  # Select final PTF values
  # If error > 10%, use user values; otherwise use PTF ensemble mean
  # --------------------------------------------------------------------------
  
  if (err_fc > 0.1 | err_wp > 0.1 | err_AWC > 0.1) {
    theta_fc <- theta_fc_user
    theta_wp <- theta_wp_user
    soil_SAT <- soil_SAT_user
  } else {
    theta_fc <- theta_fcE
    theta_wp <- theta_wpE
    soil_SAT <- soil_AWCE
  }
  
  
  # Ensure field capacity doesn't exceed saturation
  if (theta_fc > soil_SAT) {
    soil_SAT <- theta_fc
  }
  
  WRP <- data.frame(
    "SAT" = soil_SAT,
    "FC" = theta_fc,
    "WP" = theta_wp
  )
  
  return(WRP)
}


# ==============================================================================
# DATA AGGREGATION
# ==============================================================================

data_aggregation_function <- function(moisture_sensor, date_past, hh_past, temp_past, 
                                      precipitation_past, sw_rad_past, RH_past, wind_past, 
                                      irr_given_vol_original, date_fut, hh_fut, temp_fut, 
                                      precipitation_fut, sw_rad_fut, RH_fut, wind_fut,
                                      duration_fut, sensor_levels) {
  
  # Historical data
  past_data <- cbind(
    moisture_sensor,
    data.frame(
      date = date_past,
      hh = hh_past,
      temp = temp_past,
      prec = precipitation_past,
      SW = sw_rad_past,
      RH = RH_past,
      wind = wind_past,
      irrigation = irr_given_vol_original
    )
  )
  
  # Future data (moisture is NA, to be predicted)
  moisture_sensor_fut <- data.frame(matrix(NA, nrow = length(date_fut), ncol = 3))
  colnames(moisture_sensor_fut) <- colnames(moisture_sensor)
  
  future_data <- cbind(
    moisture_sensor_fut,
    data.frame(
      date = date_fut,
      hh = hh_fut,
      temp = temp_fut,
      prec = precipitation_fut,
      SW = sw_rad_fut,
      RH = RH_fut,
      wind = wind_fut,
      irrigation = rep(0, length(duration_fut))
    )
  )
  
  dataset <- rbind(past_data, future_data)
  
  return(dataset)
}




# ==============================================================================
# REFERENCE EVAPOTRANSPIRATION (FAO-56)
# ==============================================================================

FAO_ET0_function <- function(lat, temp, sw_rad, wind, RH, elev, date, hh, 
                             Units_T, Units_Rs, Units_wind, Units_RH, Units_z) {
  
  Units_T <- "°C"
  Units_Rs <- "W/m2"
  Units_wind <- "m/s"
  Units_RH <- "%"
  Units_z <- "m"
  
  
  # --------------------------------------------------------------------------
  # Convert climate data to correct units
  # --------------------------------------------------------------------------
  
  units(temp) <- Units_T
  units(temp) <- "°C"
  temp <- drop_units(temp)
  
  units(sw_rad) <- Units_Rs
  units(sw_rad) <- "MJ/m2/h"
  sw_rad <- drop_units(sw_rad)
  
  units(wind) <- Units_wind
  units(wind) <- "m/s"
  wind <- drop_units(wind)
  
  units(elev) <- Units_z
  units(elev) <- "m"
  elev <- drop_units(elev)
  
  RH <- as.numeric(unlist(RH))
  units(RH) <- Units_RH
  units(RH) <- "%"
  RH <- drop_units(RH)
  
  
  # --------------------------------------------------------------------------
  # ET0 calculation
  # --------------------------------------------------------------------------
  
  # Slope of saturation vapor pressure curve (kPa/°C)
  delta <- (4098 * (0.6108 * exp(17.27 * temp / (temp + 237.3)))) / (temp + 237.3)^2
  
  # Atmospheric pressure (kPa)
  Patm <- 101.3 * ((293 - 0.0065 * elev) / 293)^5.26
  
  # Psychrometric constant (kPa/°C)
  psy_constant <- 0.000665 * Patm
  
  DT <- (delta / (delta + psy_constant * (1 + 0.34 * wind)))
  PT <- (psy_constant) / (delta + psy_constant * (1 + 0.34 * wind))
  TT <- (37 / (temp + 273)) * wind
  
  # Saturation vapor pressure (kPa)
  es_Tx <- 0.6108 * exp((17.27 * temp) / (temp + 237.3))
  
  # Actual vapor pressure (kPa)
  ea <- RH / 100 * (es_Tx)
  
  
  # --------------------------------------------------------------------------
  # Solar radiation calculations
  # --------------------------------------------------------------------------
  
  j <- as.numeric(strftime(date, format = "%j"))
  dr <- 1 + 0.033 * cos(2 * pi * j / 365)
  solar_decli <- 0.409 * sin((2 * pi * j / 365) - 1.39)
  lat_rad <- (pi / 180) * lat
  ws <- acos(-tan(lat_rad) * tan(solar_decli))
  N <- 24 / pi * ws
  
  Lz <- 350
  Lm <- 355
  b <- 2 * pi * (j - 81) / 364
  Sc <- 0.1645 * sin(2 * b) - 0.1255 * cos(b) - 0.025 * sin(b)
  t <- hh + 0.5
  w <- pi / 12 * ((t + 0.06667 * (Lz - Lm) + Sc) - 12)
  w1 <- w - pi * 1 / 24
  w2 <- w + pi * 1 / 24
  Gsc <- 0.0820
  
  # Extraterrestrial radiation (MJ/m²/h)
  ra <- (12 * (60) / pi) * Gsc * dr * (((w2 - w1) * sin(lat_rad) * sin(solar_decli)) + 
                                         (cos(lat_rad) * cos(solar_decli) * (sin(w2) - sin(w1))))
  ra[ra < 0] <- 0
  
  rso <- (0.75 + (2 * 10^-5) * elev) * ra
  Rns <- (1 - 0.23) * sw_rad
  sigma <- 4.903 * 10^-9 / 24
  
  ratio <- sw_rad / rso
  ratio[w > ws | w < -ws] <- 0.5
  ratio[ratio >= 1] <- 1
  
  # Net longwave radiation (MJ/m²/h)
  Rnl <- sigma * (temp + 273.15)^4 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * ratio - 0.35)
  
  # Net radiation (MJ/m²/h)
  Rn <- Rns - Rnl
  
  # Soil heat flux (MJ/m²/h)
  G <- array(0.1 * Rn, dim = length(j))
  G[w > ws | w < -ws] <- 0.5 * Rn[w > ws | w < -ws]
  
  Rng <- 0.408 * (Rn - G)
  ETrad <- DT * Rng
  ETwind <- PT * TT * (es_Tx - ea)
  ET0 <- ETwind + ETrad
  ET0[ET0 < 0] <- 0
  ET0 <- as.numeric(ET0)
  
  
  # --------------------------------------------------------------------------
  # Open water evaporation
  # --------------------------------------------------------------------------
  
  VPD <- es_Tx - ea
  Ba <- 6.43 * (0.5 + (0.54 * wind * 0.75)) * VPD
  
  emiss_water <- 0.92
  Rls <- (Rnl * emiss_water) - (0.0000000567 * emiss_water * ((temp + 273.15)^4))
  Qt <- array(0, dim = length(temp))
  
  for (i in 1:length(j)) {
    if (j[i] < 183) {
      Qt[i] <- (0.5 * Rns + 0.8 * Rls)
    } else {
      Qt[i] <- (0.5 * Rns + 1.3 * Rls)
    }
  }
  
  OWET <- ((0.408 * 0.0864 * ((Rns + Rls) - Qt) * delta) + (Ba * psy_constant)) / 
    (delta + psy_constant)
  OWET[OWET < 0] <- 0
  OWET <- as.numeric(OWET)
  
  Evapotranspiration <- list("ET0" = ET0, "OWET" = OWET)
  
  return(Evapotranspiration)
}







# ==============================================================================
# ROOTING DEPTH DEVELOPMENT
# ==============================================================================

RD_increment <- function(s, h, day, L12, RD, L12_den, type) {
  
  if (type == 1) {
    # Annual crops
    daily_rooting_depth <- 
      ifelse(s < h & s <= day & (s + L12 - 1) > day, 
             ((RD[2] - RD[1]) / (L12_den)) * (day - s + 1) + RD[1], 0) +
      ifelse(s < h & (s + L12 - 1) <= day & day <= h, RD[2], 0) +
      ifelse(s > h & s <= day & (s + L12 - 1) > day, 
             ((RD[2] - RD[1]) / (L12_den)) * (day - s + 1) + RD[1], 0) +
      ifelse(s > h & (h - (LGP - L12)) > day, 
             ((RD[2] - RD[1]) / (L12_den)) * (day - s + 1) + RD[1], 0) +
      ifelse(s > h & (s + L12 - 1) <= day, RD[2], 0) +
      ifelse(s > h & h >= day & (h - (LGP - L12)) <= day, RD[2], 0)
    
  } else if (type == 2) {
    # Perennial crops
    daily_rooting_depth <- RD[2]
  }
  
  return(daily_rooting_depth)
}


# ==============================================================================
# EFFECTIVE PRECIPITATION
# ==============================================================================

P_eff <- function(sensor_spacing, soil_SAT, moisture_a, ET0, a, precipitation, irr_given_mm) {
  
  deficit <- (soil_SAT - moisture_a[1]) * 1000 * sensor_spacing
  
  # FAO-56 method
  prec_eff <- ifelse(
    (precipitation[a] + irr_given_mm) <= 0.2 * ET0[a],
    0,
    ifelse((precipitation[a] + irr_given_mm) < deficit, 
           precipitation[a] + irr_given_mm, 
           deficit)
  )
  
  return(prec_eff)
}


# ==============================================================================
# CROP COEFFICIENT (Kc)
# ==============================================================================

crop_coeff <- function(s, h, day, L1, L12, L123, L2_den, L4_den, LGP, 
                       kc_in, kc_mid, kc_end) {
  
  # Crop developing within a single year
  kc <- ifelse(h > s & day >= s & day < (s + L1 - 1), kc_in, 0) +
    ifelse(h > s & day >= (s + L1 - 1) & day < (s + L12 - 1), 
           kc_in + ((day - (s + L1) + 1) / L2_den) * (kc_mid - kc_in), 0) +
    ifelse(h > s & day >= (s + L12 - 1) & day < (s + L123 - 1), kc_mid, 0) +
    ifelse(h > s & day >= (s + L123 - 1) & day <= h, 
           kc_mid + ((day - (s + L123) + 1) / L4_den) * (kc_end - kc_mid), 0) +
    
    # Crop developing across two years
    # Stage 1
    ifelse(h < s & day >= s & day < (s + L1 - 1), kc_in, 0) +
    ifelse(h < s & day < (h - (LGP - L1)), kc_in, 0) +
    # Stage 2
    ifelse(h < s & day >= (s + L1 - 1) & day < (s + L12 - 1), 
           kc_in + ((day - (s + L1) + 1) / L2_den) * (kc_mid - kc_in), 0) +
    ifelse(h < s & day >= (h - (LGP - L1)) & day < (h - (LGP - L12)), 
           kc_in + ((366 - s - L1 + day) / L2_den) * (kc_mid - kc_in), 0) +
    # Stage 3
    ifelse(h < s & day >= (s + L12 - 1) & day < (s + L123 - 1), kc_mid, 0) +
    ifelse(h < s & day >= (h - (LGP - L12)) & day < (h - (LGP - L123)), kc_mid, 0) +
    # Stage 4
    ifelse(h < s & day >= (s + L123 - 1) & day <= (s + LGP - 1), 
           kc_mid + ((day - (s + L123) + 1) / L4_den) * (kc_end - kc_mid), 0) +
    ifelse(h < s & day >= (h - (LGP - L123)) & day <= h, 
           kc_mid + ((366 - s - L123 + day) / L4_den) * (kc_end - kc_mid), 0)
  
  return(kc)
}


# ==============================================================================
# WATER STRESS COEFFICIENT (Ks)
# ==============================================================================

ws_coeff <- function(TAW, Dr, a, RAW) {
  
  ks <- (TAW - Dr[a]) / (TAW - RAW)
  ks[is.nan(ks)] <- 0
  ks[ks < 0] <- 0
  ks[ks > 1] <- 1
  
  return(ks)
}


# ==============================================================================
# CROP COEFFICIENT CORRECTION
# ==============================================================================

kc_corr_function <- function(a, kc_corr, kc_correction, irr_given_vol, moisture_LOW, 
                             duration_past, moisture_sensor, sensor_levels) {
  
  
  #=======================================================================================================
  ####### find natural fluctuations (standard deviation) of moisture_LOW levels far from irrigation events ===========================================================================
  
  irrigation_past<-irr_given_vol[1:duration_past]
  
  #Find all the time steps far from irrigation event 
  x_prev<-NULL
  for(pos in 1:duration_past){
    if (pos > 24 && sum(irrigation_past[(pos-24):pos]) == 0) {     # if there is no irrigation in the 24 h before position x, then pick up the 5 last 5 values
      x <- c((pos - 4):(pos-1),x_prev)
    } else {
      x <-x_prev
    }
    x_prev<-x
  }
  x<-sort(unique(x))
  
  if(length(x)<10){
    
    #cat("Irrigation events are happening for most (or all) the time period, so it's not possible to detect fluctuations of moisture_LOW levels")
    sd_base=sd(moisture_LOW[irrigation_past==0],na.rm = TRUE)
    
  }else{
    
    sd_base=sd(moisture_LOW[x]) 
    
  }
  
  
  
  #==================================================================================================================
  # Perform the assessment of kc correction
  
  if( a <= duration_past ){
    
    kc_correction=kc_corr
    
  } else if ( a == (duration_past+1) ) {
    
    if(sum(irrigation_past[c(duration_past-24):duration_past])>0){ # if there is irrigation in the last 24 hours
      
      # Find positions of irrigation in the 24h before present
      irr_pos <- which(irrigation_past>0)
      irr_pos<- min(irr_pos[irr_pos>=(duration_past-24)])
      irr_last <- max(irr_pos[irr_pos>=(duration_past-24)])  # ULTIMO istante irrigato nelle ultime 24h
      
      a0=ifelse((a-24) < irr_pos, irr_pos,a-24) #if in the instant "a" irrigation happened less than 24 hours before, then take the irrigation position
      
      # --- finestre brevi post-irrigazione ---
      Aend_low  <- min(irr_last + 5, duration_past)  # 5 ore per check irrigazione "bassa"
      Aend_perc <- min(irr_last + 7, duration_past)  # 7 ore per segnale percolazione (puoi mettere 10-12 se serve)
      
      #do the ratio between the standard deviation of moisture_LOW of the last 24 hours over the base one (with natual fluctuations)
      ratio_sd=sd(moisture_LOW[a0:a])/sd_base
      ratio_sd=ifelse(is.na(ratio_sd),0,ratio_sd)
      
      #do the ratio between the standard deviation of moisture_LOW of a short window after irrigation over the base one
      ratio_sd=sd(moisture_LOW[irr_last:Aend_perc], na.rm=TRUE)/sd_base
      ratio_sd=ifelse(is.na(ratio_sd) | is.infinite(ratio_sd),0,ratio_sd)
      
      ## If there is percolation
      if(ratio_sd>2){ 
        kc_correction=kc_corr*0.9
      } else{ ## No percolation
        kc_correction=kc_corr
      }
      
      # check "low irrigation" subito dopo l'ultima irrigazione (finestra breve, non 24h)
      # (uso MAX perché più realistico: se l'acqua arriva, almeno un picco lo vedi)
      val_post <- max(as.matrix(moisture_sensor[irr_last:Aend_low, c(1:(sensor_levels-1))]), na.rm=TRUE)
      
      if(val_post < (0.7*theta_fc) ){
        kc_correction=kc_correction*1.1   # nota: moltiplico sulla correzione già fatta (non la resetto)
      }
      
    }else{
      kc_correction=kc_corr
    }
    
    # Check that kc_correction doesn't diverge too much (or too big or too low)
    if(kc_correction<(0.5)){
      kc_correction=0.5
    } else if(kc_correction>(1.5)){
      kc_correction=1.5
    }else{
      kc_correction = kc_correction
    }
    
  } else if ( a > (duration_past+1)){
    kc_correction = kc_correction
  }
  
  
  
  return(kc_correction)
  
}


# ==============================================================================
# DEFICIT CALCULATION
# ==============================================================================

def_function <- function(deficit_meteo, AWC_limit, ETa, DP) {
  
  deficit_final <- ifelse(
    deficit_meteo >= 0 & deficit_meteo <= AWC_limit,
    deficit_meteo + ETa + DP,
    ifelse(deficit_meteo > AWC_limit, AWC_limit, deficit_meteo + ETa + DP)
  )
  
  return(deficit_final)
}

def_lim_function <- function(TAW, ks, RAW) {
  
  deficit_final_limit <- TAW - ks * (TAW - RAW)
  
  return(deficit_final_limit)
}


# ==============================================================================
# IRRIGATION REQUIREMENT
# ==============================================================================

irrigation_function <- function(Dr, a) {
  
  if (Dr[a] > 0) {
    irrigation <- Dr[a]
  } else {
    irrigation <- 0
  }
  
  Irr_req[a] <- irrigation
  
  return(Irr_req[a])
}


# ==============================================================================
# IRRIGATION VOLUME
# ==============================================================================

Irr_vol_function <- function(irr_method, Irr_req, a, Area_irr) {
  
  # Irrigation efficiency by method
  if (irr_method == "surface") {
    eff_irr <- 0.65
  } else if (irr_method == "sprinkler") {
    eff_irr <- 0.75
  } else if (irr_method == "drip") {
    eff_irr <- 0.90
  } else if (irr_method == "subterranean") {
    eff_irr <- 0.95
  }
  
  Irr_vol[a] <- (Irr_req[a] / eff_irr) * (Area_irr) / 1000
  
  return(Irr_vol[a])
}




# ==============================================================================
# IRRIGATION DEFICIT
# ==============================================================================

Irr_def_function <- function(day,s,L12,L123,irr_def_array,h, Irr_vol, a, irr_def,RAW, Dr,irr_threshold_array){
  
  if(s < h){
    # ---- within year case (s < h) ----
    if(day > s & day < s + L12){
      irr_def <- irr_def_array[1]
    } else if(day > s + L12 & day < s + L123){
      irr_def <- irr_def_array[2]
    } else if(day > s + L123 & day < h){
      irr_def <- irr_def_array[3]
    } else {
      irr_def <- 1
    }
    
  } else {
    # ---- cross-year case (s > h) ----
    
    if(day > s | day < h){
      if( (day > s & day < s + L12) |
          (day < h & day < (s + L12 - 365)) ){
        irr_def <- irr_def_array[1]
      } else if( (day > s + L12 & day < s + L123) |
                 (day < h & day < (s + L123 - 365)) ){
        irr_def <- irr_def_array[2]
      } else {
        irr_def <- irr_def_array[3]
      }
    } else {
      irr_def <- 1
    }
  }
  
  
  

  Irr_vol_def[a] <- Irr_vol[a]*irr_def  #deficit irrigation volume

  return(Irr_vol_def[a])
}


# ==============================================================================
# IRRIGATION RECOMMENDATIONS
# ==============================================================================

irr_suggestion_function <- function(RAW,irr_threshold_array,a,duration_past,Dr,date){
  
  #Irrigation scenario and thresholds
  Dr_lim_early=RAW * (1 + irr_threshold_array[1]) #Early threshold (optimal irrigation). E.g.: Dr_lim = RAW * (1-0.5)= 0.5 * W
  Dr_lim_late=RAW * (1 + irr_threshold_array[2]) #Late threshold (moderate water stress). E.g: Dr_lim = RAW * (1+0)= RAW
  Dr_lim_limit=RAW * (1 + irr_threshold_array[3]) #Limit threshold (high water stress). E.g: Dr_lim = RAW * (1+0.5)= 1.5* RAW
  
  if (a>duration_past){
    if ( is.na(Date_irr$Early)==TRUE & mean(Dr[(a-5):a]) > Dr_lim_early){
      Date_irr$Early=date[a]
      
    } else if ( is.na(Date_irr$Late)==TRUE & mean(Dr[(a-5):a]) > Dr_lim_late){
      Date_irr$Late=date[a]
      
    } else if ( is.na(Date_irr$Limit)==TRUE & mean(Dr[(a-5):a]) > Dr_lim_limit){
      Date_irr$Limit =date[a]
    }
  }
  
  return(Date_irr)
}

