# =========================================================================
# PROJECT CONFIGURATION 
# =========================================================================
# Description: Centralized configuration file providing spatial coordinates 
#              and station IDs for data pipelines.
# =========================================================================

#Load library
library(tidyverse)

# -------------------------------------------------------------------------
# 1. GLOBAL TIMEFRAME & API SETTINGS
# -------------------------------------------------------------------------
START_DATE      <- "19900101"

# Static cutoff date (YYYYMMDD format) for your project snapshot
CUTOFF_DATE     <- format(Sys.Date(), "%Y%m%d")

# NASA POWER variables to request
NASA_PARAMETERS <- "T2M,T2M_MAX,T2M_MIN,PRECTOTCORR,RH2M,PS,WS2M"
