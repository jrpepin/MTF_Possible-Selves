#-------------------------------------------------------------------------------
# MTF MENTAL HEALTH PROJECT
# MH_01_measures and sample.R
# Joanna R. Pepin & Melissa Milkie
#-------------------------------------------------------------------------------

# Project Environment ----------------------------------------------------------
## The MH_00-1_setup and packages.R script should be run before this script

# source(MH_00-1_setup and packages.R)

# DATA -------------------------------------------------------------------------
load(paste0(dataDir, "/mtf_form2.Rda"))

## Load the data and create a new df containing only the variables of interest.  
data <- select(mtf_V2, V5, ARCHIVE_WT, V1, V13, TABLET,     # Survey variables
               V2239, V2312,                                # Project specific
               V2151, V2150, V2164,                         # Demographic (V2165 - momemp ! 2022)
               V2169, V2155, V2156)           

## Rename Variables
data <- dplyr::rename(data,      
                      wt7611   = V5,     wt1222   = ARCHIVE_WT,  
                      gdsp     = V2312,  getmar   = V2239,
                      year     = V1,     gender   = V2150,  
                      raceeth  = V2151,  region   = V13,
                      momed    = V2164,  religion = V2169,
                      father   = V2155,  mother = V2156)

## Sample size
count(data)

## Create crosswalk of survey year and ICPSR Study ID 
studyid    <- c( 7927,  7928,  7929,  7930,
                 7900,  9013,  9045,  8387,  8388,
                 8546,  8701,  9079,  9259,  9397,
                 9745,  9871,  6133,  6367,  6517,
                 6716,  2268,  2477,  2751,  2939,
                 3184,  3425,  3753,  4019,  4264,
                 4536, 20022, 22480, 25382, 28401,
                 30985, 34409, 34861, 35218, 36263,
                 36408, 36798, 37182, 37416, 37841,
                 38156, 38503, 38882)

surveyyear <- c(1976, 1977, 1978, 1979,
                1980, 1981, 1982, 1983, 1984,
                1985, 1986, 1987, 1988, 1989,
                1990, 1991, 1992, 1993, 1994,
                1995, 1996, 1997, 1998, 1999,
                2000, 2001, 2002, 2003, 2004,
                2005, 2006, 2007, 2008, 2009,
                2010, 2011, 2012, 2013, 2014,
                2015, 2016, 2017, 2018, 2019,
                2020, 2021, 2022)

Xwalk <- data.frame(surveyyear, studyid)
