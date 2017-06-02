#### The Aardvark permeameter ####
# ============================== #

# author: Jan Knappe
# jan.knappe@gmail.com

library(tidyverse)
library(lubridate)
library(readxl)

### DEFINE FUNCTIONS ###
# -------------------- #

# calculatePercFactor
# to calculate the geometry specific percolation factor for each percolation test according to Aardvark manual
calculatePercFactor = function(holeDia, holeDepth, tableHeight, gwDepth) {
  waterHeight = 9 + 0.003 * (holeDepth + tableHeight - 18.5)
  waterRatio = 2 * waterHeight / holeDia
  gwRatio = (gwDepth - holeDepth + waterHeight) / waterHeight
  percFactor = ifelse(gwRatio > 3 | is.na(gwDepth), 
                      60 * 24 * ((log( waterRatio + sqrt( waterRatio^2 + 1 ) ) - sqrt( waterRatio^2 + 1 ) / waterRatio + 1 / waterRatio) / (2 * pi * waterHeight^2)),
               ifelse(gwRatio <= 3 & gwRatio >= 1,
                      60 * 24 * ( (log(waterRatio) / (1/6 + gwRatio/3)) / (2 * pi * waterHeight^2)),
               ifelse(gwRatio < 1, 
                      60 * 24 * ( (log(waterRatio) / (gwRatio + gwRatio^2/2)) / (2 * pi * waterHeight^2)),
               NA)))
  return(percFactor)
}


### DATA IMPORT AND MUNGING ###
# --------------------------- #

# read site metadata
siteMeta =
  read_excel(
    "data/metadata-sites.xlsx",
    col_names = TRUE,
    na = "NA",
    col_types = c("text", "text", "text", "numeric")
  ) %>%
  mutate(siteNameLower = tolower(siteName))

# read test metadata
percMeta =
  read_excel(
    "data/metadata-perctests.xlsx",
    col_names = TRUE,
    na = "NA",
    col_types = c("text", "date", "text", "numeric", "numeric", "numeric")
  ) %>%
  # then join metadata from siteMeta
  left_join(siteMeta, by = "siteName") %>%
  # then create human readable unique percTestID
  mutate(percTestID = paste(siteID, siteCounter, sep = "-")) %>%
  #then calculate percolation factor
  mutate(percFactor = calculatePercFactor(holeDia_cm, holeDepth_cm, tableHeight_cm, gwDepth_cm)) %>%
  # then remove redundant columns
  select(-siteCounter,-siteNameLower)


# create list of all percdata files
percFiles = list.files(pattern = '^percdata', recursive = TRUE)

# find Excel percdata files
percData =
  # then read into R
  lapply(percFiles,
         function(x)
           read_excel(
             x,
             col_names = TRUE,
             na = "NA",
             col_types = c("text", "numeric")
           )) %>%
  # then parse files together
  bind_rows(.id = "percID")

# take vector of available testdata files
percTidy = percFiles %>%
  # then convert to tibble
  as_tibble %>%
  # then split the filename into columns
  separate(
    value,
    into = c("dummy1", "siteNameLower", "date", "dummy2"),
    sep = "-",
    remove = TRUE
  ) %>%
  separate(dummy2, into = c("siteCounter", "fileFormat")) %>%
  # then drop unnecessary columns
  select(-dummy1,-fileFormat) %>%
  # then convert date columns and add sequential ID
  mutate(date = as_date(date, format = "%y%m%d"),
         percID = seq.int(nrow(.))) %>%
  # then join metadata from siteMeta
  left_join(siteMeta, by = "siteNameLower") %>%
  # then create human readable unique percTestID
  mutate(percTestID = paste(siteID, siteCounter, sep = "-"))




### PLOTS ###
# --------- #

# influence of geometry on percFactor
ggplot(percMeta, aes((holeDepth_cm+tableHeight_cm)/100, percFactor*10)) + 
  geom_point(aes(size=holeDia_cm)) +
  theme_gray() +
  theme(legend.position = "bottom",
        plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Dependency of normalized percolation factor on trial geometry") +
  xlab("distance from water reservoir to bottom of trial hole [m]") +
  ylab("normalized percolation factor [m/l]") +
  scale_size_continuous(name = "Test hole Diameter:", 
                  labels = c("10.0 cm", "10.5 cm", "11.0 cm", "11.5 cm", "12.0 cm"))





