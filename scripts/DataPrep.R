#This is where avalanche danger rating data prep is happening.

# Save working files
write.csv(TP_recode,
          file = './data/TP_recode.csv',
          row.names = FALSE,
          na = "")
write.csv(probRecode,
          file = './data/TP_20251005.csv',
          row.names = FALSE,
          na = "")
#load required packages
library(dplyr)
library(lubridate)
library(cnfacML)

#show working directory. redefine with setwd() if needed
getwd()

#load data
TP_working <- read.csv(
  file = './data/TP_20251005.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)

#------- Original records (not needed unless we need to revisit original data)---------
TP_recode <- read.csv(
  file = './data/TP_recode.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)
TP_old <- read.csv(
  file = './data/TPass_2008_2024.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)
TP_RA <- read.csv(
  file = './data/Reass_2021_2025.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)
TP_2425 <- read.csv(
  file = './data/TPass_2024_2025.csv',
  header = TRUE,
  stringsAsFactors = FALSE
)[1:150, ]
#-------------------------------------------------------------------------------------------------#

#Format column names
names(TP_old)[7] <- "P2"
names(TP_old)[3:5] <- c("alp.pred", "tl.pred", "btl.pred")
names(TP_old)[6:9] <- c("p1.pred", "p2.pred", "p3.pred", "ac.pred")

names(TP_RA)[1] <- "Date"
names(TP_RA)[5:10] <- c("alp.pred", "tl.pred", "btl.pred", "alp.ob", "tl.ob", "btl.ob")
names(TP_RA)[12:15] <- c("p1.pred", "p2.pred", "p1.ob", "p2.ob")

names(TP_2425)[4] <- "Date"
names(TP_2425)[8:10] <- c("btl.pred", "tl.pred", "alp.pred")

#Get each dataset using consistent date format. Abandon timestamp on forecasts. ** This uses the dateHelper function included in the cnfacML package
TP_old$Date <- dateHelper(TP_old$Date)
TP_2425$Date <- dateHelper(TP_2425$Date)
TP_RA$Date <- dateHelper(TP_RA$Date)

old_keep <- TP_old  %>%
  select(Date, p1.pred, p2.pred, p3.pred, ac.pred, alp.pred, tl.pred, btl.pred)

ra_keep  <- TP_RA   %>%
  select(Date, `p1.ob`, `p2.ob`, `alp.ob`, `tl.ob`, `btl.ob`)

new_keep <- TP_2425 %>%
  select(Date, alp.pred, tl.pred, btl.pred)
new_keep[,2] <- as.character(new_keep[,2])
new_keep[,3] <- as.character(new_keep[,3])
new_keep[,4] <- as.character(new_keep[,4])

# Join tables
preds <- bind_rows(old_keep, new_keep) %>%
  arrange(Date)
TP_merged <- merge(preds, ra_keep, by = "Date", all = TRUE)

#rearrange columns
names(TP_merged)
TP_merged <- TP_merged[, c(1, 6:8, 11:13, 2:5, 9:10)]
View(TP_merged[, c(1, 6:8, 11:13, 2:5, 9:10)])

#rename observations in consistent format
alp.unique <- sort(unique(TP_merged[,2]))
tl.unique <- sort(unique(TP_merged[,3]))
btl.unique <- sort(unique(TP_merged[,4]))

ao <- sort(unique(TP_merged[,5]))
to_ob <- sort(unique(TP_merged[,6]))
bo <- sort(unique(TP_merged[,7]))

pred.unique <- sort(unique(c(alp.unique, tl.unique, btl.unique)))
ob.unique <- sort(unique(c(ao,to_ob,bo)))

danUn <- sort(unique(c(pred.unique, ob.unique)))
correspond <- c(NA, 1, 2, 3, 4, 3, 3, 5, 4, 4, 1, 1, 2, 2, NA)

TP_recode <- TP_merged %>%
  mutate(across(2:7, ~ as.character(.x) %>%
                  na_if("") %>%             # blanks -> NA
                  na_if("No Rating") %>%    # "No Rating" -> NA
                  recode(
                    "1"=1, "2"=2, "3"=3, "4"=4,
                    "C"=3, "Considerable"=3,
                    "Extreme"=5,
                    "H"=4, "High"=4,
                    "L"=1, "Low"=1,
                    "M"=2, "Moderate"=2,
                    .default = NA_real_
                  )))

p1R <- sort(unique(TP_recode[,"p1.pred"]))
p2R <- sort(unique(TP_recode[,"p2.pred"]))
p3R <- sort(unique(TP_recode[,"p3.pred"]))
acR <- sort(unique(TP_recode[,"ac.pred"]))

p1o <- sort(unique(TP_recode[,"p1.ob"]))
p2o <- sort(unique(TP_recode[,"p2.ob"]))

problemsRaw <- sort(unique(c(p1R, p2R, p3R, acR)))
obRaw <- sort(unique(c(p1o, p2o)))

probRecode <- TP_recode %>%
  mutate(across(8:13, ~ {
    v <- na_if(as.character(.x), "")
    dplyr::case_match(
      v,
      "Cornice.png"             ~ "C",
      "DeepPersistentSlabs.png" ~ "DS",
      "Glide.png"               ~ "G",
      "LooseDry.png"            ~ "DL",
      "LooseWet.png"            ~ "WL",
      "PersistentSlabs.png"     ~ "PS",
      "Springtim-high-low.png"  ~ "Spring",
      "StormSlabs.png"          ~ "SS",
      "WetSlabs.png"            ~ "WtS",
      "WindSlabs.png"           ~ "WdS",
      "Deep Slab" ~ "DS",
      "Dry Loose" ~ "DL",
      "Glide" ~ "G",
      "None" ~ "",
      "Persistent Slab" ~ "PS",
      "Storm Slab" ~ "SS",
      "Wet Loose" ~ "WL",
      "Wet Slab" ~ "WtS",
      "Wind Slab" ~ "WdS",
      "icon-avalanche.gif" ~ "",
      "icon-bear.png"      ~ "",
      "icon-extreme.gif"   ~ "",
      "icon-low.gif"       ~ "",
      "icon-moderate.gif"  ~ "",
      "icon-nodata.gif"    ~ "",
      "icon-obs.gif"       ~ "",
      "icon-special.gif"   ~ "",
      "icon-weather.gif"   ~ "",
      "NOICON.gif"         ~ "",
      .default = v   # or .default = v to pass through
    )
  }))
