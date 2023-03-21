## Request from Hon. Bonjuris, Chief Distric Judge for the
## 61st District (Adams, Filmore, and Quincy Counties).
## Report of time to disposition for our CY 2019 cases with
## felony or misdemeanor obnoxiousness convictions and its change over time


library(readxl)   #Reads excel files
library(ggplot2)  #Makes some plots
library(stringr)  #Stops R from dropping leading zeros
library(dplyr)
library(epitools)

getwd()

## Read xlsx files for CY2019
og <- read_excel("test.xlsx",
                 sheet = 1,
                 col_types = c("text", "numeric", "numeric",
                               "date", "numeric",  "date", "text"))

mtn <- read_excel("test.xlsx",
                  sheet = 2)

MOT <- read_excel("Xlates.xlsx",
                  sheet = "Motions")

UOR <- read_excel("Xlates.xlsx",
                  sheet = "UOR Codes",
                  col_types = c("numeric", "text", "text"))

ASCF <- read_excel("Xlates.xlsx",
                   sheet = "ASCF",
                   col_types = c("numeric", "text"))
                   
## Keep leading zeros for UOR codes
og$CHG_COUNTY_NUM <- str_pad(og$CHG_COUNTY_NUM, 3, pad="0")
mtn$MOT_COUNTY_NUM <- str_pad(mtn$MOT_COUNTY_NUM, 3, pad="0")
og$CHG_UOR_CODE <- str_pad(og$CHG_UOR_CODE, 7, pad="0")
UOR$"UOR Code" <- str_pad(UOR$"UOR Code", 7, pad="0")


## TODO: Add motions to ds or og table then include in lm summary
names(mtn)[names(mtn)=="MOT_CASE_NUMBER"] <- "CHG_CASE_NUMBER"
ds %>%
  mutate(mot = ifelse(mtn$MOT_CASE_NUMBER == og$CHG_CASE_NUMBER, mtn$Mot_Motion_Type_Code, "NA"))
## Clean values that have no CHG_DIS_DATE

View(og)
View(mtn)


## Include only  61st district
ds <- og %>% filter(CHG_COUNTY_NUM %in% c("001", "003", "006"))

## Omit na chgdispdate
ds <- na.exclude(ds)

## Calculate Time to disposition
ds$ttd <- as.numeric(difftime(ds$CHG_DISP_DATE, ds$CHG_DATE,
                              units = c("days")))

## Add ASCF code
ds$ASCF <- substr(ds$CHG_UOR_CODE, 7, 7)

## Are they Obnoxious
ds$obx <- ifelse(str_detect(ds$CHG_UOR_CODE,"000346\\d|000340\\d|000347\\d") !=0, "Obx", "Not Obx")

## Is it long?
ds$ugh <- ifelse(ds$ttd >= 30, "Long", "Short")

## Plot out TTD for all cases
ggplot(ds, aes(x = CHG_CASE_NUMBER, y = ttd, color = ASCF)) +
  geom_point()
  
lm(ttd ~ ASCF + obx + CHG_DATE, ds)

ggplot(ds, aes(x = CHG_DATE, y = ttd, color = ASCF)) +
  geom_line()

ggplot(ds, aes(ASCF, y = mean(ttd))) +
  geom_col()

ftable(ds$CHG_COUNTY_NUM + ds$obx + ds$ASCF ~ ds$ttd)
table(ds$obx)
table(ds$obx, ds$ASCF, ds$CHG_COUNTY_NUM)     #Shows amounts of obx/non-obx cased per county per ASCF code
table.main <- table(ds$obx, ds$ASCF, ds$CHG_COUNTY_NUM)
margin.table(table.main, 2)
prop.table(table.main,1)
prop.table(table(ds$ugh, ds$obx))
boxplot(ds$ttd ~ ds$CHG_COUNTY_NUM + ds$obx)
boxplot(ds$ttd ~ ds$CHG_COUNTY_NUM, )
riskratio(table(ds$ugh, ds$obx))

## linear regression on county and obx 
ds$CHG_COUNTY_NUM <- as.factor(ds$CHG_COUNTY_NUM)
fit <- lm(ttd ~ CHG_COUNTY_NUM + obx + ASCF, data = ds)
summary(fit)

new <- data.frame(CHG_COUNTY_NUM= c("002"), obx = c("Obx"), ASCF = c("0"))
predict(fit, newdata = new)
## TODO: Finish regression from statology
## TODO: create a new data frame that's easier to work with