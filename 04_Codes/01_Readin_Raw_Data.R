# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Lyrica
# Purpose:      Readin raw data
# programmer:   Zhe Liu
# Date:         2020-11-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin function ----
ReadinFunc <- function(filename) {
  print(filename)
  
  sheet.names <- excel_sheets(filename)
  
  sheet.list <- lapply(sheet.names, read_excel, path = filename) %>% 
    lapply(mutate, 
           VISIT_ID = as.numeric(VISIT_ID), 
           VISIT_DATE = sapply(VISIT_DATE, function(x) {
             ifelse(nchar(x) == 5, 
                    as.character(as.Date(as.numeric(x), origin = '1899-12-30')), 
                    as.character(x))
           }, USE.NAMES = FALSE), 
           AGE = as.numeric(AGE), 
           AMOUNT = as.numeric(AMOUNT), 
           COSTS = as.numeric(COSTS), 
           ADMISSION_DATE_TIME = sapply(ADMISSION_DATE_TIME, function(x) {
             ifelse(nchar(x) == 5, 
                    as.character(as.Date(as.numeric(x), origin = '1899-12-30')), 
                    as.character(x))
           }, USE.NAMES = FALSE), 
           DISCHARGE_DATE_TIME = sapply(DISCHARGE_DATE_TIME, function(x) {
             ifelse(nchar(x) == 5, 
                    as.character(as.Date(as.numeric(x), origin = '1899-12-30')), 
                    as.character(x))
           }, USE.NAMES = FALSE))
  
  names(sheet.list) <- sheet.names
  
  return(sheet.list)
}


##---- Outpatient data ----
filename.outpatient <- list.files('02_Inputs/门诊数据v2.0(2018-2019)', full.names = TRUE) %>% 
  c(list.files('02_Inputs/门诊数据v2.0(2020)', full.names = TRUE))

file.list.outpatient <- map(filename.outpatient, ReadinFunc)

data.outpatient <- lapply(file.list.outpatient, bind_rows) %>% 
  bind_rows()

write_feather(data.outpatient, '03_Outputs/01_Outpatient_Raw.feather')


##---- Inhospital data ----
filename.inhospital <- list.files('02_Inputs/住院信息v2.0(2018-2019)', full.names = TRUE) %>% 
  c(list.files('02_Inputs/住院数据v2.0(2020)', full.names = TRUE))

file.list.inhospital <- map(filename.inhospital, ReadinFunc)

data.inhospital <- lapply(file.list.inhospital, bind_rows) %>% 
  bind_rows()

write_feather(data.inhospital, '03_Outputs/01_Inhospital_Raw.feather')


##---- Bind ----
data.his <- bind_rows(data.outpatient, data.inhospital) %>% 
  mutate(VISIT_DATE = stri_sub(VISIT_DATE, 1, 10))

write_feather(data.his, '03_Outputs/01_HIS_Raw.feather')



