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
  
  sheet.list <- lapply(sheet.names, read_excel, path = filename)
  
  names(sheet.list) <- sheet.names
  
  return(sheet.list)
}


##---- Outpatient data ----
filename.outpatient <- list.files('02_Inputs/门诊数据v2.0(2018-2019)', 
                                  full.names = TRUE)

file.list.outpatient <- map(filename.outpatient, ReadinFunc)

data.outpatient <- lapply(file.list.outpatient, bind_rows) %>% 
  bind_rows()

write.csv(data.outpatient, '03_Outputs/01_门诊信息合并(2018-2019).csv')


##---- Inhospital data ----
filename.inhospital <- list.files('02_Inputs/住院信息v2.0(2018-2019)', 
                                  full.names = TRUE)

file.list.inhospital <- map(filename.inhospital, ReadinFunc)

data.inhospital <- lapply(file.list.inhospital, bind_rows) %>% 
  lapply(mutate, 
         VISIT_ID = as.numeric(VISIT_ID), 
         AGE = as.numeric(AGE), 
         AMOUNT = as.numeric(AMOUNT), 
         COSTS = as.numeric(COSTS)) %>% 
  bind_rows()

write.csv(data.inhospital, '03_Outputs/01_住院信息合并(2018-2019).csv')


##---- Bind ----
data.his <- data.outpatient %>% 
  mutate(VISIT_DATE = as.character(VISIT_DATE)) %>% 
  bind_rows(data.inhospital) %>% 
  mutate(VISIT_DATE = stri_sub(VISIT_DATE, 1, 10))

write_feather(data.his, '03_Outputs/01_门诊&住院信息.feather')
