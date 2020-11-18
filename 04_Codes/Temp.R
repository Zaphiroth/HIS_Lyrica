# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Lyrica
# Purpose:      Temp
# programmer:   Zhe Liu
# Date:         2020-11-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Pivot table ----
# data.his <- read_feather('03_Outputs/01_HIS_Raw.feather')
# data.standard <- read_feather('03_Outputs/02_HIS_Standard.feather')

pivot.data <- data.standard %>% 
  distinct(PKEY, VISIT_ID, DIAG_DESC, key_word_flag, DRUG_NAME, standard_prod) %>% 
  count(DIAG_DESC, key_word_flag, DRUG_NAME, standard_prod, name = 'patients')

## diagnosis & product
diag.prod.pts <- pivot.data %>% 
  filter(key_word_flag == 1) %>% 
  group_by(DIAG_DESC, standard_prod) %>% 
  summarise(patients = sum(patients, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-patients)

write.csv(diag.prod.pts, '03_Outputs/02_Diagnosis_Product_Patients.csv', row.names = FALSE)

## product
prod.pts <- pivot.data %>% 
  group_by(standard_prod) %>% 
  summarise(patients = sum(patients, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(prod.pts, '03_Outputs/02_Product_Patients.xlsx')

## diagnosis ratio of product
diag.prod.pts.ratio <- pivot.data %>% 
  group_by(standard_prod, key_word_flag) %>% 
  summarise(patients = sum(patients, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(standard_prod) %>% 
  mutate(patients_sum = sum(patients, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(key_word_flag == 1) %>% 
  mutate(ratio = patients / patients_sum) %>% 
  select(-key_word_flag)

write.xlsx(diag.prod.pts.ratio, '03_Outputs/02_Diagnosis_Product_Patients_Ratio.xlsx')

## department
dept.pts <- data.his %>% 
  distinct(PKEY, VISIT_ID, DEPT_NAME) %>% 
  count(DEPT_NAME, name = 'patients')

write.xlsx(dept.pts, '03_Outputs/02_Department_Patients.xlsx')


##---- Pack ----
## Units
pack.calc <- data.standard %>% 
  distinct(DRUG_SPEC, AMOUNT, UNITS, COSTS) %>% 
  group_by(UNITS) %>% 
  filter(row_number() <= 100) %>% 
  ungroup()

wb.pack <- createWorkbook()
map(unique(pack.calc$UNITS))

