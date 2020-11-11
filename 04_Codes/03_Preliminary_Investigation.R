# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Lyrica
# Purpose:      Preliminary investigation
# programmer:   Zhe Liu
# Date:         2020-11-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Product infomation ----
prod.info <- data.standard %>% 
  distinct(DRUG_NAME, DRUG_FORM, DRUG_SPEC, FIRM_ID) %>% 
  arrange(DRUG_NAME, FIRM_ID)

write.xlsx(prod.info, '03_Outputs/03_Product_Information.xlsx')


##---- Data integrity ----
total.num <- data.standard %>% 
  group_by(VISIT_TYPE) %>% 
  summarise_all(function(x) sum(!is.na(x))) %>% 
  ungroup() %>% 
  mutate(Index = '总条目数')

missing.num <- data.standard %>% 
  group_by(VISIT_TYPE) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  ungroup() %>% 
  mutate(Index = '空缺条目数')

data.integrity <- bind_rows(total.num, missing.num) %>% 
  pivot_longer(col = -c(VISIT_TYPE, Index), 
               names_to = 'col', 
               values_to = 'num') %>% 
  pivot_wider(id_cols = c(VISIT_TYPE, col), 
              names_from = Index, 
              values_from = num) %>% 
  mutate(`空缺率` = `空缺条目数` / (`总条目数` + `空缺条目数`)) %>% 
  pivot_longer(cols = -c(VISIT_TYPE, col), 
               names_to = 'Index', 
               values_to = 'num') %>% 
  pivot_wider(id_cols = c(VISIT_TYPE, Index), 
              names_from = col, 
              values_from = num)


##---- Content integrity ----
## product composition
# TODO LZ: 清洗Pack后查产品内容完整性
product.composition <- data.frame(x = 0)

# molecule composition
molecule.composition <- data.standard %>% 
  group_by(VISIT_TYPE, Molecule) %>% 
  summarise(`条目数` = n(), 
            COSTS = sum(COSTS, na.rm = TRUE), 
            AMOUNT = sum(AMOUNT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(VISIT_TYPE) %>% 
  mutate(`金额占比` = COSTS / sum(COSTS, na.rm = TRUE), 
         `数量占比` = AMOUNT / sum(AMOUNT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`是否有数据` = 1) %>% 
  arrange(VISIT_TYPE, -COSTS) %>% 
  select(VISIT_TYPE, Molecule, `是否有数据`, `条目数`, 
         COSTS, `金额占比`, AMOUNT, `数量占比`)

# patients ratio
patients.ratio <- data.standard %>% 
  distinct(HCODE, VISIT_TYPE, PKEY, VISIT_ID, VISIT_DATE) %>% 
  count(HCODE, VISIT_TYPE, name = 'patients') %>% 
  pivot_wider(id_cols = HCODE, 
              names_from = VISIT_TYPE, 
              values_from = patients) %>% 
  mutate(Ratio = `门诊` / `住院`)


##---- Time continuity & stability ----
# patients
trend.patients <- data.standard %>% 
  distinct(VISIT_TYPE, HCODE, PKEY, VISIT_ID, VISIT_DATE) %>% 
  mutate(VISIT_DATE = stri_sub(VISIT_DATE, 1, 7)) %>% 
  count(VISIT_TYPE, HCODE, VISIT_DATE, name = 'patients') %>% 
  arrange(VISIT_TYPE, HCODE, VISIT_DATE) %>% 
  add_count(VISIT_TYPE, HCODE, name = 'Rows') %>% 
  group_by(VISIT_TYPE, HCODE) %>% 
  mutate(n_floor = mean(patients) - sd(patients), 
         n_ceiling = mean(patients) + sd(patients), 
         n_IDT = if_else(patients > n_floor & patients < n_ceiling, 1, 0), 
         IDT = sum(n_IDT)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(VISIT_TYPE, HCODE, Rows, IDT), 
              names_from = VISIT_DATE, 
              values_from = patients, 
              values_fill = 0)

# costs
trend.costs <- data.standard %>% 
  mutate(VISIT_DATE = stri_sub(VISIT_DATE, 1, 7)) %>% 
  group_by(VISIT_TYPE, HCODE, VISIT_DATE) %>% 
  summarise(COSTS = sum(COSTS, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(VISIT_TYPE, HCODE, VISIT_DATE) %>% 
  add_count(VISIT_TYPE, HCODE, name = 'Rows') %>% 
  group_by(VISIT_TYPE, HCODE) %>% 
  mutate(n_floor = mean(COSTS) - sd(COSTS), 
         n_ceiling = mean(COSTS) + sd(COSTS), 
         n_IDT = if_else(COSTS > n_floor & COSTS < n_ceiling, 1, 0), 
         IDT = sum(n_IDT)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(VISIT_TYPE, HCODE, Rows, IDT), 
              names_from = VISIT_DATE, 
              values_from = COSTS, 
              values_fill = 0)

# rows
trend.rows <- data.standard %>% 
  mutate(VISIT_DATE = stri_sub(VISIT_DATE, 1, 7)) %>% 
  group_by(VISIT_TYPE, HCODE, VISIT_DATE) %>% 
  summarise(rows = n()) %>% 
  ungroup() %>% 
  arrange(VISIT_TYPE, HCODE, VISIT_DATE) %>% 
  add_count(VISIT_TYPE, HCODE, name = 'Rows') %>% 
  group_by(VISIT_TYPE, HCODE) %>% 
  mutate(n_floor = mean(rows) - sd(rows), 
         n_ceiling = mean(rows) + sd(rows), 
         n_IDT = if_else(rows > n_floor & rows < n_ceiling, 1, 0), 
         IDT = sum(n_IDT)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(VISIT_TYPE, HCODE, Rows, IDT), 
              names_from = VISIT_DATE, 
              values_from = rows, 
              values_fill = 0)


##---- Write out ----
pi.wb <- createWorkbook()

addWorksheet(pi.wb, '1_Integrity')
addWorksheet(pi.wb, '2_Molecule')
addWorksheet(pi.wb, '2_Product')
addWorksheet(pi.wb, '2_Patients_Ratio')
addWorksheet(pi.wb, '3_Patients')
addWorksheet(pi.wb, '3_Costs')
addWorksheet(pi.wb, '3_Rows')

writeDataTable(pi.wb, '1_Integrity', data.integrity)
writeDataTable(pi.wb, '2_Molecule', molecule.composition)
writeDataTable(pi.wb, '2_Product', product.composition)
writeDataTable(pi.wb, '2_Patients_Ratio', patients.ratio)
writeDataTable(pi.wb, '3_Patients', trend.patients)
writeDataTable(pi.wb, '3_Costs', trend.costs)
writeDataTable(pi.wb, '3_Rows', trend.rows)

saveWorkbook(pi.wb, '03_Outputs/03_Preliminary_Investigation.xlsx', overwrite = TRUE)


