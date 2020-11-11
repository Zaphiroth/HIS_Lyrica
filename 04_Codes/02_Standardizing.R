# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Lyrica
# Purpose:      Standardize data
# programmer:   Zhe Liu
# Date:         2020-11-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Distinct standardizing columns ----
{
  data.standard <- data.his %>% 
    mutate(key_word_flag = if_else(grepl('疱疹|癌|肿瘤|神经|糖尿病|腰|椎
                                       |痛|脊|关节|术|骨折', DIAG_DESC), 
                                   1, 0), 
           Molecule = case_when(
             grepl('阿米替林', DRUG_NAME) ~ '阿米替林', 
             grepl('度洛西汀', DRUG_NAME) ~ '度洛西汀', 
             grepl('文拉法辛', DRUG_NAME) ~ '文拉法辛', 
             grepl('普瑞巴林', DRUG_NAME) ~ '普瑞巴林', 
             grepl('加巴喷丁', DRUG_NAME) ~ '加巴喷丁', 
             grepl('卡马西平', DRUG_NAME) ~ '卡马西平', 
             grepl('奥卡西平', DRUG_NAME) ~ '奥卡西平', 
             grepl('丙戊酸钠', DRUG_NAME) ~ '丙戊酸钠', 
             grepl('丙戊酸镁', DRUG_NAME) ~ '丙戊酸镁', 
             grepl('吗啡', DRUG_NAME) ~ '吗啡', 
             grepl('羟考酮', DRUG_NAME) ~ '羟考酮', 
             grepl('双氯芬酸钠', DRUG_NAME) ~ '双氯芬酸钠', 
             grepl('利多卡因', DRUG_NAME) ~ '利多卡因', 
             grepl('甲钴胺', DRUG_NAME) ~ '甲钴胺', 
             grepl('洛索洛芬', DRUG_NAME) ~ '洛索洛芬', 
             grepl('美洛昔康', DRUG_NAME) ~ '美洛昔康', 
             grepl('塞来昔布', DRUG_NAME) ~ '塞来昔布', 
             grepl('艾瑞昔布', DRUG_NAME) ~ '艾瑞昔布', 
             grepl('神经妥乐平', DRUG_NAME) ~ '神经妥乐平', 
             grepl('拉莫三嗪', DRUG_NAME) ~ '拉莫三嗪', 
             grepl('托吡酯', DRUG_NAME) ~ '托吡酯', 
             grepl('曲马多', DRUG_NAME) ~ '曲马多', 
             grepl('芬太尼', DRUG_NAME) ~ '芬太尼', 
             grepl('阿司匹林', DRUG_NAME) ~ '阿司匹林', 
             grepl('布洛芬', DRUG_NAME) ~ '布洛芬', 
             grepl('巴氯芬', DRUG_NAME) ~ '巴氯芬', 
             grepl('肉毒毒素', DRUG_NAME) ~ '肉毒毒素', 
             grepl('丁螺环酮', DRUG_NAME) ~ '丁螺环酮', 
             TRUE ~ NA_character_
           ))
}
## TODO LZ: 检查缺失情况

write_feather(data.standard, '03_Outputs/02_HIS_Standard.feather')


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

