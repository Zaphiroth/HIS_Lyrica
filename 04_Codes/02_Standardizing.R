# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Lyrica
# Purpose:      Standardize data
# programmer:   Zhe Liu
# Date:         2020-11-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Standardizing ----
dept.std.mapping <- read.xlsx('02_Inputs/HIS科室清洗_veeva标准.xlsx', sheet = 3, cols = c(1, 6))

{
  data.standard <- data.his %>% 
    mutate(
      Mol_std = case_when(
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
      ), 
      Dept_std = case_when(
        grepl('急诊', DEPT_NAME) ~ '急诊科'
        grepl('儿科', DEPT_NAME) ~ '儿科'
        grepl('疼痛|麻醉', DEPT_NAME) ~ '疼痛科'
        grepl('皮', DEPT_NAME) ~ '皮肤科'
        grepl('运动医学|骨质疏松', DEPT_NAME) ~ '骨科'
        grepl('外科', DEPT_NAME) ~ '外科'
        grepl('内分泌|消化内|血液内分泌|代谢病', DEPT_NAME) ~ '内分泌科'
        grepl('精神|心理', DEPT_NAME) ~ '精神心理科'
        grepl('神经内', DEPT_NAME) ~ '神经内科'
        grepl('肿瘤外', DEPT_NAME) ~ '肿瘤外科'
        grepl('肿瘤内', DEPT_NAME) ~ '肿瘤内科'
        grepl('心脏中心|心内', DEPT_NAME) ~ '心内科'
        grepl('肾内科', DEPT_NAME) ~ '肾内科'
        grepl('普通内', DEPT_NAME) ~ '普通内科'
        grepl('泌尿肿瘤外', DEPT_NAME) ~ '泌尿外科'
        grepl('风湿免疫|血液风湿', DEPT_NAME) ~ '风湿免疫科'
        grepl('老干', DEPT_NAME) ~ '老干科',  
        TRUE ~ NA_character_
      ), 
      Diag_std = case_when(
        grepl('疱|疹', DIAG_DESC) ~ '疱疹', 
        grepl('肿瘤|癌', DIAG_DESC) ~ '肿瘤/癌症', 
        grepl('糖尿病', DIAG_DESC) ~ '糖尿病', 
        grepl('术', DIAG_DESC) & grepl('脊|颈|腰', DIAG_DESC) ~ '腰椎脊疾病术后', 
        grepl('术', DIAG_DESC) & grepl('关节', DIAG_DESC) ~ '关节疾病术后', 
        grepl('术', DIAG_DESC) & grepl('骨折', DIAG_DESC) ~ '骨折术后', 
        grepl('术', DIAG_DESC) & grepl('创伤', DIAG_DESC) ~ '创伤术后', 
        grepl('术', DIAG_DESC) ~ '其他疾病术后', 
        grepl('脊髓炎', DIAG_DESC) ~ '脊髓炎', 
        grepl('脊髓损伤', DIAG_DESC) ~ '脊髓损伤', 
        grepl('脊髓创伤', DIAG_DESC) ~ '脊髓损伤', 
        grepl('颈椎压迫', DIAG_DESC) ~ '颈椎压迫', 
        grepl('脊|颈|腰', DIAG_DESC) ~ '其他腰/颈/脊相关疾病', 
        grepl('慢性偏头痛', DIAG_DESC) ~ '慢性偏头痛', 
        grepl('头痛', DIAG_DESC) & grepl('慢性紧张', DIAG_DESC) ~ '慢性紧张型头痛', 
        grepl('头痛', DIAG_DESC) & grepl('慢性丛集', DIAG_DESC) ~ '慢性丛集性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('慢性', DIAG_DESC) ~ '其他慢性头痛', 
        grepl('偏头痛', DIAG_DESC) ~ '偏头痛', 
        grepl('头痛', DIAG_DESC) & grepl('紧张', DIAG_DESC) ~ '紧张型头痛', 
        grepl('头痛', DIAG_DESC) & grepl('丛集', DIAG_DESC) ~ '丛集性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('原发性', DIAG_DESC) ~ '其他原发性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('创伤', DIAG_DESC) ~ '创伤后头痛', 
        grepl('头痛', DIAG_DESC) & grepl('颈源', DIAG_DESC) ~ '颈源性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('高血压', DIAG_DESC) ~ '高血压头痛', 
        grepl('头痛', DIAG_DESC) & grepl('药物过量', DIAG_DESC) ~ '药物过量性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('继发性', DIAG_DESC) ~ '其他继发性头痛', 
        grepl('头痛', DIAG_DESC) & grepl('神经', DIAG_DESC) ~ '神经性头痛', 
        grepl('头痛', DIAG_DESC) ~ '头痛(未指明类型）', 
        grepl('神经痛', DIAG_DESC) & grepl('三叉', DIAG_DESC) ~ '三叉神经痛', 
        grepl('神经病变|神经痛', DIAG_DESC) ~ '其他神经痛', 
        grepl('骨折', DIAG_DESC) ~ '骨折', 
        grepl('关节炎', DIAG_DESC) ~ '关节炎', 
        TRUE ~ NA_character_
      )
    ) %>% 
    left_join(dept.std.mapping, by = c('DEPT_NAME' = 'dept_name')) %>% 
    mutate(flag_dept = if_else(is.na(Dept_std), 0, 1), 
           Dept_std1 = case_when(
             grepl('急诊', `科室清洗结果`) ~ '急诊科'
             grepl('儿科', `科室清洗结果`) ~ '儿科'
             grepl('疼痛|麻醉', `科室清洗结果`) ~ '疼痛科'
             grepl('皮', `科室清洗结果`) ~ '皮肤科'
             grepl('运动医学|骨质疏松', `科室清洗结果`) ~ '骨科'
             grepl('外科', `科室清洗结果`) ~ '外科'
             grepl('内分泌|消化内|血液内分泌|代谢病', `科室清洗结果`) ~ '内分泌科'
             grepl('精神|心理', `科室清洗结果`) ~ '精神心理科'
             grepl('神经内', `科室清洗结果`) ~ '神经内科'
             grepl('肿瘤外', `科室清洗结果`) ~ '肿瘤外科'
             grepl('肿瘤内', `科室清洗结果`) ~ '肿瘤内科'
             grepl('心脏中心|心内', `科室清洗结果`) ~ '心内科'
             grepl('肾内科', `科室清洗结果`) ~ '肾内科'
             grepl('普通内', `科室清洗结果`) ~ '普通内科'
             grepl('泌尿肿瘤外', `科室清洗结果`) ~ '泌尿外科'
             grepl('风湿免疫|血液风湿', `科室清洗结果`) ~ '风湿免疫科'
             grepl('老干', `科室清洗结果`) ~ '老干科',  
             TRUE ~ NA_character_
           ))
}

## TODO LZ: 检查缺失情况
write_feather(data.standard, '03_Outputs/02_HIS_Standard.feather')

## diagnosis
unmatched.diag <- data.standard %>% 
  filter(is.na(Diag_std)) %>% 
  distinct(DIAG_DESC)


##---- Pivot table ----
mol.diag.pivot <- data.standard %>% 
  mutate(Diag_std = if_else(is.na(Diag_std), '其他', Diag_std)) %>% 
  count(Mol_std, Diag_std) %>% 
  group_by(Mol_std) %>% 
  mutate(n_mol = sum(n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(proportion = n / n_mol)

# write.xlsx(mol.diag.pivot, '03_Outputs/02_Molecule_Diagnosis_Pivot.xlsx')



