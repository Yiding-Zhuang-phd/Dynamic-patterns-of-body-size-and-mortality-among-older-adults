dir.create("D:/Tempo Data", recursive = TRUE, showWarnings = FALSE)

options("openxlsx.tempdir" = "D:/Tempo Data")

setwd("E:/研究生文件/研0/肥胖与慢性病/Results/Stablecox 筛选完协变量后")
library(survival) 
library(survminer) 
library(tableone) 
library(haven)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(data.table)
library(Hmisc)
library(rms)
library(ggsci)
library(eoffice)
library(ggthemes)
library(paletteer)
library(tibble)
library(plyr)
library(stringr)
######1.Data import and merge######
##import the 4 city in the cohort
BMI_Shenzhen <- read_sas("D:/RAW DATA/老年队列数据集/深圳/tj_1822_rstrajectory.sas7bdat")
BMI_Luzhou <- read_sas("D:/RAW DATA/老年队列数据集/泸州/updatad_10.22/lz1723_0422_rstrajectory.sas7bdat")
BMI_Zunyi <- read_sas("D:/RAW DATA/老年队列数据集/遵义/updatad_10.22/combine2_0509_rstrajectory.sas7bdat")
BMI_Zaozhuang <- read_sas("D:/RAW DATA/老年队列数据集/枣庄/zz0905_rstrajectory.sas7bdat")
##select variables
BMI_Luzhou <- BMI_Luzhou %>%
  select(obs, SEX, age, EDU, DOPE, HEIGHT, WEIGHT, SMOKE, ALCOHOL, TC, TG, LDL, HDL, RSBP, BMI,
         deathdate_new, icd_new, FBG, WC, CVD, VD_other, KID_ULTRACAUSE, KID_ULTRACAUSE_other,
         HEARTD, HEARTD_other, VASD, VASD_other, EYED, EYED_other, NEURSD, NEURSD_other, OTHER_D,
         OTHER_D_des, HEALTH_STATUS, MAR, PAF, HB, WCELL, PLATLET, FBG, GPT, AST, TBIL,  SCR, HE_ab)
BMI_Shenzhen <- BMI_Shenzhen %>%
  select(PID, SEX, AGE, EDU, DOPE, HEIGHT, WEIGHT, SMOKE, ALCOHOL, TC, TG, LDL, HDL, SBPR, BMI,
         DEADDATE, ICD, FBG, WC, CVD, KIDD, HEARTD, VASD, EYED, NEURSD, OTHERD, MAR, PAF, HB, WCELL, PLATLET, FBG, GPT, AST,TBIL, SCR)
BMI_Zunyi <- BMI_Zunyi %>%
  select(obs, Sex, age, education_level, DOPE1, smoking_status, drinking_frequency, total_cholesterol,
         triglyceride, low_density_lipoprotein, high_density_lipoprotein, SBPR, bmi, height, weight,
         deathdate_0329, icd_0329, fasting_blood_glucoss_mmol, waist, cerebrovascular_disease, 
         cerebrovascular_disease_others, kidney_disease, kidney_disease_others, heart_disease, 
         heart_disease_others, vascular_disease, vascular_disease_others, eye_disease, eye_disease_others,
         nervous_system, nervous_system_others, others_system_diseases, others_system_diseases_others,
         marital_status, exercise_frequency, hemoglobin, leukocyte, platelet, fasting_blood_glucoss_mmol, 
         serum_alanine_transaminase, sgot, total_bilirubin, serum_creatinine, disease)
BMI_Zaozhuang <- BMI_Zaozhuang %>%
  select(
    obs = OBS, 
    SEX = sex, 
    age = AGE, 
    DOPE = CDATE, 
    HEIGHT = HT, 
    WEIGHT = WT, 
    SMOKE = SMK, 
    ALCOHOL = AF, 
    TC = TCHO, 
    TG = TG, 
    LDL = LDL, 
    HDL = HDL, 
    RSBP = SBP_R, 
    BMI = BMI, 
    deathdate_new = deathdate_0416, 
    icd_new = icd_0416, 
    FBG = FBG, 
    WC = WC, 
    CVD = CVD, 
    VD_other = CVD_OTH, 
    KID_ULTRACAUSE = KD, 
    KID_ULTRACAUSE_other = KD_OTH, 
    HEARTD = CARD, 
    HEARTD_other = CARD_OTH, 
    EYED = ED, 
    EYED_other = ED_OTH, 
    NEURSD = NEURO_OTH, 
    NEURSD_other = NEURO_OTH_oth, 
    OTHER_D = OD, 
    OTHER_D_des = OD_DESC,
    EDU = edu,
    MAR = MAR,
    PAF = EF,
    HB = HGB,
    WCELL = WBC,
    PLATLET = PLT,
    GPT = ALT,
    AST = AST,
    TBIL = TBIL,
    SCR = SCr,
    JWS = jws_time)







##Zunyi
BMI_Zunyi_formerge <- BMI_Zunyi %>%
  dplyr::rename(
    SEX = Sex, 
    EDU = education_level, 
    DOPE = DOPE1, 
    SMOKE = smoking_status, 
    ALCOHOL = drinking_frequency, 
    TC = total_cholesterol, 
    TG = triglyceride, 
    LDL = low_density_lipoprotein, 
    HDL = high_density_lipoprotein, 
    RSBP = SBPR, 
    BMI = bmi, 
    deathdate_new = deathdate_0329, 
    icd_new = icd_0329,
    FBG = fasting_blood_glucoss_mmol,
    MAR = marital_status,
    PAF = exercise_frequency,
    HB = hemoglobin,
    WCELL = leukocyte,
    PLATLET = platelet,
    GPT = serum_alanine_transaminase,
    AST = sgot,
    TBIL = total_bilirubin,
    SCR = serum_creatinine,
    JWS = disease
  )
##Luzhou
BMI_Luzhou_formerge <- BMI_Luzhou %>%
  dplyr::rename(
    height = HEIGHT,
    weight = WEIGHT,
    waist = WC,
    JWS = HE_ab
  )
##Shenzhen
BMI_Shenzhen_formerge <- BMI_Shenzhen %>%
  dplyr::rename(
    height = HEIGHT,
    weight = WEIGHT,
    waist = WC,
    obs = PID,
    age = AGE,
    RSBP = SBPR,
    deathdate_new = DEADDATE,
    icd_new = ICD
  )


##Zaozhuang
BMI_Zaozhuang_formerge <- BMI_Zaozhuang %>%
  dplyr::rename(
    height = HEIGHT,
    weight = WEIGHT,
    waist = WC,
    cerebrovascular_disease = CVD,
    cerebrovascular_disease_others = VD_other,
    kidney_disease = KID_ULTRACAUSE,
    kidney_disease_others = KID_ULTRACAUSE_other,
    heart_disease = HEARTD,
    eye_disease = EYED,
    eye_disease_others = EYED_other,
    nervous_system = NEURSD,
    nervous_system_others = NEURSD_other,
    others_system_diseases = OTHER_D,
    others_system_diseases_others = OTHER_D_des
  )


##consistency across all the variables
##Zunyi
BMI_Luzhou_formerge <- BMI_Luzhou_formerge %>%
  dplyr::mutate(
    EDU = recode(EDU,
                 `10` = "研究生", `20` = "大学本科", `30` = "大学专科和专科学校",
                 `40` = "中等专业学校", `50` = "技工学校", `60` = "高中",
                 `70` = "初中", `80` = "小学", `90` = "文盲或半文盲", `0` = "不详"),
    SMOKE = recode(SMOKE,
                   `1` = "从不吸烟", `2` = "已戒烟", `3` = "吸烟"),
    ALCOHOL = recode(ALCOHOL,
                     `1` = "从不", `2` = "偶尔", `3` = "经常", `4` = "每天")
  )
BMI_Luzhou_formerge <- BMI_Luzhou_formerge %>%
  dplyr::rename(
    cerebrovascular_disease = CVD,
    cerebrovascular_disease_others = VD_other,
    kidney_disease = KID_ULTRACAUSE,
    kidney_disease_others = KID_ULTRACAUSE_other,
    heart_disease = HEARTD,
    heart_disease_others = HEARTD_other,
    vascular_disease = VASD,
    vascular_disease_others = VASD_other,
    eye_disease = EYED,
    eye_disease_others = EYED_other,
    nervous_system = NEURSD,
    nervous_system_others = NEURSD_other,
    others_system_diseases = OTHER_D,
    others_system_diseases_others = OTHER_D_des
  ) %>%
  dplyr::mutate(
    HEALTH_STATUS = as.character(HEALTH_STATUS),
    Region = "Luzhou",
    cerebrovascular_disease = recode(cerebrovascular_disease,
                                     `1` = "未发现", `2` = "缺血性卒中", `3` = "脑出血",
                                     `4` = "蛛网膜下腔出血", `5` = "短暂性脑缺血发作", `6` = "其他"),
    kidney_disease = recode(kidney_disease,
                            `1` = "未发现", `2` = "糖尿病肾病", `3` = "肾功能衰竭",
                            `4` = "急性肾炎", `5` = "慢性肾炎", `6` = "其他"),
    heart_disease = recode(heart_disease,
                           `1` = "未发现", `2` = "心肌梗死", `3` = "心绞痛",
                           `4` = "冠状动脉学运重建", `5` = "充血性心力衰竭",
                           `6` = "心前区疼痛", `7` = "其他"),
    vascular_disease = recode(vascular_disease,
                              `1` = "未发现", `2` = "夹层动脉瘤", `3` = "动脉闭塞性疾病", `4` = "其他"),
    eye_disease = recode(eye_disease,
                         `1` = "未发现", `2` = "视网膜出血或渗出",
                         `3` = "视乳头水肿", `4` = "白内障", `5` = "其他"),
    nervous_system = recode(nervous_system,
                            `1` = "未发现", `2` = "其他"),
    others_system_diseases = recode(others_system_diseases,
                                    `1` = "未发现", `2` = "其他")
  ) %>%
  dplyr::select(-ends_with("_others"))

### --- Zunyi ---
BMI_Zunyi_formerge <- BMI_Zunyi_formerge %>%
  dplyr::mutate(
    HEALTH_STATUS = "",
    Region = "Zunyi"
  ) %>%
  dplyr::select(-ends_with("_others"))

### --- Shenzhen ---
BMI_Shenzhen_formerge <- BMI_Shenzhen_formerge %>%
  dplyr::rename(
    cerebrovascular_disease = CVD,
    kidney_disease = KIDD,
    heart_disease = HEARTD,
    vascular_disease = VASD,
    eye_disease = EYED,
    nervous_system = NEURSD,
    others_system_diseases = OTHERD
  ) %>%
  dplyr::mutate(
    HEALTH_STATUS = "",
    Region = "Shenzhen"
  )

### --- Zaozhuang ---
BMI_Zaozhuang_formerge <- BMI_Zaozhuang_formerge %>%
  dplyr::select(-c(cerebrovascular_disease_others, kidney_disease_others,
                   HEARTD_other, eye_disease_others, nervous_system_others)) %>%
  mutate(
    Region = "Zaozhuang"
  )
##consistency of value in variables
BMI_Luzhou_formerge <- BMI_Luzhou_formerge %>%
  dplyr::mutate(
    # marital status recode
    MAR = dplyr::recode(MAR,
                        `10` = "未婚",
                        `20` = "已婚",
                        `30` = "丧偶",
                        `40` = "离婚",
                        `90` = "未说明"),
    
    # physical activity recode
    PAF = dplyr::recode(PAF,
                        `1` = "每天",
                        `2` = "每周一次以上",
                        `3` = "偶尔",
                        `4` = "不锻炼")
  )

##for numercial variables
to_numeric_vars <- c("age", "weight", "height", "waist", "TC", "TG", "LDL", "HDL", "RSBP", "BMI", "FBG",
                     "HB", "WCELL", "PLATLET", "GPT", "AST", "TBIL",  "SCR")

BMI_Luzhou_formerge[to_numeric_vars] <- lapply(BMI_Luzhou_formerge[to_numeric_vars], as.numeric)
BMI_Zunyi_formerge[to_numeric_vars] <- lapply(BMI_Zunyi_formerge[to_numeric_vars], as.numeric)
BMI_Zaozhuang_formerge[to_numeric_vars] <- lapply(BMI_Zaozhuang_formerge[to_numeric_vars], as.numeric)
BMI_Shenzhen_formerge[to_numeric_vars]  <- lapply(BMI_Shenzhen_formerge[to_numeric_vars], as.numeric)

# date variables
BMI_Zaozhuang_formerge$DOPE <- as.Date(BMI_Zaozhuang_formerge$DOPE, format = "%Y/%m/%d")
# intergration for 4 dataset
BMI_Data <- bind_rows(
  BMI_Luzhou_formerge,
  BMI_Shenzhen_formerge,
  BMI_Zunyi_formerge,
  BMI_Zaozhuang_formerge
)
rm(BMI_Luzhou_formerge, BMI_Shenzhen_formerge, BMI_Zaozhuang_formerge, BMI_Zunyi_formerge)


##build unique ids for all participants across different cities
BMI_Data <- BMI_Data %>%
  mutate(unique_id = paste(Region, obs, sep = "_"))
total_records <- nrow(BMI_Data)  # document the total number of participants
unique_ids <- n_distinct(BMI_Data$unique_id)  # unique
cat("原始：BMI_Data 中有", total_records, "条记录，其中", unique_ids, "个唯一标识符。")



####deal with the abnormal values across all variables ####
BMI_Data <- BMI_Data %>%
  filter(height <= 250 & height >= 100,
         weight <= 200 & weight >= 30,
         waist <= 200 & waist >= 40,
         TC <= 35,
         TG <= 15,
         LDL < 20,
         HDL < 15,
         BMI <=45 & BMI >= 12,
         age >= 65,
         HB >= 60 & HB <= 350,
         WCELL >0 & WCELL <= 200,
         PLATLET >=40 & PLATLET <= 600,
         GPT > 0,
         AST > 0,
         TBIL >0 & TBIL < 1000,
         SCR > 0
  )
total_records <- nrow(BMI_Data)  # total number
unique_ids <- n_distinct(BMI_Data$unique_id)  # unique number
cat("after excluding abormal values：BMI_Data 中有", total_records, "条记录，其中", unique_ids, "个唯一标识符。")

##generate times of follow-up
BMI_Data <- BMI_Data %>%
  arrange(unique_id, DOPE) %>%  
  dplyr::group_by(unique_id) %>%       
  dplyr::mutate(
    visit = row_number(),            
    total_visits = n(),                   
  ) %>%
  dplyr::ungroup()  

###exclude participants with less than three times of follow-up
BMI_Data <- BMI_Data %>%
  filter(total_visits >= 3)
total_records <- nrow(BMI_Data)  
unique_ids <- n_distinct(BMI_Data$unique_id)  
cat("纳入分析的研究对象：BMI_Data 中有", total_records, "条记录，其中", unique_ids, "个唯一标识符。")



Change_Data <- BMI_Data %>%
  dplyr::group_by(unique_id) %>%
  reframe(
    BMI_first = BMI[visit == 1][1],  
    BMI_last = BMI[visit == total_visits][1],
    weight_first = weight[visit == 1][1],
    weight_last = weight[visit == total_visits][1],
    waist_first = waist[visit == 1][1],
    waist_last = waist[visit == total_visits][1],
    DOPE_first = DOPE[visit == 1][1],
    DOPE_last = DOPE[visit == total_visits][1]
  ) %>%
  mutate(
    ###absolute change patterns for weight
    weight_change = abs(weight_last - weight_first), 
    weight_change_cat = case_when(                   
      weight_change < 2.5 ~ 1,
      weight_last - weight_first >= -5 & weight_last - weight_first < -2.5 ~ 2,
      weight_last - weight_first > 2.5 & weight_last - weight_first <= 5 ~ 3,
      weight_last - weight_first < -5 ~ 4,
      weight_last - weight_first > 5 ~ 5,
      TRUE ~ NA_integer_
    ),
    ###relative change patterns for BMI, weight an d waist circumference
    BMI_change_perc = 100 * (BMI_last - BMI_first) / BMI_first,
    weight_change_perc = 100 * (weight_last - weight_first) / weight_first,
    waist_change_perc = 100 * (waist_last - waist_first) / waist_first,
    ###BMI
    BMI_change_perc_cat = case_when(
      abs(BMI_change_perc) < 5 ~ 1,
      BMI_change_perc >= -10 & BMI_change_perc <= -5 ~ 2,
      BMI_change_perc >= 5 & BMI_change_perc <= 10 ~ 3,
      BMI_change_perc < -10 ~ 4,
      BMI_change_perc > 10 ~ 5,
      TRUE ~ NA_integer_
    ),
    ###weight
    weight_change_perc_cat = case_when(
      abs(weight_change_perc) < 5 ~ 1,
      weight_change_perc >= -10 & weight_change_perc <= -5 ~ 2,
      weight_change_perc >= 5 & weight_change_perc <= 10 ~ 3,
      weight_change_perc < -10 ~ 4,
      weight_change_perc > 10 ~ 5,
      TRUE ~ NA_integer_
    ),
    ###WC
    waist_change_cat_perc = case_when(
      abs(waist_change_perc) < 5 ~ 1,
      waist_change_perc >= -10 & waist_change_perc <= -5 ~ 2,
      waist_change_perc >= 5 & waist_change_perc <= 10 ~ 3,
      waist_change_perc < -10 ~ 4,
      waist_change_perc > 10 ~ 5,
      TRUE ~ NA_integer_
    ),
    
    ###absolute change patterns for wc
    waist_change_cat_abs = case_when(
      abs(waist_last - waist_first) < 3 ~ 1,                  
      waist_last - waist_first <= -3 & waist_last - waist_first > -5 ~ 2, 
      waist_last - waist_first >= 3 & waist_last - waist_first < 5 ~ 3,   
      waist_last - waist_first <= -5 ~ 4,                    
      waist_last - waist_first >= 5 ~ 5,                      
      TRUE ~ NA_integer_
    ),
    visit_interval = as.numeric(difftime(DOPE_last, DOPE_first, units = "days")) / 365.25
  )

####calculate surviving time
BMI_Data <- BMI_Data %>%
  mutate(deathdate_new = ymd(deathdate_new),
         Status = ifelse(!is.na(deathdate_new), 1, 0))
##generate Time_years
BMI_Data <- BMI_Data %>%
  mutate(DOPE = ymd(DOPE),
         deathdate_new = ymd(deathdate_new),
         Time_years = ifelse(
           !is.na(deathdate_new),
           as.numeric(difftime(deathdate_new, DOPE, units = "days")) / 365.25,
           as.numeric(difftime(ymd("2023-12-31"), DOPE, units = "days")) / 365.25
         ),
         Time_years = round(Time_years, 2))
##merge the baseline data
Baseline_Data <- BMI_Data %>%
  filter(visit == 1)
Change_Data <- Change_Data %>%
  left_join(Baseline_Data, by = "unique_id")



###Covariates####
# education attainment: <9 years 和 ≥9 years
Change_Data <- Change_Data %>%
  mutate(
    EDU_new = case_when(
      # <9 years 学历
      EDU %in% c("小学", "文盲或半文盲", "文盲及半文盲") ~ "<9 years",
      
      # ≥9 years 学历
      EDU %in% c("初中", "高中", "大学专科和专科学校", "大学本科", "大学专科及以上", "研究生", 
                 "高中/技校/中专", "技工学校", "中等专业学校") ~ "≥9 years",
      
      # 如果不符合上述条件，赋值为 NA
      TRUE ~ NA_character_
    )
  )

#for marital status and exercise frequency
Change_Data <- Change_Data %>%
  mutate(
    # marital status
    MAR = case_when(
      MAR %in% c("已婚") ~ "Married",
      MAR %in% c("未婚", "初婚") ~ "Single",
      MAR %in% c("离婚") ~ "Divorced",
      MAR %in% c("丧偶") ~ "Widowed",
      TRUE ~ NA_character_
    ),
    
    # exercise frequency
    PAF = case_when(
      PAF %in% c("不锻炼") ~ "No exercise",
      PAF %in% c("每天") ~ "Daily",
      PAF %in% c("每周一次以上") ~ "Weekly+",
      PAF %in% c("偶尔") ~ "Occasionally",
      TRUE ~ NA_character_
    )
  )
##into factor
Change_Data <- within(Change_Data,{
  SEX <- factor(SEX, levels = c("男", "女"))
  SMOKE <- factor(SMOKE, levels = c("从不吸烟", "已戒烟", "吸烟"))
  ALCOHOL <- factor(ALCOHOL, levels = c("从不", "偶尔", "经常", "每天"))
  EDU_new <- factor(EDU_new, levels = c("<9 years", "≥9 years"))
})
Change_Data <- Change_Data %>%
  mutate(
    weight_change_cat = as.factor(weight_change_cat),
    BMI_change_perc_cat = as.factor(BMI_change_perc_cat),
    weight_change_perc_cat = as.factor(weight_change_perc_cat),
    waist_change_cat_perc = as.factor(waist_change_cat_perc),
    waist_change_cat_abs = as.factor(waist_change_cat_abs)
  )
Change_Data <- Change_Data %>%
  mutate(
    weight_change_cat = factor(weight_change_cat,
                               levels = 1:5,
                               labels = c("Stable", 
                                          "Mild decrease", 
                                          "Mild increase", 
                                          "Substantial decrease", 
                                          "Substantial increase")),
    BMI_change_perc_cat = factor(BMI_change_perc_cat,
                                 levels = 1:5,
                                 labels = c("Stable", 
                                            "Mild decrease", 
                                            "Mild increase", 
                                            "Substantial decrease", 
                                            "Substantial increase")),
    weight_change_perc_cat = factor(weight_change_perc_cat,
                                    levels = 1:5,
                                    labels = c("Stable", 
                                               "Mild decrease", 
                                               "Mild increase", 
                                               "Substantial decrease", 
                                               "Substantial increase")),
    waist_change_cat_perc = factor(waist_change_cat_perc,
                                   levels = 1:5,
                                   labels = c("Stable", 
                                              "Mild decrease", 
                                              "Mild increase", 
                                              "Substantial decrease", 
                                              "Substantial increase")),
    waist_change_cat_abs = factor(waist_change_cat_abs,
                                  levels = 1:5,
                                  labels = c("Stable", 
                                             "Mild decrease", 
                                             "Mild increase", 
                                             "Substantial decrease", 
                                             "Substantial increase"))
  )



###BMI transition patterns
Change_Data <- Change_Data %>%
  mutate(BMI_first_cat = case_when(
    BMI_first <18.5 ~ "Underweight",
    BMI_first >= 18.5 & BMI_first < 24 ~ "Normal weight",
    BMI_first >= 24 & BMI_first < 28 ~ "Overweight",
    BMI_first >= 28 ~ "Obesity",
    TRUE ~ NA_character_ )) %>% # 处理不符合条件的情况
  mutate(BMI_last_cat = case_when(
    BMI_last <18.5 ~ "Underweight",
    BMI_last >= 18.5 & BMI_last < 24 ~ "Normal weight",
    BMI_last >= 24 & BMI_last < 28 ~ "Overweight",
    BMI_last >= 28 ~ "Obesity",
    TRUE ~ NA_character_ ))

Change_Data <- Change_Data %>%
  mutate(BMI_change_pattern = case_when(
    # First category: Underweight
    BMI_first_cat == "Underweight" & BMI_last_cat == "Underweight" ~ 1,
    BMI_first_cat == "Underweight" & BMI_last_cat == "Normal weight" ~ 2,
    BMI_first_cat == "Underweight" & BMI_last_cat == "Overweight" ~ 3,
    BMI_first_cat == "Underweight" & BMI_last_cat == "Obesity" ~ 4,
    # First category: Normal weight
    BMI_first_cat == "Normal weight" & BMI_last_cat == "Underweight" ~ 5,
    BMI_first_cat == "Normal weight" & BMI_last_cat == "Normal weight" ~ 6,
    BMI_first_cat == "Normal weight" & BMI_last_cat == "Overweight" ~ 7,
    BMI_first_cat == "Normal weight" & BMI_last_cat == "Obesity" ~ 8,
    # First category: Overweight
    BMI_first_cat == "Overweight" & BMI_last_cat == "Underweight" ~ 9,
    BMI_first_cat == "Overweight" & BMI_last_cat == "Normal weight" ~ 10,
    BMI_first_cat == "Overweight" & BMI_last_cat == "Overweight" ~ 11,
    BMI_first_cat == "Overweight" & BMI_last_cat == "Obesity" ~ 12,
    # First category: Obesity
    BMI_first_cat == "Obesity" & BMI_last_cat == "Underweight" ~ 13,
    BMI_first_cat == "Obesity" & BMI_last_cat == "Normal weight" ~ 14,
    BMI_first_cat == "Obesity" & BMI_last_cat == "Overweight" ~ 15,
    BMI_first_cat == "Obesity" & BMI_last_cat == "Obesity" ~ 16,
    # Default
    TRUE ~ NA_integer_
  ))


Change_Data <- Change_Data %>%
  mutate(BMI_change_pattern = factor(
    BMI_change_pattern,
    levels = 1:16, # order
    labels = c(
      "Underweight → Underweight",
      "Underweight → Normal weight",
      "Underweight → Overweight",
      "Underweight → Obesity",
      "Normal weight → Underweight",
      "Normal weight → Normal weight",
      "Normal weight → Overweight",
      "Normal weight → Obesity",
      "Overweight → Underweight",
      "Overweight → Normal weight",
      "Overweight → Overweight",
      "Overweight → Obesity",
      "Obesity → Underweight",
      "Obesity → Normal weight",
      "Obesity → Overweight",
      "Obesity → Obesity"
    )
  ))
Change_Data <- Change_Data %>%
  mutate(BMI_change_pattern = relevel(BMI_change_pattern, ref = "Normal weight → Normal weight"))



##the missing values of the covariates
covariates <- c("SEX", "age", "EDU_new", "SMOKE", "ALCOHOL", 
                "TC", "TG", "LDL", "HDL", "RSBP", "MAR", "PAF", "FBG", "HB", "WCELL"
                , "PLATLET", "GPT", "AST", "TBIL", "SCR")
# missing values and their proportions
missing_info <- sapply(covariates, function(x) {
  missing_count <- sum(is.na(Change_Data[[x]])) # 缺失值数量
  missing_percentage <- round(missing_count / nrow(Change_Data) * 100, 2) # 缺失值占比
  return(c(missing_count, missing_percentage))
})

missing_info_df <- as.data.frame(t(missing_info))
colnames(missing_info_df) <- c("Missing Count", "Missing Percentage")
missing_info_df$Variable <- rownames(missing_info_df)

Change_Data <- Change_Data %>%
  mutate(SEX = recode(SEX,
                      "男" = "Male",
                      "女" = "Female"),
         SMOKE = recode(SMOKE,
                        "从不吸烟" = "Never smoker",
                        "已戒烟" = "Former smoker",
                        "吸烟" = "Current smoker"),
         ALCOHOL = recode(ALCOHOL,
                          "从不" = "Non-drinker",
                          "偶尔" = "Casual drinker",
                          "经常" = "Frequent drinker",
                          "每天" = "Daily drinker"))

cat_vars <- setdiff(covariates, to_numeric_vars)
# Smoking status
Change_Data$SMOKE <- factor(
  Change_Data$SMOKE,
  levels = c("Never smoker", "Former smoker", "Current smoker"),
  ordered = TRUE
)

# Alcohol consumption
Change_Data$ALCOHOL <- factor(
  Change_Data$ALCOHOL,
  levels = c("Non-drinker", "Casual drinker", "Frequent drinker", "Daily drinker"),
  ordered = TRUE
)

# exercise frequency
Change_Data$PAF <- factor(
  Change_Data$PAF,
  levels = c("No exercise", "Occasionally", "Weekly+", "Daily"),
  ordered = TRUE
)

# gender
Change_Data$SEX <- factor(
  Change_Data$SEX,
  levels = c("Female", "Male")   # 如果原数据是“1/2”或“男/女”，这里记得改对应
)

# education attainment （<9 years vs ≥9 years）
Change_Data$EDU_new <- factor(
  Change_Data$EDU_new,
  levels = c("<9 years", "≥9 years")
)

# marital status
Change_Data$MAR <- factor(
  Change_Data$MAR,
  levels = c("Married", "Single", "Widowed", "Divorced")
)



##### After converting categorical variables into dummy variables, the dataset was imported into a Python script for the Stable Cox procedure. 
##### The selected covariates included sex, age, edu, smoke, paf, mar, HB, wcell, FBG, TC, LDL, HDL, GPT, AST, SCR, TBIL, and RSBp. 
##### For the transition pattern analysis, HDL was excluded and TG was added instead. 
## The following parts remain unchanged.


####Tables for baseline#####
##BMI relative change patterns
Baseline_Table <- CreateTableOne(data = Change_Data,
                                 strata = c("BMI_change_perc_cat"),
                                 vars = c("SEX", "age", "height", "EDU_new", "MAR", "SMOKE","PAF",
                                          "RSBP","HB", "WCELL", "FBG", "TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR", "TBIL",
                                          "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                          "waist_last","visit_interval", "BMI_first_cat", "BMI_last_cat"),
                                 factorVars = c("SEX", "EDU_new", "MAR", "SMOKE", "PAF", "BMI_first_cat", "BMI_last_cat"),
                                 addOverall = TRUE,
                                 includeNA = FALSE)
Baseline_Output_Table <- print(x = Baseline_Table,
                               showAllLevels = TRUE,
                               noSpaces = TRUE,
                               printToggle = FALSE,
                               nonnormal = c("age","height",  
                                             "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                             "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                             "waist_last","visit_interval"),
                               formatOptions = list(big.mark = ",")) ##连续型变量用中位数表达

write.csv(Baseline_Output_Table, file = "baseline_table_BMI_perc.csv")

##weight absolute change patterns
Baseline_Table <- CreateTableOne(data = Change_Data,
                                 strata = c("weight_change_cat"),
                                 vars = c("SEX", "age", "height", "EDU_new", "MAR", "SMOKE","PAF",
                                          "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                          "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                          "waist_last","visit_interval", "BMI_first_cat", "BMI_last_cat"),
                                 factorVars = c("SEX", "EDU_new", "MAR", "SMOKE", "PAF", "BMI_first_cat", "BMI_last_cat"),
                                 addOverall = TRUE,
                                 includeNA = FALSE)
Baseline_Output_Table <- print(x = Baseline_Table,
                               showAllLevels = TRUE,
                               noSpaces = TRUE,
                               printToggle = FALSE,
                               nonnormal = c("age","height",  
                                             "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                             "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                             "waist_last","visit_interval"),
                               formatOptions = list(big.mark = ",")) ##连续型变量用中位数表达
write.csv(Baseline_Output_Table, file = "baseline_table_weight_cat.csv")

##wc absolute change patterns
Baseline_Table <- CreateTableOne(data = Change_Data,
                                 strata = c("waist_change_cat_abs"),
                                 c("SEX", "age", "height", "EDU_new", "MAR", "SMOKE","PAF",
                                   "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                   "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                   "waist_last","visit_interval", "BMI_first_cat", "BMI_last_cat"),
                                 factorVars = c("SEX", "EDU_new", "MAR", "SMOKE", "PAF", "BMI_first_cat", "BMI_last_cat"),
                                 addOverall = TRUE,
                                 includeNA = FALSE)
Baseline_Output_Table <- print(x = Baseline_Table,
                               showAllLevels = TRUE,
                               noSpaces = TRUE,
                               printToggle = FALSE,
                               nonnormal = c("age","height",  
                                             "RSBP","HB", "WCELL","FBG", "TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                             "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                             "waist_last","visit_interval"),
                               formatOptions = list(big.mark = ",")) ##连续型变量用中位数表达
write.csv(Baseline_Output_Table, file = "baseline_table_waist_cat.csv")

##weight relative change patterns
Baseline_Table <- CreateTableOne(data = Change_Data,
                                 strata = c("weight_change_perc_cat"),
                                 c("SEX", "age", "height", "EDU_new", "MAR", "SMOKE","PAF",
                                   "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                   "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                   "waist_last","visit_interval", "BMI_first_cat", "BMI_last_cat"),
                                 factorVars = c("SEX", "EDU_new", "MAR", "SMOKE", "PAF", "BMI_first_cat", "BMI_last_cat"),
                                 addOverall = TRUE,
                                 includeNA = FALSE)
Baseline_Output_Table <- print(x = Baseline_Table,
                               showAllLevels = TRUE,
                               noSpaces = TRUE,
                               printToggle = FALSE,
                               nonnormal = c("age","height",  
                                             "RSBP","HB", "WCELL", "FBG","TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                             "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                             "waist_last","visit_interval"),
                               formatOptions = list(big.mark = ",")) ##连续型变量用中位数表达
write.csv(Baseline_Output_Table, file = "baseline_table_weight_perc_cat.csv")

##wc relative change patterns
Baseline_Table <- CreateTableOne(data = Change_Data,
                                 strata = c("waist_change_cat_perc"),
                                 c("SEX", "age", "height", "EDU_new", "MAR", "SMOKE","PAF",
                                   "RSBP","HB", "WCELL","FBG", "TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                   "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                   "waist_last","visit_interval", "BMI_first_cat", "BMI_last_cat"),
                                 factorVars = c("SEX", "EDU_new", "MAR", "SMOKE", "PAF", "BMI_first_cat", "BMI_last_cat"),
                                 addOverall = TRUE,
                                 includeNA = FALSE)
Baseline_Output_Table <- print(x = Baseline_Table,
                               showAllLevels = TRUE,
                               noSpaces = TRUE,
                               printToggle = FALSE,
                               nonnormal = c("age","height",  
                                             "RSBP","HB", "WCELL","FBG", "TC", "TG", "LDL", "HDL", "GPT", "AST", "SCR","TBIL",
                                             "BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first",
                                             "waist_last","visit_interval"),
                               formatOptions = list(big.mark = ",")) ##连续型变量用中位数表达
write.csv(Baseline_Output_Table, file = "baseline_table_waist_perc_cat.csv")



#####COX regression for all cause mortality#####
##COX tables
##aboslute change patterns for weight 
library(ggplot2)
library(openxlsx)
wb <- createWorkbook()
fit_model_weight_cat <- coxph(Surv(Time_years, Status) ~ weight_change_cat + SEX + age + EDU_new + SMOKE + TC +FBG + TBIL + LDL + HDL + RSBP + 
                                PAF + MAR + HB + WCELL + GPT + AST + SCR, data = Change_Data)
model_weight_cat <- summary(fit_model_weight_cat)
HR <- round(model_weight_cat$coefficients[1:4, 2], 2) 
Pmark <- ifelse(model_weight_cat$coefficients[1:4, 5] < 0.001, "***",
                ifelse(model_weight_cat$coefficients[1:4, 5] >= 0.001 & model_weight_cat$coefficients[1:4, 5] < 0.01, "**",
                       ifelse(model_weight_cat$coefficients[1:4, 5] >= 0.01 & model_weight_cat$coefficients[1:4, 5] < 0.05, "*", "")))
Pvalue <- round(model_weight_cat$coefficients[1:4, 5], 3)
LCI <- round(model_weight_cat$conf.int[1:4, 3], 2)
UCI <- round(model_weight_cat$conf.int[1:4, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")" , Pmark), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("weight_change_cat", "", table$type)
##计算死亡率
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$weight_change_cat),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
addWorksheet(wb, "Weight Change")
writeData(wb, sheet = "Weight Change", table_combined)

# 拆分 HR 和 CI
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

# 手动加上 Stable 参照组
plot_data <- rbind(
  data.frame(type = "Stable",
             HR = 1, LCI = 1, UCI = 1),
  plot_data
)

# 指定顺序
plot_data$type <- factor(plot_data$type,
                         levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                        "Substantial decrease", "Substantial increase")))

# ---- 作图 
p1 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "Weight Change (kg)", y = "Hazard Ratio (95% CI)")













##WC absolute change patterns
fit_model_waist_cat <- coxph(Surv(Time_years, Status) ~ waist_change_cat_abs + SEX + age + EDU_new + SMOKE + TC +FBG + TBIL + LDL + HDL + RSBP + 
                               PAF + MAR + HB + WCELL + GPT + AST + SCR, data = Change_Data)
model_waist_cat <- summary(fit_model_waist_cat)
HR <- round(model_waist_cat$coefficients[1:4, 2], 2) 
Pmark <- ifelse(model_waist_cat$coefficients[1:4, 5] < 0.001, "***",
                ifelse(model_waist_cat$coefficients[1:4, 5] >= 0.001 & model_waist_cat$coefficients[1:4, 5] < 0.01, "**",
                       ifelse(model_waist_cat$coefficients[1:4, 5] >= 0.01 & model_waist_cat$coefficients[1:4, 5] < 0.05, "*", "")))
Pvalue <- round(model_waist_cat$coefficients[1:4, 5], 3)
LCI <- round(model_waist_cat$conf.int[1:4, 3], 2)
UCI <- round(model_waist_cat$conf.int[1:4, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")" , Pmark), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("waist_change_cat_abs", "", table$type)
##mortality rate
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$waist_change_cat_abs),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
addWorksheet(wb, "Waist Change")
writeData(wb, sheet = "Waist Change", table_combined)

# forest plot - Waist Circumference Change (cm)
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

plot_data <- rbind(
  data.frame(type = "Stable", HR = 1, LCI = 1, UCI = 1),
  plot_data
)

plot_data$type <- factor(plot_data$type,
                         levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                        "Substantial decrease", "Substantial increase")))

p2 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "Waist Circumference Change (cm)", y = "Hazard Ratio (95% CI)")













##relative change patterns for BMI
fit_model_BMI_perc_cat <- coxph(Surv(Time_years, Status) ~ BMI_change_perc_cat + SEX + age + EDU_new + SMOKE + TC +FBG + TBIL + LDL + HDL + RSBP + 
                                  PAF + MAR + HB + WCELL + GPT + AST + SCR, data = Change_Data)
model_BMI_perc_cat <- summary(fit_model_BMI_perc_cat)
HR <- round(model_BMI_perc_cat$coefficients[1:4, 2], 2) 
Pmark <- ifelse(model_BMI_perc_cat$coefficients[1:4, 5] < 0.001, "***",
                ifelse(model_BMI_perc_cat$coefficients[1:4, 5] >= 0.001 &model_BMI_perc_cat$coefficients[1:4, 5] < 0.01, "**",
                       ifelse(model_BMI_perc_cat$coefficients[1:4, 5] >= 0.01 & model_BMI_perc_cat$coefficients[1:4, 5] < 0.05, "*", "")))
Pvalue <- round(model_BMI_perc_cat$coefficients[1:4, 5], 3)
LCI <- round(model_BMI_perc_cat$conf.int[1:4, 3], 2)
UCI <- round(model_BMI_perc_cat$conf.int[1:4, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark ), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("BMI_change_perc_cat", "", table$type)
##mortality rate
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$BMI_change_perc_cat),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
addWorksheet(wb, "BMI Change (perc)")
writeData(wb, sheet = "BMI Change (perc)", table_combined)

# 绘制森林图 - BMI Change (%)
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

plot_data <- rbind(
  data.frame(type = "Stable", HR = 1, LCI = 1, UCI = 1),
  plot_data
)

plot_data$type <- factor(plot_data$type,
                         levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                        "Substantial decrease", "Substantial increase")))

p3 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "BMI Change (%)", y = "Hazard Ratio (95% CI)")






##relative change patterns for weight
fit_model_weight_perc_cat <- coxph(Surv(Time_years, Status) ~ weight_change_perc_cat ++ SEX + age + EDU_new + SMOKE + TC +FBG + TBIL + LDL + HDL + RSBP + 
                                     PAF + MAR + HB + WCELL + GPT + AST + SCR, data = Change_Data)
model_weight_perc_cat <- summary(fit_model_weight_perc_cat)
HR <- round(model_weight_perc_cat$coefficients[1:4, 2], 2) 
Pmark <- ifelse(model_weight_perc_cat$coefficients[1:4, 5] < 0.001, "***",
                ifelse(model_weight_perc_cat$coefficients[1:4, 5] >= 0.001 & model_weight_perc_cat$coefficients[1:4, 5] < 0.01, "**",
                       ifelse(model_weight_perc_cat$coefficients[1:4, 5] >= 0.01 & model_weight_perc_cat$coefficients[1:4, 5] < 0.05, "*", "")))
Pvalue <- round(model_weight_perc_cat$coefficients[1:4, 5], 3)
LCI <- round(model_weight_perc_cat$conf.int[1:4, 3], 2)
UCI <- round(model_weight_perc_cat$conf.int[1:4, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark ), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("weight_change_perc_cat", "", table$type)
##计算死亡率
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$weight_change_perc_cat),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
addWorksheet(wb, "Weight Change (perc)")
writeData(wb, sheet = "Weight Change (perc)", table_combined)

# 绘制森林图 - Weight Change (%)
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

plot_data <- rbind(
  data.frame(type = "Stable", HR = 1, LCI = 1, UCI = 1),
  plot_data
)

plot_data$type <- factor(plot_data$type,
                         levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                        "Substantial decrease", "Substantial increase")))

p4 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "Weight Change (%)", y = "Hazard Ratio (95% CI)")








##relative change patterns for wc
fit_model_waist_perc_cat <- coxph(Surv(Time_years, Status) ~ waist_change_cat_perc + SEX + age + EDU_new + SMOKE + TC +FBG + TBIL + LDL + HDL + RSBP + 
                                    PAF + MAR + HB + WCELL + GPT + AST + SCR, data = Change_Data)
model_waist_perc_cat <- summary(fit_model_waist_perc_cat)
HR <- round(model_waist_perc_cat$coefficients[1:4, 2], 2) 
Pmark <- ifelse(model_waist_perc_cat$coefficients[1:4, 5] < 0.001, "***",
                ifelse(model_waist_perc_cat$coefficients[1:4, 5] >= 0.001 & model_waist_perc_cat$coefficients[1:4, 5] < 0.01, "**",
                       ifelse(model_waist_perc_cat$coefficients[1:4, 5] >= 0.01 & model_waist_perc_cat$coefficients[1:4, 5] < 0.05, "*", "")))
Pvalue <- round(model_waist_perc_cat$coefficients[1:4, 5], 3)
LCI <- round(model_waist_perc_cat$conf.int[1:4, 3], 2)
UCI <- round(model_waist_perc_cat$conf.int[1:4, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")" , Pmark), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("waist_change_cat_perc", "", table$type)
##计算死亡率
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$waist_change_cat_perc),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
addWorksheet(wb, "Waist Change (perc)")
writeData(wb, sheet = "Waist Change (perc)", table_combined)

# 绘制森林图 - Waist Circumference Change (%)
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

plot_data <- rbind(
  data.frame(type = "Stable", HR = 1, LCI = 1, UCI = 1),
  plot_data
)

plot_data$type <- factor(plot_data$type,
                         levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                        "Substantial decrease", "Substantial increase")))

p5 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "Waist Circumference Change (%)", y = "Hazard Ratio (95% CI)")








###BMI transition patterns
fit_model_BMI_change_pattern <- coxph(Surv(Time_years, Status) ~ BMI_change_pattern + SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                                        PAF + MAR + HB + WCELL + GPT + AST + SCR +FBG + TBIL, data = Change_Data)
model_BMI_change_pattern <- summary(fit_model_BMI_change_pattern)
HR <- round(model_BMI_change_pattern$coefficients[1:15, 2], 2) 
Pmark <- ifelse(model_BMI_change_pattern$coefficients[1:15, 5] < 0.001, "***",
                ifelse(model_BMI_change_pattern$coefficients[1:15, 5] >= 0.001 & model_BMI_change_pattern$coefficients[1:15, 5] < 0.01, "**",
                       ifelse(model_BMI_change_pattern$coefficients[1:15, 5] >= 0.01 & model_BMI_change_pattern$coefficients[1:15, 5] < 0.05, "*", "")))
Pvalue <- round(model_BMI_change_pattern$coefficients[1:15, 5], 3)
LCI <- round(model_BMI_change_pattern$conf.int[1:15, 3], 2)
UCI <- round(model_BMI_change_pattern$conf.int[1:15, 4], 2)
table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")" , Pmark), "p" = Pvalue)
table$HR_model <- paste(table$Hazard, table$CI )
table <- rownames_to_column(table,var = "type")
table$type <- gsub("BMI_change_pattern", "", table$type)
##计算死亡率
CR <- aggregate(cbind(Change_Data$Status, Change_Data$Time_years),
                by = list(type = Change_Data$BMI_change_pattern),
                FUN = sum)
colnames(CR)[2:3] <- c("Death", "YPOP")
CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
table_combined <- subset(table_combined, select = c(1:4, 8))
table_combined <- table_combined %>%
  dplyr::rename(BMI_change_pattern = type)
pattern_counts <- Change_Data %>%
  dplyr::count(BMI_change_pattern, name = "Count")
table_combined <- table_combined %>%
  left_join(pattern_counts, by = "BMI_change_pattern")
table_combined <- table_combined %>%
  relocate(Count, .after = 1)  # 将 Count 移动到第二列
addWorksheet(wb, "BMI pattern")
writeData(wb, sheet = "BMI pattern", table_combined)
saveWorkbook(wb, "Table_COX_Regression_Results.xlsx", overwrite = TRUE)

# 绘制森林图 - BMI Transition Pattern
plot_data <- data.frame(
  type = table$type,
  HR = HR,
  LCI = LCI,
  UCI = UCI
)

plot_data <- rbind(
  data.frame(type = "Stable", HR = 1, LCI = 1, UCI = 1),
  plot_data
)

plot_data$type <- factor(plot_data$type, levels = rev(unique(plot_data$type)))

p6 <- ggplot(plot_data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "BMI Transition Pattern", y = "Hazard Ratio (95% CI)")


library(patchwork)
library(ggplot2)
library(showtext)
library(extrafont)
if ("Times New Roman" %in% extrafont::fonts()) {
  font_family <- "Times New Roman"
} else {
  # 备用方案：使用系统默认serif字体
  font_family <- "serif"
}

# font: times new roman
times_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size - 2, color = "black"),
      axis.text.y = element_text(face = "bold", margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      plot.tag = element_text(family = font_family, face = "bold", size = 16, 
                              margin = margin(0, 0, 10, 0))
    )
}

# 美化每个森林图的函数
beautify_forest_plot <- function(p, y_limits) {
  p +
    coord_flip(ylim = y_limits) +
    times_theme(base_size = 14) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_pointrange(aes(ymin = LCI, ymax = UCI), 
                    size = 0.8, fatten = 2.5, color = "steelblue") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = element_blank(), y = "Hazard Ratio (95% CI)") +
    theme(axis.title.x = element_text(face = "bold", margin = margin(t = 10)))
}

# 应用美化（左侧5个图形）
p1_left <- beautify_forest_plot(p1, c(0.5, 1.5))
p2_left <- beautify_forest_plot(p2, c(0.5, 1.5)) 
p3_left <- beautify_forest_plot(p3, c(0.5, 1.5))
p4_left <- beautify_forest_plot(p4, c(0.5, 1.5))
p5_left <- beautify_forest_plot(p5, c(0.5, 1.5))

# 右侧图形（不同的y轴范围）
p6_right <- beautify_forest_plot(p6, c(0.5, 2.0))


# 创建左侧面板（增加图形间距）
p1_left <- p1_left + labs(tag = "A)")
p2_left <- p2_left + labs(tag = "B)")
p3_left <- p3_left + labs(tag = "C)")
p4_left <- p4_left + labs(tag = "D)")
p5_left <- p5_left + labs(tag = "E)")

left_panel <- (p1_left + plot_spacer() + 
                 p2_left + plot_spacer() + 
                 p3_left + plot_spacer() + 
                 p4_left + plot_spacer() + 
                 p5_left) +
  plot_layout(ncol = 1, heights = c(1,0.05,1,0.05,1,0.05,1,0.05,1)) &
  theme(plot.tag.position = c(0.02, 0.98))
right_panel <- p6_right +
  labs(tag = "F)") +
  theme(plot.tag.position = c(0.02, 0.98))

# 组合两个面板（调整宽度比例）
combined <- left_panel | right_panel +
  plot_layout(widths = c(1, 1.3)) +  # 右侧稍宽以适应更多类别
  plot_annotation(theme = theme(plot.margin = margin(20, 20, 20, 20)))
#ggsave("(Don't Use) Figure_1_Forest_Plots_COX.tiff", combined, 
#       width = 16, height = 20, dpi = 600,  # 增大尺寸和分辨率
#      device = "tiff", compression = "lzw", bg = "white")





#####For figure 1######
# ---- 上面图 
# orders
bar_data_top <- bind_rows(
  data.frame(type = p1$data$type, HR = p1$data$HR, LCI = p1$data$LCI, UCI = p1$data$UCI, group = "Weight Change (kg)"),
  data.frame(type = p2$data$type, HR = p2$data$HR, LCI = p2$data$LCI, UCI = p2$data$UCI, group = "Waist Change (cm)"),
  data.frame(type = p3$data$type, HR = p3$data$HR, LCI = p3$data$LCI, UCI = p3$data$UCI, group = "BMI Change (%)"),
  data.frame(type = p4$data$type, HR = p4$data$HR, LCI = p4$data$LCI, UCI = p4$data$UCI, group = "Weight Change (%)"),
  data.frame(type = p5$data$type, HR = p5$data$HR, LCI = p5$data$LCI, UCI = p5$data$UCI, group = "Waist Change (%)")
)
lancet_colors <- c(
  "Weight Change (kg)"   = "#D55E00",  # 深橙色，替换原来的蓝色
  "Waist Change (cm)"    = "#4C78A8",  # 蓝色
  "BMI Change (%)"       = "#F58518",  # 橙色
  "Weight Change (%)"    = "#FFBF79",  # 浅橙色/粉色
  "Waist Change (%)"     = "#54A24B"   # 绿色
)

bar_data_top$group <- factor(bar_data_top$group, levels = c(
  "BMI Change (%)", "Weight Change (%)", "Waist Change (%)", "Weight Change (kg)", "Waist Change (cm)"
))
desired_order <- c("Stable", "Mild decrease", "Mild increase", "Substantial decrease", "Substantial increase")
bar_data_top$type <- factor(bar_data_top$type, levels = desired_order)
p_top <- ggplot(bar_data_top, aes(x = type, y = HR, fill = group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), position = position_dodge(width = 0.7), width = 0.25) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = lancet_colors) +
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  labs(x = "Change Pattern", y = "Hazard Ratio (95% CI)") +
  theme(
    panel.grid = element_blank(),            # 去网格
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 40, hjust = 1, family = "Times New Roman", size = 16),
    axis.text.y = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_blank(),          # 去掉 "Measure"
    legend.text = element_text(size = 16)
  )

# ---- lower panel: BMI transition pattern
bar_data_bottom <- data.frame(
  type = factor(p6$data$type, levels = unique(p6$data$type)),
  HR = p6$data$HR, LCI = p6$data$LCI, UCI = p6$data$UCI
)
bar_data_bottom$type <- as.character(bar_data_bottom$type)
bar_data_bottom$type[bar_data_bottom$type == "Stable"] <- "Normal weight → Normal weight"
p_bottom <- ggplot(bar_data_bottom, aes(x = type, y = HR)) +
  geom_col(fill = "#225EA8", width = 0.5, color = "black") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.25) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  labs(x = "BMI Transition Pattern", y = "Hazard Ratio (95% CI)") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 40, hjust = 1, family = "Times New Roman", size = 16),
    axis.text.y = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(face = "bold", size = 18)
  )
p_top <- p_top +
  theme(plot.margin = margin(t = 10, r = 100, b = 10, l = 100))  # l=50 左边宽
p_bottom <- p_bottom +
  theme(plot.margin = margin(t = 10, r = 100, b = 10, l = 100))
# ---- merge the two panel 
final_plot <- p_top / p_bottom +
  plot_layout(heights = c(2, 1))

# ---- Figure 1 export
ggsave("Figure1_Barplot_HR_CI.tiff", final_plot,
       width = 16, height = 18, dpi = 600, device = "tiff",
       compression = "lzw", bg = "white")









# ##########亚组分析：基线年龄分组——65-75，75-80，80+
# ##年龄分层：体重绝对值
# Change_Data <- Change_Data %>%
#   mutate(age_group = case_when(
#     age >= 65 & age < 75 ~ "65-75",
#     age >= 75 & age < 80 ~ "75-80",
#     age >= 80           ~ "80+",
#     TRUE                ~ "Other" # 如果存在不符合条件的年龄，归为 Other
#   ))
# age_groups <- c("65-75", "75-80", "80+")
# # 创建一个新的工作簿
# wb <- createWorkbook()
# 
# # 65-75组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "65-75"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "65-75"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_cat,
#                 data = subset(Change_Data, age_group == "65-75"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_65_75 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 75 - 80组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "75-80"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "75-80"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_cat,
#                 data = subset(Change_Data, age_group == "75-80"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_75_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 80+组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "80+"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "80+"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_cat,
#                 data = subset(Change_Data, age_group == "80+"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 合并所有年龄组的结果
# final_table <- rbind(table_combined_65_75, table_combined_75_80, table_combined_80)
# 
# # 添加一个工作表
# addWorksheet(wb, "Weight Change")
# writeData(wb, sheet = "Weight Change", final_table)
# 
# 
# 
# 
# 
# ##年龄分层：腰围绝对值
# # 65-75组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_abs + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "65-75"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:2, 2], 2)  # 修正为model而不是model_waist_cat
# Pmark <- ifelse(model$coefficients[1:2, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:2, 5] >= 0.001 & model$coefficients[1:2, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:2, 5] >= 0.01 & model$coefficients[1:2, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:2, 5], 3)
# LCI <- round(model$conf.int[1:2, 3], 2)
# UCI <- round(model$conf.int[1:2, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "65-75"
# table <- rownames_to_column(table, var = "type")
# table$type <- gsub("waist_change_cat_abs", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_abs,
#                 data = subset(Change_Data, age_group == "65-75"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_65_75 <- subset(table_combined, select = c(1:4, 8,9))
# 
# # 75 - 80组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_abs + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "75-80"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:2, 2], 2)  # 修正为model而不是model_waist_cat
# Pmark <- ifelse(model$coefficients[1:2, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:2, 5] >= 0.001 & model$coefficients[1:2, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:2, 5] >= 0.01 & model$coefficients[1:2, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:2, 5], 3)
# LCI <- round(model$conf.int[1:2, 3], 2)
# UCI <- round(model$conf.int[1:2, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "75-80"
# table <- rownames_to_column(table, var = "type")
# table$type <- gsub("waist_change_cat_abs", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_abs,
#                 data = subset(Change_Data, age_group == "75-80"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_75_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# # 80+组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_abs + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "80+"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:2, 2], 2)  # 修正为model而不是model_waist_cat
# Pmark <- ifelse(model$coefficients[1:2, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:2, 5] >= 0.001 & model$coefficients[1:2, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:2, 5] >= 0.01 & model$coefficients[1:2, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:2, 5], 3)
# LCI <- round(model$conf.int[1:2, 3], 2)
# UCI <- round(model$conf.int[1:2, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "80+"
# table <- rownames_to_column(table, var = "type")
# table$type <- gsub("waist_change_cat_abs", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_abs,
#                 data = subset(Change_Data, age_group == "80+"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# # 合并所有年龄组的结果
# final_table <- rbind(table_combined_65_75, table_combined_75_80, table_combined_80)
# 
# # 添加一个工作表
# addWorksheet(wb, "Waist Change")
# writeData(wb, sheet = "Waist Change", final_table)
# 
# 
# 
# 
# 
# 
# 
# 
# ###年龄分组：BMI变化百分比
# # 65-75组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ BMI_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "65-75"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "65-75"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("BMI_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ BMI_change_perc_cat,
#                 data = subset(Change_Data, age_group == "65-75"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_65_75 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 75 - 80组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ BMI_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "75-80"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "75-80"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("BMI_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ BMI_change_perc_cat,
#                 data = subset(Change_Data, age_group == "75-80"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_75_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 80+组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ BMI_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "80+"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "80+"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("BMI_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ BMI_change_perc_cat,
#                 data = subset(Change_Data, age_group == "80+"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 合并所有年龄组的结果
# final_table <- rbind(table_combined_65_75, table_combined_75_80, table_combined_80)
# 
# # 添加一个工作表
# addWorksheet(wb, "BMI Change (perc)")
# writeData(wb, sheet = "BMI Change (perc)", final_table)
# 
# 
# 
# 
# 
# ###年龄分组：体重变化百分比
# # 65-75组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "65-75"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "65-75"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_perc_cat,
#                 data = subset(Change_Data, age_group == "65-75"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_65_75 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 75 - 80组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "75-80"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "75-80"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_perc_cat,
#                 data = subset(Change_Data, age_group == "75-80"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_75_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 80+组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ weight_change_perc_cat + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "80+"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "80+"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("weight_change_perc_cat", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ weight_change_perc_cat,
#                 data = subset(Change_Data, age_group == "80+"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 合并所有年龄组的结果
# final_table <- rbind(table_combined_65_75, table_combined_75_80, table_combined_80)
# 
# # 添加一个工作表
# addWorksheet(wb, "Weight Change (perc)")
# writeData(wb, sheet = "Weight Change (perc)", final_table)
# 
# 
# 
# ###年龄分组：腰围变化百分比
# # 65-75组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_perc + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "65-75"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "65-75"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("waist_change_cat_perc", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_perc,
#                 data = subset(Change_Data, age_group == "65-75"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_65_75 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 75 - 80组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_perc + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "75-80"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "75-80"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("waist_change_cat_perc", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_perc,
#                 data = subset(Change_Data, age_group == "75-80"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_75_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 80+组 COX回归
# fit <- coxph(Surv(Time_years, Status) ~ waist_change_cat_perc + SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + HDL + RSBP, 
#              data = Change_Data, subset = (age_group == "80+"))
# model <- summary(fit)
# HR <- round(model$coefficients[1:4, 2], 2)
# Pmark <- ifelse(model$coefficients[1:4, 5] < 0.001, "***",
#                 ifelse(model$coefficients[1:4, 5] >= 0.001 & model$coefficients[1:4, 5] < 0.01, "**",
#                        ifelse(model$coefficients[1:4, 5] >= 0.01 & model$coefficients[1:4, 5] < 0.05, "*", "")))
# Pvalue <- round(model$coefficients[1:4, 5], 3)
# LCI <- round(model$conf.int[1:4, 3], 2)
# UCI <- round(model$conf.int[1:4, 4], 2)
# table <- data.frame("Hazard" = HR, "CI" = paste("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
# table$HR_model <- paste(table$Hazard, table$CI )
# table$age_group <- "80+"
# table <- rownames_to_column(table,var = "type")
# table$type <- gsub("waist_change_cat_perc", "", table$type)
# ##计算死亡率(按年龄组)
# CR <- aggregate(cbind(Status, Time_years) ~ waist_change_cat_perc,
#                 data = subset(Change_Data, age_group == "80+"),
#                 FUN = sum)
# colnames(CR)[1:3] <- c("type", "Death", "YPOP")
# CR$YPOP_1000 <- formatC(CR$Death / CR$YPOP * 1000, format = "f", digits = 2); CR$YPOP <- formatC(CR$YPOP, format = "f", digits = 2)
# table_combined <- plyr::join(CR, table, by = c("type"), type = "left")
# table_combined_80 <- subset(table_combined, select = c(1:4, 8,9))
# 
# 
# # 合并所有年龄组的结果
# final_table <- rbind(table_combined_65_75, table_combined_75_80, table_combined_80)
# 
# # 添加一个工作表
# addWorksheet(wb, "Waist Change (perc)")
# writeData(wb, sheet = "Waist Change (perc)", final_table)
# 
# # 保存文件
# #saveWorkbook(wb, "亚组年龄分析结果.xlsx", overwrite = TRUE)







#####subgroup analysis：age + gender #####
variables <- list(
  "Weight Change" = "weight_change_cat",
  "Waist Change" = "waist_change_cat_abs",
  "BMI Change (perc)" = "BMI_change_perc_cat",
  "Weight Change (perc)" = "weight_change_perc_cat",
  "Waist Change (perc)" = "waist_change_cat_perc",
  "BMI Change Pattern" = "BMI_change_pattern"
)
gender_groups <- c("Male", "Female")
# age group
Change_Data <- Change_Data %>%
  mutate(age_group = case_when(
    age >= 65 & age < 75 ~ "65-75",
    age >= 75 & age < 80 ~ "75-80",
    age >= 80 ~ "80+",
    TRUE ~ "Other"
  ))

age_groups <- c("65-75", "75-80", "80+")

# age group & gender
combinations <- expand.grid(
  gender = c("Male", "Female"),
  age_group = c("65-75", "75-80", "80+"),
  stringsAsFactors = FALSE
)
# new work book
wb <- createWorkbook()

# function for subgroup analysis
perform_combined_analysis <- function(data, variable, gender_val, age_group_val) {
  # different covariate selection
  if (variable == "BMI_change_pattern") {
    covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
  } else {
    covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  # COX回归
  formula <- as.formula(paste("Surv(Time_years, Status) ~", variable, "+", covariates))
  
  fit <- coxph(formula, data = data, subset = (SEX == gender_val & age_group == age_group_val))
  model <- summary(fit)
  
  # 提取系数数量
  n_coef <- length(grep(variable, rownames(model$coefficients)))
  
  if (n_coef == 0) {
    message(paste("No coefficients found for", variable, "in gender =", gender_val, "age_group =", age_group_val))
    return(NULL)
  }
  
  # 提取HR, CI, P值
  HR <- round(model$coefficients[1:n_coef, 2], 2)
  p_values <- model$coefficients[1:n_coef, 5]
  Pmark <- ifelse(p_values < 0.001, "***",
                  ifelse(p_values < 0.01, "**",
                         ifelse(p_values < 0.05, "*", "")))
  Pvalue <- round(p_values, 3)
  LCI <- round(model$conf.int[1:n_coef, 3], 2)
  UCI <- round(model$conf.int[1:n_coef, 4], 2)
  
  # table for results
  table <- data.frame(
    "Hazard" = HR,
    "CI" = paste0("(", LCI, "-", UCI, ")", Pmark),
    "p" = Pvalue
  )
  
  table$HR_model <- paste(table$Hazard, table$CI )
  table$gender <- gender_val
  table$age_group <- age_group_val
  table <- rownames_to_column(table, var = "type")
  table$type <- gsub(variable, "", table$type)
  
  # mortality rate
  CR <- aggregate(cbind(Status, Time_years) ~ get(variable), 
                  data = subset(data, SEX == gender_val & age_group == age_group_val), 
                  FUN = sum)
  colnames(CR)[1:3] <- c("type", "Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  # merge the results
  table_combined <- left_join(CR, table, by = "type")
  table_combined <- subset(table_combined, select = c(1:4, 8, 9, 10))
  
  return(table_combined)
}

# for all patterns
for (sheet_name in names(variables)) {
  variable <- variables[[sheet_name]]
  
  results_list <- list()
  
  for (i in 1:nrow(combinations)) {
    gender_val <- combinations$gender[i]
    age_group_val <- combinations$age_group[i]
    
    result <- perform_combined_analysis(Change_Data, variable, gender_val, age_group_val)
    if (!is.null(result)) {
      results_list[[paste(gender_val, age_group_val, sep = "_")]] <- result
    }
  }
  
  # merge all the results
  if (length(results_list) > 0) {
    final_table <- do.call(rbind, results_list)
    
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, final_table)
  }
}

# save tables for subgroup analysis
saveWorkbook(wb, "性别年龄组合分层分析结果.xlsx", overwrite = TRUE)


library(openxlsx)
library(ggplot2)
library(dplyr)
library(patchwork)

# 读取之前保存的Excel文件
wb <- loadWorkbook("性别年龄组合分层分析结果.xlsx")

# 读取BMI Change (perc) sheet
bmi_data <- read.xlsx(wb, sheet = "BMI Change (perc)")

head(bmi_data)
str(bmi_data)

# 提取HR, LCI, UCI
extract_hr_ci <- function(ci_string) {
  # 提取所有数字
  matches <- regmatches(ci_string, gregexpr("[0-9.]+", ci_string))[[1]]
  if (length(matches) >= 3) {
    return(list(HR = as.numeric(matches[1]),
                LCI = as.numeric(matches[2]),
                UCI = as.numeric(matches[3])))
  }
  return(list(HR = NA, LCI = NA, UCI = NA))
}

bmi_processed <- bmi_data %>%
  filter(!is.na(HR_model) & HR_model != "") %>%
  mutate(
    ci_info = lapply(HR_model, extract_hr_ci),
    HR  = sapply(ci_info, function(x) x$HR),
    LCI = sapply(ci_info, function(x) x$LCI),
    UCI = sapply(ci_info, function(x) x$UCI)
  ) %>%
  select(type, HR, LCI, UCI, gender, age_group)

bmi_processed <- bmi_processed %>%
  mutate(
    gender = recode(gender,
                    "男" = "Male",
                    "女" = "Female")
  )



# add（Stable）as reference group
reference_data <- expand.grid(
  type = "Stable",
  HR = 1,
  LCI = 1,
  UCI = 1,
  p = NA,
  gender = unique(bmi_processed$gender),
  age_group = unique(bmi_processed$age_group),
  stringsAsFactors = FALSE
)

bmi_plot_data <- bind_rows(reference_data, bmi_processed)

# order
bmi_plot_data$type <- factor(bmi_plot_data$type,
                             levels = rev(c("Stable", "Mild decrease", "Mild increase",
                                            "Substantial decrease", "Substantial increase")))

bmi_plot_data$gender <- factor(bmi_plot_data$gender)
bmi_plot_data$age_group <- factor(bmi_plot_data$age_group, 
                                  levels = c("65-75", "75-80", "80+"))

# font
if ("Times New Roman" %in% extrafont::fonts()) {
  font_family <- "Times New Roman"
} else {
  font_family <- "serif"
}

times_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size - 2, color = "black"),
      axis.text.y = element_text(face = "bold", margin = margin(r = 10)),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      strip.text = element_text(family = font_family, face = "bold", size = 12),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
}

# forest plot for subgroup analysis
create_stratified_forest_plot <- function(data, y_limits = c(0.5, 2.0)) {
  ggplot(data, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_pointrange(size = 0.6, fatten = 2, color = "steelblue") +
    coord_flip(ylim = y_limits) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    labs(x = "BMI Change (%)", y = "Hazard Ratio (95% CI)") +
    times_theme(base_size = 12) +
    facet_grid(age_group ~ gender, scales = "free_y") +
    theme(strip.text = element_text(face = "bold"),
          panel.spacing = unit(1.5, "lines"))
}

# 生成图形
bmi_stratified_plot <- create_stratified_forest_plot(bmi_plot_data)
#ggsave("(Don't use) Figure_2_Stratified_sex_age.tiff", bmi_stratified_plot,
#       width = 12, height = 10, dpi = 600,
#       device = "tiff", compression = "lzw", bg = "white")



#### Figure 2 & 3 (for A4纸) ####

###1. 5 relative and absolute change patterns
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)

# 1. 读取Excel
wb <- loadWorkbook("性别年龄组合分层分析结果.xlsx")

## 5 patterns
indicators <- c("BMI Change (perc)", "Weight Change (perc)", "Waist Change (perc)",
                "Weight Change", "Waist Change")

# font
font_family <- ifelse("Times New Roman" %in% extrafont::fonts(), "Times New Roman", "serif")
times_theme <- function(base_size = 11){
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size+1, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size-1),
      axis.line = element_line(color = "black", linewidth = 0.4),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 2. colors
lancet_colors <- c(
  "M 65-75"    = "#D55E00",  # 深橙
  "M 75-80"    = "#F58518",  # 中橙
  "M 80+"      = "#FFBF79",  # 浅橙
  "F 65-75"  = "#003F5C",  # 深蓝
  "F 75-80"  = "#2F4B7C",  # 中蓝
  "F 80+"    = "#7A5195"   # 浅蓝紫
)

# 创建详细的组别标签映射 - 确保男性在前
group_labels <- c(
  "M 65-75" = "Male, 65-75 years",
  "M 75-80" = "Male, 75-80 years", 
  "M 80+"   = "Male, ≥80 years",
  "F 65-75" = "Female, 65-75 years",
  "F 75-80" = "Female, 75-80 years",
  "F 80+"   = "Female, ≥80 years"
)

# 提取 HR, LCI, UCI 的函数
extract_hr_ci <- function(ci_string) {
  matches <- regmatches(ci_string, gregexpr("[0-9.]+", ci_string))[[1]]
  if(length(matches) >= 3) {
    return(list(HR = as.numeric(matches[1]),
                LCI = as.numeric(matches[2]),
                UCI = as.numeric(matches[3])))
  }
  return(list(HR = NA, LCI = NA, UCI = NA))
}

# 存储图对象
plots_list <- list()

# 3. 循环处理每个指标
for(i in seq_along(indicators)){
  sheet_name <- indicators[i]
  
  # 读取数据
  data_raw <- read.xlsx(wb, sheet = sheet_name)
  
  data_processed <- data_raw %>%
    filter(!is.na(HR_model) & HR_model != "") %>%
    mutate(
      ci_info = lapply(HR_model, extract_hr_ci),
      HR  = sapply(ci_info, function(x) x$HR),
      LCI = sapply(ci_info, function(x) x$LCI),
      UCI = sapply(ci_info, function(x) x$UCI)
    ) %>%
    select(type, HR, LCI, UCI, gender, age_group)
  
  # 去掉 reference
  data_plot <- data_processed %>% filter(type != "Stable")
  
  # 设置分类顺序
  data_plot$type <- factor(data_plot$type,
                           levels = c("Mild decrease","Mild increase",
                                      "Substantial decrease","Substantial increase"))
  
  # 改分组名字 - 使用简写用于颜色匹配
  data_plot$group <- paste(ifelse(data_plot$gender=="Male","M","F"), data_plot$age_group, sep=" ")
  
  # 手动排序组别 - 使用简写标签，确保男性在前
  data_plot$group <- factor(data_plot$group, 
                            levels = c("M 65-75", "M 75-80", "M 80+",
                                       "F 65-75", "F 75-80", "F 80+"))
  
  # 创建详细标签列
  data_plot$group_detailed <- group_labels[data_plot$group]
  
  # 手动排序组别 - 使用详细标签，确保男性在前
  data_plot$group_detailed <- factor(data_plot$group_detailed, 
                                     levels = c("Male, 65-75 years", "Male, 75-80 years", "Male, ≥80 years",
                                                "Female, 65-75 years", "Female, 75-80 years", "Female, ≥80 years"))
  
  # 绘图
  p <- ggplot(data_plot, aes(x = type, y = HR, fill = group)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_col(position = position_dodge(width = 0.85), width = 0.75, color = "black") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI),
                  position = position_dodge(width = 0.85), width = 0.25) +
    scale_fill_manual(values = lancet_colors, 
                      labels = group_labels,
                      breaks = c("M 65-75", "M 75-80", "M 80+",
                                 "F 65-75", "F 75-80", "F 80+"),  # 明确指定顺序
                      drop = FALSE) +
    labs(x = if(i==length(indicators)) "Change Pattern" else NULL,
         y = "Hazard Ratio (95% CI)") +
    times_theme(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = if(i==1) "top" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.spacing.y = unit(0.2, "cm"),
      legend.key.size = unit(0.8, "cm"),
      plot.title = element_blank()
    ) +
    guides(fill = guide_legend(byrow = TRUE, nrow = 2))  # 关键：分2行显示
  
  plots_list[[i]] = p
}

# 4. merge all 5 plots
final_plot <- (plots_list[[1]] / plots_list[[2]] / plots_list[[3]] / 
                 plots_list[[4]] / plots_list[[5]]) +
  plot_annotation(tag_levels = list(paste0(LETTERS, ")"))) & 
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.1, 1)
  )

# 5. export figure 2
ggsave("Figure2_Barplot_HR_CI_All_A4.tiff", final_plot,
       width = 8.27, height = 13, units = "in",
       dpi = 600, device = "tiff", compression = "lzw", bg = "white")



###2. figure 3 for BMI transition patterns
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)

# 读取Excel
wb <- loadWorkbook("性别年龄组合分层分析结果.xlsx")

# 定义字体主题
font_family <- ifelse("Times New Roman" %in% extrafont::fonts(), "Times New Roman", "serif")
times_theme <- function(base_size = 11){
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size+1, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size-1),
      axis.line = element_line(color = "black", linewidth = 0.4),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 配色
lancet_colors <- c(
  "M 65-75" = "#D55E00",
  "M 75-80" = "#F58518",
  "M 80+"   = "#FFBF79",
  "F 65-75" = "#003F5C",
  "F 75-80" = "#2F4B7C",
  "F 80+"   = "#7A5195"
)

# 创建详细的组别标签映射
group_labels <- c(
  "M 65-75" = "Male, 65-75 years",
  "M 75-80" = "Male, 75-80 years", 
  "M 80+"   = "Male, ≥80 years",
  "F 65-75" = "Female, 65-75 years",
  "F 75-80" = "Female, 75-80 years",
  "F 80+"   = "Female, ≥80 years"
)

# 提取 HR, LCI, UCI 的函数
extract_hr_ci <- function(ci_string) {
  matches <- regmatches(ci_string, gregexpr("[0-9.]+", ci_string))[[1]]
  if(length(matches) >= 3) {
    return(list(HR = as.numeric(matches[1]),
                LCI = as.numeric(matches[2]),
                UCI = as.numeric(matches[3])))
  }
  return(list(HR = NA, LCI = NA, UCI = NA))
}

# 读取数据
data_raw <- read.xlsx(wb, sheet = "BMI Change Pattern")

data_processed <- data_raw %>%
  filter(!is.na(HR_model) & HR_model != "") %>%
  mutate(
    ci_info = lapply(HR_model, extract_hr_ci),
    HR  = sapply(ci_info, function(x) x$HR),
    LCI = sapply(ci_info, function(x) x$LCI),
    UCI = sapply(ci_info, function(x) x$UCI)
  ) %>%
  select(type, HR, LCI, UCI, gender, age_group)

# 去掉 reference
data_processed <- data_processed %>% filter(type != "Stable")

# 分组
groups <- list(
  Stable = c("Underweight → Underweight", 
             "Overweight → Overweight", 
             "Obesity → Obesity"),
  ToOver = c("Underweight → Normal weight",
             "Normal weight → Overweight",
             "Overweight → Obesity"),
  ToLight = c("Normal weight → Underweight",
              "Overweight → Normal weight",
              "Overweight → Underweight",
              "Obesity → Normal weight")
)

plots_list <- list()

for(i in seq_along(groups)){
  subset_patterns <- groups[[i]]
  data_plot <- data_processed %>% filter(type %in% subset_patterns)
  
  # 顺序固定
  data_plot$type <- factor(data_plot$type, levels = subset_patterns)
  
  # 合并分组名字 - 使用简写用于颜色匹配
  data_plot$group <- paste(ifelse(data_plot$gender=="Male","M","F"), data_plot$age_group, sep=" ")
  
  # 手动排序组别 - 使用简写标签
  data_plot$group <- factor(data_plot$group, 
                            levels = c("M 65-75","M 75-80","M 80+",
                                       "F 65-75","F 75-80","F 80+"))
  
  # 绘图
  p <- ggplot(data_plot, aes(x = type, y = HR, fill = group)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_col(position = position_dodge(width = 0.80), width = 0.75, color = "black") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI),
                  position = position_dodge(width = 0.80), width = 0.25) +
    scale_fill_manual(values = lancet_colors, 
                      labels = group_labels,
                      breaks = names(group_labels),
                      drop = FALSE) +
    labs(x = if(i==length(groups)) "BMI Transition Pattern" else NULL,
         y = "Hazard Ratio (95% CI)") +
    times_theme(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = if(i==1) "top" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.spacing.y = unit(0.2, "cm"),
      legend.key.size = unit(0.8, "cm"),
      plot.title = element_blank()
    ) +
    guides(fill = guide_legend(byrow = TRUE, nrow = 2))  # 关键：分2行显示
  
  plots_list[[i]] <- p
}

# 拼接三张图（竖排，自动加 A)、B)、C)…）
final_plot <- (plots_list[[1]] / plots_list[[2]] / plots_list[[3]]) +
  plot_annotation(tag_levels = list(paste0(LETTERS, ")"))) &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    plot.tag.position = c(0.1, 1)
  )

# export figure 3
ggsave("Figure3_BMI_Transition_Patterns_A4.tiff", final_plot,
       width = 8.27, height = 11.7, units = "in",
       dpi = 600, device = "tiff", compression = "lzw", bg = "white")





######Analysis for Cause-specific Death####
##Top level Outcome —— communicble, noncommunicable and injureies 
variables <- list(
  "Weight Change" = "weight_change_cat",
  "Waist Change" = "waist_change_cat_abs",
  "BMI Change (perc)" = "BMI_change_perc_cat",
  "Weight Change (perc)" = "weight_change_perc_cat",
  "Waist Change (perc)" = "waist_change_cat_perc",
  "BMI Change Pattern" = "BMI_change_pattern"
)
Change_Data <- Change_Data %>%
  mutate(
    Top_level_outcomes = case_when(
      grepl("^[AB]", icd_new) ~ "Top_Communicable",
      grepl("^J(0[0-9]|1[0-9]|2[0-2])", icd_new) ~ "Top_Communicable",
      grepl("^[C-R]", icd_new) ~ "Top_NonCommunicable",
      grepl("^[S-Y]", icd_new) ~ "Top_Injuries",
      TRUE ~ NA_character_
    )
  )
#### 定义 Top-level 死亡原因分组
top_groups <- c("Top_Communicable", "Top_NonCommunicable", "Top_Injuries")

#### setup Functions for Top-level Cause-specific Cox
perform_top_analysis <- function(data, variable, top_val) {
  # 构造目标死因的 Status_temp
  temp_data <- data %>%
    mutate(Status_temp = ifelse(Top_level_outcomes == top_val & Status == 1, 1, 0))
  
  # 根据变量决定公式
  if (variable == "BMI_change_pattern") {
    covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
  } else {
    covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  # COX回归
  formula <- as.formula(paste("Surv(Time_years, Status_temp) ~", variable, "+", covariates))
  fit <- coxph(formula, data = temp_data)
  model <- summary(fit)
  
  # 提取 HR, CI, P 值
  n_coef <- length(grep(variable, rownames(model$coefficients)))
  if (n_coef == 0) {
    message(paste("No coefficients found for", variable, "in Top_level_outcomes =", top_val))
    return(NULL)
  }
  
  HR <- round(model$coefficients[1:n_coef, 2], 2)
  p_values <- model$coefficients[1:n_coef, 5]
  Pmark <- ifelse(p_values < 0.001, "***",
                  ifelse(p_values < 0.01, "**",
                         ifelse(p_values < 0.05, "*", "")))
  Pvalue <- round(p_values, 3)
  LCI <- round(model$conf.int[1:n_coef, 3], 2)
  UCI <- round(model$conf.int[1:n_coef, 4], 2)
  
  # 创建结果表格
  table <- data.frame(
    "Hazard" = HR,
    "CI" = paste0("(", LCI, "-", UCI, ")", Pmark),
    "p" = Pvalue
  )
  
  table$HR_model <- paste(table$Hazard, table$CI )
  table$Top_level <- top_val
  table <- rownames_to_column(table, var = "type")
  table$type <- gsub(variable, "", table$type)
  
  # 计算死亡率（目标死因）
  CR <- aggregate(cbind(Status_temp, Time_years) ~ get(variable), data = temp_data, FUN = sum)
  colnames(CR)[1:3] <- c("type", "Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  # 合并结果
  table_combined <- left_join(CR, table, by = "type")
  table_combined <- subset(table_combined, select = c(1:4, 8, 9))
  
  return(table_combined)
}

#### Tables for top levels of causes of death
wb_top <- createWorkbook()

for (sheet_name in names(variables)) {
  variable <- variables[[sheet_name]]
  results_list <- list()
  
  for (top_val in top_groups) {
    result <- perform_top_analysis(Change_Data, variable, top_val)
    if (!is.null(result)) {
      results_list[[top_val]] <- result
    }
  }
  
  if (length(results_list) > 0) {
    final_table <- do.call(rbind, results_list)
    addWorksheet(wb_top, sheet_name)
    writeData(wb_top, sheet = sheet_name, final_table)
  }
}

saveWorkbook(wb_top, "Top_level_outcomes_CauseSpecificCox.xlsx", overwrite = TRUE)













####Second level：Circulatory, respiratory, neoplasms, digestive and metabolic
Change_Data <- Change_Data %>%
  mutate(
    Major_outcomes = case_when(
      grepl("^I", icd_new) ~ "Second_Circulatory",
      grepl("^J", icd_new) ~ "Second_Respiratory",
      grepl("^C", icd_new) ~ "Second_Neoplasms",
      grepl("^K([0-9]{2})", icd_new) & as.numeric(sub("^K", "", icd_new)) <= 93 ~ "Second_Digestive",
      grepl("^E", icd_new) ~ "Second_Metabolic",
      TRUE ~ NA_character_
    )
  )
# Major-level 死因分组
major_groups <- c("Second_Circulatory", "Second_Respiratory", "Second_Neoplasms", 
                  "Second_Digestive", "Second_Metabolic")

# Cause-specific Cox 分析函数
perform_major_analysis <- function(data, variable, major_val) {
  # 构造目标死因 Status_temp
  temp_data <- data %>%
    mutate(Status_temp = ifelse(Major_outcomes == major_val & Status == 1, 1, 0))
  
  # 根据变量决定公式
  if (variable == "BMI_change_pattern") {
    covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
  } else {
    covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  # COX回归
  formula <- as.formula(paste("Surv(Time_years, Status_temp) ~", variable, "+", covariates))
  fit <- coxph(formula, data = temp_data)
  model <- summary(fit)
  
  # 提取 HR, CI, P 值
  n_coef <- length(grep(variable, rownames(model$coefficients)))
  if (n_coef == 0) return(NULL)
  
  HR <- round(model$coefficients[1:n_coef, 2], 2)
  p_values <- model$coefficients[1:n_coef, 5]
  Pmark <- ifelse(p_values < 0.001, "***",
                  ifelse(p_values < 0.01, "**",
                         ifelse(p_values < 0.05, "*", "")))
  Pvalue <- round(p_values, 3)
  LCI <- round(model$conf.int[1:n_coef, 3], 2)
  UCI <- round(model$conf.int[1:n_coef, 4], 2)
  
  # 创建结果表格
  table <- data.frame(
    "Hazard" = HR,
    "CI" = paste0("(", LCI, "-", UCI, ")", Pmark),
    "p" = Pvalue
  )
  
  table$HR_model <- paste(table$Hazard, table$CI )
  table$Major_level <- major_val
  table <- rownames_to_column(table, var = "type")
  table$type <- gsub(variable, "", table$type)
  
  # 计算死亡率
  CR <- aggregate(cbind(Status_temp, Time_years) ~ get(variable), data = temp_data, FUN = sum)
  colnames(CR)[1:3] <- c("type", "Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  table_combined <- left_join(CR, table, by = "type")
  table_combined <- subset(table_combined, select = c(1:4, 8, 9))
  
  return(table_combined)
}

# export tables for second level of causes of death
wb_major <- createWorkbook()

for (sheet_name in names(variables)) {
  variable <- variables[[sheet_name]]
  results_list <- list()
  
  for (major_val in major_groups) {
    result <- perform_major_analysis(Change_Data, variable, major_val)
    if (!is.null(result)) results_list[[major_val]] <- result
  }
  
  if (length(results_list) > 0) {
    final_table <- do.call(rbind, results_list)
    addWorksheet(wb_major, sheet_name)
    writeData(wb_major, sheet = sheet_name, final_table)
  }
}

saveWorkbook(wb_major, "Major_outcomes_CauseSpecificCox.xlsx", overwrite = TRUE)


#Step 1. for top level
wb_top <- loadWorkbook("Top_level_outcomes_CauseSpecificCox.xlsx")
top_data <- read.xlsx(wb_top, sheet = "BMI Change (perc)")
top_data <- top_data %>%
  mutate(
    # 提取 HR 和 95%CI
    HR = as.numeric(str_match(HR_model, "^([0-9.]+)")[,2]),
    ci_info = str_match(HR_model, "\\((.*?)\\-(.*?)\\)"),
    LCI = as.numeric(ci_info[,2]),
    UCI = as.numeric(ci_info[,3]),
    Top_level = gsub("^Top_", "", Top_level)   # 去掉 "Top_"
  ) %>%
  select(type, HR, LCI, UCI, Top_level)


# Step 2. for Second-level 
wb_major <- loadWorkbook("Major_outcomes_CauseSpecificCox.xlsx")
major_data <- read.xlsx(wb_major, sheet = "BMI Change (perc)")

major_data <- major_data %>%
  mutate(
    # 提取 HR 和 95%CI
    HR = as.numeric(str_match(HR_model, "^([0-9.]+)")[,2]),
    ci_info = str_match(HR_model, "\\((.*?)\\-(.*?)\\)"),
    LCI = as.numeric(ci_info[,2]),
    UCI = as.numeric(ci_info[,3]),
    Major_level = gsub("^Second_", "", Major_level)   # 去掉 "Second_"
  ) %>%
  select(type, HR, LCI, UCI, Major_level)


#Step 3. function for forest plot
forest_theme <- function(base_size = 16) {   # 原来 12 改为 16
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "serif", color = "black"),
      axis.title = element_text(size = base_size + 2, face = "bold"),  # 坐标轴标题略大
      axis.text = element_text(size = base_size, color = "black"),     # 坐标轴文字增大
      axis.line = element_line(color = "black", linewidth = 0.6),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.tag = element_text(size = base_size + 4, face = "bold"),    # tag 文字加大
      plot.title = element_text(size = base_size + 4, face = "bold"),  # 图标题加大
      plot.margin = margin(15, 15, 15, 15)
    )
}

make_forest <- function(df, title_label, tag_label, y_limits = c(0.5, 2.0)) {
  ggplot(df, aes(x = type, y = HR, ymin = LCI, ymax = UCI)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
    geom_pointrange(size = 0.8, fatten = 2.5, color = "steelblue") +
    coord_flip(ylim = y_limits) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +  # 控制刻度数
    labs(x = NULL, y = "Hazard Ratio (95% CI)", title = title_label, tag = tag_label) +
    forest_theme(base_size = 16)
}


# Step 4. 分别画 Top-level 三个图 
p1 <- make_forest(filter(top_data, Top_level == "Communicable"), "Communicable diseases", "A)")
p2 <- make_forest(filter(top_data, Top_level == "NonCommunicable"), "Non-communicable diseases", "B)")
p3 <- make_forest(filter(top_data, Top_level == "Injuries"), "Injuries", "C)")

top_panel <- p1 | p2 | p3


#Step 5. 分别画 Second-level 五个图 
q1 <- make_forest(filter(major_data, Major_level == "Circulatory"), "Circulatory", "D)")
q2 <- make_forest(filter(major_data, Major_level == "Respiratory"), "Respiratory", "E)")
q3 <- make_forest(filter(major_data, Major_level == "Neoplasms"), "Neoplasms", "F)")
q4 <- make_forest(filter(major_data, Major_level == "Digestive"), "Digestive", "G)")
q5 <- make_forest(filter(major_data, Major_level == "Metabolic"), "Metabolic", "H)")

major_panel <- q1 | q2 | q3 | q4 | q5


# Step 6. 上面一行放3个，下面一行放5个 
combined <- (top_panel) / (major_panel) +
  plot_annotation(theme = theme(plot.margin = margin(20, 20, 20, 20)))
#ggsave("Figure3_Cause_specific_TopMajor.tiff", combined,
#       width = 20, height = 14, dpi = 600,
#       device = "tiff", compression = "lzw", bg = "white")


####Figure4: cause specific mortlity & 5 change patterns####
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)
library(stringr)

# 读取两个Excel文件
wb_top <- loadWorkbook("Top_level_outcomes_CauseSpecificCox.xlsx")
wb_major <- loadWorkbook("Major_outcomes_CauseSpecificCox.xlsx")

# 指标顺序和标题
indicators <- c("BMI Change (perc)", "Weight Change (perc)", 
                "Waist Change (perc)", "Weight Change", "Waist Change")

column_labels <- c("BMI Change (%)", "Weight Change (%)", "Waist Change (%)",
                   "Weight Change (kg)", "Waist Change (cm)")

# 字体主题
font_family <- ifelse("Times New Roman" %in% extrafont::fonts(), "Times New Roman", "serif")
times_theme <- function(base_size = 15){
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size+2, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size-1),
      axis.line = element_line(color = "black", linewidth = 0.4),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 提取 HR, LCI, UCI
extract_hr_ci <- function(ci_string) {
  matches <- str_extract_all(ci_string, "[0-9.]+")[[1]]
  if(length(matches) >= 3){
    return(list(HR=as.numeric(matches[1]),
                LCI=as.numeric(matches[2]),
                UCI=as.numeric(matches[3])))
  }
  return(list(HR=NA, LCI=NA, UCI=NA))
}

#Top-level
process_top_data <- function(wb, sheet_name) {
  data_raw <- read.xlsx(wb, sheet = sheet_name)
  
  data_processed <- data_raw %>%
    dplyr::filter(!is.na(HR_model) & HR_model != "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci_info = list(extract_hr_ci(HR_model)),
      HR  = ci_info$HR,
      LCI = ci_info$LCI,
      UCI = ci_info$UCI
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Top_level = factor(Top_level, 
                                     levels = c("Top_Communicable", "Top_NonCommunicable", "Top_Injuries")),
                  level_type = "Top") %>%
    dplyr::select(type, Top_level, HR, LCI, UCI, level_type)
  
  return(data_processed)
}

#Major-level
process_major_data <- function(wb, sheet_name) {
  data_raw <- read.xlsx(wb, sheet = sheet_name)
  
  data_processed <- data_raw %>%
    dplyr::filter(!is.na(HR_model) & HR_model != "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci_info = list(extract_hr_ci(HR_model)),
      HR  = ci_info$HR,
      LCI = ci_info$LCI,
      UCI = ci_info$UCI
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Top_level = factor(Major_level,
                                     levels = c("Second_Circulatory", "Second_Respiratory", "Second_Neoplasms",
                                                "Second_Digestive", "Second_Metabolic")),
                  level_type = "Major") %>%
    dplyr::select(type, Top_level, HR, LCI, UCI, level_type)
  
  return(data_processed)
}

# tag all the causes of death
all_disease_labels <- c(
  "Top_Communicable" = "Major Cause Categories\nA)", 
  "Top_NonCommunicable" = "\nB)", 
  "Top_Injuries" = "\nC)",
  "Second_Circulatory" = "Specific Cause Subcategories\nD)",
  "Second_Respiratory" = "\nE)",
  "Second_Neoplasms" = "\nF)",
  "Second_Digestive" = "\nG)",
  "Second_Metabolic" = "\nH)"
)

plots_list <- list()

for(i in seq_along(indicators)){
  sheet_name <- indicators[i]
  
  # Top-level
  top_data <- process_top_data(wb_top, sheet_name)
  
  # Major-level
  major_data <- process_major_data(wb_major, sheet_name)
  
  # merge
  combined_data <- bind_rows(top_data, major_data)
  
  if(i == 1){
    type_levels <- unique(combined_data$type)
  }
  
  combined_data <- combined_data %>%
    dplyr::mutate(type = factor(type, levels = type_levels))
  
  # 添加灰白交错的背景
  combined_data <- combined_data %>%
    dplyr::group_by(Top_level) %>%
    dplyr::mutate(row_num = row_number(),
                  bg_color = ifelse(row_num %% 2 == 1, "gray90", "white")) %>%
    dplyr::ungroup()
  
  # 只在第一列显示疾病分层标签
  if(i == 1) {
    # 第一列：显示A-H标签（包含分类标题）
    p <- ggplot(combined_data, aes(x = HR, y = type)) +
      geom_rect(aes(ymin = as.numeric(type) - 0.5, ymax = as.numeric(type) + 0.5,
                    xmin = -Inf, xmax = Inf, fill = bg_color),
                show.legend = FALSE) +
      scale_fill_manual(values = c("gray90" = "gray90", "white" = "white")) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.4) +
      geom_point(size = 2, color = "black") +
      geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, color = "gray50") +
      facet_wrap(~Top_level, ncol = 1, labeller = as_labeller(all_disease_labels), scales = "free_y") +
      labs(x = NULL, y = NULL, title = column_labels[i]) +
      times_theme(base_size = 15) +
      theme(
        strip.text = element_text(face = "bold", hjust = 0, size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines")
      )
  } else {
    # 其他列：不显示facet标签
    p <- ggplot(combined_data, aes(x = HR, y = type)) +
      geom_rect(aes(ymin = as.numeric(type) - 0.5, ymax = as.numeric(type) + 0.5,
                    xmin = -Inf, xmax = Inf, fill = bg_color),
                show.legend = FALSE) +
      scale_fill_manual(values = c("gray90" = "gray90", "white" = "white")) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 0.4) +
      geom_point(size = 2, color = "black") +
      geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, color = "gray50") +
      facet_wrap(~Top_level, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL, title = column_labels[i]) +
      times_theme(base_size = 15) +
      theme(
        strip.text = element_blank(),  # 隐藏facet标签
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(0.5, "lines")
      )
  }
  
  plots_list[[i]] <- p
}

# 横向排列五列
final_plot <- wrap_plots(plots_list, ncol = 5)

# 调整高度以适应8行数据和分类标题
# export Figure 4
ggsave("Figure4_Combined_CauseSpecific_Forest_for_change_pattern.tiff", final_plot,
       width = 24, height = 14, units = "in", dpi = 600,
       device = "tiff", compression = "lzw", bg = "white")



#####Figure5: Cause specific mortality for BMI transition patterns#####
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(patchwork)
library(stringr)

# 读取两个Excel文件
wb_top <- loadWorkbook("Top_level_outcomes_CauseSpecificCox.xlsx")
wb_major <- loadWorkbook("Major_outcomes_CauseSpecificCox.xlsx")

# 定义Pattern分组和顺序（按你想要的从上到下顺序）
pattern_groups <- list(
  Stable = c("Underweight → Underweight", 
             "Overweight → Overweight", 
             "Obesity → Obesity"),
  ToOver = c("Underweight → Normal weight",
             "Normal weight → Overweight",
             "Overweight → Obesity"),
  ToLight = c("Normal weight → Underweight",
              "Overweight → Normal weight",
              "Obesity → Overweight")
)

# 按你想要的顺序定义pattern_order（从上到下）
pattern_order <- c(
  # Stable (最上面)
  "Underweight → Underweight",
  "Overweight → Overweight", 
  "Obesity → Obesity",
  # ToOver (中间)
  "Underweight → Normal weight",
  "Normal weight → Overweight",
  "Overweight → Obesity",
  # ToLight (最下面)
  "Normal weight → Underweight",
  "Overweight → Normal weight",
  "Obesity → Overweight"
)

# 疾病标签 (A-H)
disease_labels <- c(
  "Top_Communicable" = "A)", 
  "Top_NonCommunicable" = "B)", 
  "Top_Injuries" = "C)",
  "Second_Circulatory" = "D)",
  "Second_Respiratory" = "E)",
  "Second_Neoplasms" = "F)",
  "Second_Digestive" = "G)",
  "Second_Metabolic" = "H)"
)

# 字体主题 - 调整字体大小为15
font_family <- ifelse("Times New Roman" %in% extrafont::fonts(), "Times New Roman", "serif")
times_theme <- function(base_size = 15){
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = font_family, color = "black"),
      axis.title = element_text(family = font_family, size = base_size, face = "bold"),
      axis.text = element_text(family = font_family, size = base_size),
      axis.line = element_line(color = "black", linewidth = 0.4),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 提取 HR, LCI, UCI
extract_hr_ci <- function(ci_string) {
  matches <- str_extract_all(ci_string, "[0-9.]+")[[1]]
  if(length(matches) >= 3){
    return(list(HR=as.numeric(matches[1]),
                LCI=as.numeric(matches[2]),
                UCI=as.numeric(matches[3])))
  }
  return(list(HR=NA, LCI=NA, UCI=NA))
}

# 处理数据函数
process_pattern_data <- function(wb, sheet_name) {
  data_raw <- read.xlsx(wb, sheet = sheet_name)
  
  data_processed <- data_raw %>%
    dplyr::filter(!is.na(HR_model) & HR_model != "") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci_info = list(extract_hr_ci(HR_model)),
      HR  = ci_info$HR,
      LCI = ci_info$LCI,
      UCI = ci_info$UCI
    ) %>%
    dplyr::ungroup()
  
  return(data_processed)
}

# 处理Top-level数据
top_data <- process_pattern_data(wb_top, "BMI Change Pattern") %>%
  mutate(level = factor(Top_level, levels = names(disease_labels)[1:3]),
         level_type = "Top")

# 处理Major-level数据
major_data <- process_pattern_data(wb_major, "BMI Change Pattern") %>%
  mutate(level = factor(Major_level, levels = names(disease_labels)[4:8]),
         level_type = "Major")

# 合并数据
combined_data <- bind_rows(top_data, major_data) %>%
  mutate(level = factor(level, levels = names(disease_labels)))

# 为每个疾病创建图表
disease_plots <- list()

for (i in seq_along(names(disease_labels))) {
  disease <- names(disease_labels)[i]
  disease_label <- disease_labels[disease]
  
  # 筛选该疾病的数据
  disease_data <- combined_data %>% filter(level == disease)
  
  # 添加Pattern组信息
  disease_data <- disease_data %>%
    dplyr::mutate(
      pattern_group = case_when(
        type %in% pattern_groups$Stable ~ "Stable",
        type %in% pattern_groups$ToOver ~ "ToOver", 
        type %in% pattern_groups$ToLight ~ "ToLight",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(pattern_group))
  
  # 使用预定义的pattern_order
  disease_data <- disease_data %>%
    dplyr::filter(type %in% pattern_order) %>%
    dplyr::mutate(type = factor(type, levels = rev(pattern_order)))
  
  # 添加背景颜色信息 - 第一行和第三行为灰色，第二行为白色
  disease_data <- disease_data %>%
    dplyr::mutate(
      bg_color = case_when(
        pattern_group == "Stable" ~ "gray90",
        pattern_group == "ToLight" ~ "gray90",
        pattern_group == "ToOver" ~ "white",
        TRUE ~ "white"
      )
    )
  
  # 创建图表
  p <- ggplot(disease_data, aes(x = HR, y = type)) +
    # 添加背景色 - 第一行和第三行灰色，第二行白色
    geom_rect(aes(ymin = as.numeric(type) - 0.5, ymax = as.numeric(type) + 0.5,
                  xmin = -Inf, xmax = Inf, fill = bg_color),
              show.legend = FALSE) +
    scale_fill_manual(values = c("gray90" = "gray90", "white" = "white")) +
    # 添加组间分隔线
    geom_hline(yintercept = c(3.5, 6.5), linetype = "dashed", 
               color = "gray60", linewidth = 0.5) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.4) +
    # 点和小误差线 - 缩短95%CI两端的线
    geom_point(size = 1.5, color = "black") +
    geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.08,  # 从0.15改为0.08，缩短误差线
                   color = "gray40", linewidth = 0.3) +
    labs(x = NULL, y = NULL, title = disease_label) +
    times_theme(base_size = 15) +
    theme(
      axis.text.y = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.spacing = unit(0.5, "lines")
    ) +
    scale_y_discrete(limits = rev)
  
  # 只在第一列显示y轴标签
  if (i != 1) {
    p <- p + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  }
  
  disease_plots[[disease]] = p
}

# 创建分隔线和标签
# 在C和D之间添加垂直分隔线
vertical_divider <- function() {
  ggplot() + 
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# 创建标签 - 只保留一个Major和一个Specific标签
major_label <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Major Cause Categories", 
           size = 5, family = font_family, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

specific_label <- ggplot() + 
  annotate("text", x = 0.5, y = 0.5, label = "Specific Cause Subcategories", 
           size = 5, family = font_family, fontface = "bold") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# 重新排列图表，在C和D之间添加分隔线
final_plot <- wrap_plots(
  disease_plots[["Top_Communicable"]],
  disease_plots[["Top_NonCommunicable"]],
  disease_plots[["Top_Injuries"]],
  vertical_divider(),  # C和D之间的分隔线
  disease_plots[["Second_Circulatory"]],
  disease_plots[["Second_Respiratory"]],
  disease_plots[["Second_Neoplasms"]],
  disease_plots[["Second_Digestive"]],
  disease_plots[["Second_Metabolic"]],
  ncol = 9, widths = c(1, 1, 1, 0.05, 1, 1, 1, 1, 1)
) 

# 添加顶部标签 - 只在适当位置各放一个标签
final_plot_with_labels <- wrap_plots(
  ggplot() + theme_void(),  # 空图
  major_label,  # Major标签放在B列上方
  ggplot() + theme_void(),  # 空图
  ggplot() + theme_void(),  # 空图（分隔线位置）
  ggplot() + theme_void(),  # 空图
  specific_label,  # Specific标签放在E列上方
  ggplot() + theme_void(),  # 空图
  ggplot() + theme_void(),  # 空图
  ggplot() + theme_void(),  # 空图
  ncol = 9, heights = 0.08, widths = c(1, 1, 1, 0.05, 1, 1, 1, 1, 1)
) / final_plot +
  plot_layout(heights = c(0.08, 1))

# For Figure 5
ggsave("Figure5_BMI_Transition_Pattern_Horizontal.tiff", final_plot_with_labels,
       width = 24, height = 12, units = "in", dpi = 600,
       device = "tiff", compression = "lzw", bg = "white")




###Sensitivity Analysis 1: exclude participants with self-reported diseases#####
Change_Data <- Change_Data %>%
  mutate(
    has_chronic_disease = ifelse(
      (is.na(cerebrovascular_disease) | cerebrovascular_disease == "" | grepl("未发现", cerebrovascular_disease)) &
        (is.na(kidney_disease) | kidney_disease == "" | grepl("未发现", kidney_disease)) &
        (is.na(heart_disease) | heart_disease == "" | grepl("未发现", heart_disease)) &
        (is.na(vascular_disease) | vascular_disease == "" | grepl("未发现", vascular_disease)) &
        (is.na(eye_disease) | eye_disease == "" | grepl("未发现", eye_disease)) &
        (is.na(nervous_system) | nervous_system == "" | grepl("未发现", nervous_system)) &
        (is.na(others_system_diseases) | others_system_diseases == "" | grepl("未发现", others_system_diseases)) &
        (is.na(others_system_diseases_others) | others_system_diseases_others == "" | grepl("未发现", others_system_diseases_others)),
      0,  # 没有慢性病
      1   # 有慢性病
    )
  )

generate_cox_sheet <- function(data, variable, coef_rows, sheet_name, wb) {
  # 根据变量选择协变量集合
  if (variable == "BMI_change_pattern") {
    covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
  } else {
    covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  
  # Cox 模型公式
  formula <- as.formula(paste("Surv(Time_years, Status) ~", variable, "+", covariates))
  fit <- coxph(formula, data = data)
  model <- summary(fit)
  
  HR <- round(model$coefficients[coef_rows, 2], 2)
  Pmark <- ifelse(model$coefficients[coef_rows, 5] < 0.001, "***",
                  ifelse(model$coefficients[coef_rows, 5] < 0.01, "**",
                         ifelse(model$coefficients[coef_rows, 5] < 0.05, "*", "")))
  Pvalue <- round(model$coefficients[coef_rows, 5], 3)
  LCI <- round(model$conf.int[coef_rows, 3], 2)
  UCI <- round(model$conf.int[coef_rows, 4], 2)
  
  table <- data.frame("Hazard" = HR, "CI" = paste0("(", LCI, "-", UCI, ")", Pmark), "p" = Pvalue)
  table$HR_model <- paste(table$Hazard, table$CI )
  table <- rownames_to_column(table, var = "type")
  table$type <- gsub(variable, "", table$type)
  
  # 死亡率
  CR <- aggregate(cbind(data$Status, data$Time_years),
                  by = list(type = data[[variable]]),
                  FUN = sum)
  colnames(CR)[2:3] <- c("Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  table_combined <- plyr::join(CR, table, by = "type", type = "left")
  table_combined <- subset(table_combined, select = c(1:4, 8))
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, table_combined)
}
wb <- createWorkbook()
data_no_chronic <- Change_Data %>% filter(has_chronic_disease == 0)
generate_cox_sheet(data_no_chronic, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet(data_no_chronic, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet(data_no_chronic, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet(data_no_chronic, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet(data_no_chronic, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet(data_no_chronic, "BMI_change_pattern", 1:15, "BMI pattern", wb)
saveWorkbook(wb, "敏感性分析1：排除慢性病.xlsx", overwrite = TRUE)




#####Sensitivity Analysis 2: exclude participants with less than 3 years of follow-up######
Sensitivity_analysis_follow_up_3 <- Change_Data %>%
  filter(Time_years >= 3)
wb <- createWorkbook()
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet(Sensitivity_analysis_follow_up_3, "BMI_change_pattern", 1:15, "BMI pattern", wb)
saveWorkbook(wb, "敏感性分析2：仅纳入随访年数大于3年.xlsx", overwrite = TRUE)


##### Sensitivity Analysis 3: multiple imputations for missing value of covariates#####
library(mice)
library(survival)
library(openxlsx)

# 首先指定需要插补的变量
vars_to_impute <- c("EDU_new", "SMOKE", "RSBP", "MAR", "PAF", "FBG")

# 设置插补方法
imp_methods <- rep("", ncol(Change_Data))
names(imp_methods) <- colnames(Change_Data)

# 为需要插补的变量指定具体方法
imp_methods["EDU_new"] <- "logreg"
imp_methods["SMOKE"] <- "polyreg"
imp_methods["MAR"] <- "polyreg"
imp_methods["PAF"] <- "polyreg"
imp_methods["RSBP"] <- "pmm"
imp_methods["FBG"] <- "pmm"

# 进行多重插补
imp <- mice(Change_Data, 
            m = 5, 
            method = imp_methods, 
            maxit = 10, 
            seed = 123,
            printFlag = TRUE)

# 修正的Cox分析函数
generate_cox_sheet_mi <- function(imp_data, variable, coef_rows, sheet_name, wb) {
  
  # 方法：手动对每个插补数据集进行分析，然后pooling
  cox_results <- list()
  
  for (i in 1:imp_data$m) {
    # 获取第i个插补数据集
    data_i <- complete(imp_data, i)
    
    # 显式设置分类变量的参考组
    if (variable == "BMI_change_pattern") {
      data_i[[variable]] <- as.factor(data_i[[variable]])
      data_i[[variable]] <- relevel(data_i[[variable]], ref = "Normal weight → Normal weight")
    } else if (variable %in% c("weight_change_cat", "BMI_change_perc_cat", 
                               "weight_change_perc_cat", "waist_change_cat_perc",
                               "waist_change_cat_abs")) {
      # 其他四个变量的参考组都是水平为"1"的组
      data_i[[variable]] <- as.factor(data_i[[variable]])
      data_i[[variable]] <- relevel(data_i[[variable]], ref = "Stable")
    }
    
    # 同样处理其他分类变量
    data_i$EDU_new <- as.factor(data_i$EDU_new)
    data_i$SMOKE <- as.factor(data_i$SMOKE)
    data_i$ALCOHOL <- as.factor(data_i$ALCOHOL)
    data_i$SEX <- as.factor(data_i$SEX)
    
    # 根据变量选择协变量集合
    if (variable == "BMI_change_pattern") {
      covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
    } else {
      covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
    }
    
    # Cox 模型公式
    formula <- as.formula(paste("Surv(Time_years, Status) ~", variable, "+", covariates))
    
    # 拟合Cox模型
    fit_i <- coxph(formula, data = data_i)
    cox_results[[i]] <- fit_i
  }
  
  # 将结果转换为mira对象以便pooling
  class(cox_results) <- "mira"
  
  # 合并所有插补数据集的结果
  pooled_results <- pool(cox_results)
  pooled_summary <- summary(pooled_results, conf.int = TRUE)
  
  # 提取所需行的结果
  results <- list()
  for (i in coef_rows) {
    if (i <= nrow(pooled_summary)) {
      HR <- round(exp(pooled_summary$estimate[i]), 2)
      Pvalue <- round(pooled_summary$p.value[i], 3)
      LCI <- round(exp(pooled_summary$`2.5 %`[i]), 2)
      UCI <- round(exp(pooled_summary$`97.5 %`[i]), 2)
      
      Pmark <- ifelse(Pvalue < 0.001, "***",
                      ifelse(Pvalue < 0.01, "**",
                             ifelse(Pvalue < 0.05, "*", "")))
      
      results[[i]] <- data.frame(
        type = rownames(pooled_summary)[i],
        Hazard = HR,
        CI = paste0("(", LCI, "-", UCI, ")", Pmark),
        p = Pvalue,
        HR_model = paste(HR, paste0("(", LCI, "-", UCI, ")", Pmark), Pvalue)
      )
    }
  }
  
  # 合并所有结果
  final_table <- do.call(rbind, results)
  final_table$type <- gsub(variable, "", final_table$type)
  
  # 计算死亡率（使用第一个插补数据集作为参考）
  complete_data <- complete(imp_data, 1)
  
  # 同样设置参考组以确保一致性
  if (variable == "BMI_change_pattern") {
    complete_data[[variable]] <- as.factor(complete_data[[variable]])
    complete_data[[variable]] <- relevel(complete_data[[variable]], ref = "Normal weight → Normal weight")
  } else if (variable %in% c("weight_change_cat", "BMI_change_perc_cat", 
                             "weight_change_perc_cat", "waist_change_cat_perc",
                             "waist_change_cat_abs")) {
    complete_data[[variable]] <- as.factor(complete_data[[variable]])
    complete_data[[variable]] <- relevel(complete_data[[variable]], ref = "Stable")
  }
  
  CR <- aggregate(cbind(complete_data$Status, complete_data$Time_years),
                  by = list(type = complete_data[[variable]]),
                  FUN = sum)
  colnames(CR)[2:3] <- c("Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  # 合并结果
  table_combined <- merge(CR, final_table, by = "type", all.x = TRUE)
  table_combined <- table_combined[, c("type", "Death", "YPOP", "YPOP_1000", "HR_model")]
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, table_combined)
  
  cat("已完成:", sheet_name, "\n")
}

# 现在运行分析
wb <- createWorkbook()

# 依次生成各个分析表格
generate_cox_sheet_mi(imp, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet_mi(imp, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet_mi(imp, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet_mi(imp, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet_mi(imp, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet_mi(imp, "BMI_change_pattern", 1:15, "BMI pattern", wb)



# 定义函数：跑一个变量，输出 Cox pooled + 死亡率结果
run_analysis <- function(varname, imp, Change_Data, wb) {
  # 动态生成协变量
  covariates <- if (varname == "BMI_change_pattern") {
    "SEX + age + EDU_new + SMOKE + ALCOHOL + TC + TG + LDL + RSBP +
     PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"  # 去掉 HDL 和 GPT
  } else {
    "SEX + age + EDU_new + SMOKE + ALCOHOL + TC  + LDL + HDL + RSBP +
     PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  
  # 动态生成公式
  full_formula <- as.formula(
    paste0("Surv(Time_years, Status) ~ ", varname, " + ", covariates)
  )
  
  # Cox 回归 (m 次插补)
  fits <- lapply(1:imp$m, function(i) {
    data_i <- complete(imp, i)
    coxph(full_formula, data = data_i)
  })
  
  # Pool
  fit_model <- as.mira(fits)
  pooled_model <- pool(fit_model)
  summary_pooled <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
  
  # 提取目标变量结果
  table <- summary_pooled %>%
    filter(grepl(varname, term)) %>%
    mutate(
      Hazard = round(estimate, 2),
      CI = paste0("(", round(conf.low, 2), "-", round(conf.high, 2), ")",
                  ifelse(p.value < 0.001, "***",
                         ifelse(p.value < 0.01, "**",
                                ifelse(p.value < 0.05, "*", "")))),
      p = round(p.value, 3),
      HR_model = paste(Hazard, CI, p)
    ) %>%
    dplyr::rename(type = term) %>%
    mutate(type = gsub(varname, "", type)) %>%
    dplyr::rename(!!varname := type)
  
  # 死亡率
  CR <- aggregate(cbind(Status, Time_years) ~ get(varname), data = Change_Data, sum)
  colnames(CR)[1:3] <- c(varname, "Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  # 合并
  table_combined <- plyr::join(CR, table, by = varname, type = "left")
  table_combined <- subset(table_combined, select = c(1:4, ncol(table_combined)))
  
  # 写入 Excel
  addWorksheet(wb, varname)
  writeData(wb, sheet = varname, table_combined)
}

# ---------------- 批量执行 ----------------
vars <- c("weight_change_cat", "BMI_change_perc_cat", 
          "weight_change_perc_cat", "waist_change_cat_perc", 
          "BMI_change_pattern", "waist_change_cat_abs")

wb <- createWorkbook()
for (v in vars) {
  run_analysis(v, imp, Change_Data, wb)
}



# 保存结果
saveWorkbook(wb, "敏感性分析3：多重插补.xlsx", overwrite = TRUE)











#### Sensitivity analysis 4: Excluded extreme values of weight, waist circumference, and BMI####
#### including only participants within ±4 standard deviations.
# 计算每个变量的均值和标准差
vars <- c("BMI_first", "BMI_last", "weight_first", "weight_last", "waist_first", "waist_last")
stats <- Change_Data %>%
  dplyr::summarise(across(all_of(vars), list(mean = ~mean(.x, na.rm = TRUE),
                                             sd = ~sd(.x, na.rm = TRUE))))

# 逐个变量筛选 ±4 SD 内的样本
Change_Data_no_extreme <- Change_Data %>%
  filter(
    BMI_first  >= stats$BMI_first_mean - 4 * stats$BMI_first_sd &
      BMI_first  <= stats$BMI_first_mean + 4 * stats$BMI_first_sd &
      BMI_last   >= stats$BMI_last_mean - 4 * stats$BMI_last_sd &
      BMI_last   <= stats$BMI_last_mean + 4 * stats$BMI_last_sd &
      weight_first >= stats$weight_first_mean - 4 * stats$weight_first_sd &
      weight_first <= stats$weight_first_mean + 4 * stats$weight_first_sd &
      weight_last  >= stats$weight_last_mean - 4 * stats$weight_last_sd &
      weight_last  <= stats$weight_last_mean + 4 * stats$weight_last_sd &
      waist_first  >= stats$waist_first_mean - 4 * stats$waist_first_sd &
      waist_first  <= stats$waist_first_mean + 4 * stats$waist_first_sd &
      waist_last   >= stats$waist_last_mean - 4 * stats$waist_last_sd &
      waist_last   <= stats$waist_last_mean + 4 * stats$waist_last_sd
  )
wb <- createWorkbook()
generate_cox_sheet(Change_Data_no_extreme, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet(Change_Data_no_extreme, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet(Change_Data_no_extreme, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "BMI_change_pattern", 1:15, "BMI pattern", wb)
saveWorkbook(wb, "敏感性分析4：排除体重、腰围和BMI极端值.xlsx", overwrite = TRUE)




###### Sensitivity analysis 5: Excluded extreme values of changes in BMI, weight, and waist circumference. #####
# 先计算需要的变量
Change_Data <- Change_Data %>%
  mutate(
    weight_change_abs = abs(weight_last - weight_first)
  )
Change_Data <- Change_Data %>%
  mutate(
    waist_change = abs(waist_last - waist_first)
  )
# 定义要筛选极端值的变量
vars <- c("BMI_change_perc", "weight_change_perc", "waist_change_perc", "waist_change", "weight_change_abs")

# 计算均值和标准差
stats <- Change_Data %>%
  dplyr::summarise(across(all_of(vars), list(mean = ~mean(.x, na.rm = TRUE),
                                             sd   = ~sd(.x, na.rm = TRUE))))

# 逐个变量筛选 ±4 SD 内的样本
Change_Data_no_extreme <- Change_Data %>%
  filter(
    BMI_change_perc     >= stats$BMI_change_perc_mean - 4 * stats$BMI_change_perc_sd &
      BMI_change_perc     <= stats$BMI_change_perc_mean + 4 * stats$BMI_change_perc_sd &
      weight_change_perc  >= stats$weight_change_perc_mean - 4 * stats$weight_change_perc_sd &
      weight_change_perc  <= stats$weight_change_perc_mean + 4 * stats$weight_change_perc_sd &
      waist_change_perc   >= stats$waist_change_perc_mean - 4 * stats$waist_change_perc_sd &
      waist_change_perc   <= stats$waist_change_perc_mean + 4 * stats$waist_change_perc_sd &
      waist_change        >= stats$waist_change_mean - 4 * stats$waist_change_sd &
      waist_change        <= stats$waist_change_mean + 4 * stats$waist_change_sd &
      weight_change_abs   >= stats$weight_change_abs_mean - 4 * stats$weight_change_abs_sd &
      weight_change_abs   <= stats$weight_change_abs_mean + 4 * stats$weight_change_abs_sd
  )

# 创建工作簿并生成 Cox 分析表
wb <- createWorkbook()
generate_cox_sheet(Change_Data_no_extreme, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet(Change_Data_no_extreme, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet(Change_Data_no_extreme, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet(Change_Data_no_extreme, "BMI_change_pattern", 1:15, "BMI pattern", wb)

# 保存文件
saveWorkbook(wb, "敏感性分析5：排除BMI和体重腰围变化极端值.xlsx", overwrite = TRUE)










#### Sensitivity analysis 6: Applied the newly released BMI classification criteria for older adults. ####
# 创建新的BMI变化模式数据集 (根据高龄老年人标准)
Change_new_BMI <- Change_Data %>%
  # 首先根据首次和末次BMI值进行分类
  mutate(BMI_first_cat = case_when(
    BMI_first < 22 ~ "Below Appropriate",
    BMI_first >= 22 & BMI_first <= 26.9 ~ "Appropriate",
    BMI_first > 26.9 & BMI_first < 30 ~ "Slightly Above",
    BMI_first >= 30 ~ "Above",
    TRUE ~ NA_character_  # 处理不符合条件的情况
  )) %>%
  mutate(BMI_last_cat = case_when(
    BMI_last < 22 ~ "Below Appropriate",
    BMI_last >= 22 & BMI_last <= 26.9 ~ "Appropriate",
    BMI_last > 26.9 & BMI_last < 30 ~ "Slightly Above",
    BMI_last >= 30 ~ "Above",
    TRUE ~ NA_character_  # 处理不符合条件的情况
  ))

# 创建BMI变化模式变量 (使用数值编码提高效率)
Change_new_BMI <- Change_new_BMI %>%
  mutate(BMI_change_pattern = case_when(
    # 从"低于适宜"开始的变化
    BMI_first_cat == "Below Appropriate" & BMI_last_cat == "Below Appropriate" ~ 1,
    BMI_first_cat == "Below Appropriate" & BMI_last_cat == "Appropriate" ~ 2,
    BMI_first_cat == "Below Appropriate" & BMI_last_cat == "Slightly Above" ~ 3,
    BMI_first_cat == "Below Appropriate" & BMI_last_cat == "Above" ~ 4,
    
    # 从"适宜"开始的变化
    BMI_first_cat == "Appropriate" & BMI_last_cat == "Below Appropriate" ~ 5,
    BMI_first_cat == "Appropriate" & BMI_last_cat == "Appropriate" ~ 6,
    BMI_first_cat == "Appropriate" & BMI_last_cat == "Slightly Above" ~ 7,
    BMI_first_cat == "Appropriate" & BMI_last_cat == "Above" ~ 8,
    
    # 从"略高于"开始的变化
    BMI_first_cat == "Slightly Above" & BMI_last_cat == "Below Appropriate" ~ 9,
    BMI_first_cat == "Slightly Above" & BMI_last_cat == "Appropriate" ~ 10,
    BMI_first_cat == "Slightly Above" & BMI_last_cat == "Slightly Above" ~ 11,
    BMI_first_cat == "Slightly Above" & BMI_last_cat == "Above" ~ 12,
    
    # 从"高于"开始的变化
    BMI_first_cat == "Above" & BMI_last_cat == "Below Appropriate" ~ 13,
    BMI_first_cat == "Above" & BMI_last_cat == "Appropriate" ~ 14,
    BMI_first_cat == "Above" & BMI_last_cat == "Slightly Above" ~ 15,
    BMI_first_cat == "Above" & BMI_last_cat == "Above" ~ 16,
    
    # 默认情况
    TRUE ~ NA_integer_
  ))

# 将数值编码转换为有意义的因子标签
Change_new_BMI <- Change_new_BMI %>%
  mutate(BMI_change_pattern = factor(
    BMI_change_pattern,
    levels = 1:16,
    labels = c(
      "Below Appropriate → Below Appropriate",
      "Below Appropriate → Appropriate",
      "Below Appropriate → Slightly Above",
      "Below Appropriate → Above",
      "Appropriate → Below Appropriate",
      "Appropriate → Appropriate",
      "Appropriate → Slightly Above",
      "Appropriate → Above",
      "Slightly Above → Below Appropriate",
      "Slightly Above → Appropriate",
      "Slightly Above → Slightly Above",
      "Slightly Above → Above",
      "Above → Below Appropriate",
      "Above → Appropriate",
      "Above → Slightly Above",
      "Above → Above"
    )
  ))

# 设置参考水平为"适宜体重 → 适宜体重"
Change_new_BMI <- Change_new_BMI %>%
  mutate(BMI_change_pattern = relevel(BMI_change_pattern, ref = "Appropriate → Appropriate"))
wb <- createWorkbook()
generate_cox_sheet(Change_new_BMI, "BMI_change_pattern", 1:15, "BMI pattern", wb)
saveWorkbook(wb, "敏感性分析6：中国老年人新标准.xlsx", overwrite = TRUE)





##### Sensitivity analysis 7: Applied Benjamini–Hochberg correction for multiple testing of p values. #####
generate_cox_sheet_p <- function(data, variable, coef_rows, sheet_name, wb){
  # 根据变量选择协变量集合
  if (variable == "BMI_change_pattern") {
    covariates <- "SEX + age + EDU_new + SMOKE + TC + TG + LDL + RSBP + 
                   PAF + MAR + HB + WCELL + AST + SCR + GPT + FBG + TBIL"
  } else {
    covariates <- "SEX + age + EDU_new + SMOKE + TC  + LDL + HDL + RSBP + 
                   PAF + MAR + HB + WCELL + GPT + AST + SCR + FBG + TBIL"
  }
  
  # Cox 模型公式
  formula <- as.formula(paste("Surv(Time_years, Status) ~", variable, "+", covariates))
  fit <- coxph(formula, data = data)
  model <- summary(fit)
  
  HR <- round(model$coefficients[coef_rows, 2], 2)
  raw_p <- model$coefficients[coef_rows, 5]   # 原始 p 值
  Pmark <- ifelse(raw_p < 0.001, "***",
                  ifelse(raw_p < 0.01, "**",
                         ifelse(raw_p < 0.05, "*", "")))
  Pvalue <- round(raw_p, 3)
  LCI <- round(model$conf.int[coef_rows, 3], 2)
  UCI <- round(model$conf.int[coef_rows, 4], 2)
  
  # Benjamini–Hochberg 调整后的 p 值
  Pvalue_BH <- p.adjust(raw_p, method = "BH")
  Pvalue_BH <- round(Pvalue_BH, 3)
  
  table <- data.frame(
    Hazard = HR, 
    CI = paste0("(", LCI, "-", UCI, ")", Pmark), 
    p_raw = Pvalue,
    p_BH  = Pvalue_BH
  )
  table$HR_model <- paste(table$Hazard, table$CI )
  table <- rownames_to_column(table, var = "type")
  table$type <- gsub(variable, "", table$type)
  
  # 死亡率
  CR <- aggregate(cbind(data$Status, data$Time_years),
                  by = list(type = data[[variable]]),
                  FUN = sum)
  colnames(CR)[2:3] <- c("Death", "YPOP")
  CR$YPOP_1000 <- round(CR$Death / CR$YPOP * 1000, 2)
  
  table_combined <- plyr::join(CR, table, by = "type", type = "left")
  table_combined <- subset(table_combined, select = c(1:4, 8, 5, 6)) # 保留 raw p 和 BH p
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, table_combined)
}
#创建工作簿并生成 Cox 分析表
wb <- createWorkbook()
generate_cox_sheet_p(Change_Data, "weight_change_cat", 1:4, "Weight Change", wb)
generate_cox_sheet_p(Change_Data, "waist_change_cat_abs", 1:4, "Waist Change", wb)
generate_cox_sheet_p(Change_Data, "BMI_change_perc_cat", 1:4, "BMI Change (perc)", wb)
generate_cox_sheet_p(Change_Data, "weight_change_perc_cat", 1:4, "Weight Change (perc)", wb)
generate_cox_sheet_p(Change_Data, "waist_change_cat_perc", 1:4, "Waist Change (perc)", wb)
generate_cox_sheet_p(Change_Data, "BMI_change_pattern", 1:15, "BMI pattern", wb)

# 保存文件
saveWorkbook(wb, "敏感性分析7：Benjamini–Hochberg多重调整p值.xlsx", overwrite = TRUE)



