library(openxlsx)
library(dtplyr)
library(dplyr)
library(data.table)
library(splitstackshape)
library(plyr)
library(lubridate)
library(lazyeval)
library(RODBC)
library(RODBCext)
library(knitr)
library(reshape2)
library(rvest)
library(RODBC)
library(RODBCext)


# OPEN DATE AND GRADES #####################################################################################
dbns <- read.csv("D:/ptech/dbn9_14_list1.csv")
dbns
dbn_list <- dbns %>%
  select(DBN)


lcgms <- read.csv("D:/ptech/lcgms.csv")

lcgms_open_grades <- lcgms %>%
  select(ATS.System.Code, Open.Date, Grades.Final, Geographical.District.Code ) %>%
  rename(c('ATS.System.Code' = 'DBN'))

dbn_open <- dbns %>%
  left_join(lcgms_open_grades, by="DBN")

# TOTAL ENROLLMENT  #####################################################################################
demo_s <- read.xlsx("D:/ptech/DemographicSnapshot201213to201617Public_FINAL1.xlsx", sheet="School")

table(demo_s$DBN)
names(demo_s)
demo_s1 <- demo_s %>%
  filter(DBN == '26Q315' | 
           DBN == '13K674' | 
           DBN == '30Q258' | 
           DBN == '07X259' | 
           DBN == '06M211' | 
           DBN == '02M280' | 
           DBN == '17K122' | 
           DBN == '01M696' | 
           DBN == '24Q299' | 
           DBN == '03M541' | 
           DBN == '06M293' | 
           DBN == '07X500' | 
           DBN == '17K543' | 
           DBN == '21K468' | 
           DBN == '22K555' | 
           DBN == '24Q520' | 
           DBN == '24Q530' | 
           DBN == '25Q252' | 
           DBN == '28Q284' 
  ) %>%
  filter(Year == '2016-17')

demo_s2 <- demo_s1 %>%
  select(DBN, Grade.6, Grade.7, Grade.8, Grade.9, Grade.10, Grade.11, Grade.12, Total.Enrollment ) %>%
  mutate(total_enr_6to12 = rowSums(.[2:8]))

demo_s3 <- demo_s2 %>%
  left_join(dbn_open, by="DBN")

# EXPENDITURE INFORMATION #####################################################################################

exp_stuff <- function(link, df_name, df_name_table, dbn, df_name_table1) {
  df_name <- read_html(link)
  
  df_name_table <- df_name %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table()
  
  colnames(df_name_table) = df_name_table[1,]
  df_name_table = df_name_table[-1,]
  df_name_table = df_name_table[1:7,]
  
  df_name_table$DBN <- dbn
  
  names(df_name_table)[1] <- "This.School"
  names(df_name_table)[2] <- "Values"
  
  fn <- paste("D:/ptech/exp_html/", dbn, ".csv", sep="")
  
  write.csv(df_name_table, fn)
  
  df_name_table1 <- dcast(df_name_table, DBN ~ This.School, value.var = "Values")
  
  df_name_table1
  
}

exp_01M696_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/01/M696/AboutUs/Statistics/expenditures.htm", exp_01M696, exp_01M696_table, '01M696',exp_01M696_table1)

exp_02M280_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/02/M280/AboutUs/Statistics/expenditures.htm", exp_02M280, exp_02M280_table, '02M280',exp_02M280_table1)

exp_03M541_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/03/M541/AboutUs/Statistics/expenditures.htm", exp_03M541, exp_03M541_table, '03M541',exp_03M541_table1)

exp_06M211_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/06/M211/AboutUs/Statistics/expenditures.htm", exp_06M211, exp_06M211_table, '06M211',exp_06M211_table1)

exp_06M293_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/06/M293/AboutUs/Statistics/expenditures.htm", exp_06M293, exp_06M293_table, '06M293',exp_06M293_table1)

exp_07X259_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/07/X259/AboutUs/Statistics/expenditures.htm", exp_07X259, exp_07X259_table, '07X259',exp_07X259_table1)

exp_07X500_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/07/X500/AboutUs/Statistics/expenditures.htm", exp_07X500, exp_07X500_table, '07X500',exp_07X500_table1)

exp_13K674_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/13/K674/AboutUs/Statistics/expenditures.htm", exp_13K674, exp_13K674_table, '13K674',exp_13K674_table1)

exp_17K122_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/17/K122/AboutUs/Statistics/expenditures.htm", exp_17K122, exp_17K122_table, '17K122',exp_17K122_table1)

exp_17K543_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/17/K543/AboutUs/Statistics/expenditures.htm", exp_17K543, exp_17K543_table, '17K543',exp_17K543_table1)

exp_21K468_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/21/K468/AboutUs/Statistics/expenditures.htm", exp_21K468, exp_21K468_table, '21K468',exp_21K468_table1)

exp_22K555_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/15/K555/AboutUs/Statistics/expenditures.htm", exp_22K555, exp_22K555_table, '22K555',exp_22K555_table1)

exp_24Q299_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/24/Q299/AboutUs/Statistics/expenditures.htm", exp_24Q299, exp_24Q299_table, '24Q299',exp_24Q299_table1)

exp_24Q520_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/24/Q520/AboutUs/Statistics/expenditures.htm", exp_24Q520, exp_24Q520_table, '24Q520',exp_24Q520_table1)

exp_24Q530_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/24/Q530/AboutUs/Statistics/expenditures.htm", exp_24Q530, exp_24Q530_table, '24Q530',exp_24Q530_table1)

exp_25Q252_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/25/Q252/AboutUs/Statistics/expenditures.htm", exp_25Q252, exp_25Q252_table, '25Q252',exp_25Q252_table1)

exp_26Q315_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/26/Q315/AboutUs/Statistics/expenditures.htm", exp_26Q315, exp_26Q315_table, '26Q315',exp_26Q315_table1)

exp_28Q284_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/28/Q284/AboutUs/Statistics/expenditures.htm", exp_28Q284, exp_28Q284_table, '28Q284',exp_28Q284_table1)


exp_30Q258_table2 <- exp_stuff("http://schools.nyc.gov/SchoolPortals/30/Q258/AboutUs/Statistics/expenditures.htm", exp_30Q258, exp_30Q258_table, '30Q258',exp_30Q258_table1)


exp_comb <- (rbind(exp_01M696_table2,
                   exp_02M280_table2,
                   exp_03M541_table2,
                   exp_06M211_table2,
                   exp_06M293_table2,
                   exp_07X259_table2,
                   exp_07X500_table2,
                   exp_13K674_table2,
                   exp_17K122_table2,
                   exp_17K543_table2,
                   exp_21K468_table2,
                   exp_22K555_table2,
                   exp_24Q299_table2,
                   exp_24Q520_table2,
                   exp_24Q530_table2,
                   exp_25Q252_table2,
                   exp_26Q315_table2,
                   exp_28Q284_table2,
                   exp_30Q258_table2
))

write.csv(exp_comb, "D:/ptech/exp_html/exp_combined.csv")

demo_s4 <- demo_s3 %>%
  left_join(exp_comb, by="DBN")

### STUDENT CHARACTERISTICS #############################################################################

names(demo_s1)
# Gender, 19:22
# Ethnicity, 22:32
# Poverty, 37:38
# SWD Status, 33:34
# ELL Status, 35:36

stud_char <- demo_s1 %>%
  select(DBN, `#.Female`: Economic.Needs.Index)

str(stud_char)

sqr <- read.xlsx("D:/ptech/2015_2016_HS_SQR_Results_2017_01_05.xlsx", sheet="Summary")

stud_char1 <- sqr %>%
  select(DBN, Average.Grade.8.English.Proficiency,Average.Grade.8.Math.Proficiency) %>%
  inner_join(stud_char, by="DBN")

stud_char1

######################## This includes everything up to this point ############################
demo_s5 <- stud_char1 %>%
  left_join(demo_s4, by="DBN")
##############################################################################################

str(demo_s5)

write.csv(demo_s5, "D:/ptech/demo_s5.csv")


#### LOCAL OR TRAVEL ###############


db_ats_demo <- odbcDriverConnect('driver={SQL Server};
                              server=ES11VSINFSQL02;database=ATS_Demo;trusted_connection=true')

dist_match_count <- sqlQuery(db_ats_demo, 
                       "
                        SELECT distinct
                       [SCHOOL_DBN]
                       
                       ,count(biogdata.[STUDENT_ID])  as count_local
                        FROM [ATS_DEMO].[dbo].[BIOGDATA] biogdata
                       inner join [ATS_DEMO].[dbo].[ADDRDATA] addrdata on biogdata.STUDENT_ID = addrdata.STUDENT_ID
                       where [SCHOOL_DBN] in (
                       '26Q315' , 
                       '13K674' , 
                       '30Q258' , 
                       '07X259' , 
                       '06M211' , 
                       '02M280' , 
                       '17K122' , 
                       '01M696' , 
                       '24Q299' , 
                       '03M541' , 
                       '06M293' , 
                       '07X500' , 
                       '17K543' , 
                       '21K468' , 
                       '22K555' , 
                       '24Q520' , 
                       '24Q530' , 
                       '25Q252' , 
                       '28Q284' 
                       )
                       and addrdata.ADDR_CURRENT = 'Y'
                       and SCHOOL_DISTRICT = RES_DISTRICT
                       and biogdata.status = 'A' 
                       group by SCHOOL_DBN "
                   
                       
                  )
str(dist_match_count)
dist_not_match_count <- sqlQuery(db_ats_demo, 
                             "
                             SELECT distinct
                             [SCHOOL_DBN]
                             
                             ,count(biogdata.[STUDENT_ID])  as count_travel
                             FROM [ATS_DEMO].[dbo].[BIOGDATA] biogdata
                             inner join [ATS_DEMO].[dbo].[ADDRDATA] addrdata on biogdata.STUDENT_ID = addrdata.STUDENT_ID
                             where [SCHOOL_DBN] in (
                             '26Q315' , 
                             '13K674' , 
                             '30Q258' , 
                             '07X259' , 
                             '06M211' , 
                             '02M280' , 
                             '17K122' , 
                             '01M696' , 
                             '24Q299' , 
                             '03M541' , 
                             '06M293' , 
                             '07X500' , 
                             '17K543' , 
                             '21K468' , 
                             '22K555' , 
                             '24Q520' , 
                             '24Q530' , 
                             '25Q252' , 
                             '28Q284' 
                             )
                             and addrdata.ADDR_CURRENT = 'Y'
                             and SCHOOL_DISTRICT <> RES_DISTRICT
                             and biogdata.status = 'A' 
                             group by SCHOOL_DBN, STATUS "
)
                             


str(dist_not_match_count)

count_both <- dist_match_count %>%
                left_join(dist_not_match_count, by="SCHOOL_DBN")

str(count_both)

names(count_both)[1] <- "DBN"

demo_s5.1 <- count_both %>%
  left_join(demo_s5, by="DBN")

### CTE Enrolled, CTE Credits ##################################################################################

cte_any <- read.csv("K:/OPSR/MP Planning/Operations, Finance, & Research/Research/OPSR Data Team Files/Nadine/sam_17/method_2/full_run_onlyCTEDBN_8.8.17.csv")


cte_ptech_filter <- cte_any %>%
  filter(schooldbn == '26Q315' | 
           schooldbn == '13K674' | 
           schooldbn == '30Q258' | 
           schooldbn == '07X259' | 
           schooldbn == '06M211' | 
           schooldbn == '02M280' | 
           schooldbn == '17K122' | 
           schooldbn == '01M696' | 
           schooldbn == '24Q299' | 
           schooldbn == '03M541' | 
           schooldbn == '06M293' | 
           schooldbn == '07X500' | 
           schooldbn == '17K543' | 
           schooldbn == '21K468' | 
           schooldbn == '22K555' | 
           schooldbn == '24Q520' | 
           schooldbn == '24Q530' | 
           schooldbn == '25Q252' | 
           schooldbn == '28Q284' 
  )  %>%
  as.data.frame()


# Level Types for PTech schools
lt_ptech <- as.data.frame(table(cte_ptech_filter$schooldbn,cte_ptech_filter$leveltype)) %>%
  filter(Freq > 0)


lt_ptech_any_pgm <- lt_ptech %>%
  filter(Freq > 0) %>%
  arrange(Var1, Var2) %>%
  filter(Var2 == "Enrolled- credits in sequence" | 
           Var2 == "Enrolled - no credits in CTE attempted" |
           Var2 == "Enrolled - credits in different sequence")

names(lt_ptech_any_pgm)[1] <- "DBN"
names(lt_ptech_any_pgm)[2] <- "level_type"

str(lt_ptech_any_pgm)

lt_ptech_any_pgm_for_leftjoin <- dcast(lt_ptech_any_pgm,  DBN ~ level_type, value.var = "Freq")

lt_ptech_any_pgm_for_leftjoin

cte_creds_dbn <- cte_ptech_filter %>%
  select(schooldbn, insequence, other_credits) %>%
  group_by(schooldbn) %>%
  summarise_each(funs(sum)) %>%
  rename(c('insequence' = 'cte_creds_insequence')) %>%
  rename(c('other_credits' = 'cte_creds_not_insequence')) %>%
  rename(c('schooldbn' = 'DBN'))

cte_creds_dbn
str(demo_s5)
demo_s6 <- demo_s5.1 %>%
  left_join(lt_ptech_any_pgm_for_leftjoin,by="DBN") %>% 
  left_join(cte_creds_dbn,by="DBN") %>% 
  rename(c('Freq' = 'CTE_Enrolled')) 


str(demo_s6)      

write.csv(demo_s6, "D:/ptech/demo_s6.csv")


#### Grad Rates ##########################################################################################

grad2016 <- read.csv("K:/OPSR/MP Planning/Operations, Finance, & Research/Research/OPSR Data Team Files/RPSG data/Graduation/2015-16_Grad_StateCalc.csv")

d1 <- as.data.frame(table(grad2016$dbn, grad2016$membership_desc, grad2016$outcome))  %>%
  group_by(Var1, Var2, Var3) %>%
  summarise_each(funs(sum))

str(d1)

d1$outcome_category <- ifelse((d1$Var3 == 'Dropout' | 
                                 d1$Var3 == 'IEP or Commencement Credential' |
                                 d1$Var3 == 'Still Enrolled' | 
                                 d1$Var3 == 'Transfer' | 
                                 d1$Var3 == 'Transfer to GED'), 'NonGrad_Count', 'Grad_Count') 

d1_p <- d1 %>%
  filter(Var1 == '26Q315' | 
           Var1 == '13K674' | 
           Var1 == '30Q258' | 
           Var1 == '07X259' | 
           Var1 == '06M211' | 
           Var1 == '02M280' | 
           Var1 == '17K122' | 
           Var1 == '01M696' | 
           Var1 == '24Q299' | 
           Var1 == '03M541' | 
           Var1 == '06M293' | 
           Var1 == '07X500' | 
           Var1 == '17K543' | 
           Var1 == '21K468' | 
           Var1 == '22K555' | 
           Var1 == '24Q520' | 
           Var1 == '24Q530' | 
           Var1 == '25Q252' | 
           Var1 == '28Q284' )


str(d1_p)

d1_pmelt <- dcast(d1_p, Var1 ~ Var2 + Var3 , value.var = 'Freq') 

names(d1_pmelt)[1] <- "DBN"

tbl_df(d1_pmelt)

str(d1_p)

d1_p_degree <- dcast(d1_p, Var1 + Var2 ~ Var3, value.var = 'Freq')

str(d1_p_degree)
rm(demo_s7)
demo_s7 <- demo_s6 %>%
  left_join(d1_pmelt, by="DBN")
str(demo_s7)

d2 <- d1_p %>%
  select(-Var3) %>%
  group_by(Var1, Var2, outcome_category) %>%
  summarise_each(funs(sum))

d3 <- dcast(d2, Var1 + Var2 ~ outcome_category, value.var = 'Freq')

d3$total <- d3$G + d3$N

d3$grad_rate <- d3$G / d3$total

names(d3)[1] <- "DBN"

d3$nongrad_rate <- d3$N / d3$total

str(d3)

d3_grad_count <- dcast(d3, DBN ~ Var2, value.var=c("Grad_Count"))
d3_nongrad_count <- dcast(d3, DBN ~ Var2, value.var=c("NonGrad_Count"))
d3_grad_count

names(d3_grad_count)[2] <- "grad_2010_cohort6year"
names(d3_grad_count)[3] <- "grad_2011_cohort5year"
names(d3_grad_count)[4] <- "grad_2011_cohort5yearAug"
names(d3_grad_count)[5] <- "grad_2012_cohort4year"
names(d3_grad_count)[6] <- "grad_2012_cohort4yearAug"

names(d3_nongrad_count)[2] <- "nongrad_2010_cohort6year"
names(d3_nongrad_count)[3] <- "nongrad_2011_cohort5year"
names(d3_nongrad_count)[4] <- "nongrad_2011_cohort5yearAug"
names(d3_nongrad_count)[5] <- "nongrad_2012_cohort4year"
names(d3_nongrad_count)[6] <- "nongrad_2012_cohort4yearAug"



str(demo_s7)
demo_s8 <- demo_s7 %>%
              left_join(d3_grad_count, by="DBN") %>%
              left_join(d3_nongrad_count, by="DBN")


names(demo_s8)

sqr <-  read.xlsx("D:/ptech/2015_2016_HS_SQR_Results_2017_01_05.xlsx", sheet="Student Achievement")
names(sqr)
sqr1 <- sqr %>%
  select(DBN, `Metric.Value.-.Graduation.Rate,.4.year`, `Metric.Value.-.Graduation.Rate,.6-Year`)

d4 <- d3 %>%
  left_join(sqr1, by="DBN") %>%
  mutate(grad_rate = round(grad_rate,2)) %>%
  mutate(nongrad_rate = round(nongrad_rate,2)) %>%
  mutate(mv_grad_rate_4yr = round(`Metric.Value.-.Graduation.Rate,.4.year`,2)) %>%
  mutate(mv_grad_rate_6yr = round(`Metric.Value.-.Graduation.Rate,.6-Year`,2))

tbl_df(d4)

str(d4)

d4_grad_count <- dcast(d4, DBN ~ Var2, value.var=c("Grad_Count"))
d4_nongrad_count <- dcast(d4, DBN ~ Var2, value.var=c("NonGrad_Count"))
d4_metric_val_4and6_year <- d4 %>%
                          select(DBN, mv_grad_rate_4yr, mv_grad_rate_6yr )%>%
                          unique

str(d4_metric_val_4and6_year)

#d4_metric_val_6_year <- dcast(d4, DBN ~ Var2, value.var="mv_grad_rate_6yr")
d4_metric_val_4_year

str(demo_s8)
demo_s9 <- demo_s8 %>%
  left_join(d4_metric_val_4and6_year, by="DBN") 

names(demo_s9)
  
#### Degrees and Demographics #######################################################################################

str(dbns)

str(grad2016)
dbns1 <- dbns[c(1,2)]

str(dbns1)

d1 <- as.data.frame(table(grad2016$dbn, grad2016$ethnicity, grad2016$outcome))

names(d1)[1] <- "DBN"
names(d1)[2] <- "ethnicity"
names(d1)[3] <- "outcome"

str(d1)

h1 <- d1 %>%
  select(-DBN) %>%
  group_by(ethnicity, outcome) %>%
  summarise_each(funs(sum))

str(h1)

h1$Freq[is.na(h1$Freq)] <- 0

h1$outcome_category <- ifelse((h1$outcome == 'Dropout' | 
                                 h1$outcome == 'IEP or Commencement Credential' |
                                 h1$outcome == 'Still Enrolled' | 
                                 h1$outcome == 'Transfer' | 
                                 h1$outcome == 'Transfer to GED'), 'N', 'G')


h2 <- h1 %>%
  select(-outcome) %>%
  group_by(ethnicity, outcome_category) %>%
  summarise_each(funs(sum)) %>%
  filter(!is.na(ethnicity))

str(h2)

h2[is.na(h2)] <- 0

h3 <- dcast(h2, ethnicity ~ outcome_category, value.var = 'Freq')

tbl_df(h3)

h3$total <- h3$G + h3$N

h3$grad_rt <- h3$G/ h3$total
h3$nongrad_rt <- h3$N/ h3$total

h3$Type <- 'citywide'

h3

#########


d2 <- dbns1 %>%
  left_join(d1, by="DBN")


str(d2)

table(d2$DBN)

d2$Freq[is.na(d2$Freq)] <- 0



str(d2)

d3 <- d2 %>%
  group_by(DBN,Type, ethnicity, outcome) %>%
  summarise_each(funs(sum))
str(d3)

d3$outcome_category <- ifelse((d3$outcome == 'Dropout' | 
                                 d3$outcome == 'IEP or Commencement Credential' |
                                 d3$outcome == 'Still Enrolled' | 
                                 d3$outcome == 'Transfer' | 
                                 d3$outcome == 'Transfer to GED'), 'N', 'G')

d4 <- d3 %>%
  select(-outcome) %>%
  group_by(DBN,Type, ethnicity, outcome_category) %>%
  summarise_each(funs(sum)) %>%
  filter(!is.na(ethnicity))

str(d4)

tbl_df(d4)

d4[is.na(d4)] <- 0



d5 <- dcast(d4, DBN ~ ethnicity + outcome_category, value.var = 'Freq')


demo_s10 <- demo_s9 %>%
              left_join(d5, by="DBN")
names(demo_s10)



write.csv(demo_s10, "D:/ptech/demo_s10.csv")


cd <- as.data.frame(table(grad2016$dbn, grad2016$diploma, grad2016$graduated))

str(cd)

names(cd)[1] <- "DBN"
names(cd)[2] <- "diploma"
names(cd)[3] <- "graduated"

cd1 <- dbns1 %>%
  full_join(cd, by="DBN")

cd1$Type <- as.character(cd1$Type)
cd1$Type[is.na(cd1$Type)] <- 'allhs'

cd2 <- cd1 %>%
  filter(diploma != ' ' & diploma != 'Career Development & Occupational Studie' & diploma != 'IEP Diploma' & diploma != 'GED')

table(cd2$diploma)

table(cd2$Type, useNA="always")

str(cd1)

cd2$d_type <- ifelse((cd2$diploma == 'Regents with CTE post July 1 2001' | cd2$diploma == 'Regents with Honors&CTE post July 1 2001'), 'cte',
                     'G')


table(cd2$d_type)

tbl_df(cd2)

cd2$Freq[is.na(cd2$Freq)] <- 0

cd3 <- cd2 %>%
  filter(Freq > 0)

cd3$Freq[is.na(cd3$Freq)] <- 0

tbl_df(cd3)

cd4 <- cd3 %>%
  select(-d_type) %>%
  group_by(DBN, Type, diploma, graduated) %>%
  summarise_each(funs(sum)) %>%
  filter(Type != 'allhs')

cd4$Freq[is.na(cd4$Freq)] <- 0

tbl_df(cd4)

cd5 <- dcast(cd4, DBN  ~ graduated + diploma, value.var = 'Freq')

tbl_df(cd5)

demo_s11 <- demo_s10 %>%
              left_join(cd5, by="DBN")

write.csv(demo_s11, "D:/ptech/demo_s11.csv")

