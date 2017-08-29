## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

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
library(ggplot2)


## ----readFile, include=FALSE---------------------------------------------

#pt demo_s11.csv set made from K:\OPSR\MP Planning\Operations, Finance, & Research\Research\OPSR Data Team Files\Nadine\ptech_discovery - make_set.R
pt <- read.csv("D:/ptech/demo_s11.csv")
pt$Type <- ifelse(pt$Type == 'ptech', 's9-14', 'non-opsr')


## ----about_setup, include=FALSE------------------------------------------

about <- pt[c(2,37,43,44,36)]


## ----about_presentation, echo=FALSE--------------------------------------

kable(about)


## ----funding_setup, include=FALSE----------------------------------------

funding <- pt[c(2,37,46:52)]

# Average all schools

exp_stuff <- function(link, df_name, df_name_table, dbn, df_name_table1) {
  df_name <- read_html(link)
  
  df_name_table <- df_name %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table()
  
  colnames(df_name_table) = df_name_table[1,]
  df_name_table = df_name_table[-1,]
  df_name_table = df_name_table[10:15,]
  
  df_name_table$DBN <- dbn
  
  names(df_name_table)[1] <- "Entire.System"
  names(df_name_table)[2] <- "Values"
  
  fn <- paste("D:/ptech/exp_html/", dbn, ".csv", sep="")
  
  write.csv(df_name_table, fn)
  
  df_name_table1 <- dcast(df_name_table, DBN ~ Entire.System, value.var = "Values")
  
  df_name_table1
  
}

avg_sch <- exp_stuff("http://schools.nyc.gov/SchoolPortals/01/M696/AboutUs/Statistics/expenditures.htm", exp_avg, exp_avg_table, 'avg',exp_avg_table1)

str(avg_sch)

names(avg_sch)
names(funding)

avg_sch$Total.Dollars.Spent.at.this.School <- 'N/A'
avg_sch$Type <- 'Entire_System'

names(avg_sch)

avg_sch1 <- avg_sch[c(1,9,2:4,8,5:7)]

names(avg_sch1)
names(avg_sch1)[3] <- "Per.Student.Teach.Sal"
names(avg_sch1)[4] <- "Per.Student.Prof.Dev"
names(avg_sch1)[5] <- "Per.Student.Summer.and.After.School.Pgms"
names(avg_sch1)[6] <- "Total.Dollars.Spent"
names(avg_sch1)[7] <- "Per.Student.Cost"
names(avg_sch1)[8] <- "Per.Student.Gen.Ed.St"
names(avg_sch1)[9] <- "Per.Student.Spec.Ed.St"

names(funding)[3] <- "Per.Student.Teach.Sal"
names(funding)[4] <- "Per.Student.Prof.Dev"
names(funding)[5] <- "Per.Student.Summer.and.After.School.Pgms"
names(funding)[6] <- "Total.Dollars.Spent"
names(funding)[7] <- "Per.Student.Cost"
names(funding)[8] <- "Per.Student.Gen.Ed.St"
names(funding)[9] <- "Per.Student.Spec.Ed.St"

f_plus_avg <- rbind(funding, avg_sch1)

f_plus_avg

#classroom teacher salaries; professional development c(2,3)
funding_teachers <- f_plus_avg[c(1:4)]
#classroom total dollars spent; cost per student c(5,6)
funding_students <- f_plus_avg[c(1,2,6,7)]
# spending on general ed students and special ed students c(7,8)
funding_students_breakdown <- f_plus_avg[c(1,2,8,9)]

cost_per_student_entire_system <- funding_students %>%
                                    select(DBN, Per.Student.Cost) %>%
                                    filter(DBN == 'avg')

cost_per_student_entire_system$Per.Student.Cost
f_plus_avg

f_plus_avg_numeric <- data.frame(f_plus_avg)

f_plus_avg_numeric1 <- lapply(f_plus_avg_numeric, function(x) {
                  gsub("[$]", "", x)
              }) %>%
              as.data.frame()

f_plus_avg_numeric2 <- lapply(f_plus_avg_numeric1, function(x) {
                  gsub("[,]", "", x)
              }) %>%
              as.data.frame()

f_plus_avg_numeric2


f_plus_avg_numeric2$Per.Student.Teach.Sal <- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Teach.Sal))
f_plus_avg_numeric2$Per.Student.Prof.Dev <- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Prof.Dev))
f_plus_avg_numeric2$Per.Student.Summer.and.After.School.Pgms <- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Summer.and.After.School.Pgms))
f_plus_avg_numeric2$Total.Dollars.Spent <- as.numeric(as.character(f_plus_avg_numeric2$Total.Dollars.Spent))
f_plus_avg_numeric2$Per.Student.Cost<- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Cost))
f_plus_avg_numeric2$Per.Student.Gen.Ed.St<- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Gen.Ed.St))
f_plus_avg_numeric2$Per.Student.Spec.Ed.St<- as.numeric(as.character(f_plus_avg_numeric2$Per.Student.Spec.Ed.St))

f_plus_avg_numeric2

rf_plus_avg_group <- f_plus_avg_numeric2 %>%
                      select(-DBN)   %>%
                      group_by(Type) %>%
                      summarise_each(funs(mean))
  
                                    
rf_plus_avg_group_per_student <- rf_plus_avg_group %>%
                                  select(Type, Per.Student.Cost)


cost_per_student_9_14_opsr <- rf_plus_avg_group %>%
                                select(Type, Per.Student.Cost) %>%
                                filter(Type == 's9-14') 

cost_per_student_9_14_opsr
                                

cost_per_student_9_14_opsr$Per.Student.Cost

cost_per_student_9_14_nonopsr <- rf_plus_avg_group %>%
                                select(Type, Per.Student.Cost) %>%
                                filter(Type == 'non-opsr')

cost_per_student_9_14_nonopsr$Per.Student.Cost

cost_per_student_9_14_opsr$Per.Student.Cost


## ----group_cost_per_student_presentation, echo=FALSE---------------------

ggplot(rf_plus_avg_group_per_student, aes(Type, Per.Student.Cost, fill=Type)) + geom_col()


## ----funding_students_presentation, echo=FALSE---------------------------

kable(funding_students)


## ----funding_student_breakdown_presentation, echo=FALSE------------------

kable(funding_students_breakdown)


## ----funding_teachers_presentation, echo=FALSE---------------------------

kable(funding_teachers)


## ----student_characteristics_setup, include=FALSE------------------------

#avg_demo from K:\OPSR\MP Planning\Operations, Finance, & Research\Research\OPSR Data Team Files\Nadine\ptech_discovery\ptech_playground.R

avg_demo <- read.csv("D:/ptech/avg_demo.csv")

names(avg_demo)
names(pt)
char <-  pt[c(2,37,35,5:26)]
names(char)
avg_demo$DBN <- 'entire_system'
avg_demo$Type <- 'all'

avg_demo1 <- avg_demo[c(39,40,3,37,38,4,5,6:36)]

avg_demo1$X..Female.1 <- avg_demo1$female_avg
avg_demo1$X..Male.1 <- avg_demo1$male_avg
avg_demo1$X..Asian.1 <- avg_demo1$asian_ag
avg_demo1$X..Black.1 <- avg_demo1$black_avg
avg_demo1$X..English.Language.Learners.1 <- avg_demo1$ell_avg
avg_demo1$X..Multiple.Race.Categories.Not.Represented.1 <- avg_demo1$multi_r_avg
avg_demo1$X..White.1 <- avg_demo1$white_avg
avg_demo1$X..Hispanic.1 <- avg_demo1$hispanic_avg
avg_demo1$X..Students.with.Disabilities.1 <- avg_demo1$swd_avg
avg_demo1$X..English.Language.Learners.1 <- avg_demo1$ell_avg
avg_demo1$X..Poverty.1 <- avg_demo1$poverty_avg



avg_demo2 <- avg_demo1 %>%
                filter(School.Type == 'High School') %>%
                select(c(1:25))

avg_demo2


char_avg <- rbind(char, avg_demo2) %>%
              mutate_each(funs(round(.,2)), -DBN, -Type, -Total.Enrollment) 

str(char_avg)

names(char_avg)[3] <- "Total_Enr"
names(char_avg)[4] <- "Avg_Grade8_Eng_Prof"
names(char_avg)[5] <- "Avg_Grade8_Math_Prof"
names(char_avg)[6] <- "n_female"
names(char_avg)[7] <- "perc_female"
names(char_avg)[8] <- "n_male"
names(char_avg)[9] <- "perc_male"
names(char_avg)[10] <- "n_asian"
names(char_avg)[11] <- "perc_asian"
names(char_avg)[12] <- "n_black"
names(char_avg)[13] <- "perc_black"
names(char_avg)[14] <- "n_hispanic"
names(char_avg)[15] <- "perc_hispanic"
names(char_avg)[16] <- "n_multi_nr"
names(char_avg)[17] <- "perc_multi_nr"
names(char_avg)[18] <- "n_white"
names(char_avg)[19] <- "perc_white"
names(char_avg)[20] <- "n_swd"
names(char_avg)[21] <- "perc_swd"
names(char_avg)[22] <- "n_ell"
names(char_avg)[23] <- "perc_ell"
names(char_avg)[24] <- "n_poverty"
names(char_avg)[25] <- "perc_poverty"




char_avg_gender <- char_avg %>%
                      select(c(1:3,6:9))

char_avg_gender


char_avg_group <- char_avg %>%
                      select(-DBN) %>%
                      group_by(Type) %>%
                      summarise_each(funs(sum))


char_avg_group$perc_female <- char_avg_group$n_female / char_avg_group$Total_Enr
char_avg_group$perc_male <- char_avg_group$n_male / char_avg_group$Total_Enr
char_avg_group$perc_asian <- char_avg_group$n_asian  / char_avg_group$Total_Enr
char_avg_group$perc_black <- char_avg_group$n_black  / char_avg_group$Total_Enr
char_avg_group$perc_white <- char_avg_group$n_white  / char_avg_group$Total_Enr
char_avg_group$perc_hispanic <- char_avg_group$n_hispanic/ char_avg_group$Total_Enr
char_avg_group$perc_multi_nr <- char_avg_group$n_multi_nr/ char_avg_group$Total_Enr
char_avg_group$perc_poverty <- char_avg_group$n_poverty / char_avg_group$Total_Enr
char_avg_group$perc_ell <- char_avg_group$n_ell / char_avg_group$Total_Enr

names(char_avg_group)


#### GENDER ###############################################

char_avg_gender <- char_avg %>%
                      select(c(1:3,6:9))

char_avg_gender

names(char_avg_gender)[4] <- "n_female"
names(char_avg_gender)[5] <- "perc_female"
names(char_avg_gender)[6] <- "n_male"
names(char_avg_gender)[7] <- "perc_male"


char_avg_group_gender <- char_avg_group[c(1,2,5:8)] %>%
                                        mutate_each(funs(round(.,2)), -Type)

names(char_avg_group_gender)[3] <- "n_female"
names(char_avg_group_gender)[4] <- "perc_female"
names(char_avg_group_gender)[5] <- "n_male"
names(char_avg_group_gender)[6] <- "perc_male"

char_avg_group_gender

char_avg_group_gender_long <- melt(char_avg_group_gender) %>%
                                filter(variable == 'perc_female' | variable== "perc_male")


#### ETHNICITY ###############################################
str(char_avg)
char_avg_ethnicity <- char_avg %>%
                      select(c(1:3,11,13,15,17,19))



char_avg_ethnicity

char_avg_group_ethnicity <- char_avg_group[c(1,2,9:18)] %>%
                                        mutate_each(funs(round(.,2)), -Type)

char_avg_group_ethnicity



char_avg_group_ethnicity
char_avg_group_ethnicity_perc <- char_avg_group_ethnicity[c(1,4,6,8,10,12)]

char_avg_group_ethnicity_perc

char_avg_group_ethnicity_long <- melt(char_avg_group_ethnicity) %>%
                                filter(variable == 'perc_asian' | variable== "perc_black" | variable=="perc_hispanic" | variable=="perc_white" | variable=="perc_multi_nr")

char_avg_group_ethnicity_long


#### POVERTY ###############################################


char_avg_poverty <- char_avg %>%
                      select(c(1:3,24:25))

char_avg_group_poverty <- char_avg_group[c(1,2,23:24)] %>%
                                        mutate_each(funs(round(.,2)), -Type)

char_avg_group_poverty

char_avg_group_poverty_long <- melt(char_avg_group_poverty) %>%
                                filter(variable == 'perc_poverty')

char_avg_group_poverty_long

#### ELL ###############################################

names(char_avg)
char_avg_ell <- char_avg %>%
                      select(c(1:3,22:23))

char_avg_group_ell <- char_avg_group[c(1,2,21:22)] %>%
                                        mutate_each(funs(round(.,2)), -Type)

char_avg_group_ell

char_avg_group_ell_long <- melt(char_avg_group_ell) %>%
                                filter(variable == 'perc_ell')

char_avg_group_ell_long

#### MATH ELA ###############################################

char_avg_group_math_ela <- char_avg %>%
                      select(-DBN) %>%
                      group_by(Type) %>%
                      summarise_each(funs(mean)) %>%
                      select(c(1:4))

char_avg_group_math_ela

char_avg_math_ela <- char_avg %>%
                      select(c(1:5))

char_avg_math_ela


char_avg_group_math_ela_long <- melt(char_avg_group_math_ela) %>%
                                filter(variable == 'Avg_Grade8_Eng_Prof' | variable == 'Avg_Grade8_Math_Prof')
char_avg_group_math_ela_long


## ----gender_grouped_presentation, echo=FALSE-----------------------------

ggplot(char_avg_group_gender_long, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")

kable(char_avg_group_gender)



## ----gender_school_presentation, echo=FALSE------------------------------

kable(char_avg_gender)


## ----ethnicity_grouped_presentation, echo=FALSE--------------------------

ggplot(char_avg_group_ethnicity_long, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")

kable(char_avg_group_ethnicity_perc)



## ----ethncity_school_presentation, echo=FALSE----------------------------

kable(char_avg_ethnicity)


## ----poverty_grouped_presentation, echo=FALSE----------------------------

ggplot(char_avg_group_poverty_long, aes(Type, value)) + 
        geom_bar(aes(fill=Type), position="dodge", stat="identity")

kable(char_avg_group_poverty)



## ----poverty_school_presentation, echo=FALSE-----------------------------

kable(char_avg_poverty)


## ----ell_grouped_presentation, echo=FALSE--------------------------------

ggplot(char_avg_group_ell_long, aes(Type, value)) + 
        geom_bar(aes(fill=Type), position="dodge", stat="identity")

kable(char_avg_group_ell)



## ----ell_school_presentation, echo=FALSE---------------------------------

kable(char_avg_ell)


## ----mathela_grouped_presentation, echo=FALSE----------------------------

ggplot(char_avg_group_math_ela_long, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")

kable(char_avg_group_math_ela)



## ----mathela_school_presentation, echo=FALSE-----------------------------

kable(char_avg_math_ela)


## ----cte_setup, include=FALSE--------------------------------------------

cte_any <- read.csv("K:/OPSR/MP Planning/Operations, Finance, & Research/Research/OPSR Data Team Files/Nadine/sam_17/method_2/full_run_onlyCTEDBN_8.8.17.csv")

demo_s <- read.xlsx("D:/ptech/DemographicSnapshot201213to201617Public_FINAL1.xlsx", sheet="School") %>%
            filter(Year=='2016-17')

names(cte_any)[2] <- "DBN"

cte_any_credits <- cte_any %>%
                    select(insequence, other_credits) %>%
                    summarise_each(funs(sum)) %>%
                    mutate(DBN = "all_cte") %>%
                    mutate(Type = "all_cte")

cte_any_credits1 <- cte_any_credits[c(3,4,1,2)]

cte_any1 <- demo_s %>%
              select(DBN, Grade.9, Grade.10, Grade.11, Grade.12) %>%
              mutate(enr_9_12= (Grade.9 + Grade.10 + Grade.11 + Grade.12)) %>%
              select(DBN, enr_9_12) %>%
              right_join(cte_any, by="DBN")
          
cte_total_enr_school <- cte_any1 %>%
                          select(DBN, enr_9_12)  %>%
                          unique

cte_total_enr_school_sum <- sum(cte_total_enr_school$enr_9_12)

cte_any_lt <- as.data.frame(table(cte_any1$leveltype))

cte_any_lt$Type <- "cte_all"

cte_any_lt_melt <- dcast(cte_any_lt, Type ~ Var1, value.var="Freq")

cte_any_lt_melt1 <- cte_any_lt_melt %>%
                      select(c(1:4))

cte_any_lt_melt1$cte_enr <- cte_any_lt_melt1$`Enrolled- credits in sequence` + cte_any_lt_melt1$`Enrolled - credits in different sequence` + cte_any_lt_melt1$`Enrolled - no credits in CTE attempted`

names(cte_any_lt_melt1)[2] <- "Enr-credits_in_seq"
names(cte_any_lt_melt1)[3] <- "Enr-credits_in_diff_seq"
names(cte_any_lt_melt1)[4] <- "Enr-no_cred_CTE"

cte_any_lt_melt1$enr_9_12 <- cte_total_enr_school_sum

cte_any_lt_melt1$prop_cte <- cte_any_lt_melt1$cte_enr / cte_any_lt_melt1$enr_9_12

cte_any_lt_melt1$DBN <- 'cte_all'

cte_any_lt_melt2 <- cte_any_lt_melt1[c(8,1:7)]

cte <- pt[c(2,37,31:34,53:57)]

cte[is.na(cte)] <- 0

names(cte)[7] <- "Enr-credits_in_seq"
names(cte)[8] <- "Enr-credits_in_diff_seq"
names(cte)[9] <- "Enr-no_cred_CTE"

cte$enr_9_12 <- cte$Grade.9 + cte$Grade.10 + cte$Grade.11 + cte$Grade.12

cte$cte_enr <- cte$`Enr-credits_in_seq` + cte$`Enr-credits_in_diff_seq` + cte$`Enr-no_cred_CTE`

cte$prop_cte <- (cte$cte_enr / cte$enr_9_12)

cte_enr <- cte[c(1,2,7:9,13,12,14)] %>%
              arrange(desc(Type)) %>%
              mutate(prop_cte = round(prop_cte,2))

cte_enr_plus_all <- rbind(cte_enr, cte_any_lt_melt2)

cte_credits <-  cte[c(1,2,10,11)] %>%
                  arrange(desc(Type)) %>%
                  mutate(cte_creds_not_insequence = round(cte_creds_not_insequence,0))

names(cte_any_credits1)[3] <- "cte_creds_insequence"
names(cte_any_credits1)[4] <- "cte_creds_not_insequence"

cte_credits_plus_all <- rbind(cte_credits, cte_any_credits1)


## ----cte_enrolled_presentation, echo=FALSE-------------------------------

kable(cte_enr_plus_all)


## ----cte_credits_presentation, echo=FALSE--------------------------------

kable(cte_credits_plus_all)


## ----outcomes_setup, include=FALSE---------------------------------------

grad_non_grad <- pt[c(2,37,98:107)]

grad_non_grad[is.na(grad_non_grad)] <- 0

grad_non_grad$total_2010_cohort6year <- grad_non_grad$grad_2010_cohort6year + grad_non_grad$nongrad_2010_cohort6year
grad_non_grad$total_2011_cohort5year <- grad_non_grad$grad_2011_cohort5year + grad_non_grad$nongrad_2011_cohort5year
grad_non_grad$total_2011_cohort5yearAug <- grad_non_grad$grad_2011_cohort5yearAug + grad_non_grad$nongrad_2011_cohort5yearAug
grad_non_grad$total_2012_cohort4year <- grad_non_grad$grad_2012_cohort4year + grad_non_grad$nongrad_2012_cohort4year
grad_non_grad$total_2012_cohort4yearAug <- grad_non_grad$grad_2012_cohort4yearAug + grad_non_grad$nongrad_2012_cohort4yearAug
names(grad_non_grad)

grad_non_grad$ng_rate_2010_cohort6year <- grad_non_grad$nongrad_2010_cohort6year / grad_non_grad$total_2010_cohort6year
grad_non_grad$ng_rate_2011_cohort5year <- grad_non_grad$nongrad_2011_cohort5year / grad_non_grad$total_2011_cohort5year
grad_non_grad$ng_rate_2011_cohort5yearAug <- grad_non_grad$nongrad_2011_cohort5yearAug / grad_non_grad$total_2011_cohort5yearAug
grad_non_grad$ng_rate_2012_cohort4year <- grad_non_grad$nongrad_2012_cohort4year / grad_non_grad$total_2012_cohort4year
grad_non_grad$ng_rate_2012_cohort4yearAug <- grad_non_grad$nongrad_2012_cohort4yearAug / grad_non_grad$total_2012_cohort4yearAug

grad_non_grad$g_rate_2010_cohort6year <- grad_non_grad$grad_2010_cohort6year / grad_non_grad$total_2010_cohort6year
grad_non_grad$g_rate_2011_cohort5year <- grad_non_grad$grad_2011_cohort5year / grad_non_grad$total_2011_cohort5year
grad_non_grad$g_rate_2011_cohort5yearAug <- grad_non_grad$grad_2011_cohort5yearAug / grad_non_grad$total_2011_cohort5yearAug
grad_non_grad$g_rate_2012_cohort4year <- grad_non_grad$grad_2012_cohort4year / grad_non_grad$total_2012_cohort4year
grad_non_grad$g_rate_2012_cohort4yearAug <- grad_non_grad$grad_2012_cohort4yearAug / grad_non_grad$total_2012_cohort4yearAug


str(grad_non_grad)
only_non_grad <- grad_non_grad[c(1,2,18:22)] %>%
                                  mutate_each(funs(round(.,2)), -DBN, -Type)   

only_grad <- grad_non_grad[c(1,2,23:27)]

sqr_grad_rates <- pt[c(2,37,108,109)]

sqr_grad_rates

### Grad Rates Degree Type ###############################################

deg_type <- pt[c(2,37,60:62,68:70,76:78,84:86,92:94)]

str(deg_type)
deg_type[is.na(deg_type)] <- 0

deg_type_with_total <- grad_non_grad %>%
                          select(DBN, grad_2010_cohort6year, grad_2011_cohort5year,grad_2011_cohort5yearAug, grad_2012_cohort4year,grad_2012_cohort4yearAug) %>%
                          right_join(deg_type, by="DBN")

deg_type_with_total_group <- deg_type_with_total %>%
                                select(-DBN) %>%
                                group_by(Type) %>%
                                summarise_each(funs(sum))

deg_type_with_total_group$c6yr_local_dip <- deg_type_with_total_group$X2010.Total.Cohort...6.Year.Outcome_Local.Diploma / deg_type_with_total_group$grad_2010_cohort6year 
deg_type_with_total_group$c6yr_regents <- deg_type_with_total_group$X2010.Total.Cohort...6.Year.Outcome_Regents.Diploma / deg_type_with_total_group$grad_2010_cohort6year 
deg_type_with_total_group$c6yr_advregents <- deg_type_with_total_group$X2010.Total.Cohort...6.Year.Outcome_Regents.Diploma.with.Adv.Desig / deg_type_with_total_group$grad_2010_cohort6year 

deg_type_with_total_group$c5yr_local_dip <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome_Local.Diploma / deg_type_with_total_group$grad_2011_cohort5year 
deg_type_with_total_group$c5yr_regents <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome_Regents.Diploma / deg_type_with_total_group$grad_2011_cohort5year 
deg_type_with_total_group$c5yr_advregents <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome_Regents.Diploma.with.Adv.Desig / deg_type_with_total_group$grad_2011_cohort5year 

deg_type_with_total_group$c5yrAug_local_dip <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome...August_Local.Diploma / deg_type_with_total_group$grad_2011_cohort5yearAug 
deg_type_with_total_group$c5yrAug_regents <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome...August_Regents.Diploma / deg_type_with_total_group$grad_2011_cohort5yearAug 
deg_type_with_total_group$c5yrAug_advregents <- deg_type_with_total_group$X2011.Total.Cohort...5.Year.Outcome...August_Regents.Diploma.with.Adv.Desig / deg_type_with_total_group$grad_2011_cohort5yearAug 

deg_type_with_total_group$c4yr_local_dip <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome_Local.Diploma / deg_type_with_total_group$grad_2012_cohort4year 
deg_type_with_total_group$c4yr_regents <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome_Regents.Diploma / deg_type_with_total_group$grad_2012_cohort4year
deg_type_with_total_group$c4yr_advregents <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome_Regents.Diploma.with.Adv.Desig / deg_type_with_total_group$grad_2012_cohort4year

deg_type_with_total_group$c4yrAug_local_dip <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome...August.2016_Local.Diploma / deg_type_with_total_group$grad_2012_cohort4yearAug 
deg_type_with_total_group$c4yrAug_regents <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome...August.2016_Regents.Diploma / deg_type_with_total_group$grad_2012_cohort4yearAug
deg_type_with_total_group$c4yrAug_advregents <- deg_type_with_total_group$X2012.Total.Cohort...4.Year.Outcome...August.2016_Regents.Diploma.with.Adv.Desig / deg_type_with_total_group$grad_2012_cohort4yearAug



names(deg_type_with_total_group)
deg_type_with_total_group1 <- deg_type_with_total_group[c(1,22:35)]
deg_type_with_total_group1

deg_type_with_total_group2 <- melt(deg_type_with_total_group1, id.vars=c(1))
deg_type_with_total_group3 <- dcast(deg_type_with_total_group2, variable ~ Type, value.var = "value")

deg_type_with_total_group3

ggplot(deg_type_with_total_group2, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")

str(deg_type_with_total_group2)
deg_type_melt <- melt(deg_type)

deg_type


deg_type_group_melt <- melt(deg_type) 

deg_type_group_melt

# h1$outcome_category <- ifelse((h1$outcome == 'Dropout' | 
#                                  h1$outcome == 'IEP or Commencement Credential' |
#                                  h1$outcome == 'Still Enrolled' | 
#                                  h1$outcome == 'Transfer' | 
#                                  h1$outcome == 'Transfer to GED'), 'N', 'G')


### Disaggregated by Demographics
names(pt)
demo_deg <- pt[c(2,37,110:121)]

demo_deg[is.na(demo_deg)] <- 0

demo_deg$total_g <- demo_deg$Asian_G + demo_deg$Asian_N + demo_deg$Black_G + demo_deg$Black_N + demo_deg$Hispanic_G + demo_deg$Hispanic_N + demo_deg$MultiRacial_G + demo_deg$MultiRacial_N + demo_deg$Native.American_G + demo_deg$Native.American_N + demo_deg$White_G + demo_deg$White_N

demo_deg$p_white_grad <- demo_deg$White_G / demo_deg$total_g
demo_deg$p_white_ngrad <- demo_deg$White_N / demo_deg$total_g

demo_deg$p_black_grad <- demo_deg$Black_G / demo_deg$total_g
demo_deg$p_black_ngrad <- demo_deg$Black_N / demo_deg$total_g


demo_deg$p_asian_grad <- demo_deg$Asian_G / demo_deg$total_g
demo_deg$p_asian_ngrad <- demo_deg$Asian_N / demo_deg$total_g

demo_deg$p_hisp_grad <- demo_deg$Hispanic_G / demo_deg$total_g
demo_deg$p_hisp_ngrad <- demo_deg$Hispanic_N / demo_deg$total_g

demo_deg$p_natAm_grad <- demo_deg$Native.American_G / demo_deg$total_g
demo_deg$p_natAm_ngrad <- demo_deg$Native.American_N / demo_deg$total_g

demo_deg$p_mr_nr_grad <- demo_deg$MultiRacial_G / demo_deg$total_g
demo_deg$p_mr_nr_ngrad <- demo_deg$MultiRacial_N / demo_deg$total_g



demo_deg1 <- demo_deg[c(1,2,16:27)] %>%
                 mutate_each(funs(round(.,2)), -DBN, -Type)   
      
demo_deg2 <- demo_deg1[c(1,2,3,5,7,9,11,13)]

demo_deg2

demo_deg_grouped <- demo_deg %>%
                        select(-DBN) %>%
                        group_by(Type) %>%
                        summarise_each(funs(sum))

demo_deg_grouped$p_white_grad <- demo_deg_grouped$White_G / demo_deg_grouped$total_g
demo_deg_grouped$p_white_ngrad <- demo_deg_grouped$White_N / demo_deg_grouped$total_g

demo_deg_grouped$p_black_grad <- demo_deg_grouped$Black_G / demo_deg_grouped$total_g
demo_deg_grouped$p_black_ngrad <- demo_deg_grouped$Black_N / demo_deg_grouped$total_g


demo_deg_grouped$p_asian_grad <- demo_deg_grouped$Asian_G / demo_deg_grouped$total_g
demo_deg_grouped$p_asian_ngrad <- demo_deg_grouped$Asian_N / demo_deg_grouped$total_g

demo_deg_grouped$p_hisp_grad <- demo_deg_grouped$Hispanic_G / demo_deg_grouped$total_g
demo_deg_grouped$p_hisp_ngrad <- demo_deg_grouped$Hispanic_N / demo_deg_grouped$total_g

demo_deg_grouped$p_natAm_grad <- demo_deg_grouped$Native.American_G / demo_deg_grouped$total_g
demo_deg_grouped$p_natAm_ngrad <- demo_deg_grouped$Native.American_N / demo_deg_grouped$total_g

demo_deg_grouped$p_mr_nr_grad <- demo_deg_grouped$MultiRacial_G / demo_deg_grouped$total_g
demo_deg_grouped$p_mr_nr_ngrad <- demo_deg_grouped$MultiRacial_N / demo_deg_grouped$total_g


names(demo_deg_grouped)

demo_deg_grouped1 <- demo_deg_grouped[c(1,15:26)]

demo_deg_grouped2 <- melt(demo_deg_grouped1)

demo_deg_grouped2

demo_deg_grouped3 <- dcast(demo_deg_grouped2, variable ~ Type, value.var="value")

demo_deg_grouped3


## ----dropout_rates_presentation, echo=FALSE------------------------------

kable(only_non_grad)


## ----grad_rates_presentation, echo=FALSE---------------------------------

kable(only_grad)


## ----sqr_grad_rates_presentation, echo=FALSE-----------------------------

kable(sqr_grad_rates)


## ----grouped_grad_rates_degree_types_presentation, echo=FALSE------------

kable(deg_type_with_total_group3)

ggplot(deg_type_with_total_group2, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")


## ----grad_rates_degree_types_presentation, echo=FALSE--------------------

kable(deg_type_group_melt)


## ----grad_rates_ethnicity_grouped_presentation, echo=FALSE---------------

ggplot(demo_deg_grouped2, aes(Type, value)) + 
        geom_bar(aes(fill=variable), position="dodge", stat="identity")

kable(demo_deg_grouped3)


## ----grad_rates_ethnicity_presentation, echo=FALSE-----------------------

kable(demo_deg2)


