#This looks at differences between race/ethnicity and socioeconomic levels relating to crime using data from 
#the 2020 National Crime Victimization Survey

#library("googledrive")

#url_DS0001 = "https://drive.google.com/file/d/1yw_6RSxSFlOenZrvUpjG-CGbrzXLQB_J/view?usp=sharing" 
#drive_download(url_DS0001)

#url_DS0002 = "https://drive.google.com/file/d/1FbQlPctKWaoY3m3mYYY7Iz18meaPDNyx/view?usp=sharing" 
#drive_download(url_DS0002)

#url_DS0003 = "https://drive.google.com/file/d/1PZ59yUC1l30VtU153Qlp7V_WwO-nbyga/view?usp=sharing" 
#drive_download(url_DS0003)

#url_DS0004 = "https://drive.google.com/file/d/1tQC-nMYFV_9g9CDkdc6RHDJRmliko7Rn/view?usp=sharing" 
#drive_download(url_DS0004)

#url_DS0005 = "https://drive.google.com/file/d/19J-7vRhgZDA7Ma9s6_aPWjalc_P1RMRh/view?usp=sharing" 
#drive_download(url_DS0005)

load(file="38090-0001-Data.rda")
load(file="38090-0002-Data.rda")
load(file="38090-0003-Data.rda")
load(file="38090-0004-Data.rda")
load(file="38090-0005-Data.rda")

data_1 <- da38090.0001
data_2 <- da38090.0002
data_3 <- da38090.0003
data_4 <- da38090.0004
data_5 <- da38090.0005

#install.packages("tidyverse")
library(tidyverse)

#3071 job last week
#V4479 employed
#3020 education level
#3023A race
#SC214A income

#V4399 reported to police
#V4438 did police come
#V4439 how long did police take
#V4466 arrest or charges made
#V4467 help from victim agencies
#V4526AA hate crime

#3081 number of incidents reported

n_data_3 <- data_3 %>% mutate(V3020A = as.factor(if_else(data_3$V3020 =="(00) Nev/kindergrtn"
                                                         |data_3$V3020 == "(01) 1:Elementary"
                                                         |data_3$V3020 =="(02) 2:Elementary"
                                                         |data_3$V3020 =="(03) 3:Elementary"
                                                         |data_3$V3020 ==  "(04) 4:Elementary"                                                              
                                                         |data_3$V3020 =="(05) 5:Elementary"
                                                         |data_3$V3020 =="(06) 6:Elementary"
                                                         |data_3$V3020 == "(07) 7:Elementary"
                                                         |data_3$V3020 ==  "(08) 8:Elementary"
                                                         |data_3$V3020 ==  "(09) 9:High school"
                                                         |data_3$V3020 ==  "(10) 10:High school"
                                                         |data_3$V3020 ==  "(11) 11:High school"
                                                         |data_3$V3020 ==  "(27) 12th grade(no diploma)"
                                                         ,"(1) no diploma", as.character(data_3$V3020))))


merged_data <- merge(n_data_3, data_4, by = "IDPER")

new_merged_data <- merged_data %>% distinct(IDPER, .keep_all = TRUE) %>% droplevels()

merged_data_2 <- merge(data_2, data_4, by = "IDHH")

new_merged_data_2 <- merged_data_2 %>% distinct(IDHH, .keep_all = TRUE) %>% droplevels()

merged_data_3 <- merge(data_2, n_data_3, by = "IDHH")

new_merged_data_3 <- merged_data_3 %>% distinct(IDHH, .keep_all = TRUE) %>% droplevels()

job <- new_merged_data %>% filter((new_merged_data$V3071 == "(1) Yes" | new_merged_data$V3071 == "(2) No") 
                                  & (new_merged_data$V4467 == "(1) Yes" | new_merged_data$V4467 == "(2) No")
) %>% droplevels()

chisq.test(table(job$V3071, job$V4467))

dat <- job[!is.na(job$V4467), ]

ggplot(dat) +
  aes(x = V3071, fill = V4467) +
  geom_bar(position = "fill") +
  labs(x = "Job Last 6 Months", fill = "Help from Victim Agency")

#More victims with no job in last week got help from victim agency

job_2 <- new_merged_data %>% filter(new_merged_data$V4439 == "(1) Within 5 min" 
                                    | new_merged_data$V4439 == "(2) Within 10 min"
                                    | new_merged_data$V4439 == "(3) Within an hour"
                                    | new_merged_data$V4439 == "(4) Within a day"
                                    | new_merged_data$V4439 == "(5) More than a day") %>% droplevels()

#chisq.test(table(job_2$V4479, job_2$V4439))

job_3 <- new_merged_data %>% filter(new_merged_data$V4467 == "(1) Yes" 
                                    | new_merged_data$V4467 == "(2) No") %>% droplevels()

chisq.test(table(job_3$V4479, job_3$V4467))

jobdat <- job_3[!is.na(job_2$V4467), ]

ggplot(jobdat) +
  aes(x = V4479, fill = V4467) +
  geom_bar(position = "fill") +
  labs(x = "Employment", fill = "Help from Victim Agency")

#More victims with no employment got help from victim agency

edu_data <- new_merged_data %>% filter(new_merged_data$V3020A == "(1) no diploma" 
                                       | new_merged_data$V3020A == "(28) High school grad"
                                       | new_merged_data$V3020A == "(40) Some college(no degree)"
                                       | new_merged_data$V3020A == "(41) Associate degree" 
                                       | new_merged_data$V3020A == "(42) Bachelor degree" 
                                       | new_merged_data$V3020A == "(43) Master degree" 
                                       | new_merged_data$V3020A == "(44) Prof school degree" 
                                       | new_merged_data$V3020A == "(45) Doctorate degree") %>% droplevels()

edu <- edu_data %>% filter(edu_data$V4438 == "(1) Yes" 
                           |edu_data$V4438 == "(2) No") %>% droplevels()

chisq.test(table(edu$V3020A, edu$V4438))

edudat <- edu[!is.na(edu$V4438), ]

ggplot(edudat) +
  aes(x = V3020A, fill = V4438) +
  geom_bar(position = "fill") +
  labs(x = "Education Level", fill = "Did Police Come")

#Higher proportion of police that came for victims with less education

edu_2 <- edu_data %>% filter(edu_data$V4439 == "(1) Within 5 min" 
                             | edu_data$V4439 == "(2) Within 10 min"
                             | edu_data$V4439 == "(3) Within an hour"
                             | edu_data$V4439 == "(4) Within a day"
                             | edu_data$V4439 == "(5) More than a day") %>% droplevels()

#chisq.test(table(edu_2$V3020A, edu_2$V4439))

race_data <- new_merged_data %>% filter(new_merged_data$V3023A == "(01) White only" 
                                        | new_merged_data$V3023A == "(02) Black only"
                                        | new_merged_data$V3023A == "(04) Asian only"
                                        | new_merged_data$V3023A == "(03) Am Ind/AK native only") %>% droplevels()

race_data_2 <- new_merged_data %>% filter(new_merged_data$V3023A == "(01) White only" 
                                          | new_merged_data$V3023A == "(02) Black only") %>% droplevels()

race <- race_data %>% filter(race_data$V4438 == "(1) Yes" 
                             |race_data$V4438 == "(2) No") %>% droplevels()

chisq.test(table(race$V3023A, race$V4438))

racedat <- race[!is.na(race$V4438), ]

ggplot(racedat) +
  aes(x = V3023A, fill = V4438) +
  geom_bar(position = "fill") + 
  labs(x = "Race", fill = "Did Police Come")

#Higher proportion of police came for  Black victims, and lower for Asian victims

race_2 <- race_data_2 %>% filter(race_data_2$V4439 == "(1) Within 5 min" 
                                 | race_data_2$V4439 == "(2) Within 10 min"
                                 | race_data_2$V4439 == "(3) Within an hour"
                                 | race_data_2$V4439 == "(4) Within a day"
                                 | race_data_2$V4439 == "(5) More than a day") %>% droplevels()

chisq.test(table(race_2$V3023A, race_2$V4439))

race2dat <- race_2[!is.na(race_2$V4439), ]

ggplot(race2dat) +
  aes(x = V3023A, fill = V4439) +
  geom_bar(position = "fill") +
  labs(x = "Race", fill = "How Long Police Took")

#Police took longer to come for Black victims

hispanic <- new_merged_data %>% filter(new_merged_data$V4438 == "(1) Yes" 
                                       |new_merged_data$V4438 == "(2) No") %>% droplevels()

chisq.test(table(hispanic$V3024A, hispanic$V4438))

hispdat <- hispanic[!is.na(hispanic$V4438), ]

ggplot(hispdat) +
  aes(x = V3024A, fill = V4438) +
  geom_bar(position = "fill") +
  labs(x = "Hispanic", fill = "Did Police Come")

#Higher proportion of police came for Hispanic victims

hispanic_2 <- new_merged_data %>% filter(new_merged_data$V4439 == "(1) Within 5 min" 
                                         | new_merged_data$V4439 == "(2) Within 10 min"
                                         | new_merged_data$V4439 == "(3) Within an hour"
                                         | new_merged_data$V4439 == "(4) Within a day"
                                         | new_merged_data$V4439 == "(5) More than a day") %>% droplevels()

#chisq.test(table(hispanic_2$V3024A, hispanic_2$V4439))

income <- new_merged_data_2 %>% filter(new_merged_data_2$V4438 == "(1) Yes" 
                                       |new_merged_data_2$V4438 == "(2) No") %>% droplevels()

chisq.test(table(income$SC214A, income$V4438))

incomedat <- income[!is.na(income$V4438), ]

ggplot(incomedat) +
  aes(x = SC214A, fill = V4438) +
  geom_bar(position = "fill") +
  labs(x = "Income", fill = "Did Police Come")

#Higher proportion police came for lower income victims

income_2 <- new_merged_data_2 %>% filter(new_merged_data_2$V4439 == "(1) Within 5 min" 
                                         | new_merged_data_2$V4439 == "(2) Within 10 min"
                                         | new_merged_data_2$V4439 == "(3) Within an hour"
                                         | new_merged_data_2$V4439 == "(4) Within a day"
                                         | new_merged_data_2$V4439 == "(5) More than a day") %>% droplevels()

#chisq.test(table(income_2$SC214A, income_2$V4439))

one.way <- aov(edu_data$V3081 ~ edu_data$V3020A, data = edu_data)

one.way_2 <- aov(race_data$V3081 ~ race_data$V3023A, data = race_data)

#one.way_3 <- aov(new_merged_data$V3081 ~ new_merged_data$V3024A, data = new_merged_data)

#one.way_4 <- aov(new_merged_data_3$V3081 ~ new_merged_data_3$SC214A, data = new_merged_data_3)

#one.way_5 <- aov(new_merged_data$V3081 ~ new_merged_data$V3071, data = new_merged_data)

#one.way_6 <- aov(new_merged_data$V3081 ~ new_merged_data$V4479, data = new_merged_data)

summary(one.way)

TukeyHSD(one.way)

#Victims with Some college with no degree had significantly more incidents reported than victims with bachelor degree

ggplot(edu_data) +
  aes(x = V3020A, y = V3081, color = V3020A) +
  geom_jitter() +
  theme(legend.position = "none") +
  labs(x = "Education Level", y = "Incidents Reported")

boxplot(V3081 ~ V3020A,
        data = edu_data,
        xlab = "Education Level",
        ylab = "Incidents Reported"
)

summary(one.way_2)

TukeyHSD(one.way_2)
#Black victims had significantly more incidents reported than White victims
#White victims had significantly more incidents reported than Asian victims
#Black victims had significantly more incidents reported than Asian victims
#Indigenous victims had significantly more incidents reported than Asian victims 

ggplot(race_data) +
  aes(x = V3023A, y = V3081, color = V3023A) +
  geom_jitter() +
  theme(legend.position = "none") +
  labs(x = "Race", y = "Incidents Reported")

boxplot(V3081 ~ V3023A,
        data = race_data,
        xlab = "Race",
        ylab = "Incidents Reported"
)



