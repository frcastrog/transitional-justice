#--------------------------------Public Opinion Data---------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 07, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 27, 2024-#

# Load Packages

pacman::p_load(haven, lubridate, purrr, readr, magrittr, dplyr, haven, countrycode)

# Afrobarometer

afrobarometer_r1 <- read_sav("00-data/surveys/afrobarometer/merged_r1_data.sav")
afrobarometer_r2 <- read_sav("00-data/surveys/afrobarometer/merged_r2_data.sav")
afrobarometer_r3 <- read_sav("00-data/surveys/afrobarometer/merged_r3_data.sav")
afrobarometer_r4 <- read_sav("00-data/surveys/afrobarometer/merged_r4_data.sav")
afrobarometer_r5 <- read_sav("00-data/surveys/afrobarometer/merged_r5_data.sav")
afrobarometer_r6 <- read_sav("00-data/surveys/afrobarometer/merged_r6_data.sav", encoding = "latin1") # Round 6 has a different encoding
afrobarometer_r7 <- read_sav("00-data/surveys/afrobarometer/merged_r7_data.sav")


#- Relevant variables for each wave

## r1: country -  1=Botswana; 2=Ghana; 3=Lesotho; 4=Malawi; 5=Mali; 6=Namibia; 7=Nigeria; 8=South Africa; 9=Tanzania; 10=Uganda; 11=Zambia; 12=Zimbabwe 
# year has to be created. Namibia is 2002, Tanzania 2001, Nigeria 2001, Mali 2001, South Africa 2000, Lesotho 2000, Uganda 2000,
# Ghana 1999, Zimbabwe 1999, Zambia 1999, Namibia 1999, Malawi 1999, Botswana 1999. 

## r2: country - 1=Botswana; 2=Ghana; 3=Lesotho; 4=Malawi; 5=Mali; 6=Namibia; 7=Nigeria; 8=South Africa; 9=Tanzania; 10=Uganda; 11=Zambia; 12=Zimbabwe; 13=Cape Verde; 14=Kenya; 15=Mozambique; 16=Senegal
# year:  dateintr in the format of YYYY-MM-DD

## r3: country - 1=Benin, 2=Botswana, 3=Cape Verde, 4=Ghana, 5=Kenya, 6=Lesotho, 7=Madagascar, 8=Malawi, 9=Mali, 10=Mozambique, 11=Namibia, 12=Nigeria, 13=Senegal, 14=South Africa, 15=Tanzania, 16=Uganda, 17=Zambia, 18=Zimbabwe 
# year:  dateintr in the format of YYYY-MM-DD

## r4: COUNTRY - 1=Benin, 2=Botswana, 3=Burkina Faso, 4=Cape Verde, 5=Ghana, 6=Kenya, 7=Lesotho, 8=Liberia, 9=Madagascar, 10=Malawi, 11=Mali, 12=Mozambique, 13=Namibia, 14=Nigeria, 15=Senegal, 16=South Africa, 17=Tanzania, 18=Uganda, 19=Zambia, 20=Zimbabwe 
# year: DATEINTR in the format of YYYY-MM-DD

## r5: COUNTRY_ALPHA - 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde, 8=Cote d’Ivoire, 9=Egypt, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar, 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria, 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Swaziland, 30=Tanzania, 31=Togo, 32=Tunisia, 33=Uganda, 34=Zambia, 35=Zimbabwe
# year: DATEINTR in the format of YYYY-MM-DD

## r6: COUNTRY - 1=Algeria, 2=Benin, 3=Botswana, 4=Burkina Faso, 5=Burundi, 6=Cameroon, 7=Cape Verde, 8=Cote d'Ivoire, 9=Egypt, 10=Gabon, 11=Ghana, 12=Guinea, 13=Kenya, 14=Lesotho, 15=Liberia, 16=Madagascar, 17=Malawi, 18=Mali, 19=Mauritius, 20=Morocco, 21=Mozambique, 22=Namibia, 23=Niger, 24=Nigeria, 25=São Tomé and Príncipe, 26=Senegal, 27=Sierra Leone, 28=South Africa, 29=Sudan, 30=Swaziland, 31=Tanzania, 32=Togo, 33=Tunisia, 34=Uganda, 35=Zambia, 36=Zimbabwe 
# year: DATEINTR in the format of YYYY-MM-DD

## r7: COUNTRY - 1=Benin, 2=Botswana, 3=Burkina Faso, 4=Cabo Verde, 5=Cameroon, 6=Côte d'Ivoire, 7=eSwatini, 8=Gabon, 9=Gambia, 10=Ghana, 11=Guinea, 12=Kenya, 13=Lesotho, 14=Liberia, 15=Madagascar, 16=Malawi, 17=Mali, 18=Mauritius, 19=Morocco, 20=Mozambique, 21=Namibia, 22=Niger, 23=Nigeria, 24=São Tomé and Príncipe, 25=Senegal, 26=Sierra Leone, 27=South Africa, 28=Sudan, 29=Tanzania, 30=Togo, 31=Tunisia, 32=Uganda, 33=Zambia, 34=Zimbabwe
# year: DATEINTR in the format of YYYY-MM-DD

afrobarometer_r1 %<>%
  mutate(
    country_name = case_when(
      country == 1 ~ "Botswana",
      country == 2 ~ "Ghana",
      country == 3 ~ "Lesotho",
      country == 4 ~ "Malawi",
      country == 5 ~ "Mali",
      country == 6 ~ "Namibia",
      country == 7 ~ "Nigeria",
      country == 8 ~ "South Africa",
      country == 9 ~ "Tanzania",
      country == 10 ~ "Uganda",
      country == 11 ~ "Zambia",
      country == 12 ~ "Zimbabwe"),
    year = case_when(
      country_name == "Namibia" ~ 2002,
      country_name == "Tanzania" ~ 2001,
      country_name == "Nigeria" ~ 2001,
      country_name == "Mali" ~ 2001,
      country_name == "South Africa" ~ 2000,
      country_name == "Lesotho" ~ 2000,
      country_name == "Uganda" ~ 2000,
      country_name == "Ghana" ~ 1999,
      country_name == "Zimbabwe" ~ 1999,
      country_name == "Zambia" ~ 1999,
      country_name == "Malawi" ~ 1999,
      country_name == "Botswana" ~ 1999))

afrobarometer_r2 %<>%
  mutate(
    country_name = case_when(
      country == 1 ~ "Botswana",
      country == 2 ~ "Ghana",
      country == 3 ~ "Lesotho",
      country == 4 ~ "Malawi",
      country == 5 ~ "Mali",
      country == 6 ~ "Namibia",
      country == 7 ~ "Nigeria",
      country == 8 ~ "South Africa",
      country == 9 ~ "Tanzania",
      country == 10 ~ "Uganda",
      country == 11 ~ "Zambia",
      country == 12 ~ "Zimbabwe",
      country == 13 ~ "Cape Verde",
      country == 14 ~ "Kenya",
      country == 15 ~ "Mozambique",
      country == 16 ~ "Senegal"),
    year = year(ymd(dateintr)))

afrobarometer_r3 %<>%
  mutate(
    country_name = case_when(
      country == 1 ~ "Benin",
      country == 2 ~ "Botswana",
      country == 3 ~ "Cape Verde",
      country == 4 ~ "Ghana",
      country == 5 ~ "Kenya",
      country == 6 ~ "Lesotho",
      country == 7 ~ "Madagascar",
      country == 8 ~ "Malawi",
      country == 9 ~ "Mali",
      country == 10 ~ "Mozambique",
      country == 11 ~ "Namibia",
      country == 12 ~ "Nigeria",
      country == 13 ~ "Senegal",
      country == 14 ~ "South Africa",
      country == 15 ~ "Tanzania",
      country == 16 ~ "Uganda",
      country == 17 ~ "Zambia",
      country == 18 ~ "Zimbabwe"),
    year = year(ymd(dateintr)))

afrobarometer_r4 %<>%
  mutate(
    country_name = case_when(
      COUNTRY == 1 ~ "Benin",
      COUNTRY == 2 ~ "Botswana",
      COUNTRY == 3 ~ "Burkina Faso",
      COUNTRY == 4 ~ "Cape Verde",
      COUNTRY == 5 ~ "Ghana",
      COUNTRY == 6 ~ "Kenya",
      COUNTRY == 7 ~ "Lesotho",
      COUNTRY == 8 ~ "Liberia",
      COUNTRY == 9 ~ "Madagascar",
      COUNTRY == 10 ~ "Malawi",
      COUNTRY == 11 ~ "Mali",
      COUNTRY == 12 ~ "Mozambique",
      COUNTRY == 13 ~ "Namibia",
      COUNTRY == 14 ~ "Nigeria",
      COUNTRY == 15 ~ "Senegal",
      COUNTRY == 16 ~ "South Africa",
      COUNTRY == 17 ~ "Tanzania",
      COUNTRY == 18 ~ "Uganda",
      COUNTRY == 19 ~ "Zambia",
      COUNTRY == 20 ~ "Zimbabwe"),
    year = year(ymd(DATEINTR)))

afrobarometer_r5 %<>%
  mutate(
    country_name = case_when(
      COUNTRY_ALPHA == 1 ~ "Algeria",
      COUNTRY_ALPHA == 2 ~ "Benin",
      COUNTRY_ALPHA == 3 ~ "Botswana",
      COUNTRY_ALPHA == 4 ~ "Burkina Faso",
      COUNTRY_ALPHA == 5 ~ "Burundi",
      COUNTRY_ALPHA == 6 ~ "Cameroon",
      COUNTRY_ALPHA == 7 ~ "Cape Verde",
      COUNTRY_ALPHA == 8 ~ "Ivory Coast",
      COUNTRY_ALPHA == 9 ~ "Egypt",
      COUNTRY_ALPHA == 11 ~ "Ghana",
      COUNTRY_ALPHA == 12 ~ "Guinea",
      COUNTRY_ALPHA == 13 ~ "Kenya",
      COUNTRY_ALPHA == 14 ~ "Lesotho",
      COUNTRY_ALPHA == 15 ~ "Liberia",
      COUNTRY_ALPHA == 16 ~ "Madagascar",
      COUNTRY_ALPHA == 17 ~ "Malawi",
      COUNTRY_ALPHA == 18 ~ "Mali",
      COUNTRY_ALPHA == 19 ~ "Mauritius",
      COUNTRY_ALPHA == 20 ~ "Morocco",
      COUNTRY_ALPHA == 21 ~ "Mozambique",
      COUNTRY_ALPHA == 22 ~ "Namibia",
      COUNTRY_ALPHA == 23 ~ "Niger",
      COUNTRY_ALPHA == 24 ~ "Nigeria",
      COUNTRY_ALPHA == 25 ~ "Senegal",
      COUNTRY_ALPHA == 26 ~ "Sierra Leone",
      COUNTRY_ALPHA == 27 ~ "South Africa",
      COUNTRY_ALPHA == 28 ~ "Sudan",
      COUNTRY_ALPHA == 29 ~ "Swaziland",
      COUNTRY_ALPHA == 30 ~ "Tanzania",
      COUNTRY_ALPHA == 31 ~ "Togo",
      COUNTRY_ALPHA == 32 ~ "Tunisia",
      COUNTRY_ALPHA == 33 ~ "Uganda",
      COUNTRY_ALPHA == 34 ~ "Zambia",
      COUNTRY_ALPHA == 35 ~ "Zimbabwe"),
    year = year(ymd(DATEINTR)))

afrobarometer_r6 %<>%
  mutate(
    country_name = case_when(
      COUNTRY == 1 ~ "Algeria",
      COUNTRY == 2 ~ "Benin",
      COUNTRY == 3 ~ "Botswana",
      COUNTRY == 4 ~ "Burkina Faso",
      COUNTRY == 5 ~ "Burundi",
      COUNTRY == 6 ~ "Cameroon",
      COUNTRY == 7 ~ "Cape Verde",
      COUNTRY == 8 ~ "Ivory Coast",
      COUNTRY == 9 ~ "Egypt",
      COUNTRY == 10 ~ "Gabon",
      COUNTRY == 11 ~ "Ghana",
      COUNTRY == 12 ~ "Guinea",
      COUNTRY == 13 ~ "Kenya",
      COUNTRY == 14 ~ "Lesotho",
      COUNTRY == 15 ~ "Liberia",
      COUNTRY == 16 ~ "Madagascar",
      COUNTRY == 17 ~ "Malawi",
      COUNTRY == 18 ~ "Mali",
      COUNTRY == 19 ~ "Mauritius",
      COUNTRY == 20 ~ "Morocco",
      COUNTRY == 21 ~ "Mozambique",
      COUNTRY == 22 ~ "Namibia",
      COUNTRY == 23 ~ "Niger",
      COUNTRY == 24 ~ "Nigeria",
      COUNTRY == 25 ~ "São Tomé and Príncipe",
      COUNTRY == 26 ~ "Senegal",
      COUNTRY == 27 ~ "Sierra Leone",
      COUNTRY == 28 ~ "South Africa",
      COUNTRY == 29 ~ "Sudan",
      COUNTRY == 30 ~ "Swaziland",
      COUNTRY == 31 ~ "Tanzania",
      COUNTRY == 32 ~ "Togo",
      COUNTRY == 33 ~ "Tunisia",
      COUNTRY == 34 ~ "Uganda",
      COUNTRY == 35 ~ "Zambia",
      COUNTRY == 36 ~ "Zimbabwe"
    ),
    year = year(ymd(DATEINTR)))

afrobarometer_r7 %<>%
  mutate(
    country_name = case_when(
      COUNTRY == 1 ~ "Benin",
      COUNTRY == 2 ~ "Botswana",
      COUNTRY == 3 ~ "Burkina Faso",
      COUNTRY == 4 ~ "Cabo Verde",
      COUNTRY == 5 ~ "Cameroon",
      COUNTRY == 6 ~ "Ivory Coast",
      COUNTRY == 7 ~ "eSwatini",
      COUNTRY == 8 ~ "Gabon",
      COUNTRY == 9 ~ "Gambia",
      COUNTRY == 10 ~ "Ghana",
      COUNTRY == 11 ~ "Guinea",
      COUNTRY == 12 ~ "Kenya",
      COUNTRY == 13 ~ "Lesotho",
      COUNTRY == 14 ~ "Liberia",
      COUNTRY == 15 ~ "Madagascar",
      COUNTRY == 16 ~ "Malawi",
      COUNTRY == 17 ~ "Mali",
      COUNTRY == 18 ~ "Mauritius",
      COUNTRY == 19 ~ "Morocco",
      COUNTRY == 20 ~ "Mozambique",
      COUNTRY == 21 ~ "Namibia",
      COUNTRY == 22 ~ "Niger",
      COUNTRY == 23 ~ "Nigeria",
      COUNTRY == 24 ~ "São Tomé and Príncipe",
      COUNTRY == 25 ~ "Senegal",
      COUNTRY == 26 ~ "Sierra Leone",
      COUNTRY == 27 ~ "South Africa",
      COUNTRY == 28 ~ "Sudan",
      COUNTRY == 29 ~ "Tanzania",
      COUNTRY == 30 ~ "Togo",
      COUNTRY == 31 ~ "Tunisia",
      COUNTRY == 32 ~ "Uganda",
      COUNTRY == 33 ~ "Zambia",
      COUNTRY == 34 ~ "Zimbabwe"
    ),
    year = year(ymd(DATEINTR)))

## Extent of corruption: president, judiciary, government

## r7: Q44A (office of the presidency, 0 none - 3 all of them), Q44C (government officials), Q44F (judges)
## r6: Q53a (presidency office, 0 none 3 all of them), Q53c (government officials), Q53G (judges and magistrates), 
# Q54 (level of corruption, 1 increased - 5 decreased)
## r5: Q60A (presidency office, 0 none of them - all of them), Q60C (government officials), Q60G (judges)
#no level of corruption question.
## r4: Q50A (presidency office, 0 none - 3 all of them), Q50D (government officials), Q50G (judges)
#no level of corruption question
## r3: Q56A (office of the presidency), Q56D (govt officials), Q56H (judges)
## r2: Q51A (presidency), Q51C (govt officials), Q51F (judges)
## r1: pfpcr2 (extent of corruption overall, 1 strongly disagree/almost all - 4 strongly agree/almost none)
# pfpcr5 (past vs. present corruption, 1 strongly disagree/much more - 4 strongly agree/much less)

afrobarometer_r1 %<>%
  mutate(
    corr_overall = ifelse(pfpcr2 %in% 1:4, pfpcr2, NA),
    corr_retro = ifelse(pfpcr5 %in% 1:4, pfpcr5, NA)) %>%
  mutate(
    corr_overall = ifelse(!is.na(corr_overall), 5 - corr_overall, NA),
    corr_retro = ifelse(!is.na(corr_retro), 5 - corr_retro, NA))

afrobarometer_r2 %<>%
  mutate(
    corr_pres = ifelse(q51a %in% 0:3, q51a, NA),
    corr_gov = ifelse(q51c %in% 0:3, q51c, NA),
    corr_judges = ifelse(q51f %in% 0:3, q51f, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA)
  )


afrobarometer_r3 %<>%
  mutate(
    corr_pres = ifelse(q56a %in% 0:3, q56a, NA),
    corr_gov = ifelse(q56d %in% 0:3, q56d, NA),
    corr_judges = ifelse(q56h %in% 0:3, q56h, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA)
  )


afrobarometer_r4 %<>%
  mutate(
    corr_pres = ifelse(Q50A %in% 0:3, Q50A, NA),
    corr_gov = ifelse(Q50D %in% 0:3, Q50D, NA),
    corr_judges = ifelse(Q50G %in% 0:3, Q50G, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA)
  )


afrobarometer_r5 %<>%
  mutate(
    corr_pres = ifelse(Q60A %in% 0:3, Q60A, NA),
    corr_gov = ifelse(Q60C %in% 0:3, Q60C, NA),
    corr_judges = ifelse(Q60G %in% 0:3, Q60G, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA)
  )


afrobarometer_r6 %<>%
  mutate(
    corr_pres = ifelse(Q53A %in% 0:3, Q53A, NA),
    corr_gov = ifelse(Q53C %in% 0:3, Q53C, NA),
    corr_judges = ifelse(Q53G %in% 0:3, Q53G, NA),
    corr_level = ifelse(Q54 %in% 1:5, Q54, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA),
    corr_level = ifelse(!is.na(corr_level), 6 - corr_level, NA)
  )


afrobarometer_r7 %<>%
  mutate(
    corr_pres = ifelse(Q44A %in% 0:3, Q44A, NA),
    corr_gov = ifelse(Q44C %in% 0:3, Q44C, NA),
    corr_judges = ifelse(Q44F %in% 0:3, Q44F, NA)
  ) %>%
  mutate(
    corr_pres = ifelse(!is.na(corr_pres), corr_pres + 1, NA),
    corr_gov = ifelse(!is.na(corr_gov), corr_gov + 1, NA),
    corr_judges = ifelse(!is.na(corr_judges), corr_judges + 1, NA)
  )


## Trust in government

## r7: Q43E (trust the ruling party, 0 not at all - 3 a lot),
## r6: Q52F (trust the ruling party, 0 not at all - 3 a lot)
## r5: Q59F (trust the ruling party, 0 not at all - 3 a lot)
## r4: Q49E (trust the ruling party, 0 not at all - 3 a lot)
## r3: Q55E (trust the ruling party, 0 not at all - 3 a lot)
## r2: Q43F (trust the ruling party, 0 not at all - 3 a lot)
## r1: trspre (trust the president, 1 do not trust them/never - 4 i trust them a lot/always)

afrobarometer_r1 %<>%
  mutate(
    trust_pres = ifelse(trspre %in% 1:4, trspre, NA))

afrobarometer_r2 %<>%
  mutate(
    trust_rul_part = ifelse(q43f %in% 0:3, q43f, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )

afrobarometer_r3 %<>%
  mutate(
    trust_rul_part = ifelse(q55e %in% 0:3, q55e, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )


afrobarometer_r4 %<>%
  mutate(
    trust_rul_part = ifelse(Q49E %in% 0:3, Q49E, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )


afrobarometer_r5 %<>%
  mutate(
    trust_rul_part = ifelse(Q59F %in% 0:3, Q59F, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )


afrobarometer_r6 %<>%
  mutate(
    trust_rul_part = ifelse(Q52F %in% 0:3, Q52F, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )


afrobarometer_r7 %<>%
  mutate(
    trust_rul_part = ifelse(Q43E %in% 0:3, Q43E, NA)
  ) %>%
  mutate(
    trust_rul_part = ifelse(!is.na(trust_rul_part), trust_rul_part + 1, NA)
  )


## Satisfaction with democracy

## r7: Q36 (0 country is not a democracy, 1 not at all satisfied - 4 very satisfied)
## r6: Q41 (0 country is not a democracy, 1 not at all satisfied - 4 very satisfied)
## r5: Q43 (0 country not a democracy, 1 not at all - 4 very satisfied)
## r4: Q43 (0 country not a democracy, 1 not at all - 4 very satisfied)
## r3: Q47 (0 country not a democracy, 1 not at all - 4 very satisfied)
## r2: Q40 (0 country not a democracy, 1 not at all - 4 very satisfied)
## r1: dmpsat (0 country is not a democracy, 1 - very dissatisfied - 5 very satisfied)

afrobarometer_r1 %<>%
  mutate(
    sat_dem = ifelse(dmpsat == 0, NA, ifelse(dmpsat %in% 1:5, dmpsat, NA)))

afrobarometer_r2 %<>%
  mutate(
    sat_dem = ifelse(q40 == 0, NA, ifelse(q40 %in% 1:4, q40, NA)))

afrobarometer_r3 %<>%
  mutate(
    sat_dem = ifelse(q47 == 0, NA, ifelse(q47 %in% 1:4, q47, NA)))

afrobarometer_r4 %<>%
  mutate(
    sat_dem = ifelse(Q43 == 0, NA, ifelse(Q43 %in% 1:4, Q43, NA)))

afrobarometer_r5 %<>%
  mutate(
    sat_dem = ifelse(Q43 == 0, NA, ifelse(Q43 %in% 1:4, Q43, NA)))

afrobarometer_r6 %<>%
  mutate(
    sat_dem = ifelse(Q41 == 0, NA, ifelse(Q41 %in% 1:4, Q41, NA)))

afrobarometer_r7 %<>%
  mutate(
    sat_dem = ifelse(Q36 == 0, NA, ifelse(Q36 %in% 1:4, Q36, NA)))


#- Put everything in the same dataset

afrobarometer_r1 %<>%
  select(country_name, year,corr_overall, corr_retro, trust_pres, sat_dem)

afrobarometer_r2 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, trust_rul_part, sat_dem)

# Round 3
afrobarometer_r3 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, trust_rul_part, sat_dem)

# Round 4
afrobarometer_r4 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, trust_rul_part, sat_dem)

# Round 5
afrobarometer_r5 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, trust_rul_part, sat_dem)

# Round 6
afrobarometer_r6 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, corr_level, trust_rul_part, sat_dem)

# Round 7
afrobarometer_r7 %<>%
  select(country_name, year, corr_pres, corr_gov, corr_judges, trust_rul_part, sat_dem)

# Combine all waves into one dataset
afrobarometer_all <- bind_rows(
  afrobarometer_r1,
  afrobarometer_r2,
  afrobarometer_r3,
  afrobarometer_r4,
  afrobarometer_r5,
  afrobarometer_r6,
  afrobarometer_r7)

# Arab Barometer

arab_barometer_w1 <- read_csv("00-data/surveys/arab-barometer/wave1.csv")
arab_barometer_w2 <- read_csv("00-data/surveys/arab-barometer/wave2.csv")
arab_barometer_w3 <- read_csv("00-data/surveys/arab-barometer/wave3.csv")
arab_barometer_w4 <- read_csv("00-data/surveys/arab-barometer/wave4.csv")
arab_barometer_w5 <- read_csv("00-data/surveys/arab-barometer/wave5.csv")
arab_barometer_w6_1 <- read_csv("00-data/surveys/arab-barometer/wave6_part1.csv")
arab_barometer_w6_2 <- read_csv("00-data/surveys/arab-barometer/wave6_part2.csv")
arab_barometer_w6_3 <- read_csv("00-data/surveys/arab-barometer/wave6_part3.csv")

# Combine all parts into one data frame
arab_barometer_w6 <- bind_rows(arab_barometer_w6_1, arab_barometer_w6_2, arab_barometer_w6_3)

rm(arab_barometer_w6_1)
rm(arab_barometer_w6_2)
rm(arab_barometer_w6_3)

#- Include country and year

arab_barometer_w1 %<>%
  mutate(
    country_name = case_when(
      country == "algeria" ~ "Algeria",
      country == "bahrain" ~ "Bahrain",
      country == "jordan" ~ "Jordan",
      country == "lebanon" ~ "Lebanon",
      country == "morocco" ~ "Morocco",
      country == "palestine" ~ "Palestine",
      country == "yemen" ~ "Yemen",
      TRUE ~ country  # Keep original value if not matched
    ),
    year = case_when(
      country == "algeria" ~ 2006,
      country == "bahrain" ~ 2009,
      country == "jordan" ~ 2006,
      country == "lebanon" ~ 2006,
      country == "morocco" ~ 2007,
      country == "palestine" ~ 2006,
      country == "yemen" ~ 2007,
      TRUE ~ NA_integer_  # Set to NA if not matched
    ))

arab_barometer_w2 %<>%
  mutate(
    country_name = case_when(
      country == "1. Algeria" ~ "Algeria",
      country == "5. Egypt" ~ "Egypt",
      country == "7. Iraq" ~ "Iraq",
      country == "8. Jordan" ~ "Jordan",
      country == "10. Lebanon" ~ "Lebanon",
      country == "15. Palestine" ~ "Palestine",
      country == "17. Saudi Arabia" ~ "Saudi Arabia",
      country == "19. Sudan" ~ "Sudan",
      country == "21. Tunisia" ~ "Tunisia",
      country == "22. Yemen" ~ "Yemen",
      TRUE ~ country  # Keep original value if not matched
    ),
    year = case_when(
      country == "1. Algeria" ~ 2011,
      country == "5. Egypt" ~ 2011,
      country == "7. Iraq" ~ 2011,
      country == "8. Jordan" ~ 2010,
      country == "10. Lebanon" ~ 2010,
      country == "15. Palestine" ~ 2010,
      country == "17. Saudi Arabia" ~ 2011,
      country == "19. Sudan" ~ 2010,
      country == "21. Tunisia" ~ 2011,
      country == "22. Yemen" ~ 2011,
      TRUE ~ NA_integer_  # Set to NA if not matched
    ))

arab_barometer_w3 %<>%
  mutate(
    country_name = country,  # Directly map country to country_name
    year = case_when(
      country == "Algeria" ~ 2013,
      country == "Egypt" ~ 2013,
      country == "Iraq" ~ 2013,
      country == "Jordan" ~ 2012,
      country == "Kuwait" ~ 2014,
      country == "Lebanon" ~ 2013,
      country == "Libya" ~ 2014,
      country == "Morocco" ~ 2013,
      country == "Palestine" ~ 2012,
      country == "Sudan" ~ 2013,
      country == "Tunisia" ~ 2013,
      country == "Yemen" ~ 2013,
      TRUE ~ NA_integer_  # Set to NA if not matched
    ))

arab_barometer_w4 %<>%
  mutate(
    country_name = country,  # Directly map country to country_name
    year = case_when(
      country == "Algeria" ~ 2016,
      country == "Egypt" ~ 2016,
      country == "Jordan" ~ 2016,
      country == "Lebanon" ~ 2016,
      country == "Morocco" ~ 2016,
      country == "Palestine" ~ 2016,
      country == "Tunisia" ~ 2016,
      TRUE ~ NA_integer_  # Set to NA if not matched
    ))

arab_barometer_w5 %<>%
  mutate(
    country_name = recode(country,
                          "1" = "Algeria",
                          "5" = "Egypt",
                          "7" = "Iraq",
                          "8" = "Jordan",
                          "9" = "Kuwait",
                          "10" = "Lebanon",
                          "11" = "Libya",
                          "13" = "Morocco",
                          "15" = "Palestine",
                          "19" = "Sudan",
                          "21" = "Tunisia",
                          "22" = "Yemen"),
    year = case_when(
      country_name == "Algeria" ~ 2019,
      country_name == "Egypt" ~ 2018,
      country_name == "Iraq" ~ 2019,
      country_name == "Jordan" ~ 2018,
      country_name == "Kuwait" ~ 2019,
      country_name == "Lebanon" ~ 2018,
      country_name == "Libya" ~ 2019,
      country_name == "Morocco" ~ 2018,
      country_name == "Palestine" ~ 2018,
      country_name == "Sudan" ~ 2018,
      country_name == "Tunisia" ~ 2018,
      country_name == "Yemen" ~ 2018,
      TRUE ~ NA_integer_  # Set to NA if not matched
    ))

arab_barometer_w6 %<>%
  mutate(
    country_name = recode(COUNTRY,
                          "1" = "Algeria",
                          "7" = "Iraq",
                          "8" = "Jordan",
                          "10" = "Lebanon",
                          "11" = "Libya",
                          "13" = "Morocco",
                          "21" = "Tunisia"),
    year = format(as.Date(DATE, format="%Y-%m-%d"), "%Y"))

#- Work with relevant questions

# Widespread corruption (corr_wide)
## w1: q253 (1 hardly anyone is involved - 4 almost everyone is corrupt)
table(arab_barometer_w1$q253)
## w5: Q211A (1 hardy anyone is involved - 4 almost everyone is corrupt)
table(arab_barometer_w5$Q211A)

arab_barometer_w1 %<>%
  mutate(corr_wide = case_when(
    q253 == "hardly anyone is involved in corruption and bribery" ~ 1,
    q253 == "not a lot of officials are corrupt" ~ 2,
    q253 == "most officials are corrupt" ~ 3,
    q253 == "almost everyone is corrupt" ~ 4,
    TRUE ~ NA_real_))

arab_barometer_w5 %<>%
  mutate(corr_wide = case_when(
    Q211A == 1 ~ 1,
    Q211A == 2 ~ 2,
    Q211A == 3 ~ 3,
    Q211A == 4 ~ 4,
    TRUE ~ NA_real_ ))

# Extent corruption (corr_ext)
## w4: q210 (1 not at all - 4 to a large extent)
## w5: Q210 (1 to a large extent - 4 not at all)
## w6: Q210 (1 to a large extent - 4 not at all)
table(arab_barometer_w4$q210)
table(arab_barometer_w5$Q210)
table(arab_barometer_w6$Q210)

arab_barometer_w4 %<>%
  mutate(corr_ext = case_when(
    q210 == "Not at all" ~ 1,
    q210 == "To a small extent" ~ 2,
    q210 == "To a medium extent" ~ 3,
    q210 == "To a large extent" ~ 4,
    TRUE ~ NA_real_ ))

arab_barometer_w5 %<>%
  mutate(
    corr_ext = ifelse(Q210 %in% 1:4, Q210, NA)) %>%
  mutate(
    corr_ext = ifelse(!is.na(corr_ext), 5 - corr_ext, NA))


arab_barometer_w6 %<>%
  mutate(
    corr_ext = ifelse(Q210 %in% 1:4, Q210, NA)) %>%
  mutate(
    corr_ext = ifelse(!is.na(corr_ext), 5 - corr_ext, NA))


# Trust

## w1: q2011 (trust in the prime minister, 1 a great deal of trust - 4 none at all), q2012 (trust in the courts)
## w2: q2011 (trust in the government, 1 trust it to a great extent - 4 absolutely do not trust it), q2012 (the judiciary)
## w3: q2011
## w4: q2011 (trust in government, 1 no trust at all - 4 a great deal of trust), q2012 (trust justice)
## w5: Q201A_1 (trust in government, 1 a great deal of trust - 4 no trust at all), Q201A_2 (trust courts)
## w6: Q201A_1 (trust in government 1 a great deal of trust - 4 no trust at all), Q201A_2 (trust in courts)

table(arab_barometer_w1$q2011)
table(arab_barometer_w2$q2011)
table(arab_barometer_w3$q2011)
table(arab_barometer_w4$q2011)
table(arab_barometer_w5$Q201A_1)
table(arab_barometer_w6$Q201A_1)

arab_barometer_w1 %<>%
  mutate(trust_govt = case_when(
    q2011 == "a great deal of trust" ~ 4,
    q2011 == "quite a lot of trust" ~ 3,
    q2011 == "not very much trust" ~ 2,
    q2011 == "none at all" ~ 1,
    TRUE ~ NA_real_))

arab_barometer_w2 %<>%
  mutate(trust_govt = case_when(
    q2011 == "1. i trust it to a great extent" ~ 4,
    q2011 == "2. i trust it to a medium extent" ~ 3,
    q2011 == "3. i trust it to a limited extent" ~ 2,
    q2011 == "4. i absolutely do not trust it" ~ 1,
    TRUE ~ NA_real_))

arab_barometer_w3 %<>%
  mutate(trust_govt = case_when(
    q2011 == "I trust it to a great extent" ~ 4,
    q2011 == "I trust it to a medium extent" ~ 3,
    q2011 == "I trust it to a limited extent" ~ 2,
    q2011 == "I absolutely do not trust it" ~ 1,
    TRUE ~ NA_real_ ))

arab_barometer_w4 %<>%
  mutate(trust_govt = case_when(
    q2011 == "A great deal of trust" ~ 4,
    q2011 == "Quite a lot of trust" ~ 3,
    q2011 == "Not very much trust" ~ 2,
    q2011 == "No trust at all" ~ 1,
    TRUE ~ NA_real_ ))

arab_barometer_w5 %<>%
  mutate(
    trust_govt = ifelse(Q201A_1 %in% 1:4, Q201A_1, NA)) %>%
  mutate(
    trust_govt = ifelse(!is.na(trust_govt), 5 - trust_govt, NA))

arab_barometer_w6 %<>%
  mutate(
    trust_govt = ifelse(Q201A_1 %in% 1:4, Q201A_1, NA)) %>%
  mutate(
    trust_govt = ifelse(!is.na(trust_govt), 5 - trust_govt, NA))

arab_barometer_w1 %<>%
  mutate(trust_jud = case_when(
    q2012 == "a great deal of trust" ~ 4,
    q2012 == "quite a lot of trust" ~ 3,
    q2012 == "not very much trust" ~ 2,
    q2012 == "none at all" ~ 1,
    TRUE ~ NA_real_))

arab_barometer_w2 %<>%
  mutate(trust_jud = case_when(
    q2012 == "1. i trust it to a great extent" ~ 4,
    q2012 == "2. i trust it to a medium extent" ~ 3,
    q2012 == "3. i trust it to a limited extent" ~ 2,
    q2012 == "4. i absolutely do not trust it" ~ 1,
    TRUE ~ NA_real_))

arab_barometer_w4 %<>%
  mutate(trust_jud = case_when(
    q2012 == "A great deal of trust" ~ 4,
    q2012 == "Quite a lot of trust" ~ 3,
    q2012 == "Not very much trust" ~ 2,
    q2012 == "No trust at all" ~ 1,
    TRUE ~ NA_real_ ))

arab_barometer_w5 %<>%
  mutate(
    trust_jud = ifelse(Q201A_2 %in% 1:4, Q201A_2, NA)) %>%
  mutate(
    trust_jud = ifelse(!is.na(trust_jud), 5 - trust_jud, NA))


arab_barometer_w6 %<>%
  mutate(
    trust_jud = ifelse(Q201A_2 %in% 1:4, Q201A_2, NA)) %>%
  mutate(
    trust_jud = ifelse(!is.na(trust_jud), 5 - trust_jud, NA))


# Satisfaction government (sat_govt)
## w1: q244 (1 very unsatisfied - 10 very satisfied)
## w2: q513 (1 absolutely unsatisfied - 10 very satisfied)
## w3: q211 (1 absolutely unsatisfied - 10 very satisfied)
## w4: q513 (1 completely unsatisfied - 10 completely satisfied)
## w5: Q513 (0 completely dissatisfied - 10 completely satisfied)
## w6: Q204A (1 completely satisfied - 4 completely dissatisfied)

table(arab_barometer_w1$q244)
table(arab_barometer_w2$q513)
table(arab_barometer_w3$q513)
table(arab_barometer_w4$q513)
table(arab_barometer_w5$Q513)
table(arab_barometer_w6$Q204A_3)

arab_barometer_w1 %<>%
  mutate(sat_govt = case_when(
    q244 == "completely unsatisfied" ~ 1,
    q244 == "2" ~ 2,
    q244 == "3" ~ 3,
    q244 == "4" ~ 4,
    q244 == "5" ~ 5,
    q244 == "6" ~ 6,
    q244 == "7" ~ 7,
    q244 == "8" ~ 8,
    q244 == "9" ~ 9,
    q244 == "completely satisfied" ~ 10,
    TRUE ~ NA_real_
  ))

arab_barometer_w2 %<>%
  mutate(sat_govt = case_when(
    q513 == "1. absolutely unsatisfied" ~ 1,
    q513 == "2" ~ 2,
    q513 == "3" ~ 3,
    q513 == "4" ~ 4,
    q513 == "5" ~ 5,
    q513 == "6" ~ 6,
    q513 == "7" ~ 7,
    q513 == "8" ~ 8,
    q513 == "9" ~ 9,
    q513 == "10. very satisfied" ~ 10,
    TRUE ~ NA_real_
  ))

arab_barometer_w3 %<>%
  mutate(sat_govt = case_when(
    q513 == "Completely unsatisfied" ~ 1,
    q513 == "1" ~ 2,
    q513 == "2" ~ 3,
    q513 == "3" ~ 4,
    q513 == "4" ~ 5,
    q513 == "5" ~ 6,
    q513 == "6" ~ 7,
    q513 == "7" ~ 8,
    q513 == "8" ~ 9,
    q513 == "9" ~ 10,
    q513 == "Completely satisfied" ~ 10,
    TRUE ~ NA_real_
  ))

arab_barometer_w4 %<>%
  mutate(sat_govt = case_when(
    q513 == "Completely unsatisfied" ~ 1,
    q513 == "1" ~ 2,
    q513 == "2" ~ 3,
    q513 == "3" ~ 4,
    q513 == "4" ~ 5,
    q513 == "5" ~ 6,
    q513 == "6" ~ 7,
    q513 == "7" ~ 8,
    q513 == "8" ~ 9,
    q513 == "9" ~ 10,
    q513 == "Completely satisfied" ~ 10,
    TRUE ~ NA_real_
  ))

arab_barometer_w5 %<>%
  mutate(sat_govt = case_when(
    Q513 == 0 ~ 1,
    Q513 == 1 ~ 2,
    Q513 == 2 ~ 3,
    Q513 == 3 ~ 4,
    Q513 == 4 ~ 5,
    Q513 == 5 ~ 6,
    Q513 == 6 ~ 7,
    Q513 == 7 ~ 8,
    Q513 == 8 ~ 9,
    Q513 == 9 ~ 10,
    Q513 == 10 ~ 10,
    TRUE ~ NA_real_
  ))

arab_barometer_w6 %<>%
  mutate(
    sat_govt = ifelse(Q204A_3 %in% 1:4, Q204A_3, NA)) %>%
  mutate(
    sat_govt = ifelse(!is.na(sat_govt), 5 - sat_govt, NA))


#- Put everything in the same dataset

arab_barometer_w1 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_govt)

arab_barometer_w2 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, trust_govt, sat_govt)

arab_barometer_w3 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, trust_govt, sat_govt)

arab_barometer_w4 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, corr_ext, trust_govt, trust_jud, sat_govt)

arab_barometer_w5 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, corr_wide, corr_ext, trust_govt, trust_jud, sat_govt)

arab_barometer_w6 %<>%
  mutate(year = as.character(year)) %>%
  select(country_name, year, corr_ext, trust_govt, trust_jud, sat_govt)

# Combine all waves into one dataset
arab_barometer_all <- bind_rows(
  arab_barometer_w1,
  arab_barometer_w2,
  arab_barometer_w3,
  arab_barometer_w4,
  arab_barometer_w5,
  arab_barometer_w6
)

# Asiabarometer

asiabarometer_w1 <- read_sav("00-data/surveys/asian-barometer/WaveI_2001-2003.sav")
asiabarometer_w2 <- read_sav("00-data/surveys/asian-barometer/WaveII_2005-2008.sav")
asiabarometer_w3 <- read_sav("00-data/surveys/asian-barometer/WaveIII_2010-2012.sav")
asiabarometer_w4 <- read_sav("00-data/surveys/asian-barometer/WaveIV_merge.sav")
asiabarometer_w5 <- read_sav("00-data/surveys/asian-barometer/WaveV_merge_15.sav")

#- Country and year

## w1

asiabarometer_w1 %<>%
  mutate(
    country = as.character(country),
    country_name = recode(country,
                          "1" = "Japan",
                          "2" = "Hong Kong",
                          "3" = "Taiwan",
                          "4" = "South Korea",
                          "5" = "Thailand",
                          "6" = "Philippines",
                          "7" = "China",
                          "8" = "Indonesia"),
    year = yrsurvey)

asiabarometer_w2 %<>%
  mutate(
    country = as.character(country),
    country_name = recode(country,
                          "1" = "Japan",
                          "2" = "Hong Kong",
                          "3" = "South Korea",
                          "4" = "China",
                          "5" = "Mongolia",
                          "6" = "Philippines",
                          "7" = "Taiwan",
                          "8" = "Thailand",
                          "9" = "Indonesia",
                          "10" = "Singapore",
                          "11" = "Vietnam",
                          "12" = "Malaysia",
                          "13" = "Cambodia"),
    year = ir9_3)


asiabarometer_w3 %<>%
  mutate(
    country = as.character(country),
    country_name = recode(country,
                          "1" = "Japan",
                          "2" = "Hong Kong",
                          "3" = "Taiwan",
                          "4" = "South Korea",
                          "5" = "Thailand",
                          "6" = "Philippines",
                          "7" = "China",
                          "8" = "Mongolia",
                          "9" = "Indonesia",
                          "10" = "Singapore",
                          "11" = "Vietnam",
                          "12" = "Cambodia",
                          "13" = "Malaysia"),
    year = format(as.Date(ir9, format="%Y-%m-%d"), "%Y"))

asiabarometer_w4 %<>%
  mutate(
    country = as.character(country),
    country_name = recode(country,
                          "1" = "Japan",
                          "2" = "Hong Kong",
                          "3" = "South Korea",
                          "4" = "China",
                          "5" = "Mongolia",
                          "6" = "Philippines",
                          "7" = "Taiwan",
                          "8" = "Thailand",
                          "9" = "Indonesia",
                          "10" = "Singapore",
                          "11" = "Vietnam",
                          "12" = "Cambodia",
                          "13" = "Malaysia",
                          "14" = "Myanmar"))

asiabarometer_w5 %<>%
  mutate(
    country = as.character(COUNTRY),
    country_name = recode(country,
                          "1" = "Japan",
                          "2" = "Hong Kong",
                          "3" = "South Korea",
                          "4" = "China",
                          "5" = "Mongolia",
                          "6" = "Philippines",
                          "7" = "Taiwan",
                          "8" = "Thailand",
                          "9" = "Indonesia",
                          "10" = "Singapore",
                          "11" = "Vietnam",
                          "12" = "Cambodia",
                          "13" = "Malaysia",
                          "14" = "Myanmar",
                          "15" = "Australia",
                          "18" = "India"),
    year = Year)

#- Relevant variables


# Widespread corruption corr_wide
## w1: q115 (1 hardy anyone is involved - 4 almost everyone is corrupt)

asiabarometer_w1 %<>%
  mutate(corr_wide = case_when(
    q115 %in% 1:4 ~ q115,
    TRUE ~ NA_real_ ))

## w2: q118 (1 almost everyone is  corrupt - 4 hardly anyone is involved)

asiabarometer_w2 %<>%
  mutate(
    q118_clean = ifelse(q118 %in% 1:4, q118, NA),  # Clean the variable first
    corr_wide = case_when(
      !is.na(q118_clean) ~ 5 - q118_clean,  # Reverse the scale for valid values
      TRUE ~ NA_real_  # Set all other responses to NA
    )
  ) %>%
  select(-q118_clean)  # Remove the intermediate cleaned variable

## w3: q117 (1 hardly anyone is involved - 4 almost everyone is corrupt)

asiabarometer_w3 %<>%
  mutate(corr_wide = case_when(
    q117 %in% 1:4 ~ q117,
    TRUE ~ NA_real_ ))


## w4: q118 (1 hardy anyone is involved - 4 almost everyone is corrupt)

asiabarometer_w4 %<>%
  mutate(corr_wide = case_when(
    q118 %in% 1:4 ~ q118,
    TRUE ~ NA_real_ ))

## w5: q125 (1 hardy anyone is involved - 4 almost everyone is corrupt)

asiabarometer_w5 %<>%
  mutate(corr_wide = case_when(
    q125 %in% 1:4 ~ q125,
    TRUE ~ NA_real_))


# Trust (trust_govt and trust_jud)
## w1 q7 the courts (1 great deal of trust - 4 not at all), q8 the national government.

asiabarometer_w1 %<>%
  mutate(
    trust_jud = ifelse(q007 %in% 1:4, q007, NA),
    trust_govt = ifelse(q008 %in% 1:4, q008, NA)) %>%
  mutate(
    trust_jud = ifelse(!is.na(trust_jud), 5 - trust_jud, NA),
    trust_govt = ifelse(!is.na(trust_govt), 5 - trust_govt, NA))


## w2 q7 the presidency or prime minister (1 none at all - 4 a great deal of trust), q8 the courts
asiabarometer_w2 %<>%
  mutate(
    trust_govt = ifelse(q7 %in% 1:4, q7, NA),
    trust_jud = ifelse(q8 %in% 1:4, q8, NA))

## w3: q7 the president (1 a great deal - 4 none at all), q8 the courts

asiabarometer_w3 %<>%
  mutate(
    trust_govt = ifelse(q7 %in% 1:4, q7, NA),
    trust_jud = ifelse(q8 %in% 1:4, q8, NA)) %>%
  mutate(
    trust_govt = ifelse(!is.na(trust_govt), 5 - trust_govt, NA),
    trust_jud = ifelse(!is.na(trust_jud), 5 - trust_jud, NA))


## w4: q7 the president (1 a great deal - 4 none at all), q8 the courts

asiabarometer_w4 %<>%
  mutate(
    trust_govt = ifelse(q7 %in% 1:4, q7, NA),
    trust_jud = ifelse(q8 %in% 1:4, q8, NA)) %>%
  mutate(
    trust_govt = ifelse(!is.na(trust_govt), 5 - trust_govt, NA),
    trust_jud = ifelse(!is.na(trust_jud), 5 - trust_jud, NA))


## w5: q7 (1 fully trust - 6 fully distrust), q8 courts

asiabarometer_w5 %<>%
  mutate(
    q7_clean = ifelse(q7 %in% 1:6, q7, NA),
    q8_clean = ifelse(q8 %in% 1:6, q8, NA)
  ) %>%
  mutate(
    trust_govt = case_when(
      !is.na(q7_clean) ~ case_when(
        q7_clean == 1 ~ 4,
        q7_clean == 2 ~ 3,
        q7_clean == 3 ~ 2,
        q7_clean == 4 ~ 1,
        q7_clean == 5 ~ 1,
        q7_clean == 6 ~ 1,
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    ),
    trust_jud = case_when(
      !is.na(q8_clean) ~ case_when(
        q8_clean == 1 ~ 4,
        q8_clean == 2 ~ 3,
        q8_clean == 3 ~ 2,
        q8_clean == 4 ~ 1,
        q8_clean == 5 ~ 1,
        q8_clean == 6 ~ 1,
        TRUE ~ NA_real_
      ),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-q7_clean, -q8_clean)



# Satisfaction with democracy (sat_dem)
## w1: q98 (1 very satisfied - 4 not at all satisfied)

asiabarometer_w1 %<>%
  mutate(
    sat_dem = ifelse(q098 %in% 1:4, q098, NA)) %>%
  mutate(
    sat_dem = ifelse(!is.na(sat_dem), 5 - sat_dem, NA))


## w2: q93 (1 not at all satisfied - 4 very satisfied)

asiabarometer_w2 %<>%
  mutate(
    sat_dem = ifelse(q93 %in% 1:4, q93, NA))

## w3: q89 (1 very satisfied - 4 not at all satisfied)

asiabarometer_w3 %<>%
  mutate(
    sat_dem = ifelse(q89 %in% 1:4, q89, NA)) %>%
  mutate(
    sat_dem = ifelse(!is.na(sat_dem), 5 - sat_dem, NA))

## w4: q98 (1 very satisfied - 4 not at all satisfied)

asiabarometer_w4 %<>%
  mutate(
    sat_dem = ifelse(q98 %in% 1:4, q98, NA)) %>%
  mutate(
    sat_dem = ifelse(!is.na(sat_dem), 5 - sat_dem, NA))


## w5: q99 (1 very satisfied - 4 not at all satisfied)

asiabarometer_w5 %<>%
  mutate(
    sat_dem = ifelse(q99 %in% 1:4, q99, NA)) %>%
  mutate(
    sat_dem = ifelse(!is.na(sat_dem), 5 - sat_dem, NA))


#- Put everything in the same dataset

asiabarometer_w1 %<>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    corr_wide = as.numeric(corr_wide),
    trust_govt = as.numeric(trust_govt),
    trust_jud = as.numeric(trust_jud),
    sat_dem = as.numeric(sat_dem)
  ) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_dem)

asiabarometer_w2 %<>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    corr_wide = as.numeric(corr_wide),
    trust_govt = as.numeric(trust_govt),
    trust_jud = as.numeric(trust_jud),
    sat_dem = as.numeric(sat_dem)
  ) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_dem)

asiabarometer_w3 %<>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    corr_wide = as.numeric(corr_wide),
    trust_govt = as.numeric(trust_govt),
    trust_jud = as.numeric(trust_jud),
    sat_dem = as.numeric(sat_dem)
  ) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_dem)

asiabarometer_w4 %<>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    corr_wide = as.numeric(corr_wide),
    trust_govt = as.numeric(trust_govt),
    trust_jud = as.numeric(trust_jud),
    sat_dem = as.numeric(sat_dem)
  ) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_dem)

asiabarometer_w5 %<>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    corr_wide = as.numeric(corr_wide),
    trust_govt = as.numeric(trust_govt),
    trust_jud = as.numeric(trust_jud),
    sat_dem = as.numeric(sat_dem)
  ) %>%
  select(country_name, year, corr_wide, trust_govt, trust_jud, sat_dem)

asiabarometer_all <- bind_rows(
  asiabarometer_w1,
  asiabarometer_w2,
  asiabarometer_w3,
  asiabarometer_w4,
  asiabarometer_w5)

# LAPOP

lapop <- read_dta("00-data/surveys/lapop/lapop.dta")


#- Recode countries

lapop %<>%
  mutate(
    pais = as.character(pais),
    country_name = recode(pais,
                          "1" = "Mexico",
                          "2" = "Guatemala",
                          "3" = "El Salvador",
                          "4" = "Honduras",
                          "5" = "Nicaragua",
                          "6" = "Costa Rica",
                          "7" = "Panama",
                          "8" = "Colombia",
                          "9" = "Ecuador",
                          "10" = "Bolivia",
                          "11" = "Peru",
                          "12" = "Paraguay",
                          "13" = "Chile",
                          "14" = "Uruguay",
                          "15" = "Brazil",
                          "16" = "Venezuela",
                          "17" = "Argentina",
                          "21" = "Dominican Republic",
                          "22" = "Haiti",
                          "23" = "Jamaica",
                          "24" = "Guyana",
                          "25" = "Trinidad and Tobago",
                          "26" = "Belize",
                          "27" = "Surinam",
                          "28" = "Bahamas",
                          "29" = "Barbados",
                          "40" = "USA",
                          "41" = "Canada"
    )
  )

#- Relevant variables

# Trust
## b14 trust in government trust_gov (1 nada - 7 mucho), b10a trust_jud 

# Extent corruption (corr_ext)
## exc7new (1 none - 5 all)

# satisfaction with democracy (sat_dem)
## pn4 (1 muy satisfecho - 4 muy insatisfecho)

lapop %<>%
  mutate(
    trust_govt = ifelse(b14 %in% 1:7, b14, NA),
    trust_jud = ifelse(b10a %in% 1:7, b10a, NA),
    corr_ext = ifelse(exc7new %in% 1:5, exc7new, NA),
    sat_dem = ifelse(pn4 %in% 1:4, pn4, NA)) %>%
  mutate(
    sat_dem = ifelse(!is.na(sat_dem), 5 - sat_dem, NA)) %>% # reverse satisfaction with democracy
  select(country_name, year, trust_govt, trust_jud, corr_ext, sat_dem)


# WVS 

load("00-data/surveys/wvs/wvs_ts.rdata")

wvs_full <- WVS_TimeSeries_1981_2022_spss_v3_0
rm(WVS_TimeSeries_1981_2022_spss_v3_0)


#- Create country

wvs_full$country_name <- countrycode(wvs_full$COUNTRY_ALPHA, origin = "iso3c",
                                destination = "country.name", warn = TRUE)

wvs_full$year <- wvs_full$S020

#- Specific variables

# Extent of political corruption (corr_extent)
## E196 (1  Almost no public officials engaged in it - 4  Almost all public officials are engaged in it)

# Scale of corruption (corr_scale)
## E268 (1 There is no corruption in [my country] - 10  There is abundant corruption in [my country])

# Trust government (trust_gov)
## E069_11 (1  A great deal - 4  None at all)

# Trust judicial system (trust_jud)
## E069_17 (1  A great deal - 4  None at all)

# Satisfaction with democracy
## E110 (1  Very satisfied - 4  Not at all satisfied)

wvs_full %<>%
  mutate(
    corr_extent = ifelse(E196 %in% 1:4, E196, NA),
    corr_scale = ifelse(E268 %in% 1:10, E268, NA),
    trust_gov_clean = ifelse(E069_11 %in% 1:4, E069_11, NA),
    trust_jud_clean = ifelse(E069_17 %in% 1:4, E069_17, NA),
    sat_dem_clean = ifelse(E110 %in% 1:4, E110, NA),
    trust_govt = ifelse(!is.na(trust_gov_clean), 5 - trust_gov_clean, NA),
    trust_jud = ifelse(!is.na(trust_jud_clean), 5 - trust_jud_clean, NA),
    sat_dem = ifelse(!is.na(sat_dem_clean), 5 - sat_dem_clean, NA)
  ) %>%
  select(country_name, year, corr_extent, corr_scale, trust_govt, trust_jud, sat_dem)

# Select only relevant countries

# databases: `afrobarometer_all`, `arab_barometer_all`, `asiabarometer_all`,
# `lapop`, `wvs_full`

# countries from `tj_countries_full`

names(tj_countries_full)

#- Add country codes

tj_countries_full %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

tj_countries_full %>% filter(is.na(country_cod)) #no NAs

# Afrobarometer
afrobarometer_all %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

# Check for NAs in country_cod
afrobarometer_all %>% filter(is.na(country_cod))


# Arab Barometer
arab_barometer_all %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

# Check for NAs in country_cod
arab_barometer_all %>% filter(is.na(country_cod))

# Asia Barometer
asiabarometer_all %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

# Check for NAs in country_cod
asiabarometer_all %>% filter(is.na(country_cod))

# LAPOP
lapop %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

# Check for NAs in country_cod
lapop %>% filter(is.na(country_cod))

# WVS
wvs_full %<>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  select(country_name, country_cod, everything())

# Check for NAs in country_cod
wvs_full %>% filter(is.na(country_cod))

#- Filter only the countries that are in tj_countries_full 

# Extract the list of country codes
valid_country_codes <- unique(tj_countries_full$country_cod)

afrobarometer_filtered <- afrobarometer_all %>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  filter(country_cod %in% valid_country_codes)

arab_barometer_filtered <- arab_barometer_all %>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  filter(country_cod %in% valid_country_codes)

asiabarometer_filtered <- asiabarometer_all %>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  filter(country_cod %in% valid_country_codes)

lapop_filtered <- lapop %>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  filter(country_cod %in% valid_country_codes)

wvs_filtered <- wvs_full %>%
  mutate(country_cod = countrycode(country_name, 'country.name', 'iso3c')) %>%
  filter(country_cod %in% valid_country_codes)
