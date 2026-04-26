glimpse(insurance_raw)
summary(insurance_raw)
colSums(is.na(insurance_raw))

insurance_raw %>% 
  count(plan_type)
insurance_raw %>% 
  count(region)
insurance_raw %>% 
  count(sex)


insurance <- insurance_raw %>% 
  mutate(
    region            = str_to_title(str_trim(as.character(region))),
    smoker            = str_to_title(str_trim(as.character(smoker))),
    chronic_condition = str_to_title(str_trim(as.character(chronic_condition))),
    exercise_level    = str_to_title(str_trim(as.character(exercise_level))),
    plan_type         = str_to_title(str_trim(as.character(plan_type))),
    sex               = str_to_title(str_trim(as.character(sex)))
  ) %>% 
  mutate(
    region            = as.factor(region),
    smoker            = as.factor(smoker),
    chronic_condition = as.factor(chronic_condition),
    exercise_level    = as.factor(exercise_level),
    plan_type         = as.factor(plan_type),
    sex               = as.factor(sex)
  )

levels(insurance$smoker)

insurance <- insurance %>% 
  mutate(
    exercise_level = replace_na(as.character(exercise_level), "Unknown"),
    exercise_level = as.factor(exercise_level),
    annual_checkups = coalesce(annual_checkups, median(annual_checkups, na.rm = TRUE)),
    bmi = coalesce(bmi, median(bmi, na.rm = TRUE))
  )

#Gjorde om BMI till ett medianvärde där det tidigare va NA. Median är ett bättre standardvärde då det undviker att störas av utstickare
#Annual checkups ändrades till median också då vi inte kan veta ifall dom glömt att skriva in eller bara lämnat blankt.
#Exercise level får stå som Unknown istället för NA

colSums(is.na(insurance))

insurance <- insurance %>% 
  mutate(
    bmi_class = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25   ~ "Normal",
      bmi < 30   ~ "Overweight",
      TRUE       ~ "Obese"
    ) %>% as.factor() )

#Enkelt sätt att klassificera de olika BMI resultaten i datan. Vilket gör det enklare att snabbt och förståeligt använda BMI datan


insurance <- insurance %>% 
  mutate(
    health_score = (
    case_when(smoker == "Yes" ~ -2, TRUE ~ 0) +
      
      # Träningsnivå (Antar att nivåerna heter t.ex. High, Medium, Low)
      case_when(
        exercise_level == "High"   ~ 2,
        exercise_level == "Medium" ~ 1,
        TRUE                       ~ 0
      ) +
      
      case_when(
        bmi_class == "Underweight" ~ 0,
        bmi_class == "Normal" ~ 1,
        bmi_class == "Overweight" ~ -1,
        bmi_class == "Obese" ~ -2,
        TRUE ~ 0
      )+
      
      # Årliga kontroller är positivt
      case_when(annual_checkups >= 1 ~ 1, TRUE ~ 0) +
      
      # Kroniska tillstånd ger minuspoäng
      case_when(chronic_condition == "Yes" ~ -1, TRUE ~ 0)
  ))
# health_score kan hjälpa att avgöra för bolaget vilken typ av försäkring man ska föreslå till kunden, till vilket pris. 
#Exempelvis så kanske man bara föreslår/erbjuder en kund med hög poäng standard paketet då det är en låg sannolikhet att kunden behöver använda sig av försäkring.
# Men att en kund med låg poäng blir erbjuden/föreslagen ett mer permium paket då risken är högre och därför behöver mer täckning.

