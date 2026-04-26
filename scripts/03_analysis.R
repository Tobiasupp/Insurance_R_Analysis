#Vilka faktorer verkar hänga ihop med försäkringskostnad

# Räkna ut korrelation för numeriska variabler
cor_summary <- insurance %>%
  select(age, bmi, children, annual_checkups, health_score, charges) %>%
  cor(use = "complete.obs")


#Delar upp i Bmi klasser samt själva klasserna i rökare och icke rökare för att se olika kostnader per kategori
category_summary <- insurance %>%
  group_by(smoker, bmi_class) %>%
  summarise(
    count = n(),
    avg_charge = mean(charges),
    median_charge = median(charges),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_charge))

print(category_summary)

#Ger en liten insyn men inget att tänka för mycket på då datan är så skev med att det bara är 92 som inte hade checkup alls och över 1000 på andra.
#Så att det är högre kostnad för dom som kollat upp sig kan vara missledande.
checkup_analysis <- insurance %>%
  mutate(checkup_group = ifelse(annual_checkups > 0, "Regular", "None")) %>%
  group_by(checkup_group) %>%
  summarise(
    avg_charge = mean(charges),
    median_bmi = median(bmi),
    count = n(),
    .groups = "drop"
  )

#Delar upp åldersgrupper i 3 olika kategorier och sedan dessa kategorier i rökare och icke rökare för att se ifall det är skillnad i kostnad
age_smoker_summary <- insurance %>%
  mutate(age_group = case_when(
    age < 30 ~ "Young (<30)",
    age < 50 ~ "Middle (30-50)",
    TRUE ~ "Senior (50+)"
  )) %>%
  group_by(age_group, smoker) %>%
  summarise(avg_charge = mean(charges), .groups = "drop")
