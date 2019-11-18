rm(list = ls())

library(tidyverse)
library(readr)

# Ruwe data van Zorgmail
# bronmap <- "//hskfs01/Data/Hoofdkantoor/ICT/Informatie analyse/Aantekeningen Niels/Zorgmail/"
bronmap <- "/home/niels/workspace/R/Zorgmail/Update november 2019/"
bestand <- "addressbook_zorgmail_a15_se.csv"
bestand_edi <- "addressbook_zorgmail_a13_edi_huisartsen.csv"
bestand_crm <- "20191030 Actieve Huisartsen CRM.csv"


# crm inlezen
contacts_crm <- read_csv2(file = paste0(bronmap, bestand_crm))


# Lijst zorgmail inlezen
adressen <- read_csv(file = paste0(bronmap, bestand), 
                     cols(.default = col_character(),id = col_double(),   organizationId = col_double())
                     , col_names = TRUE)

# Lijst met edifact adressen huisartsen
adressen_edi <- read_csv(file = paste0(bronmap, bestand_edi), cols(
  .default = col_character(),
  id = col_double(),
  organizationId = col_double(),
  transactionId = col_date(format = ""),
  created = col_date(format = ""),
  lastModified = col_date(format = ""),
  endDate = col_logical(),
  department = col_character(),
  telephoneNumber = col_character()
), col_names = TRUE)

# namen aanpassen
adressen <- adressen %>% select(id, 'zorgmail_mailadres' = mailAddress, 'zorgmail_gewijzigd_op' = lastModified
                                ,'zorgmail_rol'= role, 'zorgmail_agb_code' = zvAgbCode, 'zorgmail_geslacht' =  gender
                                , 'zorgmail_achternaam' = surname, 'zorgmail_initialen' = initials, 'zorgmail_voorvoegsel' = prefix
                                , 'zorgmail_agb_code_praktijk' = prAgbCode, 'zorgmail_organisatie_type' = organizationType 
                                , 'zorgmail_organisatie_naam' = organization, 'zorgmail_organisatie_id' = organizationId 
                                , 'zorgmail_adres_straat' = street, 'zorgmail_adres_nummer' = houseNumber, 'zorgmail_adres_postcode'  = postalCode
                                , 'zorgmail_adres_plaats' = locality)


# namen aanpassen
adressen_edi <- adressen_edi %>% select(id, 'edifact_mailadres' = mailAddress, 'edifact_gewijzigd_op' = lastModified
                                ,'edifact_rol'= role, 'edifact_agb_code' = zvAgbCode, 'edifact_geslacht' =  gender
                                , 'edifact_achternaam' = surname, 'edifact_initialen' = initials, 'edifact_voorvoegsel' = prefix
                                , 'edifact_agb_code_praktijk' = prAgbCode, 'edifact_organisatie_type' = organizationType 
                                , 'edifact_organisatie_naam' = organization, 'edifact_organisatie_id' = organizationId 
                                , 'edifact_adres_straat' = street, 'edifact_adres_nummer' = houseNumber, 'edifact_adres_postcode'  = postalCode
                                , 'edifact_adres_plaats' = locality)



# Unieke lijst id's van huisartsen met een praktijk id
huisartsen <- bind_rows(
  (adressen %>% filter(zorgmail_rol == 'Huisarts'  & !is.na(zorgmail_agb_code)) %>% select(id, 'organisatie_id' = zorgmail_organisatie_id, 'agbcode' = zorgmail_agb_code) %>% mutate(bron = 'zorgmail')), 
  (adressen_edi %>% filter(edifact_rol == 'Huisarts' & !is.na(edifact_agb_code)) %>% select(id, 'organisatie_id' = edifact_organisatie_id, 'agbcode' = edifact_agb_code) %>% mutate(bron = 'edifact'))
  ) 

# zorgmail en edifact email erbij
huisartsen <- huisartsen %>% 
  left_join((adressen %>% 
               filter(zorgmail_rol == 'Huisarts')) 
             , by = 'id') %>% 
  left_join((adressen_edi %>% 
               filter(edifact_rol == 'Huisarts')) 
            , by = 'id')

# Huisarts Praktijken met een praktijkcode selecteren 
huisartspraktijken <- adressen %>% 
  filter(zorgmail_rol == 'Huisartsenpraktijk' | zorgmail_rol == 'Gezondheidscentrum') %>% 
  select('organisatie_id' = id, 'zorgmail_praktijk_mailadres' = zorgmail_mailadres, 'zorgmail_praktijk_naam' = zorgmail_organisatie_naam,
         "zorgmail_praktijk_adres_straat" = zorgmail_adres_straat, "zorgmail_praktijk_adres_plaats" = zorgmail_adres_plaats, 
         "zorgmail_praktijk_adres_huisnummer" = zorgmail_adres_nummer, "zorgmail_praktijk_adres_postcode" = zorgmail_adres_postcode)

# Huisarts Praktijken met een praktijkcode selecteren 
huisartspraktijken_edi <- adressen_edi %>% 
  filter(edifact_rol == 'Huisartsenpraktijk' | edifact_rol == 'Gezondheidscentrum') %>% 
  select('organisatie_id' = id, 'edifact_praktijk_mailadres' = edifact_mailadres, 'edifact_praktijk_naam' = edifact_organisatie_naam, 
         "edifact_praktijk_adres_straat" = edifact_adres_straat, "edifact_praktijk_adres_plaats" = edifact_adres_plaats, 
         "edifact_praktijk_adres_huisnummer" = edifact_adres_nummer, "edifact_praktijk_adres_postcode" = edifact_adres_postcode)



huisartsen <- huisartsen %>% 
  left_join(huisartspraktijken, by = 'organisatie_id') %>% 
  left_join(huisartspraktijken_edi, by = 'organisatie_id')

# Mailadres bepalen: 
huisartsen <- huisartsen %>% mutate(
  mailadres = if_else(!is.na(zorgmail_mailadres), zorgmail_mailadres, 
                      ifelse(!is.na(zorgmail_praktijk_mailadres), zorgmail_praktijk_mailadres, 
                             ifelse(!is.na(edifact_mailadres), edifact_mailadres, 
                                    ifelse(!is.na(edifact_praktijk_mailadres), edifact_praktijk_mailadres, NA))))
  ,bron_mail = if_else(!is.na(zorgmail_mailadres), "persoonlijk", 
                      ifelse(!is.na(zorgmail_praktijk_mailadres), "praktijk_zorgmail", 
                             ifelse(!is.na(edifact_mailadres), "persoonlijk", 
                                    ifelse(!is.na(edifact_praktijk_mailadres), "praktijk_edifact", NA))))
  ,achternaam = ifelse(bron == 'edifact', edifact_achternaam, zorgmail_achternaam)
  ,initialen = ifelse(bron == 'edifact', edifact_initialen, zorgmail_initialen)
  ,tussenvoegsels = ifelse(bron == 'edifact', edifact_voorvoegsel, zorgmail_voorvoegsel)
  ,plaats = ifelse(bron == 'edifact', edifact_adres_plaats, zorgmail_adres_plaats)
  ,postcode = ifelse(bron == 'edifact', edifact_adres_postcode, zorgmail_adres_postcode)
  ,straat = ifelse(bron == 'edifact', edifact_adres_straat, zorgmail_adres_straat)
  ,huisnummer = ifelse(bron == 'edifact', edifact_adres_nummer, zorgmail_adres_nummer)
)

huisartsen <- huisartsen %>% 
  mutate(plaats = ifelse(bron_mail == "praktijk_zorgmail", zorgmail_praktijk_adres_plaats, plaats), 
         postcode = ifelse(bron_mail == "praktijk_zorgmail", zorgmail_praktijk_adres_postcode, postcode),
         straat = ifelse(bron_mail == "praktijk_zorgmail", zorgmail_praktijk_adres_straat , straat),
         huisnummer = ifelse(bron_mail == "praktijk_zorgmail", zorgmail_praktijk_adres_huisnummer, huisnummer))


huisartsen_uniek_en_schoon <- huisartsen %>% 
  select(id, organisatie_id, agbcode, mailadres, initialen, tussenvoegsels, achternaam, straat, huisnummer, postcode, plaats, bron, bron_mail) %>% 
  distinct(id, organisatie_id, .keep_all = TRUE)


# Eerst crm aanvullen met de meest zekere, persoonlijke mailadressen 


contacts_crm_verrijkt <- contacts_crm %>% 
  left_join(huisartsen_uniek_en_schoon, by = c('AGB-code' = 'agbcode', "Address 1: ZIP/Postal Code" = "postcode")) %>% 
  arrange(id, organisatie_id)




contacts_crm_verrijkt <- contacts_crm %>% 
  left_join(huisartsen_uniek_en_schoon, by = c('AGB-code' = 'agbcode')) %>% 
  arrange(id, organisatie_id)

test <- contacts_crm_verrijkt %>% filter(is.na(id))


test <- filter(adressen_edi, zorgmail_agb_edifact == '01027059')



