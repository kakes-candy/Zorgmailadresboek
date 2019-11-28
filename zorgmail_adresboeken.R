

rm(list = ls())

library(tidyverse)
library(readr)
library(rjson)
library(lubridate)


# Niet voor in de versiebeheer
secrets <- fromJSON(file = 'secret.json')
user <- secrets$username
pwd <- secrets$password


# Ruwe data van Zorgmail
filepath_zm <- "data/input/addressbook_zorgmail_a15_se.csv"
filepath_edi <- "data/input/addressbook_zorgmail_a13_edi.csv"
bestand_crm <- "data/input/For Re-import - zorgmail export import.csv"




# Functie -----------------------------------------------------------------

zorgmail_api <- function(path, username, password, savepath) {
  url <- modify_url("https://api.zorgmail.nl", path = path)
  resp <- GET(url, authenticate(username, password), write_disk(path = savepath, overwrite = TRUE) )
  if(http_error(resp)) {
    stop(
      sprintf(
        "Zorgmail API request failed [%s]\n%s>", 
        status_code(resp),
        http_status(resp)$message
      )
    )
  }
}



# Data ophalen bij zorgmail api -------------------------------------------

# Edifact adressen ophalen
edifact_huisartsen_url <- "/addressbook/files/addressbook_zorgmail_a13_edi.csv.zip" 
filepath_edi = paste0('data/input/',  tail(str_split(edifact_huisartsen_url, '/')[[1]], 1))
zorgmail_api(path = edifact_huisartsen_url, username = user, password = pwd, savepath = filepath_edi)
filepath_edi <- unzip(zipfile = filepath_edi, exdir = 'data/input')



# Zorgmail adressen ophalen
zorgmail_huisartsen_url <- "/addressbook/files/addressbook_zorgmail_a15_se.csv.zip" 
filepath_zm = paste0('data/input/',  tail(str_split(zorgmail_huisartsen_url, '/')[[1]], 1))
zorgmail_api(path = zorgmail_huisartsen_url, username = user, password = pwd, savepath = filepath_zm)
filepath_zm <- unzip(zipfile = filepath_zm, exdir = 'data/input')




# Data inlezen en voorbereiden --------------------------------------------

# crm inlezen
contacts_crm <- read_csv2(file = bestand_crm) %>% 
  mutate(maildomein = sapply(str_split(`E-mail`, '@'), tail, 1)) %>% 
  mutate(crm_gewijzigd  = dmy_hm(`(Do Not Modify)Modified On`))



# Lijst zorgmail inlezen
adressen <- read_csv(file = filepath_zm, 
                     cols(.default = col_character(),id = col_double(),   organizationId = col_double())
                     , col_names = TRUE)

# Lijst met edifact adressen huisartsen
adressen_edi <- read_csv(file = filepath_edi, cols(
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
adressen <- adressen %>% 
  select(id, 'zorgmail_mailadres' = mailAddress, 'zorgmail_gewijzigd_op' = lastModified
         ,'zorgmail_rol'= role, 'zorgmail_agb_code' = zvAgbCode, 'zorgmail_geslacht' =  gender
         ,'zorgmail_achternaam' = surname, 'zorgmail_initialen' = initials, 'zorgmail_voorvoegsel' = prefix
         ,'zorgmail_agb_code_praktijk' = prAgbCode, 'zorgmail_organisatie_type' = organizationType
         ,'zorgmail_organisatie_naam' = organization, 'zorgmail_organisatie_id' = organizationId
         ,'zorgmail_adres_straat' = street, 'zorgmail_adres_nummer' = houseNumber, 'zorgmail_adres_postcode'  = postalCode
         ,'zorgmail_adres_plaats' = locality)


# namen aanpassen
adressen_edi <- adressen_edi %>% 
  select(id, 'edifact_mailadres' = mailAddress, 'edifact_gewijzigd_op' = lastModified
         ,'edifact_rol'= role, 'edifact_agb_code' = zvAgbCode, 'edifact_geslacht' =  gender
         ,'edifact_achternaam' = surname, 'edifact_initialen' = initials, 'edifact_voorvoegsel' = prefix
         ,'edifact_agb_code_praktijk' = prAgbCode, 'edifact_organisatie_type' = organizationType
         ,'edifact_organisatie_naam' = organization, 'edifact_organisatie_id' = organizationId
         ,'edifact_adres_straat' = street, 'edifact_adres_nummer' = houseNumber, 'edifact_adres_postcode'  = postalCode
         ,'edifact_adres_plaats' = locality)



# Unieke lijst id's van huisartsen met een praktijk id
huisartsen <- bind_rows(
  (adressen %>% filter(zorgmail_rol == 'Huisarts'  & !is.na(zorgmail_agb_code)) %>% select(id, 'organisatie_id' = zorgmail_organisatie_id, 'agbcode' = zorgmail_agb_code) %>% mutate(bron = 'zorgmail')), 
  (adressen_edi %>% filter(edifact_rol == 'Huisarts' & !is.na(edifact_agb_code)) %>% select(id, 'organisatie_id' = edifact_organisatie_id, 'agbcode' = edifact_agb_code) %>% mutate(bron = 'edifact'))
  ) 

# zorgmail en edifact gegevens erbij
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


# Gegevens van praktijken toevoegen waar mogelijk
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


# Alleen unieke huisartsen en praktijken houden
huisartsen_uniek_en_schoon <- huisartsen %>% 
  select(id, organisatie_id, agbcode, mailadres, initialen, tussenvoegsels, achternaam, straat, huisnummer, postcode, plaats, bron, bron_mail) %>% 
  distinct(id, organisatie_id, .keep_all = TRUE)



# CRM lijst aanvullen en bewerken -----------------------------------------

# Eerst crm met de meest betrouwbare combinatie: agb + postcode
contacts_crm_verrijkt_ronde1 <- contacts_crm %>% 
  inner_join(huisartsen_uniek_en_schoon %>% 
               mutate(postcode_tmp = postcode), 
             by = c('AGB-code' = 'agbcode', "Address 1: ZIP/Postal Code" = "postcode"), keep = TRUE) %>% 
  mutate(match = 'ronde 1') %>% 
    rename('postcode' = postcode_tmp) %>% 
  arrange(id, organisatie_id)

# niet matchbare apart zetten
geen_match_ronde1 <- contacts_crm %>%  
  anti_join(contacts_crm_verrijkt_ronde1, by = "(Do Not Modify)Contact" , keep = TRUE)


# Dan proberen op agb alleen
contacts_crm_verrijkt_ronde2 <- geen_match_ronde1 %>% 
  inner_join(huisartsen_uniek_en_schoon, by = c('AGB-code' = 'agbcode'), keep = TRUE) %>% 
  mutate(match ='ronde 2') %>% 
  arrange(id, organisatie_id)


# Alles bij elkaar vegen
contacts_crm_verrijkt_totaal <- bind_rows(contacts_crm_verrijkt_ronde1, contacts_crm_verrijkt_ronde2)


# Wat blijft er nog over na ronde 2 en verdwenen zorgmail?
de_rest <- contacts_crm %>% 
  anti_join(contacts_crm_verrijkt_totaal, by = "(Do Not Modify)Contact", keep = TRUE) %>% 
# Zorgmail zorgverleners die niet meer gevonden konden worden.
  mutate(match = if_else(maildomein == 'zorgmail.nl' |maildomein == 'ringamsterdam.nl' | maildomein == 'lms.lifeline.nl', 'niet meer in zorgmail', 'geen match', missing = 'geen match'))


# Die ook nog toevoegen
contacts_crm_verrijkt_totaal <- bind_rows(contacts_crm_verrijkt_totaal, de_rest)


contacts_crm_verrijkt_totaal <- contacts_crm_verrijkt_totaal %>% 
  # crm > geen mailadres, altijd aanvullen
  mutate(mail_nieuw = if_else(match %in% c('ronde 1', 'ronde 2') & is.na(`E-mail`) & !is.na(mailadres), 
                              mailadres, 
                              '')) %>% 
  # crm mailadres gevuld, crm gewijzigd < 1 jaar geleden, aanvullen/aanpassen
  mutate(mail_nieuw = if_else(match %in% c('ronde 1', 'ronde 2') & time_length(interval(crm_gewijzigd, Sys.time()), 'years') > 1 & !is.na(mailadres) & mailadres != `E-mail`, 
                              mailadres, 
                              mail_nieuw)) %>%
  mutate(status_nieuw = if_else(match == 'geen match' & time_length(interval(crm_gewijzigd, Sys.time()), 'years') > 1, 
                              'Inactive', 
                          Status))
  


outfile <- contacts_crm_verrijkt_totaal %>% 
  filter(mail_nieuw != '' & !is.na(mail_nieuw) | status_nieuw != Status) %>% 
  mutate(`E-mail` = if_else(mail_nieuw != '' & !is.na(mail_nieuw), mail_nieuw, `E-mail`),
         Status = status_nieuw) %>% 
  select(  `(Do Not Modify)Contact`,
           `(Do Not Modify)Checksum` ,
           `(Do Not Modify)Modified On` ,
           Contact ,
           `AGB-code` ,
           `E-mail` ,
           `First Name` ,
           `Last Name` ,
           `Address 1: City` ,
           `Address 1: Street 1`,
           `Address 1: Street 2` ,
           `Address 1: Street 3`,
           `Address 1: ZIP/Postal Code`,
           `Middle Name` ,
           Status ,
           `(Do Not Modify)Status` )


write_csv2(outfile, 'data/output/inleeslijst.csv', na = '')

write_csv2(contacts_crm_verrijkt_totaal, 'data/output/controlebestand.csv', na = '')




# 
# crm mailadres gevuld, crm gewijzigd < 2019, aanvullen/aanpassen
# crm mailadres gevuld, crm gewijzigd >= 2019, niets doen
# huisartsen zonder match < 2019, deactiveren
# huisartsen zonder match >= 2019, niets doen
# huisartsen zorgmail verdwenen, niets doen



