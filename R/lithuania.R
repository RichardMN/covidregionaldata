#' Lithuania Regional Daily COVID-19 Count Data - County
#'
#' @description Extracts daily COVID-19 data for Lithuania, stratified by county (apskritis) 
#' and municipality (savivaldybe). Some Lithuanian municipalities share names, there being both a
#' Vilnius city municipality (m.) and a Vilnius regional municipality (r.)
#' This function used to work from an open data set published by the Official Statistics Portal
#' at \url{https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0}, but this 
#' data set ceased to be updated on 11 February 2021.
#' It now works from a list of all confirmed cases of COVID-19 published by the National Public Health Centre
#' online as a \emph{very large} JSON file at \url{ftp://atviriduomenys.nvsc.lt/COVID19.json}
#' @return A data frame of COVID cases by county in Lithuania, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr %>% filter select mutate left_join rename across
#' @importFrom lubridate as_date ymd
#' @importFrom tibble tibble

get_lithuania_regional_cases_only_level_1 <- function() {

  # Lithuania only publishes data at the municipality level. To provide
  # data for the level 1 regions (Counties, Apskritis) we get the municipality
  # level data and aggregate it according to municipality

  county_data <- get_lithuania_regional_cases_with_level_2() %>%
    group_by(date,region_level_1) %>%
    summarise(across(where(is.numeric), sum))
  
  return(county_data)
  
}

#' Lithuanian Daily COVID-19 Count Data - Municipalities
#'
#' @description Extracts daily COVID-19 data for Lithuania, by municipality (savivaldybe).
#' Some Lithuanian municipalities share names, there being both a
#' Vilnius city municipality (m.) and a Vilnius regional municipality (r.)
#' This function used to work from an open data set published by the Official Statistics Portal
#' at \url{https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0}, but this 
#' data set ceased to be updated on 11 February 2021.
#' It now works from a list of all confirmed cases of COVID-19 published by the National Public Health Centre
#' online as a \emph{very large} JSON file at \url{ftp://atviriduomenys.nvsc.lt/COVID19.json}
#' @return A data.frame of COVID cases by municipality in Lithuania, ready to be used by get_regional_data().
#' @importFrom dplyr filter select mutate full_join left_join rename bind_rows transmute group_by summarise n %>% 
#' @importFrom lubridate as_date ymd
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom tibble tibble
#'
get_lithuania_regional_cases_with_level_2 <- function() {

  # The following code, adjusted from a version for France, was initially used to
  # create lookup tables of Lithuanian municipality and country codes.
  # These were then adjusted to match the format used by the 
  # Official Statistics Portal in their open data and are left as 
  # hard-coded tibbles. These codes have not changed in ten years.
  
  # level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:LT"
  # level_2_codes_table <- level_2_codes_url %>%
  #   xml2::read_html() %>%
  #   rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  #   rvest::html_table(fill = TRUE)

  municipality_county_lookup <- tibble::tribble(
    ~region_level_2,          ~region_level_1,
    "Akmenės r.",      "Šiaulių apskritis",
    "Alytaus m.",      "Alytaus apskritis",
    "Alytaus r.",      "Alytaus apskritis",
    "Anykščių r.",       "Utenos apskritis",
    "Birštono",        "Kauno apskritis",
    "Biržų r.",    "Panevėžio apskritis",
    "Druskininkų",      "Alytaus apskritis",
    "Elektrėnų",     "Vilniaus apskritis",
    "Ignalinos r.",       "Utenos apskritis",
    "Jonavos r.",        "Kauno apskritis",
    "Joniškio r.",      "Šiaulių apskritis",
    "Jurbarko r.",     "Tauragės apskritis",
    "Kaišiadorių r.",        "Kauno apskritis",
    "Kalvarijos", "Marijampolės apskritis",
    "Kauno r.",        "Kauno apskritis",
    "Kauno m.",        "Kauno apskritis",
    "Kazlų Rūdos", "Marijampolės apskritis",
    "Kėdainių r.",        "Kauno apskritis",
    "Kelmės r.",      "Šiaulių apskritis",
    "Klaipėdos r.",    "Klaipėdos apskritis",
    "Klaipėdos m.",    "Klaipėdos apskritis",
    "Kretingos r.",    "Klaipėdos apskritis",
    "Kupiškio r.",    "Panevėžio apskritis",
    "Lazdijų r.",      "Alytaus apskritis",
    "Marijampolės", "Marijampolės apskritis",
    "Mažeikių r.",       "Telšių apskritis",
    "Molėtų r.",       "Utenos apskritis",
    "Neringos",    "Klaipėdos apskritis",
    "Pagėgių",     "Tauragės apskritis",
    "Pakruojo r.",      "Šiaulių apskritis",
    "Palangos m.",    "Klaipėdos apskritis",
    "Panevėžio m.",    "Panevėžio apskritis",
    "Panevėžio r.",    "Panevėžio apskritis",
    "Pasvalio r.",    "Panevėžio apskritis",
    "Plungės r.",       "Telšių apskritis",
    "Prienų r.",        "Kauno apskritis",
    "Radviliškio r.",      "Šiaulių apskritis",
    "Raseinių r.",        "Kauno apskritis",
    "Rietavo",       "Telšių apskritis",
    "Rokiškio r.",    "Panevėžio apskritis",
    "Šakių r.", "Marijampolės apskritis",
    "Šalčininkų r.",     "Vilniaus apskritis",
    "Šiaulių r.",      "Šiaulių apskritis",
    "Šiaulių m.",      "Šiaulių apskritis",
    "Šilalės r.",     "Tauragės apskritis",
    "Šilutės r.",    "Klaipėdos apskritis",
    "Širvintų r.",     "Vilniaus apskritis",
    "Skuodo r.",    "Klaipėdos apskritis",
    "Švenčionių r.",     "Vilniaus apskritis",
    "Tauragės r.",     "Tauragės apskritis",
    "Telšių r.",       "Telšių apskritis",
    "Trakų r.",     "Vilniaus apskritis",
    "Ukmergės r.",     "Vilniaus apskritis",
    "Utenos r.",       "Utenos apskritis",
    "Varėnos r.",      "Alytaus apskritis",
    "Vilkaviškio r.", "Marijampolės apskritis",
    "Vilniaus m.",     "Vilniaus apskritis",
    "Vilniaus r.",     "Vilniaus apskritis",
    "Visagino",       "Utenos apskritis",
    "Zarasų r.",       "Utenos apskritis",
    "nenustatyta",            "nenustatyta"
  )

  # Read data --------------------------------------------------------------------
  #cases_url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
  #hosp_url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"

  #cases_url <- "https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0.csv"
  cases_url <- "ftp://atviriduomenys.nvsc.lt/COVID19.json"
  
  # cases_data <- csv_reader(file = cases_url) %>%
  #   dplyr::select(date,
  #                 region_level_2 = municipality_name,
  #                 cases_new = confirmed_cases,
  #                 cases_total = confirmed_cases_cumulative,
  #                 deaths_new = deaths,
  #                 deaths_total = deaths_cumulative,
  #                 recovered_new = recovered_cases,
  #                 recovered_total = recovered_cases_cumulative) %>%
  #   dplyr::mutate(date = lubridate::as_date(date))

  cases_data <- json_reader(file = cases_url) %>%
    transmute(
      `illness_date` = as_date(`Susirgimo data`),
      `case_confirmation_date` = as_date(`Atvejo patvirtinimo data`),
      imported = recode(`Įvežtinis`, "Taip" = TRUE, "Ne" = FALSE),
      country = `Šalis`,
      outcome = recode_factor(`Išeitis`,
                              "Gydomas" = "treated",
                              "Kita" = "other",
                              "Mirė" = "dead",
                              "Nesirgo" = "asymptomatic",
                              "Pasveiko" = "healed"),
      foreigner = recode(`Užsienietis`, "Taip" = TRUE, "Ne" = FALSE),
      patient_age = factor(`Atvejo amžius`,
                           levels =c("","0-9", "10-19",  "20-29",  "30-39",  "40-49",  "50-59",
                                     "60-69" , "70-79",  "80-89",  "90-99" , "100-109", "120-129" )
      ),
      gender = recode_factor(`Lytis`, "Vyras" = "male", "Moteris" = "female", "mot." = "female"),
      region = `Savivaldybė`,
      hospitalised = recode(`Ar hospitalizuotas`, "Taip" = TRUE, "Ne" = FALSE),
      icu_therapy = if_else(`Gydomas intensyvioje terapijoje`=="Taip", TRUE, FALSE),
      chronic_diseases = if_else(`Turi lėtinių ligų`=="Taip", TRUE, FALSE)
    ) %>%
    select(date=case_confirmation_date, region_level_2=region) %>%
    group_by(region_level_2, date) %>%
    summarise(cases_new=n())
  cases_wider <- left_join(cases_data,municipality_county_lookup, by=c("region_level_2")) %>%
    select(date, region_level_1, region_level_2, #level_2_region_code=region_2_code,
           cases_new)
    
    ## This is the list of fields which we're trying to generate, copied from get_regional_data.R
    # date, region_level_2, level_2_region_code, region_level_1, level_1_region_code, 
    # cases_new, cases_total, deaths_new, deaths_total,
    # recovered_new, recovered_total, hosp_new, hosp_total,
    # tested_new, tested_total, dplyr::everything())

  # The French code for loading hospital data is being kept here as a guide
  # for when it is added for Lithuania.
  
  # hosp_data <- csv_reader(file = hosp_url) %>%
  #   dplyr::select(date = jour,
  #                 level_2_region_code = dep,
  #                 hosp_new = incid_hosp,
  #                 deaths_new = incid_dc) %>%
  #   dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

  data <- cases_wider
  return(data)
}
