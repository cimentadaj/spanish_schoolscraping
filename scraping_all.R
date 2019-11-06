library(tidyverse)
library(xml2)
library(httr)

if_empty <- function(x) if (length(x) == 0) "-" else x

school_grabber <- function(school_url) {

  school_raw <- read_html(school_url) %>% xml_child()

  name_xpath <- "//a[@class='u-link-v5 g-font-size-18 g-color-gray-dark-v1 g-color-primary--hover']"

  school_name <-
    school_raw %>%
    xml_find_all(name_xpath) %>%
    xml_text()

  all_fields <-
    school_raw %>%
    xml_find_all("//div[@class='row g-mx-0 g-mb-0']") %>%
    xml_find_all("//div[@class='col-6 g-brd-left g-brd-bottom g-theme-brd-gray-light-v3 g-px-15 g-py-25']")  %>%
    map_chr(~ xml_find_all(.x, ".//strong") %>% xml_text() %>% paste(collapse = "; "))

  # Delete the last field which contains stuff related
  # to the calendar of the school (not interested in this).
  len <- length(all_fields)
  last_page <- len - 1

  all_fields <- all_fields[-(last_page:len)]
  
  names(all_fields) <- c("tel",
                         "email",
                         "website",
                         "fax",
                         "code",
                         "public_private",
                         "type_school",
                         "owner")

  location_str <-
    school_raw %>%
    xml_find_all("//p[@class='d-flex align-items-baseline g-mt-5']//a[@href]") %>%
    xml_attr(attr = "href")

  if (length(location_str) == 0) {
    location <- c("-", "-")
  } else {
    location <-
      location_str %>%
      str_extract_all("=.+$") %>%
      str_replace_all("=|colegio\\.longitud", "") %>% 
      str_split("&") %>%
      .[[1]]
  }

  complete_data <- data.frame(
    name = school_name,
    do.call(data.frame, as.list(c(all_fields, stringsAsFactors = FALSE))),
    lat = location[1],
    lon = location[2],
    stringsAsFactors = FALSE
  )

  complete_data
}

form_filled <- list(
  "colegio.denominacionEspecifica" = "",
  "colegio.denominacionEspecifica_widget" = "",
  "colegio.naturaleza" = "1",
  "__checkbox_colegio.concertado" = "true",
  "__checkbox_colegio.bilingue" = "true",
  "colegio.comunidad" = "0",
  "colegio.provincia" = "0",
  "colegio.localidad" = "0",
  "colegio.codigo_widget" = "",
  "colegio.actividad_widget" = "",
  "colegio.tipoCentro" = "0",
  "colegio.codPostal" = "",
  "colegio.direccion" = "",
  "colegio.ensenanza_widget" = "",
  "colegio.modalidad" = "0",
  # Number of page
  "colegio.index" = "1",
  "colegio.numRegistros" = "10",
  "colegio.latitud" = "0",
  "colegio.longitud" = "0",
  "colegio.radio" = "1",
  "colegio.descNaturaleza" = "Centro+Público",
  "colegio.descComunidad" = "Todas",
  "colegio.descProvincia" = "Todas",
  "colegio.descLocalidad" = "Todas"
)

resp <-
  POST("https://www.buscocolegio.com/Colegio/buscar-colegio.action",
       body = form_filled,
       encode = "form") %>%
  content()

max_pages <-
  resp %>%
  xml_find_all("//ul[@class='pagination']//li//a[@href]") %>%
  xml_attr("href") %>%
  .[length(.)] %>%
  str_replace_all("[^\\d]", "")

pages <- seq(1, max_pages)

xpath_school_link <- "//div[@style='box-shadow: 0 0 2px #ccc;padding: 20px 10px;margin-bottom: 10px;background-color: white;']//a[@href]"

school_links <-
  map(pages, ~ {
    print(paste0("Page ", .x))

    Sys.sleep(5)
    form_filled$colegio.index <- .x
    
    resp <-
      POST("https://www.buscocolegio.com/Colegio/buscar-colegio.action",
           body = form_filled,
           headers = list(
             `User-Agent` = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0"
           ),
           encode = "form") %>%
      content()

    school_links <-
      resp %>%
      xml_find_all(xpath_school_link) %>%
      xml_attr("href") %>%
      paste0("https://www.buscocolegio.com", .)

    school_links

  }) %>%
  unlist()

safe_school_grab <- safely(school_grabber)

all_schools <-
  map(school_links, ~ {
    print(.x)
    Sys.sleep(5)
    safe_school_grab(.x)
  })

tst_dt <-
  all_schools %>%
  transpose() %>% 
  select(lat, lon) %>% 
  mutate_all(as.numeric)

