library(httr)
library(purrr)
library(tidyverse)

getCountries <- function(idSite = 3,
                        period = "day",
                        date = "yesterday",
                        url = "https://piwik.labocleo.org/index.php?module=API",
                        revue = "cybergeo.revues.org") {
  request <- GET(url = url, 
                 query = list(
                   method = "UserCountry.getCountry",
                   idSite = idSite,
                   period = period,
                   date = date,
                   language = "fr",
                   expanded = 1,
                   flat = 1,
                   filter_limit = -1,
                   format = "json",
                   segment = paste0("customVariablePageName==domain;customVariablePageValue==", revue)),
                 authenticate(login, password))
  content(request) %>% 
    map_df(., function(x) {
      data_frame(pays = ifelse(!is.null(x[["label"]]),
                               x[["label"]],
                               "Autres"),
                 nb_visits = ifelse(!is.null(x[["nb_visits"]]),
                                    as.integer(x[["nb_visits"]]),
                                    0),
                 nb_uniq_visitors = ifelse(!is.null(x[["nb_uniq_visitors"]]),
                                           as.integer(x[["nb_uniq_visitors"]]),
                                           0),
                 nb_actions = ifelse(!is.null(x[["nb_actions"]]),
                                     as.integer(x[["nb_actions"]]),
                                     0),
                 code = ifelse(!is.null(x[["code"]]),
                               x[["code"]],
                               0))
    }, .id = "id")
})

getCountries(revue = "chs.revues.org")

map_df(c("yesterday" = "yesterday", "today" = "today"),
  function(date) {
    map_df(c("cybergeo" = "cybergeo.revues.org", "vertigo" = "vertigo.revues.org"),
       ~ getCountries(revue = ., date = date), .id = "revue")
  }, .id = "date"
)

