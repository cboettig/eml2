library(tidyverse)
library(jsonlite)
collabs <-
tribble(
   ~givenName, ~surName,  ~organizationName,  ~positionName, ~address, ~phone, ~electronicMailAddress, ~onlineUrl, ~userId,
  "Matt",       "Jones",     "NCEAS",  "Director of Informatics", "_:NCEAS", NA, "mjones@nceas.ucsb.edu", NA,       NA,
  "Bryce",      "Mecum",     "NCEAS",  "Software Developer",      "_:NCEAS", NA,  NA,                     NA,       NA)

json <- jsonlite::toJSON(collabs, pretty = TRUE, auto_unbox = TRUE)
jqr::jq(as.character(json),
" .[] | {
individualName: {givenName, surName},
organizationName,
positionName,
address,
phone,
electronicMailAddress,
onlineUrl,
userId
}") ->x

## It would be so nice if jqr returned "json" class objects!
y <- structure(as.character(jqr::combine(x)), class="json")


z <- fromJSON(y, simplifyVector = FALSE)
#class(z)<- c("emld", "list")

## whish we could drop the nulls
#jqr::jq('{"foo": 19}', '.foo // 42')
fromJSON(json, simplifyVector = FALSE)
