# Copyright (c) 2017, Rob van Eijk <rob@blaeu.com>
#
# This work is licensed under a Creative Commons Attribution 4.0 International License.
# URL: http://creativecommons.org/licenses/by/4.0/legalcode
#
# THE SOFTWARE IS PROVIDED "AS IS".

# To access the REST API, you'll need your user key: 5f059bc05c473de56615e9c0d17725be
#
# https://data.crunchbase.com/docs/odm-organizations
# 
# example: https://api.crunchbase.com/v/3/odm-organizations?domain_name=chartbeat.com&organization_types=company&user_key=5f059bc05c473de56615e9c0d17725be
#
# With Basic Access, API use is limited to using the Open Data Map and 2013 
# Snapshot in accordance with the License Agreement 

# cleanup all variables
# rm(list=ls())
# setwd("C:/workspace/crawl/stateless, 160829 eunewsfeeds/graph/ALL")

# Generic function for installing missing packages
libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
  {
    if(packageName %in% rownames(installed.packages()) == FALSE) {install.packages(c(packageName), dependencies=TRUE, repos='http://cran.rstudio.com')}
    packOK = require(packageName, character.only = TRUE)
    if(!packOK)
      warning(paste("*** The package: '", packageName, "' is not installed ***",sep=""))
  }
}

libraryRequireInstall("RCurl")
libraryRequireInstall("jsonlite")
libraryRequireInstall("devtools")
libraryRequireInstall("httr")
devtools::install_github("jayjacobs/tldextract")
library(tldextract)

# Loading data
books <- fromJSON("books.json")
distinct.nodesCentered <- fromJSON("distinctNodesCentered.json")
distinct.nodesCentered$name <- ""
distinct.nodesCentered$short_description <- ""
distinct.nodesCentered$title <- "" # tooltip

cpq = proc.time()
total.rows = nrow(distinct.nodesCentered)
# do for all domain pattern
#for (j in 1:nrow(distinct.nodesCentered)) {
for (j in 200:nrow(distinct.nodesCentered)) {
  filesdone = round((100 * (j / total.rows)), digits = 0)
  elapsed = proc.time() - cpq
  elapsedtotalsec = elapsed[3]
  elapsedhours = round(elapsedtotalsec / 3600, digits = 0)
  elapsedminutes = round((elapsedtotalsec - (elapsedhours * 3600)) / 60, digits = 0)
  timetotalseconds = (elapsedtotalsec / j) * total.rows
  etatotalsec = (elapsedtotalsec / j) * total.rows - elapsedtotalsec
  hoursleft = round(etatotalsec / 3600, digits = 0)
  minutesleft = round((etatotalsec - (hoursleft * 3600)) / 60, digits = 0)
  cat(
    sep = "",
    "\r",
    filesdone,
    "% (",
    j,
    "/",
    total.rows,
    " ETA: ",
    hoursleft,
    "\'",
    minutesleft,
    "\")     "
  )
  
  # j=70
  hosts <-
    tldextract(distinct.nodesCentered[j, 1], tldnames = getTLD())
  domain_name = paste(hosts$domain, hosts$tld, sep = ".")
  lookup.domain_name <-
    fromJSON(getURL(
      paste(
        'https://api.crunchbase.com/v/3/odm-organizations?domain_name=',
        domain_name,
        '&user_key=5f059bc05c473de56615e9c0d17725be',
        sep = ""
      )
    ))
  if (as.integer(unlist(lookup.domain_name$data$paging$total_items)) >=
      101) {
    lookup.domain_name <-
      fromJSON(getURL(
        paste(
          'https://api.crunchbase.com/v/3/odm-organizations?domain_name=',
          domain_name,
          '&organization_types=company&user_key=5f059bc05c473de56615e9c0d17725be',
          sep = ""
        )
      ))
  }
  lookup.query <-
    fromJSON(getURL(
      paste(
        'https://api.crunchbase.com/v/3/odm-organizations?query=',
        hosts$domain,
        '&user_key=5f059bc05c473de56615e9c0d17725be',
        sep = ""
      )
    ))
  if (as.integer(unlist(lookup.query$data$paging$total_items)) >= 101) {
    lookup.query <-
      fromJSON(getURL(
        paste(
          'https://api.crunchbase.com/v/3/odm-organizations?query=',
          hosts$domain,
          '&organization_types=company&user_key=5f059bc05c473de56615e9c0d17725be',
          sep = ""
        )
      ))
  }
  
  # PASS 1
  #
  if (as.integer(unlist(lookup.domain_name$data$paging$total_items)) >= 1) {
    for (k in 1:as.integer(unlist(lookup.domain_name$data$paging$total_items))) {
      lookup.domainlink <-
        as.character(unlist(lookup.domain_name$data$items$properties$permalink[k]))
      lookup.short_description <-
        as.character(unlist(
          lookup.domain_name$data$items$properties$short_description[k]
        ))
      if (lookup.domainlink == hosts$domain) {
        distinct.nodesCentered[j, 3] <- lookup.domainlink
        distinct.nodesCentered[j, 4] <-
          lookup.short_description
      }
    }
  } else {
    # PASS 2
    #
    if (as.integer(unlist(lookup.query$data$paging$total_items)) >= 1 &&
        as.integer(unlist(lookup.query$data$paging$total_items)) <= 100) {
      for (k in 1:as.integer(unlist(lookup.query$data$paging$total_items))) {
        lookup.querylink <-
          as.character(unlist(lookup.query$data$items$properties$permalink[k]))
        lookup.short_description <-
          as.character(unlist(
            lookup.query$data$items$properties$short_description[k]
          ))
        if (lookup.querylink == hosts$domain) {
          distinct.nodesCentered[j, 3] <- lookup.querylink
          distinct.nodesCentered[j, 4] <-
            lookup.short_description
        }
      }
    }
  }
  # PASS 3
  #
  if (distinct.nodesCentered[j, 4] == "") {
    # lookup with previously identified metadata
    for (b in 1:nrow(books)) {
      if (books[b, 1] == domain_name) {
        bb_ahaa = b
        b_found <- gsub("\\s*\\w*$", "", books[b, 2]) # trim
        b_found <- gsub(" ", "%20", b_found, fixed = TRUE)
        if (b_found=="") {
          b_found = books[b, 2]
        }
        lookup.domain_name <- fromJSON(getURL(paste('https://api.crunchbase.com/v/3/odm-organizations?name=',  b_found, '&user_key=5f059bc05c473de56615e9c0d17725be',  sep = "" )))
        lookup.domainlink <-  as.character(unlist(lookup.domain_name$data$items$properties$permalink))
        lookup.short_description <-  as.character(unlist( lookup.domain_name$data$items$properties$short_description))
        if (as.integer(unlist(lookup.domain_name$data$paging$total_items)) == 1) {
          distinct.nodesCentered[j, 3] <- books[b, 2]
          distinct.nodesCentered[j, 4] <-
            lookup.short_description
        } else { # PASS 4
          if (as.integer(unlist(lookup.domain_name$data$paging$total_items)) >= 1) {
            for (k in 1:as.integer(unlist(lookup.domain_name$data$paging$total_items))) {
              lookup.name <-
                as.character(unlist(lookup.domain_name$data$items$properties$name[k]))
              lookup.short_description <- as.character( unlist(lookup.domain_name$data$items$properties$short_description[k]))
              if (lookup.name == books[b, 2]) {
                distinct.nodesCentered[j, 3] <- books[b, 2]
                distinct.nodesCentered[j, 4] <-
                  lookup.short_description
              } else {
                b_found <- gsub(" ", "", books[b, 2], fixed = TRUE)
                lookup.domain_name <- fromJSON(getURL(paste('https://api.crunchbase.com/v/3/odm-organizations?name=',  b_found, '&user_key=5f059bc05c473de56615e9c0d17725be',  sep = "" )))
                lookup.domainlink <-  as.character(unlist(lookup.domain_name$data$items$properties$permalink))
                lookup.short_description <-  as.character(unlist( lookup.domain_name$data$items$properties$short_description))
                if (as.integer(unlist(lookup.domain_name$data$paging$total_items)) == 1) {
                  distinct.nodesCentered[j, 3] <- books[b, 2]
                  distinct.nodesCentered[j, 4] <-
                    lookup.short_description
                }
              }
            }
          }
        }
      }
    }
  }
}

sink(paste("tooltips_distinctNodesCentered.json"))
cat(toJSON(distinct.nodesCentered))
sink()


