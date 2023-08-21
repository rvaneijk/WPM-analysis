#!/usr/bin/env Rscript
#
# Copyright (c) 2017, Rob van Eijk <rob@blaeu.com>
#
# This work is licensed under a Creative Commons Attribution 4.0 International License.
# URL: http://creativecommons.org/licenses/by/4.0/legalcode
#
# THE SOFTWARE IS PROVIDED "AS IS".

if("optparse" %in% rownames(installed.packages()) == FALSE) {install.packages(c("optparse", dependencies=TRUE), repos='http://cran.rstudio.com')}
suppressPackageStartupMessages(library(optparse))

# command line options
option_list = list(
  make_option(c("-i", "--input"), type="character", default=NULL, 
              help="input dir name (e.g., 'NL') [required]", metavar="DIR"),
  make_option(c("-l", "--linux"), action="store_true", default=FALSE, 
              help="Linux"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Print extra output")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$input)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (--input).\n\nExample: /<path>/Rscript --vanilla script.R -i NL\n\n", call.=FALSE)
}

cat(c("",str(opt)), sep="\n")

# auto-install dependencies if missing
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages(c("devtools", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages(c("data.table", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("urltools" %in% rownames(installed.packages()) == FALSE) {install.packages(c("urltools", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("bitops" %in% rownames(installed.packages()) == FALSE) {install.packages(c("bitops", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages(c("RCurl", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages(c("jsonlite", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("httr" %in% rownames(installed.packages()) == FALSE) {install.packages(c("httr", dependencies=TRUE), repos='http://cran.rstudio.com')}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages(c("dplyr", dependencies=TRUE), repos='http://cran.rstudio.com')}
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(library(urltools))
suppressPackageStartupMessages(library(bitops))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(httr)) # urlparse
suppressPackageStartupMessages(library(dplyr))


# set runenvironment parmeters
if (opt$linux==FALSE) {
  setwd(file.path(".\\",opt$input,"\\json"))
} else {
  setwd(file.path("./",opt$input,"/json"))
}

ALLSITES=1 # both distinct and distinctCentered results 
start = proc.time()
cnt_nodes=1
cnt_edges=1
cnt_progress=1
nodes = as.data.frame(matrix(ncol=2))
names(nodes) = c("id", "label")
edges = as.data.frame(matrix(ncol=2))
names(edges) = c("from", "to")
nodesCentered = as.data.frame(matrix(ncol=2))
names(nodesCentered) = c("id", "label")
edgesCentered = as.data.frame(matrix(ncol=2))
names(edgesCentered) = c("from", "to")

#
# --- processing all json files starts here -----
#
cpq = proc.time()
files <- list.files(pattern = ".json")
crawled.sites <- as.data.frame(files)
if (opt$linux==FALSE) {
  pb <- winProgressBar(title = paste("progress bar "), min = 0, max = length(files), width = 380)
} 

for (j in 1:length(files)) { 
  #for (j in 302:302) {
  
  # monitor progress
  x <- c(j, length(files))
  x.tot=paste(x, collapse="/")
  x.long=c(opt$input, x.tot)
  y <- c(x.long, files[[j]])
  if (opt$linux==FALSE) { # progressbar under windows
    setWinProgressBar(pb, j, title=paste(y, collapse=" - "))
  }
  filesdone = round((100*(j/length(files))), digits = 0)
  elapsed = proc.time() - cpq
  elapsedtotalsec = elapsed[3]
  elapsedhours = round(elapsedtotalsec/3600, digits = 0)
  elapsedminutes = round((elapsedtotalsec - (elapsedhours*3600))/60, digits = 0 )
  timetotalseconds = (elapsedtotalsec/j)*length(files)
  etatotalsec = (elapsedtotalsec/j)*length(files) - elapsedtotalsec
  hoursleft = round(etatotalsec/3600, digits = 0)
  minutesleft = round((etatotalsec - (hoursleft*3600))/60, digits = 0 )
  cat(sep = "", "\r", filesdone, "% (", j, "/", length(files), " ETA: ", hoursleft, "\'", minutesleft, "\")     " )
  cnt_progress=0
  cnt_progress=cnt_progress+1
  
  # do for all domain pattern
  if (opt$verbose==FALSE) {
    if (opt$linux==FALSE) {
      sink("NUL") # windows: suppress console output
    } else {
      sink(file="/dev/null") # linux: suppress console output
    }
  }
  crawl <- fromJSON(files[[j]])
  for (i in 1:length(crawl$flows$request$headers)) { 
    # extract host and referrer from request header
    HAS_REFERER=FALSE
    request_header = lapply(crawl$flows$request$headers, print )
    data=request_header[[i]] # matrix
    row=match('Host', data) # lookup index
    if (is.na(row)) {
      row=match('host', data)
    } 
    resource = data[row,2] # bug host en Host, request_url instead
    if (i==1) {
      crawled.site = resource # mark first resource in crawlset
    } 
    row=match('Referer', data) # lookup index
    if (is.na(row)) {
      referer=resource
      HAS_REFERER=FALSE
    } else {
      referer <- parse_url(data[row,2])$hostname
      HAS_REFERER=TRUE
    }
    
    # extract location from 3xx response header
    if (HAS_REFERER==FALSE) {
      response_header = lapply(crawl$flows$response$headers, print )
      response_data=response_header[[i]] # matrix
      response_code = lapply(crawl$flows$response$code, print )
      if (is.null(response_data)) {
        # no response header
      } else {substr(x, 3, 3)
        if (substr(response_code[[i]], 1, 1) =='3') {
          rowLoc=match('Location', response_data) # lookup index Location
          if (is.na(rowLoc)) {
            rowLoc=match('location', response_data)
            if (is.na(rowLoc)) {
              # no L(l)ocation header
            }
          } else {
            referer=resource
            resource <- parse_url(response_data[rowLoc,2])$hostname 
            if (is.null(resource)){
              resource=referer # url on same host
            }
          }
        }
      }
    }
    
    
    # populate nodes and edge frames
    edges[cnt_edges,1] = resource
    edges[cnt_edges,2] = referer
    edgesCentered[cnt_edges,1] = resource
    edgesCentered[cnt_edges,2] = referer
    cnt_edges=cnt_edges+1
    
    nodes[cnt_nodes,1] = resource
    nodes[cnt_nodes,2] = resource
    nodesCentered[cnt_nodes,1] = resource
    nodesCentered[cnt_nodes,2] = resource
    cnt_nodes=cnt_nodes+1
    
    nodes[cnt_nodes,1] = referer
    nodes[cnt_nodes,2] = referer
    nodesCentered[cnt_nodes,1] = referer
    nodesCentered[cnt_nodes,2] = referer
    cnt_nodes=cnt_nodes+1
  }
  if (opt$verbose==FALSE) { 
    sink() # restore console output
  }
  if(ALLSITES==1) {
    nodesCentered[nodes==crawled.site]<-"crawled.io"
    edgesCentered[edges==crawled.site]<-"crawled.io"
  }
}

if (opt$linux==FALSE) {
  close(pb) # close windows progress bar
}

# post processing
distinct.edges <- edges %>%
  group_by(from, to) %>%
  tally()
distinct.edges <- unique( distinct.edges[ , 1:3 ] )
distinct.nodes <- nodes[!duplicated(nodes$id),] # uniq

#distinct.edgesCentered <- edgesCentered %>%
#  group_by(from, to) %>%
#  mutate(count = n())
distinct.edgesCentered <- edgesCentered %>%
  group_by(from, to) %>%
  tally()
distinct.edgesCentered <- unique( distinct.edgesCentered[ , 1:3 ] )
distinct.nodesCentered <- nodesCentered[!duplicated(nodesCentered$id),] # uniq

# write to json
if (opt$linux==FALSE){
  setwd(file.path("..\\graph"))
} else{
  setwd(file.path("../graph"))
}
sink(paste("edges.json"))
cat(toJSON(edges))
sink()
sink(paste("edgesCentered.json"))
cat(toJSON(edgesCentered))
sink()
sink(paste("nodes.json"))
cat(toJSON(nodes))
sink()
sink(paste("nodesCentered.json"))
cat(toJSON(nodesCentered))
sink()

sink(paste("distinctEdges.json"))
cat(toJSON(distinct.edges))
sink()
sink(paste("distinctEdgesCentered.json"))
cat(toJSON(distinct.edgesCentered))
sink()

sink(paste("distinctNodes.json"))
cat(toJSON(distinct.nodes))
sink()
sink(paste("distinctNodesCentered.json"))
cat(toJSON(distinct.nodesCentered))
sink()

# Processing time
cat(c(opt$input,""), sep="\n")
cat(c("","Total processing time"), sep="\n")
proc.time() - start # total processing time
cat(c("",""), sep="\n")
