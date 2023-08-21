# Copyright (c) 2017, Rob van Eijk <rob@blaeu.com>
#
# This work is licensed under a Creative Commons Attribution 4.0 International License.
# URL: http://creativecommons.org/licenses/by/4.0/legalcode
#
# THE SOFTWARE IS PROVIDED "AS IS".

#
# TODO: the script needs serious refactoring
#
# str(my_object)
# typeof(my_object)
# class(my_object)
# sapply(my_object, class)
# sapply(my_object, attributes)
# attributes(my_object)
# names(my_object)
#
# <progress id="progressbar" value="0" max="100"></progress>
#

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

# Process command-line options first
libraryRequireInstall("optparse")
suppressPackageStartupMessages(library(optparse))

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
cat(opt$input)

# set runenvironment parmeters
if (opt$linux==FALSE) {
  setwd(file.path(opt$input))
} else {
  setwd(file.path(opt$input))
}
if (opt$verbose==FALSE) {
  if (opt$linux==FALSE) {
    sink("NUL") # windows: suppress console output
  } else {
    sink(file="/dev/null") # linux: suppress console output
  }
}

# --- programm starts here ----------------------------------------------------------
# some libraries are not used and could be removed
libraryRequireInstall("igraph")
libraryRequireInstall("visNetwork")
libraryRequireInstall("data.table")
libraryRequireInstall("urltools")
libraryRequireInstall("bitops")
libraryRequireInstall("RCurl")
libraryRequireInstall("jsonlite")
libraryRequireInstall("httr") # url parse
libraryRequireInstall("plyr")
libraryRequireInstall("dplyr")
libraryRequireInstall("htmlwidgets")
libraryRequireInstall("XML")
libraryRequireInstall("stringr")
libraryRequireInstall("magrittr")
libraryRequireInstall("DiagrammeR")

# setwd("C:/workspace/crawl/stateless, 160829 eunewsfeeds/experimental.subset.04/graph")

# cleanup all variables
#rm(list=ls())

# Loading data
#edges <- fromJSON("edges.json")
#nodes <- fromJSON("nodes.json")
tmp_distinct.edges <- fromJSON("distinctEdges.json")
distinct.nodes <- fromJSON("distinctNodes.json")
#edgesCentered <- fromJSON("edgesCentered.json")
#nodesCentered <- fromJSON("nodesCentered.json")
tmp_distinct.edgesCentered <- fromJSON("distinctEdgesCentered.json")
distinct.nodesCentered <- fromJSON("distinctNodesCentered.json")

distinct.nodes$title = distinct.nodes$label # tooltips
distinct.nodesCentered$title = distinct.nodesCentered$label # tooltips

attach(tmp_distinct.edges)
distinct.edges <- tmp_distinct.edges[order(from),]
detach(tmp_distinct.edges)
tmp_distinct.edges=NULL

attach(tmp_distinct.edgesCentered)
distinct.edgesCentered <- tmp_distinct.edgesCentered[order(from),]
detach(tmp_distinct.edgesCentered)
tmp_distinct.edgesCentered=NULL


# graph   document.getElementById('text').innerHTML = Math.round(widthFactor*100) + '%';});
graph=visNetwork(distinct.nodes, distinct.edges, width = "1600", height = "825") %>%
  visConfigure(enabled = TRUE) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);

            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="graph.html", selfcontained = FALSE)
add_progress  <- readLines("graph.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="graph.html")

# graph_Centered
graph=visNetwork(distinct.nodesCentered, distinct.edgesCentered, width = "1600", height = "825") %>%
  visConfigure(enabled = TRUE) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="graph_Centered.html", selfcontained = FALSE)
add_progress  <- readLines("graph_Centered.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="graph_Centered.html")

# betweeness
ig = graph_from_data_frame(distinct.edges, directed=F)
clusters=as.data.frame(betweenness(ig))
clusters$name=rownames(clusters)
attach(clusters)
sorted.clusters <- clusters[order(name),]
detach(clusters)
attach(distinct.nodes)
results_betweenness <- distinct.nodes[order(id),]
detach(distinct.nodes)
results_betweenness$value = sorted.clusters$`betweenness(ig)`
graph=visNetwork(results_betweenness, distinct.edges, width = "1600", height = "825") %>%
  visConfigure(enabled = TRUE) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="betweeness.html", selfcontained = FALSE)
add_progress  <- readLines("betweeness.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="betweeness.html")

# betweeness_Centered
ig = graph_from_data_frame(distinct.edgesCentered, directed=F)
clusters=as.data.frame(betweenness(ig))
clusters$name=rownames(clusters)
attach(clusters)
sorted.clusters <- clusters[order(name),]
detach(clusters)
attach(distinct.nodesCentered)
results_betweenness_Centered <- distinct.nodesCentered[order(id),]
detach(distinct.nodesCentered)
results_betweenness_Centered$value = sorted.clusters$`betweenness(ig)`
#attach(results_betweenness_Centered)
#results_betweenness_Centered <- results_betweenness_Centered[order(-value),]
#detach(results_betweenness_Centered)
#results_betweenness_Centered[1,4]=0 # make crawled.io a small node
#distinct.nodesCentered[1,3]=0 # make sites visited a small node
sink(paste("results_betweenness_Centered.json"))
cat(toJSON(results_betweenness_Centered))
sink()
graph=visNetwork(results_betweenness_Centered, distinct.edgesCentered, width = "1600", height = "825") %>%
  visConfigure(enabled = TRUE) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="betweeness_Centered.html", selfcontained = FALSE)
add_progress  <- readLines("betweeness_Centered.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="betweeness_Centered.html")

# SHINY_cluster_edge_betweenness
ig = graph_from_data_frame(distinct.edges, directed=F)
clusters = cluster_edge_betweenness(ig)
m1 = distinct.nodes
m1[,1] <- NA
m1$id=NULL
m1$label=NULL
m1$title=NULL
m1$ctrl <- clusters$names
m1$group = clusters$membership
attach(m1)
sorted.clusters <- m1[order(ctrl),]
detach(m1)
attach(distinct.nodes)
results_cluster_edge_betweenness <- distinct.nodes[order(id),]
detach(distinct.nodes)
results_cluster_edge_betweenness$ctrl = sorted.clusters$ctrl
results_cluster_edge_betweenness$group = sorted.clusters$group
sink(paste("results_cluster_edge_betweenness.json"))
cat(toJSON(results_cluster_edge_betweenness))
sink()
graph=visNetwork(results_cluster_edge_betweenness, distinct.edges, width = "1600", height = "825") %>%
  visOptions(selectedBy = list(variable = "group", selected = "5")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness.html")

# SHINY cluster_edge_betweenness_Centered
ig = graph_from_data_frame(distinct.edgesCentered, directed=F)
clusters = cluster_edge_betweenness(ig)
m1 = distinct.nodesCentered
m1[,1] <- NA
m1$id=NULL
m1$label=NULL
m1$title=NULL
m1$ctrl <- clusters$names
m1$group = clusters$membership
attach(m1)
sorted.clusters <- m1[order(ctrl),]
detach(m1)
attach(distinct.nodesCentered)
results_cluster_edge_betweenness_Centered <- distinct.nodesCentered[order(id),]
detach(distinct.nodesCentered)
#results_cluster_edge_betweenness_Centered$ctrl = sorted.clusters$ctrl
results_cluster_edge_betweenness_Centered$group = sorted.clusters$group
sink(paste("results_cluster_edge_betweenness_Centered.json"))
cat(toJSON(results_cluster_edge_betweenness_Centered))
sink()
graph=visNetwork(results_cluster_edge_betweenness_Centered, distinct.edgesCentered, width = "1600", height = "825") %>%
  visOptions(selectedBy = list(variable = "group", selected = "5")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered.html")

# ----------------------------------------------------------------------
# SHINY cluster_edge_betweenness_Centered_weighted
ig = graph_from_data_frame(distinct.edgesCentered, directed=F)
clusters = cluster_edge_betweenness(ig)
m1 = distinct.nodesCentered
m1[,1] <- NA
m1$id=NULL
m1$label=NULL
m1$title=NULL
m1$ctrl <- clusters$names
m1$group = clusters$membership
attach(m1)
sorted.clusters <- m1[order(ctrl),]
detach(m1)
attach(distinct.nodesCentered)
results_cluster_edge_betweenness_Centered_weighted <- distinct.nodesCentered[order(id),]
detach(distinct.nodesCentered)
#results_cluster_edge_betweenness_Centered_weighted$ctrl = sorted.clusters$ctrl
results_cluster_edge_betweenness_Centered_weighted$group = sorted.clusters$group
attach(results_betweenness_Centered)
results_betweenness_Centered <- results_betweenness_Centered[order(id),]
detach(results_betweenness_Centered)
results_cluster_edge_betweenness_Centered_weighted$value = results_betweenness_Centered$value 
attach(results_cluster_edge_betweenness_Centered_weighted)
results_cluster_edge_betweenness_Centered_weighted <- results_cluster_edge_betweenness_Centered_weighted[order(-value),]
detach(results_cluster_edge_betweenness_Centered_weighted)
results_cluster_edge_betweenness_Centered_weighted[1,5]=0 # make crawled.io a small node
sink(paste("results_cluster_edge_betweenness_Centered_weighted.json"))
cat(toJSON(results_cluster_edge_betweenness_Centered_weighted))
sink()
graph=visNetwork(results_cluster_edge_betweenness_Centered_weighted, distinct.edgesCentered, width = "1600", height = "825") %>%
  visOptions(selectedBy = list(variable = "group", selected = "5")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visEdges(arrows = "from") %>%
  visEdges(arrows = "to") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_Weighted.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_Weighted.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_Weighted.html")

distinct.edgesCentered$width <- 1+distinct.edgesCentered$n/8 # line width EDGES
attach(distinct.edgesCentered)
distinct.edgesCentered <- distinct.edgesCentered[order(-width),]
detach(distinct.edgesCentered)
distinct.edgesCentered[1,4]=0 # make sites visited a small edge

graph=visNetwork(results_cluster_edge_betweenness_Centered_weighted, distinct.edgesCentered, width = "1600", height = "825") %>%
  visOptions(selectedBy = list(variable = "group", selected = "5")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visEdges(arrows = "from") %>%
  visEdges(arrows = "to") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_Weighted_nodes_edges.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_Weighted_nodes_edges.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_Weighted_nodes_edges.html")


# ----------------------------------------------------------------------
# lookup categories CENTERED Group1
headers = results_cluster_edge_betweenness_Centered_weighted
bugs <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/133c6ebd4a6300ec5c32/raw/ed41c0583c55cd0070fddcae388acb77711d83da/bugs.json'))
tags <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/2b016ceb2278ea0fa381/raw/639f8ebb85a153fd151cfefbd22019510d5648db/tags.json'))
lsos <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/99229a6d0edcda66d847/raw/067eade239afb942e0207ff44c26278df77ac6e4/lsos.json'))

# proces each domain pattern
cpq = proc.time()
classified=1
VERBOSE=0

# normalize headers, remove subdomain
# TODO
for (row in 1:nrow(headers)) {
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
  } else {
    b=paste(a[1], a[2], sep=".")
  }
  #  headers[row,1] = b
  headers[row,2] = b
}

node.frame = as.data.frame(matrix(ncol=7, nrow(headers)))
names(node.frame) = c("domain", "lso_name", "lso_type", "bugs_name", "bugs_type", "tag_type", "tags_dsc")
for (row in 1:nrow(headers)) {
  #for (row in 42:42) {
  lso=FALSE
  bug=FALSE
  tag=FALSE
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
    c=paste(".", b, sep="")
  } else {
    b=paste(a[1], a[2], sep=".")
    c=paste(".", b, sep="")
  }
  
  # do for all domain pattern
  for (i in 1:length(lsos$lsos$pattern)) {
    
    # lookup regex pattern
    if (regexpr(lsos$lsos$pattern[i],b)!=-1 || regexpr(lsos$lsos$pattern[i],c)!=-1){
      if (VERBOSE==1||VERBOSE==2) {cat(i, regexpr(lsos$lsos$pattern[i],b), b, regexpr(lsos$lsos$pattern[i],c), c, lsos$lsos$pattern[i], "\n")}
      if (VERBOSE==2) {cat("lsos[i]: ", i, "\n")}
      if (VERBOSE==2) {cat("lso_id: ", lsos$lsos$id[i], "\n")}
      if (VERBOSE==2) {cat("lso_name: ", lsos$lsos$name[i], "\n")}
      if (VERBOSE==2) {cat("lso_type: ", lsos$lsos$type[i], "\n")}
      if (VERBOSE==2) {cat("lso_cid: ", lsos$lsos$cid[i], "\n")}
      if (VERBOSE==2) {cat("lso_aid: ", lsos$lsos$aid[i], "\n")}
      
      # node classification: lso features
      node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], "","","","")
      lso=TRUE
      x=a[1]
      y=a[2]
      z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
      r=z[1]
      
      p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
      if (length(p)!=0){
        if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
        if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
        if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
        if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
        
        # node classification: bug features
        node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat,"","")
        bug=TRUE
        
        # lookup tag for each tag-id
        q=as.character(unlist(bugs$apps[[p]]$tag))
        if (length(q)!=0){
          for (k in 1:length(q)) {
            for (j in 1:length(tags$tags$id)) {
              if (q[k]==tags$tags$id[j]){
                if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
                
                # node classification: tag features
                node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
                classified=classified+1
                tag=TRUE
              }
            }
          }
        }
      }
    }
  }
  if(lso!=TRUE){
    x=a[1]
    y=a[2]
    b=paste(a[1], a[2], sep=".")
    z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
    r=z[1]
    
    p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
    if (length(p)!=0){
      if (VERBOSE==1||VERBOSE==2) {cat(b, node.frame[classified,2], node.frame[classified,3], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
      if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
      if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
      
      # node classification: bug features
      node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "", "")
      bug=TRUE
      
      # lookup tag for each tag-id
      q=as.character(unlist(bugs$apps[[p]]$tag))
      if (length(q)!=0){
        for (k in 1:length(q)) {
          for (j in 1:length(tags$tags$id)) {
            if (q[k]==tags$tags$id[j]){
              if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
              
              # node classification: tag features
              node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
              classified=classified+1
              tag=TRUE
            }
          }
        }
      }
    }
  }
  # increase counter for remaining vectors
  if (lso==TRUE && tag!=TRUE) {classified=classified+1}
  if (lso!=TRUE && bug==TRUE && tag!=TRUE) {classified=classified+1}
}
proc.time() - cpq


# convert node.frame to distinct csv
features.frame = as.data.frame(matrix(ncol=3, nrow(node.frame[1])))
names(features.frame) = c("domain", "name", "type")
for (i in 1:nrow(node.frame[1])) {
  #for (i in 1:10) {
  r <- node.frame[i,1] # domain pattern
  s <- node.frame[i,2] # lso_name
  s <- gsub(",", "", s)
  t <- node.frame[i,3] # lso_type
  u <- node.frame[i,4] # bug_name
  v <- node.frame[i,5] # bug_type
  w <- node.frame[i,6] # tag_type
  z=""
  if (s=="") {
    z=paste(r, u, sep=",")
  } else {
    z=paste(r, s, sep=",")
  }
  if (t=="") {
    z=paste(z, v, sep=",")
  } else if (v=="") {
    z=paste(z, t, sep=",")
  } else{
    z=paste(z, t, sep=",")
  }
  a=unlist(strsplit(as.character(z), "[,]"))
  features.frame[i,] = c(a[1], a[2], a[3])
}

# uniq
distinct.features <- features.frame[!duplicated(features.frame$domain),]

# update header categories
books <- as.data.frame.matrix(distinct.features)
authors <- as.data.frame.matrix(headers)
(m1 <- merge(authors, books, by.x = "label", by.y = "domain", all = TRUE)) # merge results back
m1$name = NULL
names(m1)[4] <- "old_group"
names(m1)[6] <- "group"
m1[c("group")][is.na(m1[c("group")])] <- "unknown"
m1$title = m1$id # tooltips
m1$label = m1$id # tooltips

# scrub all groups > 1 to other_groups
for (row in 1:nrow(m1)) {
  if (m1[row,4] != 1) {
    m1[row,6] = "other_groups"
  }
}
m1$old_group = NULL
#m1$value = NULL
distinct.edgesCentered$width=NULL

graph=visNetwork(m1, distinct.edgesCentered, width = "1600", height = "825") %>%
  visLegend() %>%
  visOptions(selectedBy = list(variable = "group", selected = "unknown")) %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_group1.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_group1.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_group1.html")

distinct.edgesCentered$width <- 1+distinct.edgesCentered$n/8 # line width EDGES
attach(distinct.edgesCentered)
distinct.edgesCentered <- distinct.edgesCentered[order(-width),]
detach(distinct.edgesCentered)
distinct.edgesCentered[1,4]=0 # make sites visited a small edge

graph=visNetwork(m1, distinct.edgesCentered, width = "1600", height = "825") %>%
  visLegend() %>%
  visOptions(selectedBy = list(variable = "group", selected = "unknown")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_group1_weighted_edges.html", selfcontained = FALSE)
#distinct.edgesCentered$width = NULL
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_group1_weighted_edges.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_group1_weighted_edges.html")


# ----------------------------------------------------------------------
# lookup categories
# cleanup all variables
rm(list=ls())

# --- experimental --------
edges <- fromJSON("edges.json")
nodes <- fromJSON("nodes.json")
distinct.edges <- fromJSON("distinctEdges.json")
distinct.nodes <- fromJSON("distinctNodes.json")
edgesCentered <- fromJSON("edgesCentered.json")
nodesCentered <- fromJSON("nodesCentered.json")
distinct.edgesCentered <- fromJSON("distinctEdgesCentered.json")
distinct.nodesCentered <- fromJSON("distinctNodesCentered.json")
headers = distinct.nodes
bugs <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/133c6ebd4a6300ec5c32/raw/ed41c0583c55cd0070fddcae388acb77711d83da/bugs.json'))
tags <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/2b016ceb2278ea0fa381/raw/639f8ebb85a153fd151cfefbd22019510d5648db/tags.json'))
lsos <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/99229a6d0edcda66d847/raw/067eade239afb942e0207ff44c26278df77ac6e4/lsos.json'))

# proces each domain pattern
cpq = proc.time()
classified=1
VERBOSE=0

# normalize headers, remove subdomain
# TODO
for (row in 1:nrow(headers)) {
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
  } else {
    b=paste(a[1], a[2], sep=".")
  }
  #  headers[row,1] = b
  headers[row,2] = b
}

node.frame = as.data.frame(matrix(ncol=7, nrow(headers)))
names(node.frame) = c("domain", "lso_name", "lso_type", "bugs_name", "bugs_type", "tag_type", "tags_dsc")
for (row in 1:nrow(headers)) {
  #for (row in 42:42) {
  lso=FALSE
  bug=FALSE
  tag=FALSE
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
    c=paste(".", b, sep="")
  } else {
    b=paste(a[1], a[2], sep=".")
    c=paste(".", b, sep="")
  }
  
  # do for all domain pattern
  for (i in 1:length(lsos$lsos$pattern)) {
    
    # lookup regex pattern
    if (regexpr(lsos$lsos$pattern[i],b)!=-1 || regexpr(lsos$lsos$pattern[i],c)!=-1){
      if (VERBOSE==1||VERBOSE==2) {cat(i, regexpr(lsos$lsos$pattern[i],b), b, regexpr(lsos$lsos$pattern[i],c), c, lsos$lsos$pattern[i], "\n")}
      if (VERBOSE==2) {cat("lsos[i]: ", i, "\n")}
      if (VERBOSE==2) {cat("lso_id: ", lsos$lsos$id[i], "\n")}
      if (VERBOSE==2) {cat("lso_name: ", lsos$lsos$name[i], "\n")}
      if (VERBOSE==2) {cat("lso_type: ", lsos$lsos$type[i], "\n")}
      if (VERBOSE==2) {cat("lso_cid: ", lsos$lsos$cid[i], "\n")}
      if (VERBOSE==2) {cat("lso_aid: ", lsos$lsos$aid[i], "\n")}
      
      # node classification: lso features
      node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], "","","","")
      lso=TRUE
      x=a[1]
      y=a[2]
      z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
      r=z[1]
      
      p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
      if (length(p)!=0){
        if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
        if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
        if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
        if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
        
        # node classification: bug features
        node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat,"","")
        bug=TRUE
        
        # lookup tag for each tag-id
        q=as.character(unlist(bugs$apps[[p]]$tag))
        if (length(q)!=0){
          for (k in 1:length(q)) {
            for (j in 1:length(tags$tags$id)) {
              if (q[k]==tags$tags$id[j]){
                if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
                
                # node classification: tag features
                node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
                classified=classified+1
                tag=TRUE
              }
            }
          }
        }
      }
    }
  }
  if(lso!=TRUE){
    x=a[1]
    y=a[2]
    b=paste(a[1], a[2], sep=".")
    z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
    r=z[1]
    
    p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
    if (length(p)!=0){
      if (VERBOSE==1||VERBOSE==2) {cat(b, node.frame[classified,2], node.frame[classified,3], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
      if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
      if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
      
      # node classification: bug features
      node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "", "")
      bug=TRUE
      
      # lookup tag for each tag-id
      q=as.character(unlist(bugs$apps[[p]]$tag))
      if (length(q)!=0){
        for (k in 1:length(q)) {
          for (j in 1:length(tags$tags$id)) {
            if (q[k]==tags$tags$id[j]){
              if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
              
              # node classification: tag features
              node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
              classified=classified+1
              tag=TRUE
            }
          }
        }
      }
    }
  }
  # increase counter for remaining vectors
  if (lso==TRUE && tag!=TRUE) {classified=classified+1}
  if (lso!=TRUE && bug==TRUE && tag!=TRUE) {classified=classified+1}
}
proc.time() - cpq


# convert node.frame to distinct csv
features.frame = as.data.frame(matrix(ncol=3, nrow(node.frame[1])))
names(features.frame) = c("domain", "name", "type")
for (i in 1:nrow(node.frame[1])) {
  #for (i in 1:10) {
  r <- node.frame[i,1] # domain pattern
  s <- node.frame[i,2] # lso_name
  s <- gsub(",", "", s)
  t <- node.frame[i,3] # lso_type
  u <- node.frame[i,4] # bug_name
  v <- node.frame[i,5] # bug_type
  w <- node.frame[i,6] # tag_type
  z=""
  if (s=="") {
    z=paste(r, u, sep=",")
  } else {
    z=paste(r, s, sep=",")
  }
  if (t=="") {
    z=paste(z, v, sep=",")
  } else if (v=="") {
    z=paste(z, t, sep=",")
  } else{
    z=paste(z, t, sep=",")
  }
  a=unlist(strsplit(as.character(z), "[,]"))
  features.frame[i,] = c(a[1], a[2], a[3])
}

# uniq
distinct.features <- features.frame[!duplicated(features.frame$domain),]

# update header categories
books <- as.data.frame.matrix(distinct.features)
authors <- as.data.frame.matrix(headers)
(m1 <- merge(authors, books, by.x = "label", by.y = "domain", all = TRUE)) # merge results back
m1$name = NULL
names(m1)[3] <- "group"
m1[c("group")][is.na(m1[c("group")])] <- "unknown"
m1$title = m1$id # tooltips
m1$label = m1$id # tooltips
#ig = graph_from_data_frame(centeredEedges, directed=F)
#m1$value = betweenness(ig)
sink(paste("M1_distinctEdges.json"))
cat(toJSON(m1))
sink()
graph=visNetwork(m1, distinct.edges, width = "1600", height = "825") %>%
  visLegend() %>%
  visOptions(selectedBy = list(variable = "group", selected = "unknown")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_lookupCategories.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_lookupCategories.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_lookupCategories.html")


# ----------------------------------------------------------------------
# lookup categories CENTERED
# cleanup all variables
rm(list=ls())

# --- experimental --------
edges <- fromJSON("edges.json")
nodes <- fromJSON("nodes.json")
distinct.edges <- fromJSON("distinctEdges.json")
distinct.nodes <- fromJSON("distinctNodes.json")
edgesCentered <- fromJSON("edgesCentered.json")
nodesCentered <- fromJSON("nodesCentered.json")
distinct.edgesCentered <- fromJSON("distinctEdgesCentered.json")
distinct.nodesCentered <- fromJSON("distinctNodesCentered.json")
headers = distinct.nodesCentered
bugs <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/133c6ebd4a6300ec5c32/raw/ed41c0583c55cd0070fddcae388acb77711d83da/bugs.json'))
tags <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/2b016ceb2278ea0fa381/raw/639f8ebb85a153fd151cfefbd22019510d5648db/tags.json'))
lsos <- fromJSON(getURL('https://gist.githubusercontent.com/rvaneijk/99229a6d0edcda66d847/raw/067eade239afb942e0207ff44c26278df77ac6e4/lsos.json'))

# proces each domain pattern
cpq = proc.time()
classified=1
VERBOSE=0

# normalize headers, remove subdomain
# TODO
for (row in 1:nrow(headers)) {
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
  } else {
    b=paste(a[1], a[2], sep=".")
  }
  #  headers[row,1] = b
  headers[row,2] = b
}

node.frame = as.data.frame(matrix(ncol=7, nrow(headers)))
names(node.frame) = c("domain", "lso_name", "lso_type", "bugs_name", "bugs_type", "tag_type", "tags_dsc")
for (row in 1:nrow(headers)) {
  #for (row in 42:42) {
  lso=FALSE
  bug=FALSE
  tag=FALSE
  a=unlist(strsplit(as.character(headers[row,1]), "[.]"))
  if (length(a)==3){
    b=paste(a[2], a[3], sep=".")
    c=paste(".", b, sep="")
  } else {
    b=paste(a[1], a[2], sep=".")
    c=paste(".", b, sep="")
  }
  
  # do for all domain pattern
  for (i in 1:length(lsos$lsos$pattern)) {
    
    # lookup regex pattern
    if (regexpr(lsos$lsos$pattern[i],b)!=-1 || regexpr(lsos$lsos$pattern[i],c)!=-1){
      if (VERBOSE==1||VERBOSE==2) {cat(i, regexpr(lsos$lsos$pattern[i],b), b, regexpr(lsos$lsos$pattern[i],c), c, lsos$lsos$pattern[i], "\n")}
      if (VERBOSE==2) {cat("lsos[i]: ", i, "\n")}
      if (VERBOSE==2) {cat("lso_id: ", lsos$lsos$id[i], "\n")}
      if (VERBOSE==2) {cat("lso_name: ", lsos$lsos$name[i], "\n")}
      if (VERBOSE==2) {cat("lso_type: ", lsos$lsos$type[i], "\n")}
      if (VERBOSE==2) {cat("lso_cid: ", lsos$lsos$cid[i], "\n")}
      if (VERBOSE==2) {cat("lso_aid: ", lsos$lsos$aid[i], "\n")}
      
      # node classification: lso features
      node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], "","","","")
      lso=TRUE
      x=a[1]
      y=a[2]
      z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
      r=z[1]
      
      p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
      if (length(p)!=0){
        if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
        if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
        if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
        if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
        
        # node classification: bug features
        node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat,"","")
        bug=TRUE
        
        # lookup tag for each tag-id
        q=as.character(unlist(bugs$apps[[p]]$tag))
        if (length(q)!=0){
          for (k in 1:length(q)) {
            for (j in 1:length(tags$tags$id)) {
              if (q[k]==tags$tags$id[j]){
                if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
                
                # node classification: tag features
                node.frame[classified,] = c(b, lsos$lsos$name[i], lsos$lsos$type[i], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
                classified=classified+1
                tag=TRUE
              }
            }
          }
        }
      }
    }
  }
  if(lso!=TRUE){
    x=a[1]
    y=a[2]
    b=paste(a[1], a[2], sep=".")
    z=as.character(unlist(bugs$patterns$host[[y]][[x]]))
    r=z[1]
    
    p=as.character(unlist(bugs$bugs[[r]]))   # may contain aid
    if (length(p)!=0){
      if (VERBOSE==1||VERBOSE==2) {cat(b, node.frame[classified,2], node.frame[classified,3], bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_aid: ", p, "\n")}
      if (VERBOSE==2) {cat("bugs_name: ",bugs$apps[[p]]$name, "\n")}
      if (VERBOSE==2) {cat("bugs_type: ",bugs$apps[[p]]$cat, "\n")}
      if (VERBOSE==2) {cat("bugs_tag: ",bugs$apps[[p]]$tag, "\n")}  # may contain tag-id(s)
      
      # node classification: bug features
      node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, "", "")
      bug=TRUE
      
      # lookup tag for each tag-id
      q=as.character(unlist(bugs$apps[[p]]$tag))
      if (length(q)!=0){
        for (k in 1:length(q)) {
          for (j in 1:length(tags$tags$id)) {
            if (q[k]==tags$tags$id[j]){
              if (VERBOSE==2) {cat("tag[", q[k], "]", tags$tags$name[j], ":", tags$tags$description[j], "\n")}
              
              # node classification: tag features
              node.frame[classified,] = c(b, "", "", bugs$apps[[p]]$name, bugs$apps[[p]]$cat, tags$tags$name[j], tags$tags$description[j])
              classified=classified+1
              tag=TRUE
            }
          }
        }
      }
    }
  }
  # increase counter for remaining vectors
  if (lso==TRUE && tag!=TRUE) {classified=classified+1}
  if (lso!=TRUE && bug==TRUE && tag!=TRUE) {classified=classified+1}
}
proc.time() - cpq


# convert node.frame to distinct csv
features.frame = as.data.frame(matrix(ncol=3, nrow(node.frame[1])))
names(features.frame) = c("domain", "name", "type")
for (i in 1:nrow(node.frame[1])) {
  #for (i in 1:10) {
  r <- node.frame[i,1] # domain pattern
  s <- node.frame[i,2] # lso_name
  s <- gsub(",", "", s)
  t <- node.frame[i,3] # lso_type
  u <- node.frame[i,4] # bug_name
  v <- node.frame[i,5] # bug_type
  w <- node.frame[i,6] # tag_type
  z=""
  if (s=="") {
    z=paste(r, u, sep=",")
  } else {
    z=paste(r, s, sep=",")
  }
  if (t=="") {
    z=paste(z, v, sep=",")
  } else if (v=="") {
    z=paste(z, t, sep=",")
  } else{
    z=paste(z, t, sep=",")
  }
  a=unlist(strsplit(as.character(z), "[,]"))
  features.frame[i,] = c(a[1], a[2], a[3])
}

# uniq
distinct.features <- features.frame[!duplicated(features.frame$domain),]

# update header categories
books <- as.data.frame.matrix(distinct.features)
sink(paste("books.json"))
cat(toJSON(books))
sink()
authors <- as.data.frame.matrix(headers)
(m1 <- merge(authors, books, by.x = "label", by.y = "domain", all = TRUE)) # merge results back
m1$name = NULL
names(m1)[3] <- "group"
m1[c("group")][is.na(m1[c("group")])] <- "unknown"
m1$title = m1$id # tooltips
m1$label = m1$id # tooltips
#ig = graph_from_data_frame(centeredEedges, directed=F)
#m1$value = betweenness(ig)
sink(paste("M1_distinctEdgesCentered.json"))
cat(toJSON(m1))
sink()
graph=visNetwork(m1, distinct.edgesCentered, width = "1600", height = "825") %>%
  visLegend() %>%
  visOptions(selectedBy = list(variable = "group", selected = "unknown")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = TRUE)
saveWidget(graph, file="shiny_Centered_lookupCategories.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_Centered_lookupCategories.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_Centered_lookupCategories.html")


# ---------------------------------------------------
# subgraph groep 8
ig = graph_from_data_frame(distinct.edgesCentered, directed=F)
G1 <- fromJSON("results_cluster_edge_betweenness_Centered_weighted.json")
nodes.Group8 <- subset(G1, group=="8")
books <- as.data.frame.matrix(nodes.Group8)
authors <- as.data.frame.matrix(distinct.edgesCentered)
m1=NULL
(m1 <- merge(authors, books, by.x = "from", by.y = "id", all = TRUE)) # merge results back
books <- as.data.frame.matrix(nodes.Group8)
m1<-subset(m1, !is.na(label))
m1$title=NULL
m1$group=NULL
m1$value=NULL
nodes.Group8 <- subset(G1, group=="8")
nodes.Group8$group=NULL
books <- as.data.frame.matrix(nodes.Group8)
authors <- as.data.frame.matrix(distinct.edgesCentered)
m2=NULL
(m2 <- merge(authors, books, by.x = "to", by.y = "id", all = TRUE)) # merge results back
m2<-subset(m2, !is.na(label))
m2$title=NULL
m2$group=NULL
m2$value=NULL
edges.Group8 <- rbind(m1, m2) 
edges.Group8$label=NULL
edges.Group8 <- unique( edges.Group8[ , 1:3 ] )
edges.Group8<-subset(edges.Group8, !is.na(n))

ig = graph_from_data_frame(edges.Group8, directed=F)
clusters = cluster_edge_betweenness(ig)
m1 = nodes.Group8
m1[,1] <- NA
m1$id=NULL
m1$label=NULL
m1$title=NULL
m1$ctrl <- clusters$names
m1$group = clusters$membership
attach(m1)
sorted.clusters <- m1[order(ctrl),]
detach(m1)
attach(edges.Group8)
results_cluster_edge_betweenness_Centered_weighted_Group8 <- edges.Group8[order(id),]
detach(distinct.nodesCentered)
#results_cluster_edge_betweenness_Centered_weighted_Group8$ctrl = sorted.clusters$ctrl
results_cluster_edge_betweenness_Centered_weighted_Group8$group = sorted.clusters$group

# ----------
graph=visNetwork(results_cluster_edge_betweenness_Centered_weighted_Group8, edges.Group8, width = "1600", height = "825") %>%
  visOptions(selectedBy = list(variable = "group", selected = "8")) %>%
  visEvents(stabilizationProgress = "function(params) {
            var maxWidth = 1000;
            var minWidth = 20;
            var widthFactor = params.iterations/params.total;
            var width = Math.max(minWidth,maxWidth * widthFactor);
            
            document.getElementById('progressbar').max = maxWidth;
            document.getElementById('progressbar').value = width;}") %>%
  visEvents(stabilizationIterationsDone = "function() {
            document.getElementById('progressbar').remove();
            }") %>%
  visEdges(arrows = "from, to") %>%
  visConfigure(enabled = TRUE) %>%
  visInteraction(hideEdgesOnDrag = FALSE, tooltipStay=10000, multiselect=TRUE, navigationButtons=TRUE, zoomView = TRUE, keyboard = FALSE)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_Weighted_Group8.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_Weighted_Group8.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_Weighted_Group8.html")


graph=visNetwork(nodes.Group8, edges.Group8, width = "1600", height = "825") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)
saveWidget(graph, file="shiny_cluster_edge_betweenness_Centered_Weighted_Group8_circle.html", selfcontained = FALSE)
add_progress  <- readLines("shiny_cluster_edge_betweenness_Centered_Weighted_Group8_circle.html")
added_progres  <- gsub(pattern = '<div id="htmlwidget_container">', replace = '<progress id="progressbar" value="0" max="100"></progress><div id="htmlwidget_container">', x = add_progress)
writeLines(added_progres, con="shiny_cluster_edge_betweenness_Centered_Weighted_Group8_circle.html")
