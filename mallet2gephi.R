library(tidyverse)

input_file <- file.choose()

# Open the file once with default column names, just so we can see how many columns (topics) there are.
input <- read.csv(input_file, header = FALSE, sep="\t")
num_topics <- length(input) - 2

# Since we have to refer to nodes by their ID in the edges table, 
# we use this function to take a label and return the ID of the corresponding node from the node table. 
get_node_id <- function(nodes, lab) {
  result <- subset(nodes, label==lab)
  return (as.numeric(result[1]$id))
}

# Just a way to generate nice names for our columns since the input file doesn't provide  a header.
# If you leave out the col.names argument, then the colums will just be named V1, V2, .. Vn
topics <- list()
for(i in 1:num_topics) {
  topics[i] = paste("topic", i, sep="")
} 
colnames <- c('id', 'file', topics)

# Read in the file
# sep = "\t" here tells R that the file is tab separated.
input <- read.csv(input_file, header = FALSE, sep="\t", col.names = colnames)
input$file <- as.character(input$file) # By default interpreted as a factor, we just what string values.
input$file <- basename(input$file) # We only care about the file name itself, not the whole path. The basename funciton strips all of the path info out of the text.
files <- input$file

# Build Nodes table
# In a two-mode (bipartite) network, the nodes table needs three columns:
# Id, Lablel, Type
types <- c(rep("file", length(files)), rep("topic", length(topics)))
labels <- c(files, topics)
ids <- seq(0, length(labels) -1)
nodes <- tibble(id = seq(0, length(labels) -1), label = labels, type = types)

# Build Edges table, which is composed of three columns - Source id, Target id, and the Weight
# In the input file, each row represents one file with one value for the % match for each topic. Eg: 
# id   file      Topic 1     Topic 2       ... Topic N
# 0    1660.rtf  0.005547615 6.268492e-06  ... 0.005221654

# Basic structure for this is as follows: 
# * Read each row
# * For each column starting at column 3, and create one row in the output dataset for each topic
# * Look up the ID of the file node in the nodes table
# * Look up the ID of the topic node in the nodes table
# * Add the source, target, and weight values to the output sets for the 
sources <- vector()
targets <- vector()
weights <- vector()
for (i in 1:nrow(input)) { # Once for each file. Each file has one row in the input file
  row <- input[i,]
  for (j in 3:length(input)) { # Once for each topic column.
    fn <- as.character(row["file"])
    sid <- get_node_id(nodes, fn)
    tid <- get_node_id(nodes, as.character(colnames[j]))
    sources <- append(sources, sid)
    targets <- append(targets, tid)
    weights <- append(weights, row[j])
  }
}

edges <- tibble(source = sources, target = targets, weight = weights)

output_base <- unlist(strsplit(input_file, '.', fixed = TRUE))[1]

# The CSV writer throws errors for some column types, 
# so we're explicitly converting everything to character data (text) before we write the output file.
edges <- apply(edges, 2, as.character)
outfile_edges <- paste(output_base, '-edges.csv', sep="")
write.csv(edges, outfile_edges, row.names=FALSE)

nodes <- apply(nodes, 2, as.character)
outfile_nodes <- paste(output_base, '-nodes.csv', sep="")
write.csv(nodes, outfile_nodes, row.names=FALSE)


