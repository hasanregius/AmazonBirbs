############################
# Thamnomanes network anal #
############################

# Loading the required packages and data ----
library(ergm)         # Package for ERGMs, which includes Bernoulli models
library(statnet)      # For handling network class objects
library(igraph)       # Visualization of network objects
library(RSQLite)      # SQLite Integration in R
library(DBI)

# Loading and preparing the network data ----
## Reading the csv file ----
net15s = as.data.frame(read.csv("network_15s.csv"))
## Creating a database in MySQL -----
db_connection = dbConnect(RSQLite::SQLite(), ":THAR_network_anal")
dbWriteTable(db_connection, "net15s_matrix", net15s)
### Checking the database -----
dbReadTable(db_connection, "net15s_matrix")
# We're gucci. Moving on.
### Creating a table again for some reason ----
net15s = dbReadTable(db_connection, "net15s_matrix", 
                     row.names = pkgconfig::get_config("RSQLite::row.names.table", FALSE),
                     check.names = TRUE)
net15s = as.matrix(net15s)

# Creating a network object ----
num_nodes = 8 # We have 8 species total, and so 8 nodes 
network_matrix = matrix(round(runif(num_nodes*num_nodes)),
                        nrow = num_nodes, ncol = num_nodes)
diag(network_matrix) = 0 # Self-edges should be zero for our experiments
net = as.network(x = network_matrix, # Creating the network object
                 directed = FALSE, loops = FALSE, matrix.type = "adjacency")
network.vertex.names(net) = c("HARU","LEAM","MYAX","MYME","PLPL","THAR",
                              "THSY","XIEL") # Abbreviated genus and species


# Fitting the ERGM model, based on Farine et al. (2017) ----
ergm_model = ergm(net ~ edges)    
# Only setting null based on the edges because it's the only
# method that pertains to our experimental design
summary(ergm_model)

# To print the output
# sink("ergm.txt")
# summary(ergm_model)
# sink()

# Creating the visualization ----
net15s_nodes = data.frame(id = colnames(net15s)[2:9]) 
# Need to derive our node list first
# This is where we'd list attributes if we had any
# First col is X, which is row number/name in R
net15s = as.matrix(net15s)
rownames(net15s) = net15s[,1]
net15s = net15s[c(1,2,3,4,6,8),c(1,2,3,4,6,8)] # Confidentiality pruning
network = graph_from_adjacency_matrix(net15s, weighted = T, diag = F) 
# Plot
E(network)$width = E(network)$weight # Adding weights
# Run to save
tiff("THAR_net.tiff", width = 3000, height = 3000, units="px", res=400)
plot(network, vertex.shape = "none", vertex.label.cex = 1,
     vertex.size = 30, edge.lty = 1, edge.arrow.size = 0,
     vertex.color = "white", vertex.frame.color = "white",
     vertex.label.font = 2, vertex.label.color = "black",
     vertex.label.family = "Helvetica", layout = layout_in_circle)
dev.off()

