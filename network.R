library(igraph)
net15s = read.csv("network_15s.csv")
net15s = as.matrix(net15s)
rownames(net15s) = net15s[,1]
net15s = net15s[,2:9]
net = graph.adjacency(net15s, mode="undirected",
                      weighted=TRUE, diag=FALSE)
thar15s_edge =  graph_from_data_frame(net15s, directed = FALSE)
thar15s_links = as.matrix(get.edgelist(thar15sedge))
net = thar15s_edge
plot(net)

plotz = plot.igraph(net,vertex.label=V(net)$name,
            layout=layout.circle, 
            edge.color="black", edge.width=E(net)$weight)

# Network 30s
net30s = read.csv("network_30s.csv")
net30s = as.matrix(net30s)
rownames(net30s) = net30s[,1]
net30s = net30s[,2:9]
net = graph.adjacency(net30s, mode="undirected", 
                      weighted=TRUE, diag=FALSE)
plotz = plot.igraph(net,vertex.label=V(net)$name,
                    layout=layout.circle, 
                    edge.color="black", edge.width=E(net)$weight)
