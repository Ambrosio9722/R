rm(list = ls())
cat("\014")  # clear console
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)

library(igraph)

file_name <- 'exemplo_sala_novo.csv'
num_nodes <- 30
num_arcs <- 30

max_connections <- ceiling(num_nodes * (num_nodes - 1) / 2)

# create a matrix to keep track of the connections between nodes
connections <- matrix(0, nrow = num_nodes, ncol = num_nodes)

df <- data.frame(origem = numeric(num_arcs),
                 destino = numeric(num_arcs),
                 custo = numeric(num_arcs))

for (i in 1:num_arcs) {
  repeat {
    origem <- sample(1:num_nodes, size = 1)
    destino <- sample(1:num_nodes, size = 1)
    if (origem != destino && !any(df$origem == origem & df$destino == destino) &&
        connections[origem, destino] < 1 && connections[destino, origem] < 1) {
      df[i, "origem"] <- origem
      df[i, "destino"] <- destino
      df[i, "custo"] <- round(runif(1, 1, 10), 2)
      connections[origem, destino] <- 1
      connections[destino, origem] <- 1
      break
    }
    if (sum(connections) == max_connections) break  # stop if all possible connections have been made
  }
}

# add arcs to nodes with less than two connections
for (node in 1:num_nodes) {
  num_connections <- sum(connections[node, ])
  if (num_connections < 2) {
    repeat {
      dest_node <- sample(1:num_nodes, size = 1)
      if (node != dest_node && !any(df$origem == node & df$destino == dest_node) &&
          connections[node, dest_node] < 1 && connections[dest_node, node] < 1) {
        df <- rbind(df, data.frame(origem = node, destino = dest_node, custo = round(runif(1, 1, 10), 2)))
        connections[node, dest_node] <- 1
        connections[dest_node, node] <- 1
        break
      }
    }
  }
}


vinicial <- 1
vfinal <- num_nodes

graph <- graph_from_data_frame(df, directed = FALSE)

# Set the color of the nodes
V(graph)$color <- ifelse(V(graph)$name %in% c(vinicial, vfinal), "orange", "white")

# Plot the graph
plot(graph)

#write.csv(df, file_name, row.names = FALSE, sep = ' ')
write.table(df, file = file_name, row.names = FALSE, col.names = TRUE, sep = " ", quote = FALSE)




