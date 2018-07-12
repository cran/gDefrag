edge.creation <-
function(nodes, land_polyg, min_length = 0, min_pol_area = 0, plot = TRUE, shape = FALSE, shape_name_edges = "shape_edges"){

autoID <- NULL
road_length <- NULL
node_A <- NULL
node_B <- NULL

  if (!(all.equal(nodes@proj4string@projargs, land_polyg@proj4string@projargs)))
    stop("Input maps have different CRS.")

    message("Extracting road lines from polygon borders...")
    borders <- gDifference(as(land_polyg, "SpatialLines"), as(gUnaryUnion(land_polyg), "SpatialLines"), byid = TRUE)
    road_L <- disaggregate(gLineMerge(gUnion(borders, borders)))

  message("Creating edges...")

  node_T <- nodes@data 
  
  adj <- as.data.frame(gTouches(land_polyg, byid = TRUE))  
  rownames(adj) <- colnames(adj) <- as.character(1:length(land_polyg))  
  adj <- upper.tri(adj, diag = FALSE) * adj
  adj_names <- rownames(adj)
  adj <- as.matrix(adj)
  adj <- array(as.logical(adj), dim(adj))
  rownames(adj)<- adj_names
  colnames(adj) <- adj_names
  ID1 <- which(adj,TRUE)
  ID1[,1] <- as.numeric(rownames(adj)[ID1[,1]])
  ID1[,2] <- as.numeric(colnames(adj)[ID1[,2]])

  rownames(ID1) <- as.character(1:nrow(ID1))
  for (i in rownames(ID1)) {
    P1 <- land_polyg[ID1[i, 1], ]
    P2 <- land_polyg[ID1[i, 2], ]
    if (!inherits(gIntersection(P1, P2, byid = TRUE), "SpatialLines"))
      ID1 <- ID1[-which(rownames(ID1) == i), ]
  }  
  
  
  dist1 <- dist(node_T)
  dist1 <- as.data.frame(as.matrix(dist1))
  rownames(dist1) <- adj_names
  colnames(dist1) <- adj_names

  d2 <- rep(NA,nrow(ID1))

  for(i in 1:nrow(ID1)){

    row1 <- ID1[i,]
    ID_A <- row1[1]
    ID_B <- row1[2]

    d1 <- dist1[as.character(ID_A), as.character(ID_B)]

    d2[i] <- d1

  }

  ID1 <- cbind(ID1, d2)


  x1_A <- rep(NA, nrow(ID1))
  y1_A <- rep(NA, nrow(ID1))

  x1_B <- rep(NA, nrow(ID1))
  y1_B <- rep(NA, nrow(ID1))

  area_A <- rep(NA, nrow(ID1))
  area_B <- rep(NA, nrow(ID1))


  for(i in 1:nrow(ID1)){


    row2 <- ID1[i,]
    nodeA <- row2[1]
    nodeB <- row2[2]

    lineA <- which(node_T$node_ID == nodeA)
    lineB <- which(node_T$node_ID == nodeB)
    nodeA <- node_T[lineA, ]
    nodeB <- node_T[lineB, ]

    x1_A[i] <- nodeA[2]
    y1_A[i] <- nodeA[3]

    x1_B[i] <- nodeB[2]
    y1_B[i] <- nodeB[3]

    area_A[i] <- nodeA[4]
    area_B[i] <- nodeB[4]

  }

  x1_A <- as.numeric(x1_A)
  y1_A <- as.numeric(y1_A)

  x1_B <- as.numeric(x1_B)
  y1_B <- as.numeric(y1_B)

  area_A <- as.numeric(area_A)
  area_B <- as.numeric(area_B)

  edge_T <- cbind(ID1, x1_A, y1_A, x1_B, y1_B, area_A, area_B)
  rownames(edge_T) <- 1:nrow(edge_T)
  edge_T <- as.data.frame(edge_T)

  colnames(edge_T) <- c("node_A", "node_B", "distance", "x_node_A", "y_node_A", "x_node_B", "y_node_B", "value_A", "value_B")

  out_lines <- vector("list", nrow(edge_T))
  for(i in 1:nrow(edge_T)){
    l0 <- edge_T[i,]
    xA <- as.numeric(l0["x_node_A"])
    yA <- as.numeric(l0["y_node_A"])
    xB <- as.numeric(l0["x_node_B"])
    yB <- as.numeric(l0["y_node_B"])
    pts <- matrix(c(xA, yA, xB, yB), nrow=2, ncol=2, byrow=TRUE)
    l1 <- Line(pts)
    l2 <- Lines(l1, ID=rownames(edge_T)[i])
    out_lines[[i]] <- l2
  }

  edge_l <- SpatialLines(out_lines) 
  edge_L <- SpatialLinesDataFrame(edge_l, data = edge_T)  
  edge_L@proj4string@projargs <- nodes@proj4string@projargs 


  if (class(road_L) == "SpatialLinesDataFrame")  
    road_L@data$autoID <- 1:nrow(road_L@data)  
  else if (class(road_L) == "SpatialLines")  
    road_L <- SpatialLinesDataFrame(road_L, data = data.frame(autoID = 1:length(road_L))) 

  land_polyg@data$autoID <- 1:nrow(land_polyg@data) 
  line_dists <- matrix(data = NA, nrow = length(road_L), ncol = length(land_polyg))

  for (l in 1:length(road_L)) {  
    road <- subset(road_L, autoID == l)
    l_centr <- SpatialLinesMidPoints(road)
    for (p in 1:length(land_polyg)) {
      line_dists[l, p] <- gDistance(subset(land_polyg, autoID == p), l_centr)
    }  
  }  


  line_neighbours <- vector("list", length(road_L))
  for (d in 1:nrow(line_dists)) {
    line_neighbours[[d]] <- which(line_dists[d, ] %in% sort(line_dists[d, ])[1:2]) 
	}  

  names(line_neighbours) <- as.character(1:length(line_neighbours))


  edge_road_IDs <- vector("list", length(edge_L))
  for (e in 1:length(edge_L)) {
    road_index <- which(sapply(line_neighbours, setequal, t(edge_L@data[e, c("node_A", "node_B")])))

    edge_road_IDs[[e]] <- as.integer(names(line_neighbours)[road_index])
  }  


  edge_L@data$road_length <- NA
  for (i in 1:nrow(edge_L@data)) {
    edge_road <- road_L[road_L$autoID %in% edge_road_IDs[[i]], ]
    edge_road_sl <- as(edge_road, "SpatialLines")  
    edge_L@data[i, "road_length"] <- sum(SpatialLinesLengths(edge_road_sl))
  }  


  edge_L <- subset(edge_L, road_length >= min_length)

  big_pol_IDs <- node_T[node_T$pol_area >= min_pol_area, ]$node_ID
  edge_L <- subset(edge_L, node_A %in% big_pol_IDs & node_B %in% big_pol_IDs)


  if (shape==TRUE){
    suppressWarnings(writeOGR(edge_L, ".", shape_name_edges, driver="ESRI Shapefile",overwrite_layer=TRUE))
    message("Shapefile created! Check the working directory, please.")
  }

  if (plot) {
    plot(land_polyg)
    plot(edge_L, col = "blue", add = TRUE)
  }

  message("Done!")
  return(edge_L)
}
