# glycan mapping
glycan_color = c(
  'glyWhite'='#FFFFFF',
  'glyBlue'='#0385AE', 'glyGreen'='#058F60',
  'glyYellow'='#FCC326','glyOrange'='#EF6130',
  'glyPink'='#F39EA0', 'glyPurple'='#A15989',
  'glyLightBlue'='#91D3E3', 'glyBrown'='#9F6D55',
  'glyRed'='#C23537'
)

glycan_dict <- list(
  'Hex'=c('Hex','glyWhite'),
  'Glc'=c('Hex','glyBlue'),'Man'=c('Hex','glyGreen'),
  'Gal'=c('Hex','glyYellow'),'Gul'=c('Hex','glyOrange'),
  'Alt'=c('Hex','glyPink'),'All'=c('Hex','glyPurple'),
  'Tal'=c('Hex','glyLightBlue'),'Ido'=c('Hex','glyBrown'),

  'HexNAc'=c('HexNAc','glyWhite'),
  'GlcNAc'=c('HexNAc','glyBlue'),'ManNAc'=c('HexNAc','glyGreen'),
  'GalNAc'=c('HexNAc','glyYellow'),'GulNAc'=c('HexNAc','glyOrange'),
  'AltNAc'=c('HexNAc','glyPink'),'AllNAc'=c('HexNAc','glyPurple'),
  'TalNAc'=c('HexNAc','glyLightBlue'),'IdoNAc'=c('HexNAc','glyBrown'),

  'HexN'=c('HexN','glyWhite','glyWhite'),
  'GlcN'=c('HexN','glyBlue','glyWhite'),'ManN'=c('HexN','glyGreen','glyWhite'),
  'GalN'=c('HexN','glyYellow','glyWhite'),'GulN'=c('HexN','glyOrange','glyWhite'),
  'AltN'=c('HexN','glyPink','glyWhite'),'AllN'=c('HexN','glyPurple','glyWhite'),
  'TalN'=c('HexN','glyLightBlue','glyWhite'),'IdoN'=c('HexN','glyBrown','glyWhite'),

  'HexA'=c('HexA','glyWhite','glyWhite'),
  'GlcA'=c('HexA','glyBlue','glyWhite'),'ManA'=c('HexA','glyGreen','glyWhite'),
  'GalA'=c('HexA','glyYellow','glyWhite'),'GulA'=c('HexA','glyOrange','glyWhite'),
  'AltA'=c('HexA','glyPink','glyWhite'),'AllA'=c('HexA','glyPurple','glyWhite'),
  'TalA'=c('HexA','glyLightBlue','glyWhite'),'IdoA'=c('HexA','glyBrown','glyWhite'),

  'dHex'=c('dHex','glyWhite'),
  'Qui'=c('dHex','glyBlue'),'Rha'=c('dHex','glyGreen'),
  '6dGul'=c('dHex','glyOrange'),'6dAlt'=c('dHex','glyPink'),
  '6dTal'=c('dHex','glyLightBlue'),'Fuc'=c('dHex','glyRed'),
  'FucUp'=c('dHexUp','glyRed'),

  'dHexNAc'=c('dHexNAc','glyWhite','glyWhite'),
  'QuiNAc'=c('dHexNAc','glyBlue','glyWhite'),'RhaNAc'=c('dHexNAc','glyGreen','glyWhite'),
  '6dAltNAc'=c('dHexNAc','glyPink','glyWhite'),'6dTalNAc'=c('dHexNAc','glyLightBlue','glyWhite'),
  'FucNAc'=c('dHexNAc','glyRed','glyWhite'),

  'Pen'=c('Pen','glyWhite'),
  'Ara'=c('Pen','glyGreen'),'Lyx'=c('Pne','glyYellow'),
  'Xyl'=c('Pen','glyOrange'),'Rib'=c('Pen','glyPink'),

  'dNon'=c('dNon','glyWhite'),
  'Kdn'=c('dNon','glyGreen'),'Neu5Ac'=c('dNon','glyPurple'),
  'Neu5Gc'=c('dNon','glyLightBlue'),'Neu'=c('dNon','glyBrown'),
  'Sia'=c('dNon','glyRed'),

  'ddNon'=c('ddNon','glyWhite'),
  'Pse'=c('ddNon','glyGreen'),'Leg'=c('ddNon','glyYellow'),
  'Aci'=c('ddNon','glyPink'),'4eLeg'=c('ddNon','glyLightBlue'),

  'UnKnown'=c('UnKnown','glyWhite'),
  'Bac'=c('UnKnown','glyBlue'),'LDmanHep'=c('UnKnown','glyGreen'),
  'Kdo'=c('UnKnown','glyYellow'),'Dha'=c('UnKnown','glyOrange'),
  'DDmanHep'=c('UnKnown','glyPink'),'MurNAc'=c('UnKnown','glyPurple'),
  'MurNGc'=c('UnKnown','glyLightBlue'),'Mur'=c('UnKnown','glyBrown'),

  'Assigned'=c('Assigned','glyWhite'),
  'Api'=c('Assigned','glyBlue'),'Fru'=c('Assigned','glyGreen'),
  'Tag'=c('Assigned','glyYellow'),'Sor'=c('Assigned','glyOrange'),
  'Psi'=c('Assigned','glyPink')
)

glycan_shape <- list(
  'Hex' = data.frame(x=cos(seq(0,2*pi,length.out = 50)), # The center is the Core of shape
                     y=sin(seq(0,2*pi,length.out = 50))),
  'HexNAc' = data.frame(x=c(-1,-1,1,1,-1),
                        y=c(-1,1,1,-1,-1)),
  'HexN' = data.frame(x=c(-1,1,1,-1),
                      y=c(1,1,-1,1),
                      xx=c(-1,1,-1,-1),
                      yy=c(1,-1,-1,1)),
  'HexA' = data.frame(x=c(-1,0,1,-1),
                      y=c(0,1,0,0),
                      xx=c(1,0,-1,1),
                      yy=c(0,-1,0,0)),
  'dHex' = data.frame(x=c(-1,0,1,-1),
                      y=c(-0.33*sqrt(3),0.67*sqrt(3),-0.33*sqrt(3),-0.33*sqrt(3))), # The center is the Midpoint of the Base
  'dHexUp' = data.frame(x=c(-1,0,1,-1),
                        y=c(0.33*sqrt(3),-0.67*sqrt(3),0.33*sqrt(3),0.33*sqrt(3))),
  'dHexNAc' = data.frame(x=c(0,1,0,0),
                         y=c(0.67*sqrt(3),-0.33*sqrt(3),-0.33*sqrt(3),0.67*sqrt(3)),
                         xx=c(0,-1,0,0),
                         yy=c(0.67*sqrt(3),-0.33*sqrt(3),-0.33*sqrt(3),0.67*sqrt(3))),
  'ddHex' = data.frame(x=c(-1,1,1,-1),
                       y=c(0.5,0.5,-0.5,-0.5)),
  'Pen' = data.frame(x=c(0,0.2345,0.9516,0.3798,0.5872,0,-0.5872,-0.3798,-0.9516,-0.2345,0),
                     y=c(0.905,0.2289,0.2132,-0.219,-0.905,-0.4961,-0.905,-0.219,0.2132,0.2289,0.905)),
  'dNon' = data.frame(x=c(0,1,0,-1,0),
                      y=c(1,0,-1,0,1)),
  'ddNon' = data.frame(x=c(0,1.2,0,-1.2,0),
                       y=c(0.8,0,-0.8,0,0.8)),
  'UnKnown' = data.frame(x=c(0.6,1,0.6,-0.6,-1,-0.6,0.6),
                         y=c(0.6,0,-0.6,-0.6,0,0.6,0.6)),
  'Assigned' = data.frame(x=c(0,0.9516,0.5872,-0.5872,-0.9516,0),
                          y=c(0.905,0.2132,-0.905,-0.905,0.2132,0.905))
)

#' Title Initialize Vertices Coordinate
#'
#' @param structure an igraph object
#'
#' @returns Initialized Coordinate which Y=0, X are correct
#' @noRd
coor_initialization <- function(structure){
  ver_num <- length(structure)
  coor <- matrix(
    ini_xy <- rep(0,2*ver_num),
    ncol = 2
  )
  colnames(coor) <- c('x','y')
  # Gain x coordinate, where initial position of the structure is length(structure)
  init_X <- igraph::distances(structure)[length(structure),]*(-1)
  coor[,'x'] <- init_X
  for (i in seq(1,length(structure))){
    if (igraph::V(structure)[[i]]$mono == 'Fuc'){
      coor[i,'x'] <- coor[i,'x']+1
    }
  }
  return (coor)
}

#' Title Find Sequence Number of Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns the child vertices of structure
#'
#' @examples chil_coor(structure, 3)
#' @noRd
chil_coor <- function(structure,ver){
  vers <- igraph::bfs(structure, ver, mode='out',unreachable = FALSE)$order
  return(vers)
}

#' Title Offset y-Coordinate of Vertex Sub-module
#'
#' @param structure an igraph object
#' @param ver an integer
#' @param coor a matrix
#' @param offset a float
#'
#' @returns offset coordinate
#'
#' @examples offset_chil_coor(structure, 3, coor, 0.5)
#' @noRd
offset_chil_coor <- function(structure,ver,coor,offset){
  vers <- chil_coor(structure,ver)
  coor[vers,'y'] <- coor[vers,'y']+offset
  return(coor)
}

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 along the Path
#'
#' @param structure an igraph integer
#' @param ver the Sequence Number of Vertices
#'
#' @returns the number of vertices which neighbors >=2 on the path between vertex and begin vertex
#'
#' @examples out_degree(structure, 1)
#' @noRd
out_degree <- function(structure,ver){
  path_vertex <- igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]]
  num <- 0
  pos <- c()
  for (i in path_vertex){
    if (length(igraph::neighbors(structure,i)) >= 2 &&
        !('Fuc' %in% igraph::neighbors(structure,i)$mono)){
      num <- num+1
      pos <- c(pos,i)
    }
  }
  return(c(num,pos))
}

#' Title Judge whether the Vertex is in the Middle Position of Sub-module
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param ver an integer
#' @param par_ver an integer
#'
#' @returns bool
#'
#' @examples mid_pos(structure, coor, 1, 3)
#' @noRd
mid_pos <- function(structure,coor,ver,par_ver){
  pos_mid <- FALSE
  vers <- chil_coor(structure,par_ver)
  col_vers <- vers[which(coor[vers,'x']==coor[ver,'x'])]
  ver_y <- coor[ver,'y']
  col_y <- sort(coor[col_vers,'y'])
  if (ver_y != min(col_y) && ver_y != max(col_y)){
    pos_mid <- TRUE
  }
  return(pos_mid)
}

#' Title Judge whether the 'Fucose' Vertex is in the Middle Position of Sub-module
#'
#' @param structure an igraph object
#' @param coor a matrix
#' @param ver an integer
#' @param par_ver an integer
#'
#' @returns bool
#'
#' @examples fuc_mid_pos(structure, coor, 1, 3)
#' @noRd
fuc_mid_pos <- function(structure,coor,ver,par_ver){
  vers <- chil_coor(structure,par_ver)
  col_vers <- vers[which(coor[vers,'x']==coor[ver,'x'])]
  ver_y <- coor[ver,'y']
  col_y <- sort(coor[col_vers,'y'])
  ver_index <- which(col_y == ver_y)
  lag_diff <- abs(col_y - dplyr::lag(col_y)) < 1
  lead_diff <- abs(col_y - dplyr::lead(col_y)) < 1
  result <- lag_diff & lead_diff
  result[is.na(result)] <- FALSE
  return(result[ver_index])
}

#' Title Calculate the Amount and Sequence Number of Vertices that Out-degree >= 2 from Specified 'Fucose' vertex along the Path
#'
#' @param structure an igraph object
#' @param ver an integer
#' @param coor a matrix
#'
#' @returns the Amount and Sequence Number of vertices that Out-degree >= 2
#'
#' @examples fuc_out_degree(structure, 6, coor)
#' @noRd
fuc_out_degree <- function(structure,ver,coor){
  path_vertex <- igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]]
  num <- 0
  pos <- c()
  for (i in path_vertex){
    if (length(igraph::neighbors(structure,i)) %in% c(2,3) &&
        !('Fuc' %in% igraph::neighbors(structure,i)$mono) &&
        fuc_mid_pos(structure,coor,ver,i)){
      num <- num+1
      pos <- c(pos,i)
    }
  }
  return(c(num,pos))
}

#' Title Process the y-Coordinate of 'Fucose' Vertex
#'
#' @param structure an igraph object
#' @param fuc_pos an integer
#'
#' @returns the Offset of Specified 'Fucose' Vertex
#'
#' @examples fuc_offset(structure, 1)
#' @noRd
fuc_offset <- function(structure,fuc_pos){
  linkage_str <- igraph::E(structure)[fuc_pos]$linkage
  linkage_pos <- strsplit(linkage_str,'-')[[1]][2]
  offset <- 0.99
  if (linkage_pos %in% c('2','3')){
    offset <- -0.99
  }
  return(offset)
}

#' Title Process the Vertices which Out-degree >=2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param fuc_pos an integer
#' @param temp_coor a matrix
#'
#' @returns Processed Coordinate
#'
#' @examples process_fucose_branches(coor, structure, 2, temp_coor)
#' @noRd
process_fucose_branches <- function(coor,structure,fuc_pos,temp_coor){
  num <- fuc_out_degree(structure,fuc_pos,temp_coor)[1]
  for (i in seq(1,num)){
    par_pos <- fuc_out_degree(structure,fuc_pos,temp_coor)[i+1]
    neigh_pos <- igraph::neighbors(structure,par_pos)
    if (length(neigh_pos) == 2){
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-i+1)))
      coor <- offset_chil_coor(structure,neigh_pos[2],coor,-1/(2**(num-i+1)))
    }
    else if (length(neigh_pos) == 3){
      coor <- offset_chil_coor(structure,neigh_pos[1],coor,1/(2**(num-i+1)))
      coor <- offset_chil_coor(structure,neigh_pos[3],coor,-1/(2**(num-i+1)))
    }
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree >=2 and more than 2 Father Vertices that Out-degree >= 2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_multiple_branches(coor, structure, 3)
#' @noRd
process_multiple_branches <- function(coor,structure,ver){
  num <- out_degree(structure,ver)[1] # Numbers of vertices which out-degree > 1 (e.g. branch vertices)
  for(j in seq(1,num-1)){ # Traverse all branch vertices except for self
    pos <- out_degree(structure,ver)[j+1]
    pos_max <- out_degree(structure,ver)[num] # Position of branch vertices with second largest index
    neigh_pos <- igraph::neighbors(structure,pos)
    neigh_linkage <- as.numeric(sub('.*-','',igraph::E(structure)$linkage[neigh_pos]))
    arrange_neigh_pos <- neigh_pos[order(neigh_linkage,decreasing = TRUE)]
    # Judge whether child vertex is in the middle
    ver_neigh_pos <- igraph::neighbors(structure,ver)
    chil_in_middle <- mid_pos(structure,coor,min(ver_neigh_pos),pos) | mid_pos(structure,coor,max(ver_neigh_pos),pos) # TRUE or FALSE
    if (length(arrange_neigh_pos)==2){
      coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,1/(2**(num-j+1))+0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
      coor <- offset_chil_coor(structure,arrange_neigh_pos[2],coor,-1/(2**(num-j+1))-0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
    }
    else if(length(arrange_neigh_pos)==3 && chil_in_middle){
      coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,1/(2**(num-j))+0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
      coor <- offset_chil_coor(structure,arrange_neigh_pos[3],coor,-1/(2**(num-j))-0.25*mid_pos(structure,coor,ver,pos)*(pos!=pos_max))
    }
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree =2 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_two_neighbors(coor, structure, 5)
#' @noRd
process_two_neighbors <- function(coor,structure,ver){
  neigh_pos <- igraph::neighbors(structure,ver)
  neigh_linkage <- as.numeric(sub('.*-','',igraph::E(structure)$linkage[neigh_pos]))
  arrange_neigh_pos <- neigh_pos[order(neigh_linkage,decreasing = TRUE)]
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,arrange_neigh_pos[2],coor,-0.5)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # More than 1 branch along the path
    coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,arrange_neigh_pos[2],coor,-0.5)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Vertices which Out-degree =3 along the Path
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_three_neighbors(coor, structure, 6)
#' @noRd
process_three_neighbors <- function(coor,structure,ver){
  neigh_pos <- igraph::neighbors(structure,ver)
  neigh_linkage <- as.numeric(sub('.*-','',igraph::E(structure)$linkage[neigh_pos]))
  arrange_neigh_pos <- neigh_pos[order(neigh_linkage,decreasing = TRUE)]
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,1)
    coor <- offset_chil_coor(structure,arrange_neigh_pos[3],coor,-1)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # More than 1 branch along the path
    coor <- offset_chil_coor(structure,arrange_neigh_pos[1],coor,1)
    coor <- offset_chil_coor(structure,arrange_neigh_pos[3],coor,-1)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Vertices adjacent to 'Fucose'
#'
#' @param coor a matrix
#' @param structure an igraph object
#' @param ver an integer
#'
#' @returns Processed coordinate
#'
#' @examples process_contain_fucose_neighbors(coor, structure, 7)
#' @noRd
process_contain_fucose_neighbors <- function(coor,structure,ver){
  # Gain 'Fuc' position
  neigh_pos <- igraph::neighbors(structure,ver)
  fuc_pos <- neigh_pos[which(neigh_pos$mono=='Fuc')]
  # Gain other vertices position
  other_neigh_pos <- neigh_pos[!neigh_pos %in% c(fuc_pos)]
  neigh_linkage <- as.numeric(sub('.*-','',igraph::E(structure)$linkage[other_neigh_pos]))
  arrange_other_neigh_pos <- other_neigh_pos[order(neigh_linkage,decreasing = TRUE)]
  # Process other vertices coordinate
  if (out_degree(structure,ver)[1] == 1){
    coor <- offset_chil_coor(structure,arrange_other_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,arrange_other_neigh_pos[2],coor,-0.5)
  }
  else if(out_degree(structure,ver)[1] >= 1){ # More than 1 branch along the path
    coor <- offset_chil_coor(structure,arrange_other_neigh_pos[1],coor,0.5)
    coor <- offset_chil_coor(structure,arrange_other_neigh_pos[2],coor,-0.5)
    coor <- process_multiple_branches(coor,structure,ver)
  }
  return(coor)
}

#' Title Process the Coordinate of all Vertices
#'
#' @param structure an igraph object
#'
#' @returns Processed Coordinate
#'
#' @examples coor_cal(structure)
#' @noRd
coor_cal <- function(structure){
  coor <- coor_initialization(structure)
  structure_length <- seq(length(structure),1)
  for (i in structure_length) {
    gly_neighbors <- igraph::neighbors(structure,i)
    if (length(gly_neighbors) == 2 &&
        !('Fuc' %in% gly_neighbors$mono)){
      coor <- process_two_neighbors(coor,structure,i)
    }
    if (length(gly_neighbors) == 3 &&
        !('Fuc' %in% gly_neighbors$mono)){
      coor <- process_three_neighbors(coor,structure,i)
    }
    if (length(gly_neighbors) == 3 &&
        'Fuc' %in% gly_neighbors$mono){
      coor <- process_contain_fucose_neighbors(coor,structure,i)
    }
  }
  fuc_list <- c()
  for (i in structure_length){
    if ('Fuc' %in% igraph::neighbors(structure,i)$mono){
      neigh_pos <- igraph::neighbors(structure,i)
      fuc_pos <- neigh_pos[which(neigh_pos$mono=='Fuc')]
      fuc_list <- c(fuc_list,fuc_pos)
      coor[fuc_pos,'y'] <- coor[fuc_pos,'y']+fuc_offset(structure,fuc_pos)
    }
  }
  temp_coor <- coor
  for (i in fuc_list){
    if (fuc_out_degree(structure,i,temp_coor)[1] >=1){
      coor <- process_fucose_branches(coor,structure,i,temp_coor)
    }
  }
  return(coor)
}

#' Title Analysis the Connection Information of all Vertices
#'
#' @param structure an igraph object
#' @param coor a matrix
#'
#' @returns Connection Information
#'
#' @examples connect_info(structure, coor)
#' @noRd
connect_info <- function(structure,coor){
  edges <- igraph::as_edgelist(structure, names = FALSE)
  if (nrow(edges) == 0) {
    return(list(start_x = numeric(0), start_y = numeric(0), end_x = numeric(0), end_y = numeric(0)))
  }

  list(
    start_x = coor[edges[, 1], 'x'],
    start_y = coor[edges[, 1], 'y'],
    end_x = coor[edges[, 2], 'x'],
    end_y = coor[edges[, 2], 'y']
  )
}

#' Title Gain the glycoforms corresponding to the vertices
#'
#' @param structure an igraph object
#'
#' @returns list of glycoform
#'
#' @examples glycoform_info(structure)
#' @noRd
glycoform_info <- function(structure){
  glycoform <- igraph::V(structure)$mono
  for (i in c(which(glycoform == 'Fuc'))){
    if (fuc_offset(structure,i)=='0.99'){
      glycoform[i] <- 'FucUp'
    }
  }
  return(glycoform)
}

#' Title Adjust the coordinate of glycan annotation text
#'
#' @param chil_glyx a float
#' @param chil_glyy a float
#' @param par_glyx a float
#' @param par_glyy a float
#'
#' @returns coordinate list of annotations
#'
#' @examples annotation_coordinate(chil_glyx, chil_glyy, par_glyx, par_glyy)
#' @noRd
annotation_coordinate <- function(chil_glyx, chil_glyy, par_glyx, par_glyy){
  chil_direction <- matrix(c(par_glyx-chil_glyx, par_glyy-chil_glyy),ncol = 1, byrow = FALSE)
  par_direction <- matrix(c(chil_glyx-par_glyx, chil_glyy-par_glyy),ncol = 1, byrow = FALSE)
  chil_location <- 0.4*chil_direction/norm(chil_direction, type = '2')
  par_location <- 0.4*par_direction/norm(par_direction, type = '2')
  rotate_angle <- 1/10 * pi
  chil_rotate_matrix <- matrix(c(cos(rotate_angle),sin(rotate_angle),
                                 -sin(rotate_angle),cos(rotate_angle)), ncol = 2, byrow = TRUE)
  par_rotate_matrix <- matrix(c(cos(rotate_angle),-sin(rotate_angle),
                                sin(rotate_angle),cos(rotate_angle)), ncol = 2, byrow = TRUE)
  chil_annot_loc <- chil_rotate_matrix %*% chil_location
  par_annot_loc <- par_rotate_matrix %*% par_location
  annot_loc <- list("chil" = chil_annot_loc, "par" = par_annot_loc)
  return(annot_loc)
}

#' Title Map the glycan coordinate and annotation text
#'
#' @param structure an igraph object
#' @param coor a matrix
#'
#' @returns dataframe of glycan annotation and coordinate
#'
#' @examples gly_annotation(structure,coor)
#' @noRd
gly_annotation <- function(structure,coor){
  structure_length <- length(structure)
  if (igraph::ecount(structure) == 0) {
    return(data.frame(
      vertice = integer(0),
      annot = character(0),
      x = numeric(0),
      y = numeric(0)
    ))
  }
  struc_annot_coor <- data.frame(matrix(nrow = 0, ncol = 4))
  for (ver in seq_len(structure_length - 1)){
    par_ver <- dplyr::nth(as.vector(igraph::shortest_paths(structure,length(structure),ver)$vpath[[1]]),-2)
    # Read annotation information and relative position
    linkage_str <- igraph::E(structure)[ver]$linkage
    gly_annot_coor <- annotation_coordinate(coor[ver,1],coor[ver,2],coor[par_ver,1],coor[par_ver,2])
    # Calculate annotation coordinate >> c(annotate_information, x, y)
    chil_annotation <- c(ver, strsplit(linkage_str,'-')[[1]][1])
    chil_annotation <- c(chil_annotation, as.vector(gly_annot_coor$chil)+coor[ver,])
    par_annotation <- c(par_ver,strsplit(linkage_str,'-')[[1]][2])
    par_annotation <- c(par_annotation, as.vector(gly_annot_coor$par)+coor[par_ver,])
    # Bind to data.frame
    struc_annot_coor <- rbind(struc_annot_coor, chil_annotation)
    struc_annot_coor <- rbind(struc_annot_coor, par_annotation)
  }
  colnames(struc_annot_coor) <- c('vertice','annot','x','y')
  struc_annot_coor$annot[struc_annot_coor$annot == 'b1'] <- "beta"
  struc_annot_coor$annot[struc_annot_coor$annot %in% c('a1','a2')] <- "alpha"
  struc_annot_coor$x <- as.numeric(struc_annot_coor$x)
  struc_annot_coor$y <- as.numeric(struc_annot_coor$y)
  return(struc_annot_coor)
}

#' Title Match the coordinates of glycan shape
#'
#' @param gly_list a list
#'
#' @returns the coordinate list of glycan shape
#'
#' @examples create_polygon_coor(gly_list)
#' @noRd
create_polygon_coor <- function(gly_list, point_size) {
  polygon_coor <- gly_list |>
    purrr::pmap_dfr(function(center_x, center_y, glycoform) {
      composition <- glycan_dict[[glycoform]][1] # Mapping the Composition of Glycoform, e.g.'Fuc'->'dHex'
      df1 <- data.frame(
        point_x = c(point_size * glycan_shape[[composition]]$x + center_x),
        point_y = c(point_size * glycan_shape[[composition]]$y + center_y),
        # For Distinguishing the Coordinates of each point
        group = paste0(glycoform, center_x, "_", center_y),
        color = glycan_dict[[glycoform]][2]
      )
      if (length(glycan_dict[[glycoform]]) > 2) {
        df2 <- data.frame(
          point_x = c(point_size * glycan_shape[[composition]]$xx + center_x),
          point_y = c(point_size * glycan_shape[[composition]]$yy + center_y),
          # For Distinguishing the Coordinates of each point
          group = paste0(glycoform, center_x, "_", center_y, 'remain'),
          color = glycan_dict[[glycoform]][3]
        )
        df1 <- dplyr::bind_rows(df1, df2)
      }
      return(df1)
    })
  return(polygon_coor)
}

#' Draw a Symbol Nomenclature For Glycan (SNFG)
#'
#' @param structure A [glyrepr::glycan_structure()] scalar,
#'   or a string or any glycan structure text nomenclatures.
#' @param mono_size Sizes of the monosaccharide. Default to 0.2.
#'   Setting this to large might make the residue overlap with linkage annotations.
#' @param show_linkage Show linkage annotation or not. Default is TRUE.
#' @param orient The orientation of glycan structure. "H" for horizontal, "V" for vertical.
#'   Default is "H"
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples
#' draw_cartoon("Gal(b1-3)GalNAc(a1-")
draw_cartoon <- function(structure, mono_size = 0.2, show_linkage = TRUE, orient = c("H","V")){
  structure <- .ensure_one_structure(structure)
  structure <- glyrepr::get_structure_graphs(structure, return_list = FALSE)
  orient <- rlang::arg_match(orient)
  # Coordinate of Glycans
  if (orient == 'H'){
    coor <- coor_cal(structure)
  } else{
    coor <- coor_cal(structure)
    temp <- coor
    coor[,1] <- temp[,2]
    coor[,2] <- -temp[,1]
  }
  gly_list <- data.frame(coor,'glycoform' = glycoform_info(structure))
  # Rename colnames of gly_list
  colnames(gly_list) <- c('center_x','center_y','glycoform')
  # Draw Glycan Shape, where gly_list contains center_x, center_y, glycoform 3 columns
  polygon_coor <- create_polygon_coor(gly_list, mono_size)
  filled_color <- glycan_color[as.character(polygon_coor$color)]

  struc_annotation <- gly_annotation(structure,coor)

  # connect information
  gly_connect <- connect_info(structure, coor)
  connect_df <- data.frame(
    start_x = gly_connect$start_x,
    start_y = gly_connect$start_y,
    end_x   = gly_connect$end_x,
    end_y   = gly_connect$end_y
  )

  gly_graph <- ggplot2::ggplot()+
    ggplot2::geom_segment(
      data = connect_df,
      ggplot2::aes(x = .data$start_x, y = .data$start_y, xend = .data$end_x, yend = .data$end_y),
      linewidth = 0.5
    )+
    ggplot2::geom_polygon(
      data = polygon_coor,
      ggplot2::aes(x = .data$point_x, y = .data$point_y, group = .data$group),
      fill=filled_color, color='black',linewidth = 0.5
    )+
    ggplot2::coord_fixed(ratio = 1, clip = "off") +
    ggplot2::theme_void()
  if (show_linkage){
    gly_graph <- gly_graph+
      ggplot2::geom_text(
        data = struc_annotation,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$annot),
        parse = TRUE,
        size = 6,
        hjust = 0.5,
        vjust = 0.5
      )
  }
  class(gly_graph) <- c("glydraw_cartoon", class(gly_graph))
  gly_graph
}

#' @export
print.glydraw_cartoon <- function(x, border_px = 50, dpi = 300, ...) {
  plot <- .strip_glydraw_class(x)
  plot <- .apply_border(plot, border_px, dpi = dpi)
  size <- .decide_size(plot, border_px = border_px)
  ggimage::ggpreview(
    plot = plot,
    width = size$width,
    height = size$height,
    units = "px",
    dpi = dpi
  )
  invisible(x)
}

#' Save fixed-size glycan cartoon image to local device.
#'
#' In theory, you can just use `ggplot2::ggsave()` to save the cartoons plotted by [draw_cartoon()].
#' However, you can have trouble finding the best sizes for each cartoon
#' to make them look alike.
#' This function is designed to save the cartoons with self-adjusted sizes,
#' based on the size of the glycans,
#' so that when glycans with different sizes are put together, they will look alike.
#'
#' @param cartoon A ggplot2 object returned by [draw_cartoon()].
#' @param filename File name of glycan cartoon.
#' @param path Path of the directory to save plot to:
#'   path and filename are combined to create the fully qualified file name.
#'   Defaults to the working directory.
#' @param dpi Dots per inch, default = 300.
#' @param border_px Width of the border around the image in pixels. Default 50.
#'
#' @export
#' @examples
#' cartoon <- draw_cartoon("Gal(b1-3)GalNAc(a1-")
#' save_cartoon(cartoon, "p1.png", tempdir(), dpi = 300)
save_cartoon <- function(cartoon, filename, path = NULL, dpi = 300, border_px = 50){
  plot <- .strip_glydraw_class(cartoon)
  plot <- .apply_border(plot, border_px, dpi = dpi)
  size <- .decide_size(plot, border_px = border_px)
  # Save image with absolute pixel size ensuring the same glycan size.
  ggplot2::ggsave(
    filename = filename,
    path = path,
    plot = plot,
    width = size$width,
    height = size$height,
    units = 'px',
    dpi = dpi
  )
}

.decide_size <- function(cartoon, border_px = 0) {
  panel_width <- 3 * 118 * diff(ggplot2::get_panel_scales(cartoon)$x$range$range)
  panel_height <- 3 * 118 * diff(ggplot2::get_panel_scales(cartoon)$y$range$range)
  width <- panel_width + 2 * border_px
  height <- panel_height + 2 * border_px
  return(list(width = width, height = height))
}

.apply_border <- function(plot, border_px, dpi = 300) {
  if (is.null(border_px) || border_px <= 0) {
    return(plot)
  }
  border_pt <- (border_px / dpi) * 72
  plot + ggplot2::theme(
    plot.margin = ggplot2::margin(
      t = border_pt,
      r = border_pt,
      b = border_pt,
      l = border_pt,
      unit = "pt"
    )
  )
}

.strip_glydraw_class <- function(x) {
  class(x) <- setdiff(class(x), "glydraw_cartoon")
  x
}

.ensure_one_structure <- function(x) {
  if (is.character(x)) {
    x <- glyparse::auto_parse(x)
  } else if (!glyrepr::is_glycan_structure(x)) {
    cli::cli_abort(c(
      "{.arg structure} must be either a structure string or {.fn glyrepr::glycan_structure}.",
      "x" = "Got: {.cls {class(x)}}."
    ))
  }
  if (length(x) > 1) {
    cli::cli_abort(c(
      "Must provide exactly one glycan structure.",
      "x" = "Get length: {.val {length(x)}}."
    ))
  }
  x
}
