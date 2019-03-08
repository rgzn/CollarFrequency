#(networkD3)

# library routines added to app.R, added in check for M:/drive - get files from M: if exists, o/w use local copy 
#  use msAccess db (query) for most recent copy of database if running 32 bit R

#### Constants: ####
MIN_FREQ = 159.0
MAX_FREQ = 161.0
FREQ_INC = 0.001
ALL_FREQUENCIES = seq(MIN_FREQ, MAX_FREQ, by=FREQ_INC)
DEFAULT_FREQ_MARGIN = 0.0049

attr_file = "attributes.csv"
network_file = "network.csv"


#### Initialization: ####

# Read the frequency interference spreadsheet into a graph:
pop_attributes = read.csv(attr_file, stringsAsFactors=F)
pop_network = read.csv(network_file, stringsAsFactors=F)
pop_adjacency = pop_network[,!(names(pop_network) %in% c('population'))]
pop_adjacency = as.matrix(pop_adjacency)
pop_graph = graph.adjacency(pop_adjacency, weighted=NULL, mode='undirected')

# Assign population attributes to population network graph:
ix = match(V(pop_graph)$name, pop_attributes$population)
V(pop_graph)$species = pop_attributes$species[ix]
V(pop_graph)$location = pop_attributes$location[ix]
V(pop_graph)$lat = pop_attributes$lat[ix]
V(pop_graph)$lon = pop_attributes$lon[ix]

# Make Readable Strings for populations names:
pretty_pops = paste(pop_attributes$species, pop_attributes$location, sep=" @ ")
pop_codes = paste0("(", pop_attributes$population, ")")
pretty_pops = paste(pretty_pops, pop_codes)

# Read the All Collars Database into a dataframe:  check for 32 / 64 bit and network connectivity use Union Query if 32, o/w read txt file
# network location    M:\DatabaseTables

test.sys <- Sys.getenv("R_ARCH")
if (test.sys == "/i386" & dir.exists("M:/")) {
  # 32 bit allows for using MS Access database directly
  myconn <- odbcConnectAccess("M:/AllProgramCollars.mdb")
  sql1 <-
    "SELECT Bobcat.Frequency, Bobcat.CaptDate, Bobcat.AnimalType, Bobcat.ID, Bobcat.Location, Bobcat.Type,Bobcat.Make, Bobcat.Status, Bobcat.HU FROM Bobcat"
  sql2 <-
    "SELECT SheepCollarsOffice.CollarFreq,'' AS ddate,  SheepCollarsOffice.AnimalType, '' AS id,SheepCollarsOffice.Location, SheepCollarsOffice.CollarType, SheepCollarsOffice.CollarMake, SheepCollarsOffice.Status, SheepCollarsOffice.HU FROM SheepCollarsOffice"
  sql3 <-
    "SELECT Elk.Frequency, Elk.CaptDate, Elk.AnimalType, Elk.ID, Elk.Location, Elk.Type, Elk.Make, Elk.Status, Elk.HU FROM Elk"
  sql4 <-
    "SELECT DogCollars.Frequency, DogCollars.CaptDate, DogCollars.AnimalType, DogCollars.ID, DogCollars.Location, DogCollars.Type, DogCollars.Make, DogCollars.Status, DogCollars.HU FROM DogCollars"
  sql5 <-
    "SELECT OtherSpecies.Frequency, OtherSpecies.CaptDate, OtherSpecies.AnimalType, OtherSpecies.ID, OtherSpecies.Location, OtherSpecies.Type, OtherSpecies.Make, OtherSpecies.Status, OtherSpecies.HU FROM OtherSpecies"
  sql6 <-
    "SELECT Pronghorn.Frequency, Pronghorn.CaptDate, Pronghorn.AnimalType, Pronghorn.ID, Pronghorn.Location, Pronghorn.Type, Pronghorn.Make, Pronghorn.Status, Pronghorn.HU FROM Pronghorn"
  sql7 <-
    "SELECT SageGrouse2014.Frequency, SageGrouse2014.CaptDate, SageGrouse2014.AnimalType, SageGrouse2014.ID, SageGrouse2014.Location, SageGrouse2014.Type, SageGrouse2014.Make, SageGrouse2014.Status, SageGrouse2014.HU FROM SageGrouse2014"
  sql8 <-
    "SELECT RVDall.CollarFreq, '' AS [date], 'deer' AS AnimalType, RVDall.DeerID, Left([DeerID],2) AS Location, 'VHF/GPS' AS type, 'Various' AS Make, RVDall.Status, Left([DeerID],2) AS HU FROM RVDall"
  sql9 <-
    "SELECT AllSheepCollars.VHFColFreq, left([AllSheepCollars].[Datedt],10) as date1, 'bighorn' AS AnimalType, AllSheepCollars.AnimalID,AllSheepCollars.RU, AllSheepCollars.Type, AllSheepCollars.VHFMake AS Make, AllSheepCollars.StatusVHF, AllSheepCollars.Herd FROM AllSheepCollars WHERE (((AllSheepCollars.StatusVHF) Like 'ow' Or (AllSheepCollars.StatusVHF)='aw' Or (AllSheepCollars.StatusVHF)='ORD'))"
  sql10 <-
    "SELECT CurrentCollarRVDOffice.CollarFreq, '' AS [date], 'deer' AS AnimalType, CurrentCollarRVDOffice.DeerID, Left([DeerID],2) AS Location, 'VHF' AS type, 'ATS' AS Make, CurrentCollarRVDOffice.Status, Left([DeerID],2) AS HU FROM CurrentCollarRVDOffice"
  sql11 <-
    "SELECT AllSheepCollars.GPSColFreq, Left(AllSheepCollars.Datedt,10) AS date1, 'bighorn' AS AnimalType, AllSheepCollars.AnimalID, AllSheepCollars.RU, IIf([StatusGPS] Like '*w','GPS','') AS Type, AllSheepCollars.GPSMake AS Make, AllSheepCollars.StatusGPS, AllSheepCollars.Herd FROM AllSheepCollars WHERE (((AllSheepCollars.GPSColFreq) Is Not Null) AND ((AllSheepCollars.StatusGPS) Like 'aw*' Or (AllSheepCollars.StatusGPS) Like 'ow' Or (AllSheepCollars.StatusGPS) Like 'ORD  '))"
  sql12 <-
    "SELECT CollarsOnOrder.Frequency, CollarsOnOrder.CaptDate, CollarsOnOrder.AnimalType, CollarsOnOrder.ID, CollarsOnOrder.Location, CollarsOnOrder.Type, CollarsOnOrder.Make, CollarsOnOrder.Status, CollarsOnOrder.HU FROM CollarsOnOrder ORDER BY Bobcat.Frequency;"
  # sql13 <-
  #  "SELECT CurrentLion.VHF, CurrentLion.CaptDate, CurrentLion.AnimalType, CurrentLion.ID, CurrentLion.Location, CurrentLion.Type,CurrentLion.Make, CurrentLion.Status, CurrentLion.HU FROM CurrentLion"
  qry <-
    paste(sql1,
          sql2,
          sql3,
          sql4,
          sql5,
          sql6,
          sql7,
          sql8,
          sql9,
          sql10,
          sql11,
          sql12,
          sep = " UNION ")
  collars <- sqlQuery(myconn, qry)
  close(myconn)
} else{
  # 64 bit solution (no RODBC)
  if (dir.exists("M:/")) {
    dir = 'M:/DatabaseTables'
    collars_file = paste(dir, 'AllCollarsList.txt', sep = '/')
  } else {
    collars_file = 'AllCollarsList.txt'
  }
  collars = read.csv(collars_file,
                       header = F,
                       stringsAsFactors = F)
}
names(collars) = c(
  "frequency",
  "capture_date",
  "species",
  "animal_id",
  "region",
  "type",
  "model",
  "status",
  "location"
)
collars$species <- as.character(collars$species)
collars$location <- as.character(collars$location)

# Add population field to the collars table:
collars = full_join(collars, pop_attributes[c('species','location','population')])
#### Graphics: ####
# Plot network graph:
network_plot = ggnet2(pop_graph, mode='kamadakawai',
       color = "species", 
       palette = "Set1",
       label=T,
       label.size=3,
       edge.size=1,
       size = 11)

# Geographic map:
# baseMap = get_map(location = c(-118.62581, 37.40818), 
#                    zoom=8, source="stamen", 
#                    maptype="terrain-background", color="bw")
# baseMapPlot = ggmap(baseMap, zoom=14)
# x=ca_border[grep("(inyo|mono|tuolumne|mariposa|alpine|tulare|fresno|madera)", ca_border$subregion), ]
# county_base = ggplot(x,aes(x=long, y=lat)) + geom_polygon(aes(group = group), color = "grey65", fill = "#f0f0f0", size = 0.4)
# 
# network_map_co = ggnetworkmap( county_base, net = pop_graph,
#                         great.circles=T,
#                         node.group="species",
#                         segment.color="red", palette ="Set3"
# )
# network_map = ggnetworkmap( baseMapPlot, net = pop_graph,
#                                great.circles=T,
#                                node.group="species",
#                                segment.color="red", palette ="Set3"
# )
#netMap


# interactive network graph:
#pop_graph_d3 <- igraph_to_networkD3

#### General Use network functions: ####
get_pop_code = function(attr, species, location) {
  code = attr[attr$species == species & attr$location == location, ]$population
  return(code)
}

df_from_nodes = function(nodes, pop_df=pop_attributes) {
  output_df = pop_df[pop_df$population %in% nodes$name, ]
  return(output_df)
}

find_neighborhood = function(pop_graph, input_pops) {
  nodes = V(pop_graph)[V(pop_graph)$name %in% input_pops]
  neighborhoods = ego(pop_graph, 1, nodes=nodes, mindist=0)
  neighborhood = unique(Reduce(c, neighborhoods))
  return(neighborhood)
}

find_neighborhood_df = function(pop_graph, pop_attributes, input_pops) {
  neighborhood = find_neighborhood(pop_graph, input_pops)$name
  neighborhood_df = pop_attributes[ pop_attributes$population %in% neighborhood, ]
  return(neighborhood_df[,1:3])
}


#### Functions for finding available populations: ####

find_conflict_collars = function(pop_graph, collars, input_freq, freq_margin = DEFAULT_FREQ_MARGIN) {
  collars_aw = collars[grep("(AW|ORD)",collars$status ),]
  collars_near = collars_aw[abs(collars_aw$frequency - input_freq) < freq_margin,]
  return(collars_near)
}

find_collars_in_range = function(pop_graph, collars, lo_f, hi_f) {
  collars_aw = collars[grep("(AW|ORD)",collars$status ),]
  collars_in_range = collars_aw[(collars_aw$frequency >= lo_f) & (collars_aw$frequency <= hi_f) , ]
  return(collars_in_range)
}

find_occupied_pops = function(pop_graph, collars, input_freq, freq_margin = DEFAULT_FREQ_MARGIN) {
  collars_near = find_conflict_collars(pop_graph, collars, input_freq, freq_margin=freq_margin)
  occupied_pops = unique(collars_near[c('population', 'species','location')])
  return(occupied_pops)
}

find_conflict_pops = function(pop_graph, collars, input_freq, freq_margin = DEFAULT_FREQ_MARGIN) {
  occupied_pops = find_occupied_pops(pop_graph, collars, input_freq=input_freq, freq_margin=freq_margin)
  # occupied_nodes = V(pop_graph)[V(pop_graph)$name %in% occupied_pops$population]
  # occupied_neighborhoods = ego(pop_graph, 1, nodes = occupied_nodes, mindist = 0)
  # conflict_pops = unique(Reduce(c, occupied_neighborhoods))
  conflict_pops = find_neighborhood(pop_graph, occupied_pops$population)
  
  return(conflict_pops)
}

find_available_pops = function(pop_graph, collars, input_freq, input_species = 'none', freq_margin = DEFAULT_FREQ_MARGIN) {
  conflict_pops = find_conflict_pops(pop_graph, collars, input_freq, freq_margin=freq_margin)
  available_pops = difference(V(pop_graph), conflict_pops)
  if(input_species != 'none') {
    available_pops = available_pops[available_pops$species == input_species]
  }
  return(available_pops)
}



##### Functions for finding available frequencies: ####

within_margin = function(f, set, margin = DEFAULT_FREQ_MARGIN){
  return(all(abs(f - set) > margin, na.rm=T))
}

band_complement = function(input_freqs, all_freqs = ALL_FREQUENCIES, freq_margin = DEFAULT_FREQ_MARGIN) {
  ix = sapply(all_freqs, function(x) within_margin(f=x, set=input_freqs, margin=freq_margin))
  return(all_freqs[ix])
}

find_available_freqs = function(pop_graph, collars, input_pops, freq_margin=DEFAULT_FREQ_MARGIN, all_freqs=ALL_FREQUENCIES) {
  neighborhood_pops = find_neighborhood(pop_graph, input_pops)
  neighborhood_freqs = collars[collars$population %in% neighborhood_pops$name, 'frequency']
  neighborhood_freqs = unique(sort(neighborhood_freqs))
  avail_freqs = band_complement(input_freqs=neighborhood_freqs, all_freqs=all_freqs, freq_margin=freq_margin)
  return(avail_freqs)
}

plot_available = function(pop_graph, collars, input_pops, freq_margin=DEFAULT_FREQ_MARGIN, all_freqs=ALL_FREQUENCIES) {
  f = find_available_freqs(pop_graph, collars, input_pops, freq_margin = freq_margin, all_freq = all_freqs)
  f = data.frame(f)
  ylabel = do.call(paste, as.list(input_pops))
  ggplot(f, aes(f)) + 
    geom_histogram(binwidth=0.001) + 
    scale_y_continuous(breaks=NULL) + 
    scale_x_continuous(breaks=seq(159,161,by=0.20)) + 
    theme(axis.title.y= element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), 
          panel.background = element_rect(fill="transparent",colour=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

