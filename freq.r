library(igraph)
#(networkD3)
library(network)
library(sna)
library(ggplot2)
library(intergraph)
library(GGally)
library(dplyr)
library(data.table)
library(ggmap)
# library(ggraph)

#### Constants: ####
MIN_FREQ = 159.0
MAX_FREQ = 161.0
FREQ_INC = 0.001
ALL_FREQUENCIES = seq(MIN_FREQ, MAX_FREQ, by=FREQ_INC)

attr_file = "attributes.csv"
network_file = "network.csv"

collar_headers= c("frequency",
                  "capture_date",
                  "species",
                  "animal_id",
                  "region",
                  "type",
                  "model",
                  "status",
                  "location")



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

# Read the All Collars Database into a dataframe:
collars_file = 'AllCollarsList.txt'
collars = read.csv(collars_file, header=FALSE, stringsAsFactors=F, col.names=collar_headers)

# Add population field to the collars table:
collars = full_join(collars, pop_attributes[c('species','location','population')], by = c("species", "location"))

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

find_conflict_collars = function(pop_graph, collars, input_freq, freq_margin = 0.005) {
  collars_aw = collars[grep("AW",collars$status ),]
  collars_near = collars_aw[abs(collars_aw$frequency - input_freq) < freq_margin,]
  return(collars_near)
}

find_collars_in_range = function(pop_graph, collars, lo_f, hi_f) {
  collars_aw = collars[grep("(AW|order)",collars$status ),]
  collars_in_range = collars_aw[(collars_aw$frequency >= lo_f) & (collars_aw$frequency <= hi_f) , ]
  return(collars_in_range)
}

find_occupied_pops = function(pop_graph, collars, input_freq, freq_margin = 0.005) {
  collars_near = find_conflict_collars(pop_graph, collars, input_freq, freq_margin=freq_margin)
  occupied_pops = unique(collars_near[c('population', 'species','location')])
  return(occupied_pops)
}

find_conflict_pops = function(pop_graph, collars, input_freq, freq_margin = 0.005) {
  occupied_pops = find_occupied_pops(pop_graph, collars, input_freq=input_freq, freq_margin=freq_margin)
  # occupied_nodes = V(pop_graph)[V(pop_graph)$name %in% occupied_pops$population]
  # occupied_neighborhoods = ego(pop_graph, 1, nodes = occupied_nodes, mindist = 0)
  # conflict_pops = unique(Reduce(c, occupied_neighborhoods))
  conflict_pops = find_neighborhood(pop_graph, occupied_pops$population)
  
  return(conflict_pops)
}

find_available_pops = function(pop_graph, collars, input_freq, input_species = 'none', freq_margin = 0.005) {
  conflict_pops = find_conflict_pops(pop_graph, collars, input_freq, freq_margin=freq_margin)
  available_pops = difference(V(pop_graph), conflict_pops)
  if(input_species != 'none') {
    available_pops = available_pops[available_pops$species == input_species]
  }
  return(available_pops)
}



##### Functions for finding available frequencies: ####

within_margin = function(f, set, margin = 0.005){
  return(all(abs(f - set) > margin, na.rm=T))
}

band_complement = function(input_freqs, all_freqs = ALL_FREQUENCIES, freq_margin = 0.005) {
  ix = sapply(all_freqs, function(x) within_margin(f=x, set=input_freqs, margin=freq_margin))
  return(all_freqs[ix])
}

find_available_freqs = function(pop_graph, collars, input_pops, freq_margin=0.005, all_freqs=ALL_FREQUENCIES) {
  neighborhood_pops = find_neighborhood(pop_graph, input_pops)
  neighborhood_freqs = collars[collars$population %in% neighborhood_pops$name, 'frequency']
  neighborhood_freqs = unique(sort(neighborhood_freqs))
  avail_freqs = band_complement(input_freqs=neighborhood_freqs, all_freqs=all_freqs, freq_margin=freq_margin)
  return(avail_freqs)
}

plot_available = function(pop_graph, collars, input_pops, freq_margin=0.005, all_freqs=ALL_FREQUENCIES) {
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

