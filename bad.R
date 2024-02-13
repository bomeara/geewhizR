# This is an example of a bad way to do an R script. It works, but it's hard to debug and hard to maintain and future you will have little idea what it all does. It also has values programmed into it that might be hard to change in the future or recognize

options(repos = c(
CRAN = 'https://cloud.r-project.org',
phylotastic = 'https://phylotastic.r-universe.dev')
)
	
packages=c("datasauRus", "tidyverse", "ggplot2", "geodata", "rgbif", "ggmap", "ape", "datelife", "strap", "knitr", "rinat", "leaflet")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
	install.packages(packages[!installed_packages])
}

library(datasauRus)
library(tidyverse)
library(ggplot2)
library(geodata)
library(rgbif)
library(ggmap)
library(ape)
library(datelife)
library(strap)
library(knitr)
library(rinat)
library(leaflet)

data('datasaurus_dozen', package="datasauRus")

print(unique(datasaurus_dozen$dataset))

grouped_dataset <- datasaurus_dozen |> group_by(dataset) |> summarize(count = n(), mean_x = mean(x), mean_y = mean(y), sd_x = sd(x), sd_y = sd(y), correlation = cor(x, y))

print(grouped_dataset)

plot_a <- ggplot(datasaurus_dozen, aes(x=x, y=y)) + geom_point(alpha=0.2) + facet_wrap(~dataset, nrow=2) + coord_fixed(ratio=1)
print(plot_a)

taxon_key <- rgbif::name_backbone(name="Caudata", kingdom="Animalia", rank="order")$usageKey 

salamander_occurrences <- rgbif::occ_search(taxonKey=taxon_key, country="US", decimalLongitude=paste0(-84.933809, ',', -82.557402), decimalLatitude=paste0(34.715593, ',', 36.151430), year='2022,2023', limit=10000)$data 

salamander_occurrences <- subset(salamander_occurrences, !is.na(salamander_occurrences$species) ) 
salamander_occurrences$latitude <- as.numeric(salamander_occurrences$decimalLatitude) 
salamander_occurrences$longitude <- as.numeric(salamander_occurrences$decimalLongitude)


summarized_salamanders <- salamander_data |> group_by(species) |> summarize( count = n(), mean_lat = mean(decimalLatitude), mean_lon = mean(decimalLongitude), sd_lat = sd(decimalLatitude), sd_lon = sd(decimalLongitude) ) |> arrange(desc(count))
	
top_species <- summarized_salamanders$species[1:min(max_species, nrow(summarized_salamanders))]
	
salamander_data$summarized_species <- ifelse(!(salamander_data$species %in% top_species), "Other", salamander_data$species)
	
species_colors <- leaflet::colorFactor(palette = "Set1", domain = salamander_data$summarized_species)

map_output <- leaflet(salamander_data) |> addProviderTiles(providers$CartoDB.Positron) |> addCircleMarkers(popup = ~species, radius=4, color=~species_colors(summarized_species), stroke=FALSE, fillOpacity=0.8) |> addLegend(pal = species_colors, values = ~summarized_species, opacity = 1)	

print(map_output)

datelife_result <- datelife::datelife_search(input=unique(salamander_occurrences$species))
salamander_tree <- datelife_result[[which.max(sapply(datelife_result, ape::Ntip))]]
salamander_tree$root.time <- max(ape::branching.times(salamander_tree))


pdf(file="salamander_tree.pdf", width=10, height=10)
strap::geoscalePhylo(salamander_tree, units="Period", cex.tip=1, cex.age=1, cex.ts=1)	
dev.off()

# We can also get information from iNaturalist for the Eastern Newt in Tennessee

obs <- rinat::get_inat_obs(query="Notophthalmus viridescens", maxresults=50, place_id=45, quality="research")
images <- obs[nchar(obs$image_url)>0,] # get rid of observations that don't have images
inat_results <- select(images, id, observed_on, image_url, latitude, longitude, license, user_name, place_guess)

print(inat_results)