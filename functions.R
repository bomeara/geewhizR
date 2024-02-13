## List of packages we will need

#' Install needed packages
#' 
#' This will install the packages we need for the analysis, then load all the packages
#' @param packages A vector of package names to install and load
#' @return NULL
get_needed_packages <- function(packages=c("datasauRus", "tidyverse", "ggplot2", "geodata", "rgbif", "ggmap", "ape", "datelife", "strap", "knitr", "rinat", "leaflet")) {

	## Where to get packages from:

	options(repos = c(
	CRAN = 'https://cloud.r-project.org',
	phylotastic = 'https://phylotastic.r-universe.dev')
	)
	
	# Check if packages are installed, install if not
	installed_packages <- packages %in% rownames(installed.packages())
	if (any(installed_packages == FALSE)) {
	install.packages(packages[!installed_packages])
	}

	# Packages loading
	invisible(lapply(packages, library, character.only = TRUE))
}

#' Summarize the datasaurus dataset
#' 
#' This function takes a dataset and returns a summary of the dataset
#' @param my_data A dataset to summarize
#' @return A data.frame summary of the dataset
summarize_dataset <- function(my_data) {
	grouped_dataset <- my_data |> group_by(dataset) |>
	summarize(
		count = n(),
		mean_x = mean(x),
		mean_y = mean(y),
		sd_x = sd(x),
		sd_y = sd(y),
		correlation = cor(x, y)
	)
	return(grouped_dataset)
}

#' Plot the datasaurus dataset
#' 
#' This function takes a dataset and returns a ggplot2 object. You will still have to print() it to see the plot
#' @param my_data A dataset to plot
#' @return A ggplot2 object
plot_dataset <- function(my_data) {
	plot_a <- ggplot(my_data, aes(x=x, y=y)) + 
	geom_point(alpha=0.2) + 
	facet_wrap(~dataset, nrow=2) + 
	coord_fixed(ratio=1)
	return(plot_a)
}

#' Get iNaturalist observations
#' 
#' This function gets observations from iNaturalist that are research grade and have images
#' @param focal_taxon The taxon to get observations for
#' @param focal_location_id The location to get observations for; by default, this is Tennessee
#' @return A data.frame of iNaturalist observations
get_inat_observations <- function(focal_taxon="Notophthalmus viridescens", focal_location_id=45) {
	obs <- rinat::get_inat_obs(query=focal_taxon, maxresults=50, place_id=focal_location_id, quality="research")
	images <- obs[nchar(obs$image_url)>0,] # get rid of observations that don't have images
	return(select(images, id, observed_on, image_url, latitude, longitude, license, user_name, place_guess))
}


#' Get salamander locations
#' 
#' This function gets salamander locations from GBIF
#' @param taxon_name The name of the taxon to get locations for
#' @param taxon_kingdom The kingdom of the taxon to get locations for
#' @param taxon_rank The rank of the taxon to get locations for
#' @param min_lat The minimum latitude to get locations for
#' @param max_lat The maximum latitude to get locations for
#' @param min_lon The minimum longitude to get locations for
#' @param max_lon The maximum longitude to get locations for
#' @return A data.frame of salamander locations
get_locations <- function(taxon_name="Caudata",taxon_kingdom="Animalia", taxon_rank="order", min_lat = 34.715593, max_lat = 36.151430, min_lon = -84.933809, max_lon = -82.557402) {
	taxon_key <- rgbif::name_backbone(
		name=taxon_name, 
		kingdom=taxon_kingdom, 
		rank=taxon_rank)$usageKey # get the taxon key for salamanders: Caudata means taxon 953 within GBIF, but what if they change the number later? Avoid hard-coding numbers like this whenever possible.

	salamander_occurrences <- rgbif::occ_search(
		taxonKey=taxon_key, # from above
		country="US", # only get data from the US
		decimalLongitude=paste0(min_lon, ',', max_lon), # GBIF can use a range like -65,-60 to get all data between -65 and -60
		decimalLatitude=paste0(min_lat, ',', max_lat), 
		year='2022,2023', # Only getting salamander data from these two years
		limit=10000 # limit to 10,000 points
		)$data # rgbif::occ_search returns a list, and we want the data part of the list

	salamander_occurrences <- subset(salamander_occurrences, !is.na(salamander_occurrences$species) ) # clean up the data to remove ones that are just identified to family	
	salamander_occurrences$latitude <- as.numeric(salamander_occurrences$decimalLatitude) 
	salamander_occurrences$longitude <- as.numeric(salamander_occurrences$decimalLongitude)
	return(salamander_occurrences)
}

#' Summarize salamander data
#' 
#' This function takes a dataset of salamander occurrences and returns a summary of the dataset by species
#' @param salamander_data A dataset of salamander occurrences
#' @return A data.frame summary of the dataset
summarize_salamanders <- function(salamander_data) {
	salamander_summary <- salamander_data |> 
	group_by(species) |>
	summarize(
		count = n(),
		mean_lat = mean(decimalLatitude),
		mean_lon = mean(decimalLongitude),
		sd_lat = sd(decimalLatitude),
		sd_lon = sd(decimalLongitude)
	) |> arrange(desc(count))
	return(salamander_summary)
}

#' Plot the top x species of salamanders
#' 
#' This function takes a dataset of salamander occurrences and returns a leaflet map of the most commonly recorded species
#' @param salamander_data A dataset of salamander occurrences
#' @param max_species The number of species to plot
#' @return A leaflet map
plot_top_x_species <- function(salamander_data, max_species=7) {
	summarized_salamanders <- summarize_salamanders(salamander_data)
	
	top_species <- summarized_salamanders$species[1:min(max_species, nrow(summarized_salamanders))]
	
	salamander_data$summarized_species <- ifelse(!(salamander_data$species %in% top_species), "Other", salamander_data$species)
	
	species_colors <- leaflet::colorFactor(palette = "Set1", domain = salamander_data$summarized_species)

	map_output <- leaflet(salamander_data) |> addProviderTiles(providers$CartoDB.Positron) |> addCircleMarkers(popup = ~species, radius=4, color=~species_colors(summarized_species), stroke=FALSE, fillOpacity=0.8) |> addLegend(pal = species_colors, values = ~summarized_species, opacity = 1)	
	
	return(map_output)
}

#' Get a phylogeny of the salamanders
#' 
#' This function takes a dataset of salamander occurrences and returns a phylogeny of the species in the species column
#' @param salamander_occurrences A dataset of salamander occurrences
#' @return A phylogeny of the species
get_datelife_tree <- function(salamander_occurrences) {
	datelife_result <- datelife::datelife_search(input=unique(salamander_occurrences$species))
	salamander_tree <- datelife_result[[which.max(sapply(datelife_result, ape::Ntip))]]
	salamander_tree$root.time <- max(ape::branching.times(salamander_tree))
	return(salamander_tree)
}