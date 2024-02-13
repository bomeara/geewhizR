source('functions.R')

# Install and load packages we need

get_needed_packages()

# Load the datasaurus_dozen dataset
data('datasaurus_dozen', package="datasauRus")

# List the datasets in the datasaurus_dozen dataset
print(unique(datasaurus_dozen$dataset))

# We created a function to tell us summary stats about the datasets

print(summarize_dataset(my_data=datasaurus_dozen))

# Ok, interesting. Let's plot the datasets, also using a function

print(plot_dataset(my_data=datasaurus_dozen))


# Now for something completely different: let's get some salamander data

# We use GBIF to get the data using a function we created

salamander_data <- get_locations()

# Show where the most common species are in our region

print(plot_top_x_species(salamander_data, max_species=7))

# Let us get a phylogeny of the salamanders in our region

salamander_tree <- get_datelife_tree(summarize_salamanders(salamander_data))

pdf(file="salamander_tree.pdf", width=10, height=10)
strap::geoscalePhylo(salamander_tree, units="Period", cex.tip=1, cex.age=1, cex.ts=1)	
dev.off()

# We can also get information from iNaturalist for the Eastern Newt in Tennessee

inat_results <- get_inat_observations(focal_taxon="Notophthalmus viridescens")

print(inat_results)