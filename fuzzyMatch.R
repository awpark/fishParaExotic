# pass missing fish species to gbif

fish0 <- taxize::classification(missingFish,db="gbif")

q <- tibble(x1=character(0),x2=character(0))

for (i in 1:57){
  q %<>% add_case(x1=names(fish0[i]),x2=fish0[[i]][6,1])
}


library(ape)
newTree <- ape::read.tree("12862_2017_958_MOESM2_ESM.tre")

for (i in 1:length(newTree$tip.label)){
newTree$tip.label[i] <- paste(strsplit(newTree$tip.label[i],split="_")[[1]][1],
                              strsplit(newTree$tip.label[i],split="_")[[1]][2],
                              sep=" ")
}

setdiff(f$Parasite_species,newTree$tip.label)

library(fishtree)
f %<>% dplyr::filter(grepl("sp\\.",Host_species)==F)
f %<>% dplyr::filter(grepl("sp\\.",Parasite_species)==F)

tr <- fishtree::fishtree_phylogeny(species=f$Host_species)
missF <- setdiff(f$Host_species,tr$tip.label)
missF <- gsub("_"," ",missF)
library(rfishbase)
tmp <- rfishbase::synonyms(missF)
tmp %<>% mutate(alt=0)

for (i in 1:dim(tmp)[1]){
  tmp$alt[i] <- if_else(tmp$synonym[i]==tmp$Species[i],0,1)
}

tmp %<>% dplyr::select(synonym,Species)
tmp %<>% dplyr::mutate(manual="TBD")

library(fuzzyjoin)
tr <- fishtree::fishtree_phylogeny()
df1 <- as_tibble(missF)
df2 <- as_tibble(tr$tip.label)
df1 %<>% stringdist_join(.,df2, 
                by='value', #match based on team
                mode='left', #use left join
                method = "jw", #use jw distance metric
                max_dist=99, 
                distance_col='dist')%>%
                group_by(value.x) %>%
                slice_min(order_by=dist, n=1)

