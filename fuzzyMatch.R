# pass missing fish species to gbif

fish0 <- taxize::classification(missingFish,db="gbif")

q <- tibble(x1=character(0),x2=character(0))

for (i in 1:57){
  q %<>% add_case(x1=names(fish0[i]),x2=fish0[[i]][6,1])
}


newTree <- ape::read.tree("12862_2017_958_MOESM2_ESM.tre")

for (i in 1:length(newTree$tip.label)){
newTree$tip.label[i] <- paste(strsplit(newTree$tip.label[i],split="_")[[1]][1],
                              strsplit(newTree$tip.label[i],split="_")[[1]][2],
                              sep=" ")
}

setdiff(f$Parasite_species,newTree$tip.label)



tmp <- rfishbase::synonyms(missF)
tmp %<>% mutate(alt=0)

for (i in 1:dim(tmp)[1]){
  tmp$alt[i] <- if_else(tmp$synonym[i]==tmp$Species[i],0,1)
}

tmp %<>% dplyr::select(synonym,Species)
tmp %<>% dplyr::mutate(manual="TBD")

##### MAYBE JUST NEED FROM HERE

library(tidyverse)
library(magrittr)
library(ape)
library(fishtree)
library(rfishbase)
library(fuzzyjoin)


source(knitr::purl("paraPrep.qmd",quiet=T))


f %<>% dplyr::filter(grepl("sp\\.",Host_species)==F)
f %<>% dplyr::filter(grepl("sp\\.",Parasite_species)==F)

tr <- fishtree::fishtree_phylogeny()
missF <- setdiff(f$Host_species,tr$tip.label)

fz <- as_tibble(missF)
df2 <- as_tibble(tr$tip.label)
fz %<>% stringdist_join(.,df2, 
                by='value', #match based on team
                mode='left', #use left join
                method = "jw",p=0, #use jw distance metric, p=0 means jaro vs winkler
                max_dist=99, 
                distance_col='dist')%>%
                group_by(value.x) %>%
                slice_min(order_by=dist, n=1)

