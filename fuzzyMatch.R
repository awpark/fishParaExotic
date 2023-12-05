##### 1. FUZZY MATCHING FOR TYPOS

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
                slice_min(order_by=dist, n=1) %>% 
                dplyr::arrange(dist) %>%
                dplyr::rename(db.name=value.x,tr.name=value.y)

fz %<>% dplyr::mutate(swap=if_else(dist<0.04,1,0)) # these are obvious typos
# manual acceptance:
fz$swap[which(fz$db.name=="Cyprinodon_labiosus")] <- 1
fz$swap[which(fz$db.name=="Cyprinodon_variegatus")] <- 1 # check with co-authors (this sub sp. or another?)
fz$swap[which(fz$db.name=="Strongylura_notata")] <- 1 # check with co-authors (this sub sp. or another?)
fz$swap[which(fz$db.name=="Poeciliopsis_pleurospiluspleurospilus")] <- 1 

##### 2. CHECK FOR SYNONYMS

missF2 <- fz %>% dplyr::filter(swap==0) %>% pull(db.name)
missF2 <- gsub("_"," ",missF2)

#fish02 <- taxize::classification(missF2,db="gbif")
#save(fish02,file="get_fish02.Rda")
load("get_fish02.Rda")

q <- tibble(x1=character(0),x2=character(0))

for (i in 1:length(fish02)){
  q %<>% add_case(x1=names(fish02[i]),x2=fish02[[i]][6,1])
}

q %<>% dplyr::mutate(match=if_else(q$x1==q$x2,1,0))


alts <- q %>% dplyr::filter(match==0) %>% pull(x2)
alts %<>% gsub(" ","_",.)
synMatches <- intersect(alts,tr$tip.label)

idx.1 <- which(q$x2==gsub("_"," ",synMatches[1]))
idx.2 <- which(fz$db.name==gsub(" ","_",q$x1[idx.1]))
fz$tr.name[idx.2] <- "Astyanax_fasciatus"
fz$swap[idx.2] <- 1

idx.1 <- which(q$x2==gsub("_"," ",synMatches[2]))
idx.2 <- which(fz$db.name==gsub(" ","_",q$x1[idx.1]))
fz$tr.name[idx.2] <- "Profundulus_labialis"
fz$swap[idx.2] <- 1


#THIS CODE IDENTIFIES "Vieja_guttulata" AS EXOTIC AND NATIVE
#OF 12 INSTANCES, 11 ARE NATIVE, 1 IS EXOTIC AND THEREFORE ASSUMED TYPO

#exoStatus <- f %>% dplyr::select(Host_species,Exotic_or_native) %>%
#  dplyr::mutate(exo=if_else(Exotic_or_native=="Exotic",1,0)) %>% 
#  dplyr::group_by(Host_species) %>%
#  dplyr::summarize(prExo=mean(exo))

idx.1 <- which(f$Host_species=="Vieja_guttulata")
idx.2 <- which(f$Exotic_or_native=="Exotic")
idx <- intersect(idx.1,idx.2)
f$Exotic_or_native[idx] <- "Native"

exoStatus <- f %>% dplyr::select(Host_species,Exotic_or_native) %>%
  dplyr::distinct() %>% dplyr::rename(db.name=Host_species)

fz %<>% left_join(.,exoStatus)

problemFish <- fz %>% dplyr::filter(swap==0 & Exotic_or_native=="Native") %>% pull(db.name)

sort(problemFish[grep("Vieja",problemFish)])
sort(tr$tip.label[grep("Cichlasoma",tr$tip.label)])

write_csv(data.frame(sort(problemFish)),file="problemFish.csv")

