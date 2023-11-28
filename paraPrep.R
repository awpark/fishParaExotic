## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo=F,message=F,warning=F,tidy=T)


## ----loadLibs------------------------------------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(taxize) # classification function


## ----readData------------------------------------------------------------------------------------------------------
f <- read_csv("FISH_PARASITE_DATASET.csv")


## ----taxize, eval=F------------------------------------------------------------------------------------------------
## ##TAXIZE##
## paras <- f %>% dplyr::select(Parasite_species) %>% distinct()
## 
## paraTaxa <- taxize::classification(paras$Parasite_species,db="gbif")
## save(paraTaxa,file="get_paraTaxa.Rda")


## ----loadTaxize----------------------------------------------------------------------------------------------------
load("get_paraTaxa.Rda")


## ----makeTaxaTable-------------------------------------------------------------------------------------------------

myTaxa <- tibble(kingdom=character(0),phylum=character(0),class=character(0),order=character(0),family=character(0),genus=character(0),species=character(0),Parasite_species=character(0))
for (i in 1:length(paraTaxa)){
  thisDf <- paraTaxa[[i]]
  if (dim(thisDf)[1]>=7){
  kingdom <- thisDf$name[which(thisDf$rank=="kingdom")]
  phylum <- thisDf$name[which(thisDf$rank=="phylum")]
  class <- thisDf$name[which(thisDf$rank=="class")]
  order <- thisDf$name[which(thisDf$rank=="order")]
  family <- thisDf$name[which(thisDf$rank=="family")]
  genus <- thisDf$name[which(thisDf$rank=="genus")]
  species <- thisDf$name[which(thisDf$rank=="species")]}
  else if (dim(thisDf)[1]==6){
  kingdom <- thisDf$name[which(thisDf$rank=="kingdom")]
  phylum <- thisDf$name[which(thisDf$rank=="phylum")]
  class <- thisDf$name[which(thisDf$rank=="class")]
  order <- thisDf$name[which(thisDf$rank=="order")]
  family <- thisDf$name[which(thisDf$rank=="family")]
  genus <- thisDf$name[which(thisDf$rank=="genus")]
  species <- gsub("_"," ",names(paraTaxa)[i]) }
  else {
  kingdom <- thisDf$name[which(thisDf$rank=="kingdom")]
  phylum <- thisDf$name[which(thisDf$rank=="phylum")]
  class <- "TBD"
  order <- thisDf$name[which(thisDf$rank=="order")]
  family <- thisDf$name[which(thisDf$rank=="family")]
  genus <- thisDf$name[which(thisDf$rank=="genus")]
  species <- gsub("_"," ",names(paraTaxa)[i])
  }
  Parasite_species <- names(paraTaxa[i])
  myTaxa %<>% add_case(kingdom=kingdom,phylum=phylum,class=class,order=order,family=family,genus=genus,species=species,Parasite_species=Parasite_species)
}

#resolve missing "class" data
idx <- which(myTaxa$order=="Trichinellida")
myTaxa$class[idx] <- "Enoplea" # verified


## ----pseudoDuplicateParas------------------------------------------------------------------------------------------
dup <- names(which(table(myTaxa$species)>1))

idx <- grep(strsplit(dup[1]," ")[[1]][1],f$Parasite_species) # Megalonia ictaluri
f$Parasite_species[idx] <- gsub(" ","_",dup[1])

idx <- grep(gsub(" ","_",dup[2]),f$Parasite_species) # Rhabdochona kidderi
f$Parasite_species[idx] <- gsub(" ","_",dup[2])

idx <- grep(substr(gsub(" ","_",dup[3]),1,13),f$Parasite_species) # Wallinia chavarriae
f$Parasite_species[idx] <- gsub(" ","_",dup[3])

myTaxa %<>% distinct()


## ----problemParas--------------------------------------------------------------------------------------------------
# 483 parasite species in f, when loaded
# 3 of these are "extra" (typos [2] or subspecies [1])
# 480 "real" parasite species
# myTaxa has 469 unique parasite species
# so, what happened to the missing 11?

# they've all been investigated and are cases of:
# minor typos, species synonyms, genus renaming - so, all is good


## ----paraRename----------------------------------------------------------------------------------------------------
paraNames4join <- myTaxa %>% dplyr::select(Parasite_species,species)
f %<>% left_join(.,paraNames4join)
f %<>% dplyr::rename(Parasite_species_old=Parasite_species,Parasite_species=species)

