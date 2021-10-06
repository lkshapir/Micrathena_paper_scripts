install.packages(c("tidyverse", "ggtree", "treeio", "deeptime", "phytools", "phyloch", "strap"))

library(treeio)


# tree<- read.newick("unpruned_newick.phy")

Beast_tree <- read.beast("Unpruned_no_embira.editednames.txt")

Beast_tree@phylo$tip.label


species <- c("Achaeranea_sp._GB", "Gasteracantha_DR_784515","Micrathena_macfarlanei", "Micrathena_miles", "Micrathena_duodecimspinosa", "Micrathena_sp.", "Micrathena_digitata", "Micrathena_cubana", "Micrathena_similis", "Micrathena_mitrata", "Micrathena_bimucronata", "Micrathena_plana", "Micrathena_banksi", "Micrathena_militaris_DR", "Micrathena_militaris_PR", "Micrathena_sagittata_FL", "Micrathena_sagittata_MX", "Micrathena_sexspinosa", "Micrathena_brevipes", "Micrathena_reimoseri", "Micrathena_cornuta", "Micrathena_schreibersi_CA", "Micrathena_schreibersi_TR", "Micrathena_schreibersi_SA", "Micrathena_gracilis", "Micrathena_horrida_CU", "Micrathena_horrida_CA", "Micrathena_horrida_JA", "Micrathena_horrida_SA", "Micrathena_forcipata_CU", "Micrathena_forcipata_DR", "Micrathena_spinulata", "Micrathena_annulata", "Micrathena_yanomami", "Micrathena_swainsoni", "Micrathena_beta", "Micrathena_perfida")

library(tidyverse)
library(ggtree)


##Prune beast tree to only include tips in biogeobears analysis
pruned_beast_tree <- drop.tip(Beast_tree, setdiff(Beast_tree@phylo$tip.label, species))

#Visualize HPD intervals (age ranges)
pruned_beast_tree

library(deeptime)
library(phytools)
library(phyloch)
library(strap)
library(wesanderson)

ggtree(pruned_beast_tree) + geom_tiplab() + geom_range("height_0.95_HPD", color="seashell4", size=2, alpha=0.5)

pruned_beast_tree

#-----------------------------------------------
#Plot with geologic timescale using deeptime (no x axis labels)


pal<- wes_palette("Zissou1", 8, type="continuous")
pal2 <- gray.colors(4, start = 0.4, end = 0.9, gamma = 2)

beasttree <- ggtree(pruned_beast_tree, right=FALSE) +  
  theme_tree2() + 
  geom_tiplab(offset=1.5, align=T,size=2, hjust=0) + geom_range("height_0.95_HPD", color="seashell4", size=2, alpha=0.5) + 
  coord_geo(dat=list("epochs", "periods"),xlim=c(-90, 30), ylim=c(-2, 39), height=unit(0.75, "line"), size=list(2.5,3), pos=list("b", "b"),neg=TRUE, abbrv= FALSE, fill = list(pal, pal2), center_end_labels = TRUE) + 
  scale_x_continuous(breaks=seq(-80,0,10), minor_breaks =seq(-80,0,5)) + 
  theme(panel.grid.major = element_line(color="grey", size=0.2), panel.grid.minor =element_line(color="grey", size=.2, linetype = "dotted"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

revts(beasttree)


geom_text(aes(x=max(x), label=label), size=1.5, hjust=-0.15)
#---------------------------------------------
