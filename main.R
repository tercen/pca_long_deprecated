library(tercen)
library(tercenApi)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

ctx = tercenCtx()

scale = ctx$op.value("scale", type=as.logical, default=FALSE)  
center = ctx$op.value("center", type=as.logical, default=TRUE)  
tol = ctx$op.value("tol", type=as.double, default=0)  
maxComp = ctx$op.value("maxComp", type=as.integer, default=5)  

bTranspose = "observations.in.columns" == ctx$op.value("input.convention", type=as.character, default="observations.in.columns") 

data.matrix = if (bTranspose){ t(ctx %>% as.matrix()) } else { ctx %>% as.matrix() }
rowOrColumnRelation = if (bTranspose){ ctx$crelation } else { ctx$rrelation }
names = if (bTranspose){ ctx$cnames } else { ctx$rnames }

aPca = data.matrix %>% prcomp(scale = scale, center = center, tol = tol)

maxComp = if (maxComp > 0){ min(maxComp, nrow(aPca$rotation)) } else { nrow(aPca$rotation) }

eigenRelation = tibble(pc.eigen.values = aPca$sdev^2) %>% 
  mutate(var_explained = .$pc.eigen.values / sum(.$pc.eigen.values))%>% 
  mutate(PC = sprintf(paste0("PC%0", nchar(as.character(nrow(.))), "d"), 1:nrow(.))) %>%  # pad left with 0 to ensure alphabetic order
  ctx$addNamespace() %>%
  as_relation()

loadingRelation = aPca$rotation[,1:maxComp] %>% as_tibble() %>% 
  setNames(0:(ncol(.)-1)) %>%  
  pivot_longer(everything(),
               names_to = ".pc.eigen.values.rids",
               values_to = "pc.loading",
               names_transform=list(.pc.eigen.values.rids=as.integer)) %>% 
  ctx$addNamespace() %>%
  as_relation()

scoresRelation = aPca$x[,1:maxComp] %>%
  as_tibble() %>%
  setNames(0:(ncol(.)-1)) %>%  
  mutate(.i=0:(nrow(.)-1)) %>% 
  pivot_longer(-.i,
               names_to = ".pc.eigen.values.rids",
               values_to = "pc.value",
               names_transform=list(.pc.eigen.values.rids=as.integer)) %>% 
  ctx$addNamespace() %>% 
  as_relation() %>%
  left_join_relation(rowOrColumnRelation,
                     ".i",
                     rowOrColumnRelation$rids)

# link all 3 relation into one and save 
eigenRelation %>%
  left_join_relation(loadingRelation, eigenRelation$rids, ".pc.eigen.values.rids") %>%
  left_join_relation(scoresRelation, eigenRelation$rids, ".pc.eigen.values.rids")  %>% 
  as_join_operator(names, names) %>%
  save_relation(ctx)
