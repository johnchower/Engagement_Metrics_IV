# Function: find_true_first_champ

# give it a vector of champion ids it returns:

# 0 if the users' only connections are internal (e.g. 0 => "internal")
# -1 if the user's first champion is still ambiguous
# the id if the users' true first champ if it's unambiguous.

find_true_first_champ <-
  function(
    champvec
    , champ.subset.relation = champion_subset_relation
    , internal.champs = internal_champs
  ){
    legit_champs <- 
      data.frame(champion_id = champvec) %>% 
      filter(!(champion_id %in% internal.champs)) 
    
    if(nrow(legit_champs) == 0){
      out <- 0
    } else{
      out <- legit_champs %>%
        rename(champion_id_x = champion_id) %>%
        {merge(., rename(., champion_id_y = champion_id_x), all = T)} %>% 
        filter(champion_id_x != champion_id_y) %>% 
        merge(
          select(champ.subset.relation, champion_id_x, champion_id_y, y_contains_x)
        ) %>% 
        filter(y_contains_x) %>% 
        {.$champion_id_y} %>% 
        {setdiff(legit_champs$champion_id,.)} %>%
        {
          if(length(.) == 0){
            out_1 <- 0
          } else if(length(.) == 1){
            out_1 <- .
          } else {out_1 <- -1}
          return(out_1)
        }
    }
    
    return(out)
  }