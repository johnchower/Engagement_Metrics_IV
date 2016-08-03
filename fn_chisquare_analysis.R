# Function: chisquare_analysis

# Takes in a long data frame with two categorical columns and a "count" column.
# Returns a long data frame with the z-score variable (observed - expected)/sqrt(expected)
#  , the chi-square variable (z^2), the number of degrees of freedom, the overall p-value,
#  and a data frame with the individual p-values.

chisquare_analysis <- 
  function(df, err_pvalue = .99){
    df %<>% ungroup 
    
    orig_names <- colnames(df)
    
    colnames(df) <- c("v1", "v2", "observed")
    
    total_observations <- df %>%
      {.$observed} %>%
      sum
    
    degrees_of_freedom <- 
      (length(unique(df$v1))-1)*(length(unique(df$v2))-1)
    
    df_jointprob <- df %>%
      mutate(jointprob = observed/total_observations)
    
    df_jointprob_marginalprob <- df_jointprob %>%
      group_by(v1) %>%
      mutate(marginal1 = sum(jointprob)) %>%
      ungroup %>%
      group_by(v2) %>%
      mutate(marginal2 = sum(jointprob)) %>%
      ungroup
    
    df_jointprob_marginalprob_expected <- df_jointprob_marginalprob %>%
      mutate(
        expected = total_observations*marginal1*marginal2
        , zscore = (observed - expected)/sqrt(expected)
        , chiscore = zscore^2
        , pvalue = pchisq(chiscore, degrees_of_freedom, lower.tail = F)
        , err = sqrt(qchisq(err_pvalue, degrees_of_freedom)*expected)
      ) %>%
      {
        colnames(.)[1:3] <- orig_names
        return(.)
      }
    
    total_chiscore <- sum(df_jointprob_marginalprob_expected$chiscore)
    total_pvalue <- pchisq(total_chiscore, degrees_of_freedom, lower.tail = F)
    
    return(
      list(
        pvalue = total_pvalue
        , chisquared = total_chiscore
        , results = df_jointprob_marginalprob_expected
        , degrees.of.freedom = degrees_of_freedom
      )
    )
    
  }