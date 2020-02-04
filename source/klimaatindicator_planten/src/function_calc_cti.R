calc_cti <- function(traitdata,
                     yeareffectdata,
                     nrep,
                     interval = 0.95,
                     relatief = TRUE) {
  
  # sortering garanderen
  yeareffectdata <- yeareffectdata %>%
    arrange(NaamWetenschappelijk, Jaar)
  
  traitdata <- traitdata %>%
    arrange(NaamWetenschappelijk)
  
  stopifnot(all.equal(unique(traitdata$NaamWetenschappelijk),
                      unique(yeareffectdata$NaamWetenschappelijk)
  ))
  
  
  njaar <- length(unique(yeareffectdata$Jaar))
  
  nspec <- length(unique(yeareffectdata$NaamWetenschappelijk))
  
  if (njaar * nspec > nrow(yeareffectdata)) {
    alle_combinaties <- expand.grid(
      Jaar = unique(yeareffectdata$Jaar),
      NaamWetenschappelijk = unique(yeareffectdata$NaamWetenschappelijk),
      stringsAsFactors = FALSE)
    
    yeareffectdata <- alle_combinaties %>%
      left_join(yeareffectdata,
                by = c("Jaar", "NaamWetenschappelijk")) %>%
      as_tibble()
  }
  
  
  yr_est_vec <- yeareffectdata$year_est + yeareffectdata$intercept + 
    yeareffectdata$listlength_est
  
  yr_se_vec <- yeareffectdata$year_se
  
  absprop_vec <- plogis(
    rnorm(n = length(yr_est_vec) * nrep,
          mean = yr_est_vec,
          sd = yr_se_vec)
  ) # sortering simulatie soort jaar
  
  abs_props <- array(absprop_vec, 
                     dim = c(njaar, nspec, nrep), #snelts variërende variabele eerst
                     dimnames = list(jaar = sort(unique(yeareffectdata$Jaar)),
                                     soort = sort(unique(yeareffectdata$NaamWetenschappelijk)),
                                     simulatie = 1:nrep
                     )
  )
  
  # maximum per soort en simulatie over de jaren
  max_props <- apply(
    X = abs_props, 
    MARGIN = c(2,3), 
    FUN = max,
    na.rm = TRUE)
  
  rel_props <- abs_props
  
  for (i in 1:njaar) {
    rel_props[i,,] <- abs_props[i,,] / max_props
  }
  
  # temperatuurwaarden simuleren
  temp_vec <- traitdata$temp
  temp_se <- traitdata$temp_se
  temp_sim_vec <- rnorm(
    n = length(temp_vec) * nrep,
    mean = temp_vec, 
    sd = temp_se)
  
  temp_sim <- array(data = temp_sim_vec, 
                    dim = c(nspec, nrep),
                    dimnames = list(soort = sort(unique(yeareffectdata$NaamWetenschappelijk)),
                                    simulatie = 1:nrep))
  
  # berekening cti (elk jaar één waarde, voor elke simulatie)
  out <- array(data = NA, 
               dim = c(njaar, nrep),
               dimnames = list(jaar = sort(unique(yeareffectdata$Jaar)),
                               simulatie = 1:nrep)
  )
  
  # alle NA waarden in rel_props en abs_props op 0 zetten
  # zodat ze niet meetellen bij de matrixvermenigvuldiging
  abs_props[is.na(abs_props)] <- 0
  rel_props[is.na(rel_props)] <- 0
  
  for (i in 1:nrep) {
    # gewogen gemiddelde mbv matrixvermenigvuldiging
    if (relatief) {
      out[,i] <- (rel_props[,,i] / rowSums(rel_props[,,i])) %*%
        temp_sim[,i]
    } else {
      out[,i] <- (abs_props[,,i] / rowSums(abs_props[,,i])) %*% 
        temp_sim[,i]
    }
  }
  
  # gemiddelde en betrouwbaarheidsinterval
  cti_est <- apply(out, 1, mean)
  cti_se <- apply(out, 1, sd)
  cti_lwr <- apply(out, 1, quantile, probs = (1 - interval) / 2)
  cti_upr <- apply(out, 1, quantile, probs = (1 + interval) / 2)
  
  cti <- data.frame(cti_est = cti_est,
                    cti_se = cti_se,
                    cti_lwr = cti_lwr,
                    cti_upr = cti_upr) %>%
    as_tibble(rownames = "Jaar")
  
  return(cti)
}






