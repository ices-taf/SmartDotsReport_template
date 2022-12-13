

# Determine modal age and CV ##################################################

# For each fish the cv and modal age are calculated.
# If one age is more frequent than others, that age is chosen as modal age.
# If no age is more frequent, then the average of all ages are chosen or
# if two (or more) ages are equally frequent then the age read by the most
# expericed reader will be chosen as modal age.
# WHich method to use is set in the ma_method variable.
# If the modal age is 0 the CV is set to 0 as well.

add_modal_trad <- function(ad, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, reader, age) %>%
    ddply(.(FishID, age), summarise, count=length(reader)) %>%
    spread(key = age, value = count)
  
  out[is.na(out)]=0
  
  ages <- out %>% select(-c(FishID))
  
  
  # Determine modal age depending on ma_method
  out$modal_trad <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(ages, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }
  
  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1,dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max)])
  }
  
  out$NModes_trad=countcases
  
  # calculate CV
  out$cv <- apply(ages, 1, cv_I)
  out$cv[is.na(out$modal_age) | out$modal_age == 0] <- NA
  
  # merge CV and modal age to data, first add proper column name (should work no matter n age class)
  out <- out %>% rename_with(.fn=~paste0(.,".trd"), .cols = -c(FishID, modal_trad, NModes_trad, cv))
  right_join(ad, out, by = c("FishID"))
}



add_modal_linearweight <- function(ad, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, weight_I, age) %>%
    ddply(.(FishID, age), summarise, readerweight=sum(weight_I)) %>%
    spread(key = age, value = readerweight)
  
  out[is.na(out)]=0
  
  ages <- out %>% select(-c(FishID))
  
  # Determine modal age stage depending on ma_method
  out$modal_linearweight <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(ages, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }
  
  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1, dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max)])
  }
  
  out$NModes_linear=countcases
  
  # merge CV and modal age to data
  out <- out %>% rename_with(.fn=~paste0(.,".lin"), .cols = -c(FishID, modal_linearweight, NModes_linear))
  right_join(ad, out, by = c("FishID"))
}




add_modal_negexpweight <- function(ad, ma_method) {
  
  # ages by fish
  out <-
    ad %>%
    select(FishID, weight_II, age) %>%
    ddply(.(FishID, age), summarise, readerweight=sum(weight_II)) %>%
    spread(key = age, value = readerweight)
  
  out[is.na(out)]=0
  
  ages <- out %>% select(-c(FishID))
  
  # Determine modal age stage depending on ma_method
  out$modal_negexpweight <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(ages, 1,
            function(x) {
              if (!is.null(Mode_I(x))) {
                Mode_I(x)
              } else {
                trunc(mean(x, na.rm = TRUE) + 0.5)
              }
            })
    }

  countcases=vector(length=dim(out)[1])
  for(e in 1:dim(out)[1])
  {
    sel=out[e,]
    df=sel[,-c(1, dim(out)[2])]
    max=max(df)
    countcases[e]=length(df[which(df==max)])
  }
  
  out$NModes_negexp=countcases
  
  # merge CV and modal age to data
  out <- out %>% rename_with(.fn=~paste0(.,".neg"), .cols = -c(FishID, modal_negexpweight, NModes_negexp))
  right_join(ad, out, by = c("FishID"))
}




select_mode=function(ad, ma_method, mode_definition){
  if(mode_definition=="multistage")
  {
  dat = ad %>%
    select(FishID, modal_trad, NModes_trad, modal_linearweight, NModes_linear, modal_negexpweight, NModes_negexp) %>%
    distinct()

  dat$modal_age <-
    if (ma_method == "Mean") {
      stop ("mean not implemented yet")
    } else if (ma_method == "Mode") {
      apply(dat, 1,
            function(x) {
              ifelse(x[3]==1, as.numeric(x[2]), ifelse(x[5]==1, as.numeric(x[4]), ifelse(x[7]==1, as.numeric(x[6]),"Multimode")))
              })
    }
  
  dat= dat %>% select(FishID, modal_age)  
  
  right_join(ad, dat, by = c("FishID"))
  
  } else {
    
    dat = ad %>%
      select(FishID, modal_trad, NModes_trad) %>%
      distinct()

    dat$modal_age <-
      if (ma_method == "Mean") {
        stop ("mean not implemented yet")
      } else if (ma_method == "Mode") {
        apply(dat, 1,
              function(x) {
                as.numeric(x[2])
                })
      }
    
    dat= dat %>% select(FishID, modal_age)  
    
    right_join(ad, dat, by = c("FishID"))
    
  }
  
}


