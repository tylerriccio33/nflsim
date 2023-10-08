
library(tidyverse)
library(rlang)
library(glue)
library(gt)
library(gtExtras)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(dataveRse)

clean_colnames_vec <- function(x) {
  glue("{str_replace_all(x, '_', ' ') %>% str_to_title()}")
}

# create lookup of traits
Stat <- function(name, text, max = T) {
  
  # max method
  if (max) {
    max_fn <- function(x) {x}
  } else {
    max_fn <- function(x) {
      rev(x)
    }
  }
  
  # set attrs
  cls <- list(
    name = name,
    text = text,
    max = max,
    max_fn = list(max_fn)
  )
  
  return(cls)
  
}


lookup_replace <- function(df1, df2, by, df2_replace, quiet = F) {
  
  if (!quiet) {
    df2_cols <- ncol(df2)
    if (df2_cols != 2) {
      cli::cli_danger("df2 without 2 columns (lookup and by) may have unexpected results.")
    }
  }
  
  if (!is_named(by)) {
    cli::cli_abort("by should be a named list")
  }
  
  names <- names(by)
  
  tryCatch({
    replaced <- df1 %>%
      left_join(
        df2,
        by = by
      )
  }, error = function(e) {
    cli::cli_abort(glue("Error in join, likely relating to `by` arg -> {e}"))
  })
  
  if (length(names) > 1) {
    cli::cli_inform("First name of by will be used to rename the lookup")
  }
  
  name <- names[[1]]
  tryCatch({
    replaced <- replaced %>%
      select(-all_of(names)) %>%
      rename({{name}} := {{df2_replace}})
  }, error = function(e) {
    cli::cli_abort(glue("Error in removing and renaming -> {e}"))
  })
  
  
  return(replaced)
  
}

extract_scores <- function(result) {
  
  result %>%
    select(id_posteam, posteam_score) %>%
    group_by(id_posteam) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
}

extract_score_differential <- function(result) {
  
  slice_tail(result, n = 1) %>% 
    pull(score_differential) %>%
    .[[1]]
  
}

extract_variable <-
  function(data,
           ...,
           result_col = replication_results,
           slice_function = first) {
    
    # unpack values
    # no tidyselect, just enquos
    
    cols <- enquos(...) %>% map_chr(quo_name)
    
    conversion_func <- function(vals) {
      
      type <- typeof(vals)
      
      if(type == 'character') {
        return(map_chr)
      }
      if(type == 'double') {
        return(map_dbl)
      }
      if(type == 'integer') {
        return(map_int)
      }
      
      rlang::abort("Conversion function logic breakdown")
      
    }
    
    for (col in cols) {
      
      example_results <- pull(data, {{result_col}}) %>% .[[1]]
      values <- pull(example_results, {{col}})
      fn <- conversion_func(values)
      
      data <- data %>%
        mutate({{col}} := fn({{result_col}}, \(x) slice_function(pull(x, {{col}}) )))
      
    }
    
    return(data)
    
  }


calculate_qb_stats <- function(result) {
  drop_na(result, id_passer) %>%
    group_by(id_passer) %>%
    summarize(
      completions = sum(complete_pass == 1),
      incompletions = sum(incomplete_pass == 1),
      dropbacks = n(),
      passes = completions + incompletions,
      total_yards = sum(yards_gained),
      passing_yards = sum(passing_yards, na.rm = T),
      rushing_yards = sum(rushing_yards, na.rm = T),
      cp = completions / passes,
      cpoe = mean(cpoe, na.rm = T),
      epa = mean(qb_epa[qb_dropback==1]),
      tds = sum(touchdown == 1 & complete_pass == 1),
      sacks = sum(sack == 1),
      interceptions = sum(interception == 1),
      scrambles = sum(qb_scramble == 1),
      receiver_diversity = n_distinct(id_receiver_player),
      max_air_yards = max(air_yards, na.rm = T),
      receiver_help = sum(yards_after_catch, na.rm = T) / total_yards,
      adot = mean(air_yards[(incomplete_pass == 1 | complete_pass == 1) & qb_dropback == 1])
    )
  
}


summarize_game <- function(game) {
  group_by(game, id_posteam) %>%
    summarize(
      tds = sum(touchdown),
      interceptions = sum(interception == 1),
      qb_hits = sum(qb_hit[qb_dropback == 1]),
      pass_wpa = sum(wpa[complete_pass == 1]),
      rush_wpa = sum(wpa[qb_dropback == 0]),
      early_down_passing = mean(pass_oe[down %in% c(1:2)]),
      air_yards = mean(air_yards[pass_attempt==1]),
      yac_wpa = mean(yac_wpa[complete_pass==1]),
      top = sum(total_play_time)
    )
}

