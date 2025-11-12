
block_category_labels <- c(
  `1` = "Off-limits", 
  `2` = "Empty/not-urban", 
  `3` = "Insecure", 
  `4` = "Strata 1: High probability", 
  `5` = "Strata 2: Low probability"
)

# Cleaning data and selecting final sample 

#countries <- c('Nigeria', 'India', 'Indonesia', 'Ethiopia')

countries <- c('Brazil')

for (country in countries) {
  
  # Importing partner categorization of blocks 
  file_path <- paste0("data/", country, "/", "stage1_sampling_125_blocks", ".csv")
  
  initial_sample <- read_csv(file_path, show_col_types = FALSE) %>% 
      mutate(
        block_id = as.character(block_id), 
        block_category_str = block_category_labels[block_category], 
        block_label = paste("[Block ID:] ", block_id, "[Category:] ", block_category_str, "[Notes:]", block_observations, sep = " "), 
        # Tagging blocks eligible for enumeration
        block_eligible = ifelse(block_category <= 3, 0, 1)
      ) %>% 
      select(block_id, block_label, block_category_str, block_eligible)
  
  table(initial_sample$block_category_str)
  
  total_eligible <- sum(initial_sample$block_eligible)
  
  print(sprintf("%s: Total blocks eligible for enumeration: %i", country, total_eligible))
  
  if (total_eligible > 100) { 
    
    # Randomly selecting 100 blocks from eligible
    initial_sample %>% filter(block_eligible == 1) %>% slice_sample(n = 100, replace = FALSE) %>% mutate(in_final_sample = 1) %>% select(block_id, in_final_sample) -> final_sample
    
  } else { 
    
    initial_sample %>% filter(block_eligible == 1) %>% mutate(in_final_sample = 1) %>% select(block_id, in_final_sample) -> final_sample
    
  }
  
  stage1_blocks <- initial_sample %>% left_join(final_sample, by = "block_id") %>% mutate(in_final_sample = ifelse(is.na(in_final_sample), 0, in_final_sample))
  
  output_path <- paste0("data/", country, "/", "stage1_blocks_final.csv")
  write_csv(stage1_blocks, output_path)

}

