
'%not_in%' <- function(x,y)!('%in%'(x,y))

get_adjacent_blocks <- function(origin_id, grid) {
  # Ensure that sf_object is indeed an sf object
  if (!inherits(grid, "sf")) {
    stop("The input sf_object must be an sf object.")
  }
  
  # Check if block_id column exists
  if (!"block_id" %in% colnames(grid)) {
    stop("The sf_object must contain a 'block_id' column.")
  }
  
  # Get the block_ids of sampled blocks 
  sampled_ids <- grid %>% filter(in_sample ==1) %>% pull(block_id)
  
  # Get the geometry of the block
  block_geom <- grid %>% filter(block_id == origin_id)
  
  # Check if block_geom is empty
  if (length(block_geom) == 0) {
    stop("The specified block_id does not exist in the provided sf object.")
  }
  
  # Select blocks that are adjacent by checking if they touch the target block's geometry
  adjacent_blocks <- grid[st_touches(grid, block_geom, sparse = FALSE), ]
  
  # Remove the origin block from the list of adjacent blocks if it's included
  adjacent_blocks <- adjacent_blocks[adjacent_blocks$block_id %not_in% c(origin_id, sampled_ids), ]
  
  return(adjacent_blocks)
}

expand_network <- function(origin_id, grid, N) {
  # Ensure grid is an sf object
  if (!inherits(grid, "sf")) {
    stop("grid must be an sf object.")
  }
  
  # Initialize the list of blocks to explore with the origin
  blocks_to_explore <- grid %>% filter(block_id == origin_id)
  
  # Add an iteration column to the origin block
  blocks_to_explore$iteration <- 0
  
  # Initialize an sf object to store explored blocks, copying the structure of grid
  explored_blocks <- blocks_to_explore
  
  for (i in 1:N) {
    # Temporary storage for new blocks found in this iteration
    new_blocks <- vector("list", nrow(blocks_to_explore))
    
    # Iterate over each block to explore
    for (j in 1:nrow(blocks_to_explore)) {
      # Use get_adjacent_blocks to find adjacent blocks
      adjacent_blocks <- get_adjacent_blocks(blocks_to_explore[j, ]$block_id, grid)
      
      # Add an iteration column to these blocks
      adjacent_blocks$iteration <- i
      
      # Store the found adjacent blocks
      new_blocks[[j]] <- adjacent_blocks
    }
    
    # Combine all newly found blocks into one sf object
    new_blocks <- do.call(rbind, new_blocks)
    
    # Filter out blocks that have already been explored
    new_blocks <- new_blocks %>% 
      filter(!block_id %in% explored_blocks$block_id)
    
    # Update the list of blocks to explore with the newly found blocks
    blocks_to_explore <- new_blocks
    
    # Update the list of explored blocks
    explored_blocks <- rbind(explored_blocks, new_blocks)
  }
  
  # Return the unique list of all blocks found, including the geometry and iteration columns
  return(unique(explored_blocks) %>% filter(block_id != origin_id))
}
