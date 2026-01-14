library(dplyr)
library(tidyr)
library(gt)
library(purrr)

# Function to create the summary table as requested
create_client_summary_table <- function(df) {
  # Ensure proper factor levels for timepoint and treatment
  df <- df %>%
    mutate(
      timepoint = factor(timepoint, levels = levels(df$timepoint)),
      treatment = factor(treatment, levels = levels(df$treatment))
    )
  
  # Get all unique combinations of timepoint and treatment
  all_combinations <- expand.grid(
    timepoint = levels(df$timepoint),
    treatment = levels(df$treatment)
  ) %>%
    as_tibble() %>%
    arrange(timepoint, treatment)
  
  # Count clients with data points (where IoD_reduced is not NA) for each combination
  # Fix: Count distinct client_id for each timepoint-treatment combination
  clients_with_data <- df %>%
    filter(!is.na(IoD_reduced)) %>%  # Only consider rows where IoD_reduced has data
    group_by(timepoint, treatment) %>%
    summarise(
      clients_with_data = n_distinct(client_id),
      .groups = "drop"
    )
  
  # Join to get all combinations
  result <- all_combinations %>%
    left_join(clients_with_data, by = c("timepoint", "treatment")) %>%
    mutate(clients_with_data = replace_na(clients_with_data, 0))
  
  # Add client IDs with data (where IoD_reduced is not NA)
  client_ids_with_data <- df %>%
    filter(!is.na(IoD_reduced)) %>%  # Only consider rows where IoD_reduced has data
    group_by(timepoint, treatment) %>%
    summarise(
      client_ids_with_data = paste(unique(client_id), collapse = ", "),
      .groups = "drop"
    )
  
  result <- result %>%
    left_join(client_ids_with_data, by = c("timepoint", "treatment"))
  
  # Calculate clients who miss data at current timepoint but have it at next timepoint
  result <- result %>%
    mutate(
      clients_missing_then_having = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for next timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx < length(timepoint_order)) {
          next_tp <- timepoint_order[current_idx + 1]
          clients_next_timepoint <- df %>%
            filter(timepoint == next_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were missing at current but present at next
          clients_missing_then_having <- setdiff(unique(df$client_id), clients_at_timepoint)
          clients_missing_then_having <- intersect(clients_missing_then_having, clients_next_timepoint)
          length(clients_missing_then_having)
        } else {
          0
        }
      }) %>% unlist(),
      
      client_ids_missing_then_having = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for next timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx < length(timepoint_order)) {
          next_tp <- timepoint_order[current_idx + 1]
          clients_next_timepoint <- df %>%
            filter(timepoint == next_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were missing at current but present at next
          clients_missing_then_having <- setdiff(unique(df$client_id), clients_at_timepoint)
          clients_missing_then_having <- intersect(clients_missing_then_having, clients_next_timepoint)
          paste(clients_missing_then_having, collapse = ", ")
        } else {
          ""
        }
      }) %>% unlist()
    )
  
  # Create a helper function to track already seen dropout clients
  # First, calculate all dropout values without deduplication
  result <- result %>%
    mutate(
      temp_clients_dropout = map2(timepoint, treatment, function(tp, tr) {
        # Get all clients for this specific timepoint and treatment
        clients_at_timepoint <- df %>%
          filter(timepoint == tp & treatment == tr & !is.na(IoD_reduced)) %>%
          distinct(client_id) %>%
          pull(client_id)
        
        # Get all clients for previous timepoint with same treatment
        timepoint_order <- levels(df$timepoint)
        current_idx <- which(timepoint_order == tp)
        if(current_idx > 1) {
          prev_tp <- timepoint_order[current_idx - 1]
          clients_prev_timepoint <- df %>%
            filter(timepoint == prev_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Get all clients for next timepoint with same treatment
          next_tp <- timepoint_order[current_idx + 1]
          clients_next_timepoint <- df %>%
            filter(timepoint == next_tp & treatment == tr & !is.na(IoD_reduced)) %>%
            distinct(client_id) %>%
            pull(client_id)
          
          # Clients who were in previous timepoint but not in next timepoint
          clients_dropout <- setdiff(clients_prev_timepoint, clients_next_timepoint)
          
          # Don't exclude clients_missing_then_having anymore - include ALL dropout clients
          paste(clients_dropout, collapse = ", ")
        } else {
          ""
        }
      }) %>% unlist()
    )
  
  # Now process the temp column to remove duplicates across rows
  # We'll do this in a separate step to avoid reference issues
  result <- result %>%
    mutate(
      # Initialize the final dropout columns
      clients_dropout = rep(0, nrow(result)),
      client_ids_dropout = rep("", nrow(result))
    )
  
  # Process each row individually to ensure no duplicate client IDs across rows
  for(i in 1:nrow(result)) {
    tp <- result$timepoint[i]
    tr <- result$treatment[i]
    
    # Skip calculation for the last timepoint (no next timepoint)
    timepoint_order <- levels(df$timepoint)
    current_idx <- which(timepoint_order == tp)
    if(current_idx == length(timepoint_order)) {
      # Last timepoint - no dropout possible
      result$clients_dropout[i] <- 0
      result$client_ids_dropout[i] <- ""
    } else {
      # Not last timepoint - calculate dropout
      # Get the temporary dropout IDs for this row
      temp_dropout_ids <- strsplit(result$temp_clients_dropout[i], ",") %>% 
        unlist() %>% 
        trimws() %>% 
        na.omit()
      
      # Get all previously processed dropout IDs for same treatment (only up to current row)
      previous_dropout_ids <- result %>%
        slice(1:(i-1)) %>%
        filter(treatment == tr) %>%
        pull(temp_clients_dropout) %>%
        strsplit(",") %>%
        unlist() %>%
        trimws() %>%
        na.omit()
      
      # Remove duplicates from current row
      unique_dropout_ids <- setdiff(temp_dropout_ids, previous_dropout_ids)
      
      # Update the final columns
      result$clients_dropout[i] <- length(unique_dropout_ids)
      result$client_ids_dropout[i] <- paste(unique_dropout_ids, collapse = ", ")
    }
  }
  
  # Clean up temporary column
  result <- result %>%
    select(-temp_clients_dropout)
  
  # Reshape data to have timepoint as rows and treatment as span headers
  # Group by timepoint and create wide format
  final_result <- result %>%
    pivot_wider(
      names_from = treatment,
      values_from = c(clients_with_data, client_ids_with_data, clients_missing_then_having, 
                     client_ids_missing_then_having, clients_dropout, client_ids_dropout),
      names_glue = "{treatment}_{.value}"
    ) %>%
    arrange(timepoint)
  
  # Create the gt table with span headers
  tbl <- final_result %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("Client Data Summary"),
      subtitle = gt::md("Number of clients with data points by timepoint and treatment")
    ) %>%
    # Set column labels for the span headers - corrected order
    gt::cols_label(
      timepoint = "Timepoint",
      Intervention_clients_with_data = "Intervention - Clients with Data",
      Placebo_clients_with_data = "Placebo - Clients with Data",
      Intervention_client_ids_with_data = "Intervention - Client IDs with Data",
      Placebo_client_ids_with_data = "Placebo - Client IDs with Data",
      Intervention_clients_missing_then_having = "Intervention - Missing Then Having",
      Placebo_clients_missing_then_having = "Placebo - Missing Then Having",
      Intervention_client_ids_missing_then_having = "Intervention - Client IDs Missing Then Having",
      Placebo_client_ids_missing_then_having = "Placebo - Client IDs Missing Then Having",
      Intervention_clients_dropout = "Intervention - Dropout",
      Placebo_clients_dropout = "Placebo - Dropout",
      Intervention_client_ids_dropout = "Intervention - Client IDs Dropout",
      Placebo_client_ids_dropout = "Placebo - Client IDs Dropout"
    ) %>%
    # Add span headers
    gt::tab_spanner(
      label = "Clients with Data",
      columns = vars(Intervention_clients_with_data, Placebo_clients_with_data)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs with Data",
      columns = vars(Intervention_client_ids_with_data, Placebo_client_ids_with_data)
    ) %>%
    gt::tab_spanner(
      label = "Missing Then Having",
      columns = vars(Intervention_clients_missing_then_having, Placebo_clients_missing_then_having)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs Missing Then Having",
      columns = vars(Intervention_client_ids_missing_then_having, Placebo_client_ids_missing_then_having)
    ) %>%
    gt::tab_spanner(
      label = "Dropout",
      columns = vars(Intervention_clients_dropout, Placebo_clients_dropout)
    ) %>%
    gt::tab_spanner(
      label = "Client IDs Dropout",
      columns = vars(Intervention_client_ids_dropout, Placebo_client_ids_dropout)
    )
  
  return (tbl)
}

# Usage example (replace 'your_data_frame' with your actual data frame name):
tbl <- create_client_summary_table(dat_complete %>% select(client_id, treatment, timepoint, IoD_reduced))