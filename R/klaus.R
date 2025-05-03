# Generic function for interacting with the ChatAI API
chatai <- function(x = "",
                   system_prompt = "You are a helpful assistant",
                   model = "meta-llama-3.1-8b-instruct",
                   temperature = 0) {

  # --- Input Validation ---
  stopifnot(
    is.character(x), length(x) == 1,
    is.character(system_prompt), length(system_prompt) == 1,
    is.character(model), length(model) == 1, nzchar(model),
    is.numeric(temperature), length(temperature) == 1, temperature >= 0
  )

  # --- API Key ---
  api_key <- Sys.getenv("chatai_key")
  if (api_key == "") {
    stop("API key not found. Please set the 'chatai_key' environment variable.", call. = FALSE)
  }

  # --- API Endpoint and Headers ---
  url <- "https://chat-ai.academiccloud.de/v1/chat/completions"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key),
    "Content-Type" = "application/json"
  )

  # --- Request Body ---
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = x)
    ),
    temperature = temperature
  )

  # --- API Call ---
  response <- tryCatch(
    httr::POST(url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json"), # Specify encoding type
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )

  # --- Status Code Check ---
  status_code <- httr::status_code(response)
  if (status_code >= 400) {
    # Attempt to get error message from response body
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  if (status_code >= 300) {
    # Handle redirects or other non-2xx success codes if necessary
    warning(sprintf("API request returned status %d.", status_code))
  }
  if (status_code != 200) {
    # Handle unexpected success codes if needed
    warning(sprintf("API request returned unexpected status %d.", status_code))
  }

  # --- Response Parsing ---
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )

  # --- Response Structure Validation ---
  content_out <- tryCatch(
    response_parsed$choices$message$content[[1]], # Assuming single choice often
    error = function(e) NULL # Return NULL if path doesn't exist
  )

  if (is.null(content_out) || !is.character(content_out)) {
    stop("Unexpected API response structure. Could not extract content.",
         "\nParsed response: ", utils::str(response_parsed), call. = FALSE)
  }

  # --- Return Content ---
  return(content_out)
}

# Function to list the available models
chatai_models <- function() {
  # --- API Key ---
  api_key <- Sys.getenv("chatai_key")
  if (api_key == "") {
    stop("API key not found. Please set the 'chatai_key' environment variable.", call. = FALSE)
  }

  # --- API Endpoint and Headers ---
  url <- "https://chat-ai.academiccloud.de/v1/models"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key)
    # Content-Type is typically not needed for GET requests
  )

  # --- API Call (using GET) ---
  response <- tryCatch(
    httr::GET(url,
              httr::add_headers(.headers = headers)
    ),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )

  # --- Status Code Check ---
  status_code <- httr::status_code(response)
  if (status_code != 200) { # Expecting 200 OK for GET success
    # Attempt to get error message from response body
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }

  # --- Response Parsing ---
  # Check content type before parsing (optional but good practice)
  if (!grepl("application/json", httr::http_type(response), fixed = TRUE)) {
    warning("API did not return JSON content type.")
  }

  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )

  # --- Response Structure Validation ---
  if (!is.list(response_parsed) || !("data" %in% names(response_parsed))) {
    stop("Unexpected API response structure. Missing 'data' element.",
         "\nParsed response structure: ", utils::str(response_parsed), call. = FALSE)
  }

  return(response_parsed$data)
}

# Helper function to convert a CSV file to JSON to illustrate categories and desired structure to the LLM
parse_codebook <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1)
  df <- tryCatch(
    readr::read_csv(filepath, show_col_types = FALSE),
    error = function(e) {
      # Stop execution with an informative error if file reading fails
      stop(paste("Failed to read CSV file:", filepath, "\nOriginal error:", conditionMessage(e)), call. = FALSE)
    }
  )
  required_cols <- c("category", "label", "instructions")
  if (!all(required_cols %in% names(df))) {
    missing_cols <- setdiff(required_cols, names(df))
    stop(paste("CSV must contain 'category', 'label', and 'instructions' columns. Missing:", paste(missing_cols, collapse=", ")), call. = FALSE)
  }
  df_cleaned <- df %>%
    dplyr::filter(!is.na(category))
  if (nrow(df_cleaned) == 0) {
    stop("No valid 'category' entries found after removing rows with NA category.", call. = FALSE)
  }
  unique_categories <- unique(df_cleaned$category)
  output_list <- list()
  for (cat_name in unique_categories) {
    summary_data <- df_cleaned %>%
      dplyr::filter(category == cat_name) %>%
      dplyr::summarise(
        label = list(label),          # Collect labels into a list
        instructions = list(instructions), # Collect instructions into a list
        .groups = 'drop'              # Drop grouping structure after summarise
      )
    output_list[[cat_name]] <- list(
      label = summary_data$label[[1]],
      instructions = summary_data$instructions[[1]]
    )
  }
  json_output <- jsonlite::toJSON(output_list, auto_unbox = TRUE, pretty = TRUE)
  return(invisible(json_output))
}

# Main function to code content
code_content <- function(x,
                         general_instructions,
                         formatting_instructions, # Make sure this is complete!
                         codebook, # Assumes JSON string from parse_codebook
                         model = "meta-llama-3.1-8b-instruct",
                         temperature = 0,
                         sleep = 1,
#                         multiple_labels = TRUE,
                         drop_json = TRUE,
                         drop_instructions = TRUE,
                         keep_all_original_rows = TRUE) {

  # --- Input Validation ---
  stopifnot(
    is.data.frame(x),
    "text" %in% names(x),
    is.character(x$text),
    # Add a temporary ID column if it doesn't exist to ensure uniqueness
    # Handle potential name clashes if user already has '.original_row_id'
    !(".original_row_id" %in% names(x)),
    is.character(general_instructions), length(general_instructions) == 1,
    is.character(formatting_instructions), length(formatting_instructions) == 1,
    is.character(codebook), length(codebook) == 1, jsonlite::validate(codebook), # Basic JSON check
    is.character(model), length(model) == 1, nzchar(model),
    is.numeric(temperature), length(temperature) == 1, temperature >= 0,
    is.numeric(sleep), length(sleep) == 1, sleep >= 0,
    is.logical(drop_json), length(drop_json) == 1,
    is.logical(drop_instructions), length(drop_instructions) == 1,
    is.logical(keep_all_original_rows), length(keep_all_original_rows) == 1
  )

  # --- Add original row identifier ---
  x <- dplyr::mutate(x, .original_row_id = dplyr::row_number())

  system_prompt <- paste0(formatting_instructions, "\n\n", codebook)
  n_rows <- nrow(x)
  # Pre-allocate list for results
  api_results <- vector("list", n_rows)

  message(glue::glue("Coding data with {model} ({n_rows} rows)"))
  message("Iterating over content...")

  # --- Loop 1: API Calls ---
  for (i in 1:n_rows) {
    user_prompt <- paste0(general_instructions, "\n\n----\n\n", x$text[i])
    result <- tryCatch({
      chatai(user_prompt,
             system_prompt = system_prompt,
             model = model,
             temperature = temperature)
    },
    error = function(e) {
      warning(glue::glue("API call failed for row {i} (ID: {x$.original_row_id[i]}): {conditionMessage(e)}"), call. = FALSE)
      return(NA_character_)
    }
    )
    api_results[[i]] <- result
    message(glue::glue("Processed text {i} of {n_rows}"))
    if (sleep > 0) Sys.sleep(sleep)
  }

  message(glue::glue("Parsing JSON responses ({n_rows} rows)."))

  # --- Process Results ---
  # Use map to process each result
  parsed_list <- purrr::map(seq_along(api_results), function(j) {
    # Get original ID for this row
    current_id <- x$.original_row_id[[j]]
    raw_json <- api_results[[j]]

    if (is.na(raw_json)) {
      return(NULL)
    }

    # --- Add Cleaning Step ---
    # Attempt to extract the first JSON object {...} or array [...]
    # This handles leading/trailing text and markdown fences like ```json
    json_content <- stringr::str_extract(raw_json, "\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]")

    # Check if extraction was successful
    if (is.na(json_content)) {
      warning(glue::glue("Could not extract JSON structure {{...}} or [...] from response for row {j} (ID: {current_id}). Skipping parsing. Raw content: {raw_json}"), call.=FALSE)
      return(NULL) # Fail parsing for this row if no structure found
    }
    # --- End Cleaning Step ---

    parsed_json <- tryCatch({
      jsonlite::fromJSON(json_content)
    }, error = function(e) {
      warning(glue::glue("JSON parsing failed for row {j} (ID: {current_id}): {conditionMessage(e)} \nRaw content: {raw_json}"), call. = FALSE)
      return(NULL)
    })

    if (is.null(parsed_json)) {
      return(NULL)
    }

    flattened_data <- tryCatch({
      # Key change: Add the original row ID to the flattened data
      purrr::map_dfr(parsed_json, ~ .x, .id = "category") %>%
        dplyr::mutate(.original_row_id = current_id) # Add ID here
    }, error = function(e) {
      warning(glue::glue("Failed to flatten parsed JSON structure for row {j} (ID: {current_id}): {conditionMessage(e)}"), call. = FALSE)
      return(NULL)
    })

    return(flattened_data)
  })

  # Combine only the non-NULL results from the list
  valid_results <- dplyr::bind_rows(parsed_list) # bind_rows handles NULLs gracefully

#  if (multiple_labels == FALSE) {
#    # --- Add filtering step ---
#    # Example: Keep only the first row encountered for each original ID
#    if (any(duplicated(valid_results$.original_row_id))) {
#      warning("Multiple categories per ID found after flattening. Keeping only the first row per original ID.", call. = FALSE)
#      valid_results <- valid_results %>%
#        dplyr::group_by(.original_row_id) %>%
#        dplyr::slice_head(n = 1) %>% # Or slice(1), slice_min(order_by=...), etc.
#        dplyr::ungroup()
#    }
#    # --- End filtering step ---
#  }

  if (nrow(valid_results) == 0) {
    message("No rows were successfully processed and parsed.")
    x$response_json <- unlist(api_results) # Add raw results if nothing parsed
    x$.original_row_id <- NULL # remove temp id
    return(x)
  }

  # --- Join results back to original data ---
  if (keep_all_original_rows) {
    # Keep all original rows, merging results where available (NA otherwise)
    data_coded <- dplyr::left_join(x, valid_results, by = ".original_row_id")
  } else {
    # Keep only original rows for which results were successfully parsed
    data_coded <- dplyr::inner_join(x, valid_results, by = ".original_row_id")
  }

  # Optional cleanup
  if (drop_json) {
    # We didn't explicitly add response_json column back during the join,
    # but if it existed in original x, remove it if desired.
    # Or add it first if needed: data_coded$response_json <- unlist(api_results)[data_coded$.original_row_id] ? Complex.
    # Let's assume we don't have it unless added explicitly.
  } else {
    # If user wants to keep JSON, add it back based on original row ID
    # Create a lookup tibble first
    json_lookup <- dplyr::tibble(.original_row_id = x$.original_row_id, response_json = unlist(api_results))
    data_coded <- dplyr::left_join(data_coded, json_lookup, by = ".original_row_id")
  }


  if (drop_instructions && "instructions" %in% names(data_coded)) {
    data_coded$instructions <- NULL
  }

  # Remove the temporary ID column
  data_coded$.original_row_id <- NULL

  # Re-evaluate if distinct is needed. Probably not.
  data_coded <- dplyr::distinct(data_coded)

  message(glue::glue("Done. Joined results for {nrow(data_coded)} parsed rows."))
  return(data_coded)
}
