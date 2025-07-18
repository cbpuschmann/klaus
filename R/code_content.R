#' Code Text Content Using an API and a Codebook
#' @param x data.frame. Input data containing at least a 'text' column with the
#' texts to be coded.
#' @param general_instructions character. General instructions for the coding task
#' provided to the language model.
#' @param formatting_instructions character. Specific instructions for the model
#' on how to format its JSON output.
#' @param codebook data.frame. A data frame representing the codebook.
#' @param provider character. The API used. Must be "claude", "gemini", "openai", 
#' "ollama", "chatai", "blablador" or "openwebui.
#' @param model character. The identifier of the model to use.
#' @param temperature numeric. The sampling temperature (0 for deterministic output).
#' @param sleep Seconds to pause between API calls to avoid rate limits.
#' @param drop_json logical. If TRUE, removes the raw JSON response column from
#' the final output.
#' @param drop_instructions logical. If TRUE, removes the 'instructions' column
#' (if present in the parsed results) from the final output.
#' @param keep_all_original_rows logical. If TRUE, removes the 'instructions' column
#' (if present in the parsed results) from the final output.
#' @param keep_all_original_rows logical. If TRUE (default), the output includes
#' all rows from the original input data frame \code{x}, with NA values in coded
#' columns for rows that failed processing. If FALSE, only rows that were
#' successfully coded and parsed are returned.
#' @return A data.frame combining the original data (or a subset based on
#' \code{keep_all_original_rows}) with the parsed coding results from the API.
#' Each coded category typically results in a separate row if multiple
#' categories are defined in the codebook.
#' @description Iterates through rows of a data frame containing text, sends each text along
#' with instructions and a codebook to the API for classification/coding,
#' parses the JSON responses, and joins the results back to the original data.
#' @details This function relies on external API calls and, hence, requires the 
#' \code{OPENAI_API_KEY}, \code{GOOGLE_API_KEY}, \code{ANTHROPIC_API_KEY},
#' \code{CHATAI_API_KEY}, \code{BLABLADOR_API_KEY}, or \code{OPENWEBUI_API_KEY}
#' environment variables to be set. For ollama, no API key is required, 
#' but ollama must be installed. The structure of the expected JSON response from
#' the API should align with the \code{formatting_instructions} provided and the 
#' parsing logic within this function.
#' @examples
#' \dontrun{
#' # Requires API key to be set in environment variable
#' data_to_code <- data.frame(text = c("It is terrible what is happening to this country", 
#' "The movie was awesome.", 
#' "The stock market has falled dramatically because of the governments' ruinous fiscal policies", 
#' "The situation has been calm following the recent local elections"))
#' general_instructions <- "You are a highly accurate and consistent text classification model that specializes in analyzing English-language Twitter posts. Your task is to determine the sentiment of the tweet reproduced below. You must strictly follow the classification rules without deviation. Do not return any additional information outside the classification scheme. Use JSON."
#' formatting_instructions <- "Always return a single JSON object for each coded text with the category name as the key. The value should be an object containing a 'label' key and a single value among multiple options. Each JSON object should have the following structure:"
#' codebook <- data.frame(category = c("sentiment", "sentiment", "sentiment"), 
#'                        label = c("positive", "negative", "neutral"), 
#'                        instructions = c("Code this if the sentiment of the tweet is positive", "Code this if the sentiment of the tweet is negative", "Code this if the sentiment of the tweet is neutral"))
#' 
#' coded_results <- code_content(
#'   x = data_to_code,
#'   general_instructions = general_instructions,
#'   formatting_instructions = formatting_instructions,
#'   codebook = codebook
#' )
#' 
#' print(head(coded_results))
#' }

code_content <- function(x,
                         general_instructions,
                         formatting_instructions, # Make sure this is complete!
                         codebook, 
                         provider = "openai",
                         model = NULL, # Will set set to a default according to the provider
                         temperature = 0,
                         sleep = 0,
                         drop_json = TRUE,
                         drop_instructions = TRUE,
                         keep_all_original_rows = TRUE) {
  
  # Check if provider is supported (can be expanded)
  supported_providers <- c("chatai", "claude", "gemini", "openai", "ollama", "blablador", "openwebui")
  if (!(provider %in% supported_providers)) {
    stop(glue::glue("Unsupported API provider: '{provider}'. Supported: {paste(supported_providers, collapse=', ')}"), call. = FALSE)
  }
  
  # --- Add original row identifier ---
  x <- dplyr::mutate(x, .original_row_id = dplyr::row_number())
  
  codebook_json <- parse_codebook(codebook)
  system_prompt <- paste0(formatting_instructions, "\n\n", codebook_json)
  
  last_cat <- ""
  si <- character(0L)
  for (i in 1:nrow(codebook)) {
    this_cat <- codebook$category[i]
    if (this_cat != last_cat) si <- c(si, paste0("# ", codebook$category[i]), "\n")
    si <- c(si, paste0("*label*: ", codebook$label[i]))
    si <- c(si, paste0("*instructions*: ", codebook$instructions[i]), "\n")
    last_cat <- codebook$category[i]
  }
  specific_instructions <- paste0(si, collapse = "\n")
  n_rows <- nrow(x)
  api_results <- vector("list", n_rows) # Pre-allocate list
  
  message(glue::glue("Coding data with {provider} ({n_rows} rows)"))
  message("Iterating over content...")
  
  # --- Loop 1: API Calls ---
  for (i in 1:n_rows) {
    user_prompt <- paste0(
      general_instructions,
      "\n\n",
      specific_instructions,
      "\n\n----\n\n", 
      x$text[i]
    )
    # Use tryCatch around the internal helper call
    result <- tryCatch({
      .call_llm( # Call the internal helper
        provider = provider,
        model = model,
        user_prompt = user_prompt,
        system_prompt = system_prompt,
        temperature = temperature
      )
    },
    error = function(e) {
      # Catch errors from .call_llm (which includes specific API errors)
      warning(glue::glue("API call failed for row {i} (ID: {x$.original_row_id[i]}): {conditionMessage(e)}"), call. = FALSE)
      return(NA_character_) # Assign NA on error
    }
    )
    api_results[[i]] <- result
    message(glue::glue("Processed text {i} of {n_rows}"))
    if (sleep > 0) Sys.sleep(sleep)
  }
  
  # --- Process Results (Parsing Loop) ---
  message(glue::glue("Parsing JSON responses ({n_rows} rows)."))
  parsed_list <- purrr::map(seq_along(api_results), function(j) {
    current_id <- x$.original_row_id[[j]]
    raw_response <- api_results[[j]] # This is now the text content from the LLM
    
    if (is.na(raw_response)) {
      return(NULL)
    }
    
    # --- JSON Cleaning Step ---
    json_content <- stringr::str_extract(raw_response, "\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]")
    if (is.na(json_content)) {
      warning(glue::glue("Could not extract JSON structure {{...}} or [...] from response for row {j} (ID: {current_id}). Skipping parsing. Raw content: {raw_response}"), call.=FALSE)
      return(NULL)
    }
    # --- End Cleaning Step ---
    
    parsed_json <- tryCatch({
      jsonlite::fromJSON(json_content) # Parse extracted content
    }, error = function(e) {
      warning(glue::glue("JSON parsing failed for row {j} (ID: {current_id}): {conditionMessage(e)} \nAttempted to parse: '{json_content}'"), call. = FALSE)
      return(NULL)
    })
    
    if (is.null(parsed_json)) {
      return(NULL)
    }
    
    # --- Flattening ---
    flattened_data <- tryCatch({
      purrr::map_dfr(parsed_json, ~ .x, .id = "category") %>%
        dplyr::mutate(.original_row_id = current_id)
    }, error = function(e) {
      warning(glue::glue("Failed to flatten parsed JSON structure for row {j} (ID: {current_id}): {conditionMessage(e)}"), call. = FALSE)
      return(NULL)
    })
    
    return(flattened_data)
  }) # End purrr::map
  
  # --- Combine and Join Results ---
  valid_results <- dplyr::bind_rows(parsed_list)
  
  if (nrow(valid_results) == 0) {
    message("No rows were successfully processed and parsed.")
    x$response_raw <- unlist(api_results) # Add raw results if nothing parsed
    x$.original_row_id <- NULL
    return(x)
  }
  
  if (keep_all_original_rows) {
    data_coded <- dplyr::left_join(x, valid_results, by = ".original_row_id")
  } else {
    data_coded <- dplyr::inner_join(x, valid_results, by = ".original_row_id")
  }
  
  # --- Optional Cleanup ---
  if (drop_json) {
    # We don't store the raw JSON by default anymore unless requested
  } else {
    # Add raw response text back if requested
    response_lookup <- dplyr::tibble(.original_row_id = x$.original_row_id, response_raw = unlist(api_results))
    data_coded <- dplyr::left_join(data_coded, response_lookup, by = ".original_row_id")
  }
  if (drop_instructions && "instructions" %in% names(data_coded)) {
    data_coded$instructions <- NULL
  }
  
  # Remove temporary ID
  data_coded$.original_row_id <- NULL
  
  # Distinct call might still be needed if map_dfr creates duplicates somehow? Re-evaluate.
  data_coded <- dplyr::distinct(data_coded)
  
  message(glue::glue("Done. Joined results for {nrow(valid_results)} parsed rows.")) # Note: nrow(valid_results) might differ from final nrow
  return(data_coded)
}