# k l a u s
# 
# *** content anlysis with llms ***
#

# 1) Interaction with the ChatAI API

chatai <- function(x = "",
                   system_prompt = "You are a helpful assistant",
                   model = "meta-llama-3.1-8b-instruct",
                   temperature = 0) {
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:8", message="chatai function entry", data=list(model=model, temperature=temperature, input_length=nchar(x)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="A"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  stopifnot(
    is.character(x), length(x) == 1,
    is.character(system_prompt), length(system_prompt) == 1,
    is.character(model), length(model) == 1, nzchar(model),
    is.numeric(temperature), length(temperature) == 1, temperature >= 0
  )
  api_key <- Sys.getenv("CHATAI_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'CHATAI_API_KEY' environment variable.", call. = FALSE)
  }
  url <- "https://chat-ai.academiccloud.de/v1/chat/completions"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key),
    "Content-Type" = "application/json"
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = x)
    ),
    temperature = temperature
  )
  response <- tryCatch(
    httr::POST(url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json"),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:48", message="chatai status code check", data=list(status_code=status_code), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="E"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  # Improved: Simplified redundant status code checks
  if (status_code >= 400) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  if (status_code >= 300 && status_code < 400) {
    warning(sprintf("API request returned redirect status %d.", status_code))
  } else if (status_code != 200) {
    warning(sprintf("API request returned unexpected status %d.", status_code))
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:68", message="chatai before response parsing", data=list(has_choices=!is.null(response_parsed$choices), choices_length=ifelse(is.null(response_parsed$choices), 0, length(response_parsed$choices))), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="C"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  content_out <- tryCatch(
    response_parsed$choices$message$content[[1]], # Assuming single choice often
    error = function(e) NULL # Return NULL if path doesn't exist
  )
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:75", message="chatai after response parsing", data=list(is_null=is.null(content_out), is_character=is.character(content_out), content_length=ifelse(is.character(content_out), nchar(content_out), 0)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="C"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  if (is.null(content_out) || !is.character(content_out)) {
    stop("Unexpected API response structure. Could not extract content.",
         "\nParsed response: ", utils::str(response_parsed), call. = FALSE)
  }
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:82", message="chatai function exit", data=list(success=TRUE), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="A"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  return(content_out)
}

# 2) List the available ChatAI API models

chatai_models <- function() {
  api_key <- Sys.getenv("CHATAI_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'CHATAI_API_KEY' environment variable.", call. = FALSE)
  }
  url <- "https://chat-ai.academiccloud.de/v1/models"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key)
  )
  response <- tryCatch(
    httr::GET(url,
              httr::add_headers(.headers = headers)
    ),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  if (status_code != 200) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  if (!is.list(response_parsed) || !("data" %in% names(response_parsed))) {
    stop("Unexpected API response structure. Missing 'data' element.",
         "\nParsed response structure: ", utils::str(response_parsed), call. = FALSE)
  }
  return(response_parsed$data)
}

# 3) Interaction with the Blablador API

blablador <- function(x = "",
                      system_prompt = "You are a helpful assistant",
                      model = "1 - Llama3 405 the best general model and big context size",
                      temperature = 0) {
  stopifnot(
    is.character(x), length(x) == 1,
    is.character(system_prompt), length(system_prompt) == 1,
    is.character(model), length(model) == 1, nzchar(model),
    is.numeric(temperature), length(temperature) == 1, temperature >= 0
  )
  api_key <- Sys.getenv("BLABLADOR_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'BLABLADOR_API_KEY' environment variable.", call. = FALSE)
  }
  url <- "https://api.helmholtz-blablador.fz-juelich.de/v1/chat/completions"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key),
    "Content-Type" = "application/json"
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = x)
    ),
    temperature = temperature
  )
  response <- tryCatch(
    httr::POST(url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json"),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  # Improved: Simplified redundant status code checks
  if (status_code >= 400) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  if (status_code >= 300 && status_code < 400) {
    warning(sprintf("API request returned redirect status %d.", status_code))
  } else if (status_code != 200) {
    warning(sprintf("API request returned unexpected status %d.", status_code))
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  content_out <- tryCatch(
    response_parsed$choices$message$content[[1]], # Assuming single choice often
    error = function(e) NULL # Return NULL if path doesn't exist
  )
  if (is.null(content_out) || !is.character(content_out)) {
    stop("Unexpected API response structure. Could not extract content.",
         "\nParsed response: ", utils::str(response_parsed), call. = FALSE)
  }
  return(content_out)
}

# 4) List the available Blablador API models

blablador_models <- function() {
  api_key <- Sys.getenv("BLABLADOR_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'BLABLADOR_API_KEY' environment variable.", call. = FALSE)
  }
  url <- "https://api.helmholtz-blablador.fz-juelich.de/v1/models"
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key)
  )
  response <- tryCatch(
    httr::GET(url,
              httr::add_headers(.headers = headers)
    ),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  if (status_code != 200) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  if (!is.list(response_parsed) || !("data" %in% names(response_parsed))) {
    stop("Unexpected API response structure. Missing 'data' element.",
         "\nParsed response structure: ", utils::str(response_parsed), call. = FALSE)
  }
  return(response_parsed$data)
}

# 5) Interaction with the Open WebUI API

openwebui <- function(x = "",
                      base_url ="",
                      system_prompt = "You are a helpful assistant",
                      model = "llama4:latest",
                      temperature = 0) {
  stopifnot(
    is.character(x), length(x) == 1,
    is.character(base_url), length(base_url) == 1,
    is.character(system_prompt), length(system_prompt) == 1,
    is.character(model), length(model) == 1, nzchar(model),
    is.numeric(temperature), length(temperature) == 1, temperature >= 0, temperature <= 1
  )
  api_key <- Sys.getenv("OPENWEBUI_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'OPENWEBUI_API_KEY' environment variable.", call. = FALSE)
  }
  url <- paste0(base_url,"/api/chat/completions")
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key),
    "Content-Type" = "application/json"
  )
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = x)
    ),
    temperature = temperature
  )
  response <- tryCatch(
    httr::POST(url,
               httr::add_headers(.headers = headers),
               body = jsonlite::toJSON(body, auto_unbox = TRUE),
               encode = "json"),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  # Improved: Simplified redundant status code checks
  if (status_code >= 400) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  if (status_code >= 300 && status_code < 400) {
    warning(sprintf("API request returned redirect status %d.", status_code))
  } else if (status_code != 200) {
    warning(sprintf("API request returned unexpected status %d.", status_code))
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  content_out <- tryCatch(
    response_parsed$choices$message$content[[1]], # Assuming single choice often
    error = function(e) NULL # Return NULL if path doesn't exist
  )
  if (is.null(content_out) || !is.character(content_out)) {
    stop("Unexpected API response structure. Could not extract content.",
         "\nParsed response: ", utils::str(response_parsed), call. = FALSE)
  }
  return(content_out)
}

# 6) List the available Open WebUI API models

openwebui_models <- function(base_url = "") {
  stopifnot(
    is.character(base_url), length(base_url) == 1
  )
  api_key <- Sys.getenv("OPENWEBUI_API_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the 'OPENWEBUI_API_KEY' environment variable.", call. = FALSE)
  }
  url <- paste0(base_url, "/api/models")
  headers <- c(
    "Accept" = "application/json",
    "Authorization" = paste0("Bearer ", api_key)
  )
  response <- tryCatch(
    httr::GET(url,
              httr::add_headers(.headers = headers)
    ),
    error = function(e) {
      stop("HTTP request failed: ", e$message, call. = FALSE)
    }
  )
  status_code <- httr::status_code(response)
  if (status_code != 200) {
    error_content <- httr::content(response, as = "text", encoding = "UTF-8")
    stop(sprintf("API request failed with status %d. Response: %s",
                 status_code, error_content), call. = FALSE)
  }
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  response_parsed <- tryCatch(
    jsonlite::fromJSON(response_text),
    error = function(e) {
      stop("Failed to parse JSON response: ", e$message,
           "\nRaw response: ", response_text, call. = FALSE)
    }
  )
  if (!is.list(response_parsed) || !("data" %in% names(response_parsed))) {
    stop("Unexpected API response structure. Missing 'data' element.",
         "\nParsed response structure: ", utils::str(response_parsed), call. = FALSE)
  }
  return(response_parsed$data)
}

# 7) Helper function to convert a data frame to JSON to explain the coding schema to the LLM

parse_codebook <- function(x) {
  required_cols <- c("category", "label", "instructions")
  if (!all(required_cols %in% names(x))) {
    missing_cols <- setdiff(required_cols, names(x))
    stop(paste("CSV must contain 'category', 'label', and 'instructions' columns. Missing:", paste(missing_cols, collapse=", ")), call. = FALSE)
  }
  df_cleaned <- x %>%
    dplyr::filter(!is.na(.data$category))
  if (nrow(df_cleaned) == 0) {
    stop("No valid 'category' entries found after removing rows with NA category.", call. = FALSE)
  }
  unique_categories <- unique(df_cleaned$category)
  output_list <- list()
  for (cat_name in unique_categories) {
    summary_data <- df_cleaned %>%
      dplyr::filter(.data$category == cat_name) %>%
      dplyr::summarise(
        label = list(.data$label),
        .groups = 'drop'
      )
    output_list[[cat_name]] <- list(
      label = summary_data$label[[1]]
    )
  }
  json_output <- jsonlite::toJSON(output_list, auto_unbox = TRUE, pretty = TRUE)
  return(invisible(json_output))
}

# 8) Internal helper function to call different LLM APIs

.call_llm <- function(provider, model, user_prompt, system_prompt, temperature, base_url = NULL) {
  providers_tidyllm <- c("claude", "gemini", "openai", "ollama")
  if (provider == "chatai") {
    if (is.null(model)) model <- "meta-llama-3.1-8b-instruct"
    # Improved: Print message before API call, not after
    message(glue::glue("Coding data with {provider} / {model}..."))
    # #region agent log
    cat(jsonlite::toJSON(list(location="klaus.R:395", message="before chatai API call", data=list(provider=provider, model=model), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="D"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
    # #endregion
    response <- tryCatch({
      chatai(
        x = user_prompt,
        system_prompt = system_prompt,
        model = model, 
        temperature = temperature
      )
    },
    error = function(e) {
      stop(glue::glue("Chatai API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
    }
    )
    # #region agent log
    cat(jsonlite::toJSON(list(location="klaus.R:411", message="after chatai API call", data=list(provider=provider, model=model, response_length=nchar(response)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="D"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
    # #endregion
    return(response)
  } 
  if (provider == "blablador") {
    if (is.null(model)) model <- "1 - Llama3 405 the best general model and big context size"
    # Improved: Print message before API call
    message(glue::glue("Coding data with {provider} / {model}..."))
    response <- tryCatch({
      blablador(
        x = user_prompt,
        system_prompt = system_prompt,
        model = model, 
        temperature = temperature
      )
    },
    error = function(e) {
      stop(glue::glue("Blablador API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
    }
    )
    return(response)
  }
  if (provider == "openwebui") {
    if (is.null(base_url)) {
      stop("Base URL for Open WebUI API must be provided.", call. = FALSE)
    }
    if (is.null(model)) model <- "llama4:latest"
    # Improved: Print message before API call
    message(glue::glue("Coding data with {provider} / {model}..."))
    response <- tryCatch({
      openwebui(
        x = user_prompt,
        system_prompt = system_prompt,
        model = model, 
        temperature = temperature,
        base_url = base_url
      )
    },
    error = function(e) {
      stop(glue::glue("Open WebUI API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
    }
    )
    return(response)
  }
  else if (provider %in% providers_tidyllm) {
    # Improved: Print message before API call for all tidyllm providers
    if (provider == "claude") {
      if (is.null(model)) model <- "claude-3-7-sonnet-20250219"
      message(glue::glue("Coding data with {provider} / {model}..."))
      response <- tryCatch({
        tidyllm::llm_message(user_prompt, .system_prompt = system_prompt) |> tidyllm::chat(tidyllm::claude(), .temperature = temperature, .model = model) |> tidyllm::get_reply()
      },
      error = function(e) {
        stop(glue::glue("Anthropic API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
      }
      )
    }
    if (provider == "gemini") {
      if (is.null(model)) model <- "gemini-2.0-flash"
      message(glue::glue("Coding data with {provider} / {model}..."))
      response <- tryCatch({
        tidyllm::llm_message(user_prompt, .system_prompt = system_prompt) |> tidyllm::chat(tidyllm::gemini(), .temperature = temperature, .model = model) |> tidyllm::get_reply()
      },
      error = function(e) {
        stop(glue::glue("Gemini API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
      }
      )
    }
    if (provider == "openai") {
      if (is.null(model)) model <- "gpt-4o"
      message(glue::glue("Coding data with {provider} / {model}..."))
      response <- tryCatch({
        tidyllm::llm_message(user_prompt, .system_prompt = system_prompt) |> tidyllm::chat(tidyllm::openai(), .temperature = temperature, .model = model) |> tidyllm::get_reply()
      },
      error = function(e) {
        stop(glue::glue("OpenAI API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
      }
      )
    }
    if (provider == "ollama") {
      if (is.null(model)) model <- "gemma3"
      message(glue::glue("Coding data with {provider} / {model}..."))
      response <- tryCatch({
        tidyllm::llm_message(user_prompt, .system_prompt = system_prompt) |> tidyllm::chat(tidyllm::ollama(), .temperature = temperature, .model = model) |> tidyllm::get_reply()
      },
      error = function(e) {
        stop(glue::glue("Ollama API call failed for model '{model}': {conditionMessage(e)}"), call. = FALSE)
      }
      )
    }
    if (is.null(response) || !is.character(response) || length(response) != 1) {
      stop(glue::glue("Unexpected content structure in tidyllm response for model '{model}'. Expected a single character string."), call. = FALSE)
    }
    return(response)
  } else {
    stop(glue::glue("Unsupported API provider specified: '{provider}'"), call. = FALSE)
  }
}

# 9) Main function for coding content

code_content <- function(x,
                         general_instructions,
                         formatting_instructions, # Make sure this is complete!
                         codebook,
                         provider = "openai",
                         model = NULL, # Will set set to a default according to the provider
                         base_url = NULL, # Added base_url argument
                         temperature = 0,
                         sleep = 0,
                         drop_json = TRUE,
                         drop_instructions = TRUE,
                         keep_all_original_rows = TRUE) {
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:486", message="code_content function entry", data=list(provider=provider, n_rows=nrow(x), codebook_rows=nrow(codebook), has_base_url=!is.null(base_url)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="A"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  
  # Check if provider is supported (can be expanded)
  supported_providers <- c("chatai", "claude", "gemini", "openai", "ollama", "blablador", "openwebui")
  if (!(provider %in% supported_providers)) {
    stop(glue::glue("Unsupported API provider: '{provider}'. Supported: {paste(supported_providers, collapse=', ')}"), call. = FALSE)
  }
  
  # --- Conditional check for base_url with openwebui ---
  if (provider == "openwebui" && is.null(base_url)) {
    stop("The 'base_url' argument must be specified when 'provider' is 'openwebui'.", call. = FALSE)
  }
  # Improved: Add URL format validation
  if (provider == "openwebui" && !is.null(base_url)) {
    if (!grepl("^https?://", base_url)) {
      stop("The 'base_url' must be a valid URL starting with http:// or https://", call. = FALSE)
    }
    # #region agent log
    cat(jsonlite::toJSON(list(location="klaus.R:536", message="base_url validation check", data=list(base_url=base_url, is_valid_url=TRUE), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="F"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
    # #endregion
  }
  
  # --- Add original row identifier ---
  x <- dplyr::mutate(x, .original_row_id = dplyr::row_number())
  
  codebook_json <- parse_codebook(codebook)
  system_prompt <- paste0(formatting_instructions, "\n\n", codebook_json)
  
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:557", message="before string building", data=list(codebook_rows=nrow(codebook)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="B"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  # Improved: Use vectorized approach while preserving original row-by-row format
  build_start_time <- Sys.time()
  # Pre-allocate vector for better performance
  n_codebook_rows <- nrow(codebook)
  
  # Vectorized approach: build all parts at once
  category_changed <- c(TRUE, codebook$category[-1] != codebook$category[-n_codebook_rows])
  si_parts <- ifelse(
    category_changed,
    paste0("# ", codebook$category, "\n", "*label*: ", codebook$label, "\n", "*instructions*: ", codebook$instructions, "\n"),
    paste0("*label*: ", codebook$label, "\n", "*instructions*: ", codebook$instructions, "\n")
  )
  build_end_time <- Sys.time()
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:571", message="after string building", data=list(build_time_ms=as.numeric((build_end_time - build_start_time) * 1000), si_parts_length=length(si_parts)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="B"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  specific_instructions <- paste0(si_parts, collapse = "\n")
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
        temperature = temperature,
        base_url = base_url # Pass base_url to .call_llm
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
  # #region agent log
  cat(jsonlite::toJSON(list(location="klaus.R:639", message="code_content function exit", data=list(final_rows=nrow(data_coded), valid_results=nrow(valid_results)), timestamp=as.numeric(Sys.time())*1000, sessionId="debug-session", runId="run1", hypothesisId="A"), auto_unbox=TRUE), "\n", file="/Users/cp/Documents/GitHub/klaus/.cursor/debug.log", append=TRUE)
  # #endregion
  return(data_coded)
}