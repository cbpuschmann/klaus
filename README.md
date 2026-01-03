# A light-weight R package for LLM-based content analysis

![klaus](logo.png?raw=true "klaus")

Modern large language models (LLMs) offer considerable advantages for standardized content analysis. `klaus` facilitates use of both proprietary and open source LLMs by offering a simple interface through which to serve data and apply categorization. Presently the package supports the proprietary APIs of OpenAI, Anthropic and Google, as well as for local use via [ollama](https://ollama.com/) (through [tidyllm](https://cran.r-project.org/package=tidyllm)). In addition, for academic research, it is also possible to use the non-commercial [ChatAI API](https://docs.hpc.gwdg.de/services/saia/index.html) service provided by [GWDG](https://gwdg.de/en/), [Blablador](https://helmholtz.cloud/services/?serviceID=d7d5c597-a2f6-4bd1-b71e-4d6499d98570&sortByAttribute=userCount) provided by the [Forschungszentrum Jülich](https://www.fz-juelich.de/en), or [Open WebUI](https://openwebui.com/). 

## Installation

`klaus` can be installed from source via the [remotes](https://cran.r-project.org/package=remotes) package.

    remotes::install_github("cbpuschmann/klaus")
    
    library(klaus)
  
The package requires API keys to be stored as environment variables. These can be set via the [usethis](https://cran.r-project.org/package=usethis) package.

    usethis::edit_r_environ()
    
The keys should be called `OPENAI_API_KEY`, `GOOGLE_API_KEY`, `ANTHROPIC_API_KEY`, `CHATAI_API_KEY`, `BLABLADOR_API_KEY`, and `OPENWEBUI_API_KEY`. Simply add an 
entry in the form `OPENAI_API_KEY="xxxxxxxxxxxx"` to your `.Renviron` file. 

## Usage

The main function of the package is `code_content()` which takes general instructions, formatting instructions, and a codebook as its arguments. The function supports multiple LLM providers and handles batch processing of text data.

### Basic Example

First, prepare the general and formatting instructions.
      
    general_instructions <- "You are a highly accurate and consistent text classification model that specializes in analyzing 
    English-language Twitter posts. Your task is to determine the sentiment of the tweet reproduced below. You must strictly 
    follow the classification rules without deviation. Do not return any additional information outside the classification 
    scheme. Use JSON."
  
    formatting_instructions <- "Always return a single JSON object for each coded text with the category name as the key. 
    The value should be an object containing a 'label' key and a single value among multiple options. Each JSON object should 
    have the following structure:"
  
Then load your data (or use the example below). The data must be a data frame or tibble with a column named 'text'.

    data_to_code <- data.frame(text = c("It is terrible what is happening to this country", 
                                      "The movie was awesome.", 
                                      "The stock market has falled dramatically because of the governments' bad fiscal policies", 
                                      "The situation has been calm following the recent local elections"))
  
Prepare the codebook. The codebook should be a data frame or tibble with the columns 'category', 'label' and 'instructions'. 

    codebook <- data.frame(
      category = c("sentiment", "sentiment", "sentiment"), 
      label = c("positive", "negative", "neutral"), 
      instructions = c("Code this if the sentiment of the tweet is positive", 
                       "Code this if the sentiment of the tweet is negative", 
                       "Code this if the sentiment of the tweet is neutral"))
  
Code the data (in this example with gpt-4o, the default OpenAI model)

    result <- code_content(data_to_code, general_instructions, formatting_instructions, codebook)
    
    Coding data with openai (4 rows)
    Iterating over content...
    Coding data with openai / gpt-4o...
    Processed text 1 of 4
    Coding data with openai / gpt-4o...
    Processed text 2 of 4
    Coding data with openai / gpt-4o...
    Processed text 3 of 4
    Coding data with openai / gpt-4o...
    Processed text 4 of 4
    Parsing JSON responses (4 rows).
    Done. Joined results for 4 parsed rows.

Output the result

    result$label
    [1] "negative" "positive" "negative" "neutral"

### Additional Parameters

The `code_content()` function supports several optional parameters:

- **`provider`**: Choose the LLM provider (default: "openai"). Options: "chatai", "claude", "gemini", "openai", "ollama", "blablador", "openwebui"
- **`model`**: Specify a model identifier. If NULL, provider-specific defaults are used (see below)
- **`base_url`**: Required for Open WebUI provider. The base URL of your Open WebUI instance (e.g., "http://localhost:3000")
- **`temperature`**: Sampling temperature (0 for deterministic output). Default: 0
- **`sleep`**: Seconds to pause between API calls to avoid rate limits. Default: 0
- **`drop_json`**: If TRUE (default), removes raw JSON response column from output
- **`drop_instructions`**: If TRUE (default), removes instructions column from output
- **`keep_all_original_rows`**: If TRUE (default), includes all original rows even if coding failed (with NA values)

### Default Models by Provider

- **openai**: "gpt-4o"
- **claude**: "claude-3-7-sonnet-20250219"
- **gemini**: "gemini-2.0-flash"
- **ollama**: "gemma3"
- **chatai**: "meta-llama-3.1-8b-instruct"
- **blablador**: "1 - Llama3 405 the best general model and big context size"
- **openwebui**: "llama4:latest"

### Error Handling

The function handles API errors gracefully:
- If an API call fails for a specific row, a warning is issued
- Failed rows are marked with NA values in coded columns (if `keep_all_original_rows = TRUE`)
- The function continues processing remaining rows even if some fail
- JSON parsing errors are caught and logged with warnings 

## Coding with the ChatAI API

ChatAI can be specified as the API to use with the *provider* parameter. In the example below, we specify use of ChatAI and LLama 3.3 70b.

    coded_data_chatai <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "chatai", 
                                      model = "llama-3.3-70b-instruct")

For convenience, the `chatai_models()` function lists all models available via the ChatAI API.

## Coding with the Blablador API

To use the Blablador API, you need to [request an API key](https://sdlaml.pages.jsc.fz-juelich.de/ai/guides/blablador_api_access/). Blablador can be specified as the API to use with the *provider* parameter. In the example below, we specify use of Blablador as provider and Llama3 405 as the model.

    coded_data_blablador <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "blablador", 
                                      model = "1 - Llama3 405 the best general model and big context size")

For convenience, the `blablador_models()` function lists all models available via the Blablador API.

## Coding with the Open WebUI API

Open WebUI can be specified as the API to use with the *provider* parameter. In the example below, we specify use of Open WebUI as provider and Llama4 as the model. **The `base_url` parameter is required** when using Open WebUI. If you are running Open WebUI locally, this is typically `http://localhost:3000`. If your institution is hosting an instance, you will need to specify the URL of that instance. The URL must start with `http://` or `https://`.

    coded_data_openwebui <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "openwebui", 
                                      model = "llama4:latest",
                                      base_url = "http://localhost:3000")

For convenience, the `openwebui_models()` function lists all models available via the Open WebUI API of your installation/instance. **You must also specify the `base_url` argument** when using this function:

    models <- openwebui_models(base_url = "http://localhost:3000")

## Coding with ollama

Coding with ollama requires having [ollama](https://ollama.com/) installed and running locally, and the model you would like to use must be installed. No API key is required for ollama. 

    coded_data_ollama <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "ollama",
                                      model = "llama3.1")

To see available models, use `ollama list` in your terminal, or check the tidyllm package documentation.



## Prompting

Note that while adjusting the instructions to your specific use case is essential, the remaining generic instructions should largely remain unchanged to ensure that the API responds with parseable results.

### Tips for Best Results

1. **Be explicit about JSON format**: The `formatting_instructions` should clearly specify the expected JSON structure
2. **Include examples**: When possible, include examples of the desired output format in your instructions
3. **Use consistent terminology**: Use the same terms in your codebook and instructions
4. **Test with a small sample**: Before processing large datasets, test with a few rows to verify the output format
5. **Handle rate limits**: Use the `sleep` parameter to add delays between API calls if you encounter rate limiting
6. **Monitor progress**: The function prints progress messages showing which row is being processed

## Troubleshooting

### Common Issues

- **API key errors**: Ensure your API key environment variable is set correctly. Restart R after setting environment variables.
- **JSON parsing errors**: Check that your `formatting_instructions` clearly specify the JSON structure. The function extracts JSON using pattern matching, so responses may include additional text.
- **Empty results**: If `keep_all_original_rows = FALSE` and all rows fail, the function returns an empty data frame. Check warnings for error messages.
- **Open WebUI connection errors**: Verify that `base_url` is correct and that your Open WebUI instance is running and accessible.
- **Rate limiting**: If you encounter rate limits, increase the `sleep` parameter to add delays between API calls.
