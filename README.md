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

The main function of the package is `code_content()` which takes general instructions, formatting instructions, and a codebook as its arguments.

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
  
Code the data (in this example with gpt-4o)

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

    coded_data_chatai <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "blablador", 
                                      model = "1 - Llama3 405 the best general model and big context size")

For convenience, the `blablador_models()` function lists all models available via the Blablador API.

## Coding with the Open WebUI API

Open WebUI can be specified as the API to use with the *provider* parameter. In the example below, we specify use of Open WebUI as provider and Llama4 as the model. In addition to the usual arguments, for using Open WebUI, you also need to specify a `base_url`. If you are running Open WebUI locally, this is likely to be `http://localhost:3000`. If your institution is hosting an instance, you will need to specify the URL of that instance.

    coded_data_openwebui <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "openwebui", 
                                      model = "llama4:latest")

For convenience, the `openwebui_models()` function lists all models available via the Open WebUI API. Please note that you also need to specify the `base_url` argument when using this function.

## Coding with ollama

Coding with ollama requires having [ollama](https://ollama.com/) and the model you would like to use installed. 

    coded_data_ollama <- code_content(data_to_code, 
                                      general_instructions, 
                                      formatting_instructions, 
                                      codebook, 
                                      provider = "ollama",
                                      model = "llama3.1")



## Prompting

Note that while adjusting the instructions to your specific use case is essential, the remaining generic instructions should largely remain unchanged to ensure that the API responds with parseable results.
