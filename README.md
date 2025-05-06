# A light-weight R package for LLM-based content analysis

![klaus](logo_black.png?raw=true "klaus")

Modern large language models offer considerable advantages for standardized content analysis. `klaus` facilitates use of both proprietary and open source LLMs by offering a simple interface through which to serve data and apply categorization. Presently the package supports the proprietary APIs of OpenAI, Anthropic and Google (through [tidyllm](https://cran.r-project.org/package=tidyllm)), as well as the non-commercial [ChatAI API](https://docs.hpc.gwdg.de/services/saia/index.html) service provided by [GWDG](https://gwdg.de/en/). 

## Installation

`klaus` can be installed from source via the [remotes](https://cran.r-project.org/package=remotes) package.

  remotes::install_github("cbpuschmann/klaus")
  
## Usage

The package's main function `code_content()` , takes general instructions, coding instructions and a codebook as its arguments.
  
  general_instructions <- "You are a highly accurate and consistent text classification model that specializes in analyzing English-language Twitter posts. Your task is to determine the sentiment of the tweet reproduced below. You must strictly follow the classification rules without deviation. Do not return any additional information outside the classification scheme. Use JSON."
  
  formatting_instructions <-  "Always return a single JSON object with the category name as the key for each coded text. The value should be an object containing a 'label' key and a single value among multiple options. Each JSON object should have the following structure:"
  
  data_to_code <- data.frame(text = c("It is terrible what is happening to this country", "The movie was awesome.", "The stock market has falled dramatically because of the governments' ruinous fiscal policies", "The situation has been calm following the recent local elections"))
  
  codebook <- data.frame( category = c("sentiment", "sentiment", "sentiment"), label = c("positive", "negative", "neutral"), instructions = c( "Code this if the sentiment of the tweet is positive", "Code this if the sentiment of the tweet is negative", "Code this if the sentiment of the tweet is neutral" ) )
  
  result <- code_content(data_to_code, general_instructions, formatting_instructions, codebook)
  
  