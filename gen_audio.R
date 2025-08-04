#-------------------------------------------------------------------------------
# R Program to Generate Text-to-Speech (TTS) using Google Gemini API
#
# This script is an improved version of the previous program.
# It makes a direct API call to the gemini-2.5-flash-preview-tts model,
# receives raw PCM audio data, and converts it into a playable .wav file.
#
# Prerequisites:
# 1. An API key for the Google Gemini API.
# 2. R packages: httr2, jsonlite, and tuneR.
#    You can install them by running:
#    install.packages(c("httr2", "jsonlite", "tuneR"))
#
# This version simplifies the raw data conversion process, making the code
# more robust and easier to maintain.
#-------------------------------------------------------------------------------

# Load the necessary libraries
library(httr2)
library(jsonlite)
library(audio)
library(tuneR)

# Attempt to load the tuneR package and handle potential errors
if (!requireNamespace("tuneR", quietly = TRUE)) {
  stop("The 'tuneR' package is required but not installed. Please install it using install.packages('tuneR').")
}
library(tuneR)
output_filename <- "out.wav"
# --- USER CONFIGURATION ---
# IMPORTANT: Replace "YOUR_API_KEY" with your actual Gemini API key.
# It's highly recommended to set this as an environment variable for security.
# For example, in your .Renviron file: GOOGLE_API_KEY="your_api_key_here"


# Check that the API key is available
api_key <- Sys.getenv("GOOGLE_API_KEY")
if (api_key == "") {
  stop("API key is not set. Please set the GOOGLE_API_KEY environment variable.")
}
story_prompt <- 
  readLines('./text/4_臧僖伯谏观鱼_评书版.md')|>
  paste(collapse = "\n")
#story_prompt <- "TTS the following conversation between Joe and Jane:
#                Joe: Hows it going today Jane?
 #               Jane: Not too bad, how about you?"

# --- API REQUEST SETUP ---

# Define the API endpoint URL
api_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-tts:generateContent"

# Construct the request body as a list in R
request_body <- list(
  contents = list(
    list(
      parts = list(
        list(
          text = story_prompt
        )
      )
    )
  ),
  generationConfig =  list(
    responseModalities = list("AUDIO"),
    speechConfig = list(
      multiSpeakerVoiceConfig = list(
        speakerVoiceConfigs = list(
          list(
            speaker = 'Speaker 1',
            voiceConfig = list(
              prebuiltVoiceConfig = list(
                voiceName = 'Algieba'
              )
            )
          ),
          list(
            speaker = 'Speaker 2',
            voiceConfig = list(
              prebuiltVoiceConfig = list(
                voiceName = 'Laomedeia'
              )
            )
          )
        )
      )
    )
  )
 )

# --- MAKING THE API CALL ---

# Use httr2 to build and execute the request
# The request includes the API key as a query parameter
# and the request body is automatically converted to JSON.
tryCatch({
  response <- request(api_url) |>
    req_url_query(key = api_key) |>
    req_body_json(data = request_body) |>
    req_perform()

  # Check for HTTP errors
  if (response$status_code != 200) {
    stop(paste("API request failed with status code:", response$status_code))
  }

  # --- PROCESSING THE RESPONSE ---

  # Parse the JSON response
  parsed_response <- fromJSON(rawToChar(response$body), simplifyVector = FALSE)

  # Extract the base64-encoded audio data
  audio_data_base64 <- parsed_response$candidates[[1]]$content$parts[[1]]$inlineData$data

  # Decode the base64 string to raw binary data
  raw_audio_data <- jsonlite::base64_dec(audio_data_base64)

  # --- CONVERTING TO WAV AND SAVING THE FILE ---

  # Define audio parameters returned by the API
  # The API documentation specifies signed 16-bit PCM at 24kHz.
  sample_rate <- 24000
  bit_depth <- 16
  channels <- 1

  # Convert the raw binary data to a vector of signed 16-bit integers
  pcm_data <- readBin(raw_audio_data, "integer", n = length(raw_audio_data) / 2, size = 2, signed = TRUE, endian = "little")

  # A new, robust approach to write the WAV file manually, bypassing
  # potential bugs in the tuneR::Wave constructor.
  write_wav_from_pcm <- function(pcm_data, sample_rate, bit_depth, filename) {
    # Check if a file connection exists and close it if necessary
    if (file.exists(filename)) {
      file.remove(filename)
    }
    con <- file(filename, "wb")

    # WAV header information
    header_size <- 44
    data_size <- length(pcm_data) * bit_depth / 8
    file_size <- header_size + data_size - 8

    # RIFF header
    writeBin(charToRaw("RIFF"), con)
    writeBin(as.integer(file_size), con, size = 4, endian = "little")
    writeBin(charToRaw("WAVE"), con)

    # Format chunk
    writeBin(charToRaw("fmt "), con)
    writeBin(as.integer(16), con, size = 4, endian = "little")
    writeBin(as.integer(1), con, size = 2, endian = "little") # PCM format
    writeBin(as.integer(1), con, size = 2, endian = "little") # Channels
    writeBin(as.integer(sample_rate), con, size = 4, endian = "little")
    writeBin(as.integer(sample_rate * 1 * bit_depth / 8), con, size = 4, endian = "little") # Byte rate
    writeBin(as.integer(1 * bit_depth / 8), con, size = 2, endian = "little") # Block align
    writeBin(as.integer(bit_depth), con, size = 2, endian = "little")

    # Data chunk
    writeBin(charToRaw("data"), con)
    writeBin(as.integer(data_size), con, size = 4, endian = "little")
    writeBin(pcm_data, con, size = 2, endian = "little")

    close(con)
  }


  write_wav_from_pcm(pcm_data, sample_rate, bit_depth, output_filename)

  cat(paste("Successfully generated and saved audio to '", output_filename, "'\n", sep = ""))

}, error = function(e) {
  # Handle any errors during the process
  cat("An error occurred:\n")
  cat(e$message, "\n")
  return(e)
})

sound <- readWave(output_filename)

play(sound) 
