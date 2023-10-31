library(tidyverse)



voicelist <- function(){
  result <- system2("say", "-v?", stdout = TRUE)

  names <- str_extract(result, pattern = "(.*).._..", group = 1) %>% str_squish()
  languages <- str_extract(result, pattern = ".._..")

  dta <- data.frame(name=names, lang=languages)
  return(dta)
}


voicecheck <- function(language = "en_US"){
  voices <- voicelist() %>% filter(lang == language)
  for(i in voices$name){
    cat(paste("Voice:",i),"\n")
    say(voice=i)
  }

}



#' Uses the say command of OSX to generate text to speech.
#'
#' @param text The text that should be transformed into audio.
#' @param voice The voice to be used, leave as NULL to use system default.
#' @param rate The words per minute, leave as NULL to use system default.
#' @param filename An .aiff filename to write the audio to. Leave as NULL to play the audio.
#' @param overwrite Overwrite an output file if it already exists.
#'
#' @return nothing
#' @export
#'
#' @examples
say <- function(text = "This is Apple siri.", voice = NULL, rate = NULL, filename = NULL, overwrite = FALSE, fileinput = NULL){

  # check if voice is installed.
  if(!is.null(voice)){
    if (!(voice %in% voicelist()$name) ){
      warning("This voice is not installed on this machine. Use voicelist() to find installed voices.")
    }
  }

  voice_param <- ifelse(is.null(voice), "", paste0("--voice=\"",voice,"\""))

  rate_param <- ifelse(is.null(rate), "", paste0("--rate=",rate))

  file_param <- ""
  if(is.null(filename) || !file.exists(filename) || overwrite){
    file_param <- ifelse(is.null(filename), "", paste0("-o ", filename))
  } else {
    stop("File already exists use overwrite = TRUE to overwrite file.")
  }


  if(is.null(fileinput)) {
    # combine parameters
    params <- paste(voice_param, rate_param, file_param, "\"", text, "\"")
  } else {
    if(file.exists(fileinput)){
      input_param <- paste0("-f",fileinput)
      params <- paste(voice_param, rate_param, file_param, input_param)
    }
  }

  #execute say command

  system2("say", params)
}





if(FALSE){
  voicecheck(lang = "en_US")
  say("This is the default voice of macos.", rate = 180)
  say("This is the default voice of macos.")
  say(voice = "Samantha", rate = 180, filename = "test_demo.aiff")

  say(fileinput = "script.txt")
}


