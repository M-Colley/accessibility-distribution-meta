library(rstudioapi)
library(pacman)

# xslx, tabulizer
pacman::p_load(pastecs, readxl, pdfsearch, formattable, htmltools, webshot, DT, Rfast, ggplot2, beepr)

# set to current file-directory
# TODO we saved these in a directory called "Scripts"
setwd(dirname(getActiveDocumentContext()$path))


# outsource all definitions for better overview

source("definitions.R", echo = FALSE)

# Depending on the number of keywords, we need a vector with "TRUE" for pdfsearch to ignore the case
# using TRUE suffices
# vector_ignore_case_cognitive <- rep(TRUE, length(keyword_disability_cognitive))
# vector_ignore_case_general <- rep(TRUE, length(keyword_disability_general))
# vector_ignore_case_hearing <- rep(TRUE, length(keyword_disability_hearing))
# vector_ignore_case_mobility <- rep(TRUE, length(keyword_disability_mobility))
# vector_ignore_case_older <- rep(TRUE, length(keyword_disability_older))
# vector_ignore_case_psychological <- rep(TRUE, length(keyword_disability_psychological))
# vector_ignore_case_vision <- rep(TRUE, length(keyword_disability_vision))


number_of_problems <- 0
number_of_not_categorized <- 0

potentially_related_work <- 0
unrelated_work <- 0

categorized_by_title <- 0
categorized_by_keywords <- 0

df_occurrences <- data.frame(
  Conference = character(),
  Vision = integer(),
  Hearing = integer(),
  Cognitive = integer(),
  Psychological = integer(),
  Mobility = integer(),
  General = integer(),
  Elderly = integer(),
  stringsAsFactors = FALSE
)

df_occurrences_un_categorized <- data.frame(
  Conference = character(),
  Vision = integer(),
  Hearing = integer(),
  Cognitive = integer(),
  Psychological = integer(),
  Mobility = integer(),
  General = integer(),
  Elderly = integer(),
  stringsAsFactors = FALSE
)

df_accessibility_related_keywords_found <- data.frame(
  Conference = character(),
  number = integer(),
  stringsAsFactors = FALSE
)



# Set directory to get all relevant pdfs, do this for all years
# our setup was as:
# Analysis
# ...Conference-Directory
# ...e.g., CHI
# ......year
# ......e.g., 2017

# TODO set this to the desired directory
setwd("../Testordner2/")


# get all conference directories
conference_directories <- list.dirs(".", recursive = FALSE)

sink("output_pdf_search_newest.txt")
sink(stdout(), type = "message")

for (d in 1:length(conference_directories)) {

  # go into x'th conference
  setwd(conference_directories[d])

  df_conference <- data.frame(
    Year = character(),
    "Vision Disability" = integer(),
    "Hearing Disability" = integer(),
    "Cognitive Disability" = integer(),
    "Psychological Disability" = integer(),
    "Mobility Disability" = integer(),
    "General Disability" = integer(),
    "Elderly" = integer(),
    stringsAsFactors = FALSE
  )
  
  df_conference_names <- data.frame(
    Year = character(),
    "Vision Disability" = character(),
    "Hearing Disability" = character(),
    "Cognitive Disability" = character(),
    "Psychological Disability" = character(),
    "Mobility Disability" = character(),
    "General Disability" = character(),
    "Elderly" = character(),
    stringsAsFactors = FALSE
  )


  # get all years
  year_directories <- list.dirs(".", recursive = FALSE)


  for (y in 1:length(year_directories)) {
    vision_related_publications <- 0
    hearing_related_publications <- 0
    cognitive_related_publications <- 0
    psychological_related_publications <- 0
    mobility_related_publications <- 0
    general_related_publications <- 0
    elderly_related_publications <- 0
    
    vision_related_publications_names <- ""
    hearing_related_publications_names <- ""
    cognitive_related_publications_names <- ""
    psychological_related_publications_names <- ""
    mobility_related_publications_names <- ""
    general_related_publications_names <- ""
    elderly_related_publications_names <- ""

    # go into x'th year
    setwd(year_directories[y])

    year <- as.integer(substring(year_directories[y], 3))

    files <- list.files(pattern = "\\.pdf$")


    for (i in 1:length(files)) {

      # message all the names of the files
      # message(i, ":     ", substr(files[i], 1, nchar(files[i])-4))


      # pdf_file <- system.file('pdf', files[i], package = 'pdfsearch')
      # The pdftools function for extracting text is pdf_text.
      pdf_file <- files[i]

      result <- read_Metadata(pdf_file = pdf_file)
      title_available <- TRUE


      # Title was extracted
      # Attention: result if gone right is an array of relevant information including author, title
      # if gone wrong, then it is simply ""
      # Metadata can be available but title can still be NULL!
      # Use '||' to avoid error: "$ operator is invalid for atomic vectors"
      # see https://stat.ethz.ch/R-manual/R-devel/library/base/html/Logic.html
      if (length(result) == 1 || is.null(result$title)) {
        message("Title of PDF NOT AVAILABLE")
        title_available <- FALSE
      } else {
        message("Title of PDF is:", result$title)
      }


      # we search in the title and if any keyword was found, we assume that this is the disability studied
      # we once don't look at this to get information on the distribution!
      # TODO change this to "title_available"!!!!!!!!!!
      if (title_available) {
        # message("Looking in TITLE")

        pdf_title <- result$title


        result_disability_vision <- keyword_search(pdf_title,
          keyword = keyword_disability_vision,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_hearing <- keyword_search(pdf_title,
          keyword = keyword_disability_hearing,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_cognitive <- keyword_search(pdf_title,
          keyword = keyword_disability_cognitive,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_psychological <- keyword_search(pdf_title,
          keyword = keyword_disability_psychological,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_mobility <- keyword_search(pdf_title,
          keyword = keyword_disability_mobility,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )


        result_disability_older <- keyword_search(pdf_title,
          keyword = keyword_disability_older,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_general <- keyword_search(pdf_title,
          keyword = keyword_disability_general,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        # anyone with a keyword?

        if (dim(result_disability_vision)[1] > 0) {
          # a term was found!
          message("Vision disability term was found in TITLE. Number:", dim(result_disability_vision)[1])
          # message(head(result_disability_vision))
          vision_related_publications <- vision_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          vision_related_publications_names <- paste(vision_related_publications_names, pdf_title, sep = ";\n\n")
          
          
          # next to avoid double counting!
          # but only next when there really is one, otherwise we look in entire document
          next
        }

        if (dim(result_disability_hearing)[1] > 0) {
          # a term was found in TITLE!
          message("Hearing disability term was found in TITLE. Number:", dim(result_disability_hearing)[1])
          # message(head(result_disability_hearing))
          hearing_related_publications <- hearing_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          hearing_related_publications_names <- paste(hearing_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }

        if (dim(result_disability_cognitive)[1] > 0) {
          # a term was found in TITLE!
          message("Cognitive disability term was found in TITLE. Number:", dim(result_disability_cognitive)[1])
          # message(head(result_disability_cognitive))
          cognitive_related_publications <- cognitive_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          cognitive_related_publications_names <- paste(cognitive_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }

        if (dim(result_disability_psychological)[1] > 0) {
          # a term was found in TITLE!
          message("Psychological disability term was found in TITLE. Number:", dim(result_disability_psychological)[1])
          # message(head(result_disability_psychological))
          psychological_related_publications <- psychological_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          psychological_related_publications_names <- paste(psychological_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }

        if (dim(result_disability_mobility)[1] > 0) {
          # a term was found in TITLE!
          message("Mobility disability term was found in TITLE. Number:", dim(result_disability_mobility)[1])
          # message(head(result_disability_mobility))
          mobility_related_publications <- mobility_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          mobility_related_publications_names <- paste(mobility_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }


        if (dim(result_disability_older)[1] > 0) {
          # a term was found in TITLE!
          message("Elderly disability term was found in TITLE. Number:", dim(result_disability_older)[1])
          # message(head(result_disability_older))
          elderly_related_publications <- elderly_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          elderly_related_publications_names <- paste(elderly_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }

        if (dim(result_disability_general)[1] > 0) {
          # a term was found in TITLE!
          message("General disability term was found in TITLE. Number:", dim(result_disability_general)[1])
          # message(head(result_disability_general))
          general_related_publications <- general_related_publications + 1
          categorized_by_title <- categorized_by_title + 1
          general_related_publications_names <- paste(general_related_publications_names, pdf_title, sep = ";\n\n")
          next
        }
      }


      keywords_available <- TRUE


      # keywords_available was extracted
      # Attention: result if gone right is an array of relevant information including author, title
      # if gone wrong, then it is simply ""
      # see https://stat.ethz.ch/R-manual/R-devel/library/base/html/Logic.html
      if (length(result) == 1 || is.null(result$keywords)) {
        message("Keywords of PDF NOT AVAILABLE")
        keywords_available <- FALSE
      } else {
        message("Keywords are:", result$keywords)
      }


      # we search in the keywords and if any keyword was found, we assume that this is the disability studied
      # we once don't look at this to get information on the distribution!
      # TODO change this to "keywords_available"!!!!!!!!!!
      if (keywords_available) {
        # message("Looking in KEYWORDS")

        pdf_keywords <- result$keywords

        result_disability_vision <- keyword_search(pdf_keywords,
          keyword = keyword_disability_vision,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_hearing <- keyword_search(pdf_keywords,
          keyword = keyword_disability_hearing,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_cognitive <- keyword_search(pdf_keywords,
          keyword = keyword_disability_cognitive,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_psychological <- keyword_search(pdf_keywords,
          keyword = keyword_disability_psychological,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_mobility <- keyword_search(pdf_keywords,
          keyword = keyword_disability_mobility,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )


        result_disability_older <- keyword_search(pdf_keywords,
          keyword = keyword_disability_older,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        result_disability_general <- keyword_search(pdf_keywords,
          keyword = keyword_disability_general,
          path = FALSE,
          ignore_case = TRUE, split_pdf = FALSE
        )

        # anyone with a keyword?

        if (dim(result_disability_vision)[1] > 0) {
          # a term was found!
          message("Vision disability term was found in KEYWORDS Number:", dim(result_disability_vision)[1])
          # message(head(result_disability_vision))
          vision_related_publications <- vision_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1

          if(title_available){
            vision_related_publications_names <- paste(vision_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            vision_related_publications_names <- paste(vision_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          
          # next to avoid double counting!
          # but only next when there really is one, otherwise we look in entire document
          next
        }

        if (dim(result_disability_hearing)[1] > 0) {
          message("Hearing disability term was found in KEYWORDS Number:", dim(result_disability_hearing)[1])
          # message(head(result_disability_hearing))
          hearing_related_publications <- hearing_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            hearing_related_publications_names <- paste(hearing_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            hearing_related_publications_names <- paste(hearing_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }

        if (dim(result_disability_cognitive)[1] > 0) {
          message("Cognitive disability term was found in KEYWORDS Number:", dim(result_disability_cognitive)[1])
          # message(head(result_disability_cognitive))
          cognitive_related_publications <- cognitive_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            cognitive_related_publications_names <- paste(cognitive_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            cognitive_related_publications_names <- paste(cognitive_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }

        if (dim(result_disability_psychological)[1] > 0) {
          message("Psychological disability term was found in KEYWORDS Number:", dim(result_disability_psychological)[1])
          # message(head(result_disability_psychological))
          psychological_related_publications <- psychological_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            psychological_related_publications_names <- paste(psychological_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            psychological_related_publications_names <- paste(psychological_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }

        if (dim(result_disability_mobility)[1] > 0) {
          message("Mobility disability term was found in KEYWORDS Number:", dim(result_disability_mobility)[1])
          # message(head(result_disability_mobility))
          mobility_related_publications <- mobility_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            mobility_related_publications_names <- paste(mobility_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            mobility_related_publications_names <- paste(mobility_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }


        if (dim(result_disability_older)[1] > 0) {
          message("Elderly disability term was found in KEYWORDS Number:", dim(result_disability_older)[1])
          # message(head(result_disability_older))
          elderly_related_publications <- elderly_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            elderly_related_publications_names <- paste(elderly_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            elderly_related_publications_names <- paste(elderly_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }

        if (dim(result_disability_general)[1] > 0) {
          message("General disability term was found in KEYWORDS Number:", dim(result_disability_general)[1])
          # message(head(result_disability_general))
          general_related_publications <- general_related_publications + 1
          categorized_by_keywords <- categorized_by_keywords + 1
          
          if(title_available){
            general_related_publications_names <- paste(general_related_publications_names, pdf_title, sep = ";\n\n")
          }else{
            general_related_publications_names <- paste(general_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
          }
          next
        }
      }


      # check for each keyword for the disability
      if (TRUE) {
        result_disability_vision <- keyword_search(pdf_file,
          keyword = keyword_disability_vision,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_hearing <- keyword_search(pdf_file,
          keyword = keyword_disability_hearing,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_cognitive <- keyword_search(pdf_file,
          keyword = keyword_disability_cognitive,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_psychological <- keyword_search(pdf_file,
          keyword = keyword_disability_psychological,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_mobility <- keyword_search(pdf_file,
          keyword = keyword_disability_mobility,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_older <- keyword_search(pdf_file,
          keyword = keyword_disability_older,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )

        result_disability_general <- keyword_search(pdf_file,
          keyword = keyword_disability_general,
          path = TRUE, remove_hyphen = TRUE, surround_lines = 1,
          ignore_case = TRUE, split_pdf = TRUE
        )
      }


      # CHECK for which criteria is met -- TRUE to always do it
      if (TRUE) {

        # create row for all found keywords
        df_one_year <- data.frame(
          Conference = substring(conference_directories[d], 3),
          Vision = dim(result_disability_vision)[1],
          Hearing = dim(result_disability_hearing)[1],
          Cognitive = dim(result_disability_cognitive)[1],
          Psychological = dim(result_disability_psychological)[1],
          Mobility = dim(result_disability_mobility)[1],
          General = dim(result_disability_general)[1],
          Elderly = dim(result_disability_older)[1],
          stringsAsFactors = FALSE
        )

        # string these together
        df_occurrences <- rbind(df_occurrences, df_one_year)


        # now get total number of found acc. related keywords
        accessibility_related_occurrences <- dim(result_disability_vision)[1] + dim(result_disability_hearing)[1] + dim(result_disability_cognitive)[1] + dim(result_disability_general)[1] + dim(result_disability_hearing)[1] + dim(result_disability_mobility)[1] + dim(result_disability_older)[1] + dim(result_disability_psychological)[1]
        message("Number of accesibility related occurrences: ", accessibility_related_occurrences)

        # look how many accessibility_related_occurrences are found in paper
        df_accessibility_found_one_year <- data.frame(
          Conference = substring(conference_directories[d], 3),
          number = accessibility_related_occurrences,
          stringsAsFactors = FALSE
        )

        # string these together
        df_accessibility_related_keywords_found <- rbind(df_accessibility_related_keywords_found, df_accessibility_found_one_year)





        # TODO die Zahl MINIMUM_OCCURRENCES_FOR_ACCEPTANCE und MAX_... anpassen!
        if (dim(result_disability_vision)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {

          # compute all ratios
          # ratioVisionHearing <- dim(result_disability_vision)[1] / dim(result_disability_hearing)[1]
          # ratioVisionCognitive <- dim(result_disability_vision)[1] / dim(result_disability_cognitive)[1]
          # ratioVisionPsychological <- dim(result_disability_vision)[1] / dim(result_disability_psychological)[1]
          # ratioVisionMobility <- dim(result_disability_vision)[1] / dim(result_disability_mobility)[1]
          # ratioVisionOlder <- dim(result_disability_vision)[1] / dim(result_disability_older)[1]
          # ratioVisionGeneral <- dim(result_disability_vision)[1] / dim(result_disability_general)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_cognitive)[1], dim(result_disability_psychological)[1], dim(result_disability_mobility)[1], dim(result_disability_older)[1], dim(result_disability_general)[1])
          ratioVisionsecondHighest <- dim(result_disability_vision)[1] / secondHighest



          # if(ratioVisionHearing > MINIMUM_RATIO & ratioVisionCognitive > MINIMUM_RATIO & ratioVisionPsychological > MINIMUM_RATIO & ratioVisionMobility > MINIMUM_RATIO & ratioVisionOlder > MINIMUM_RATIO & ratioVisionGeneral > MINIMUM_RATIO){
          if (ratioVisionsecondHighest > MINIMUM_RATIO | ratioVisionsecondHighest == Inf) {
            vision_related_publications <- vision_related_publications + 1
            
            if(title_available){
              vision_related_publications_names <- paste(vision_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              vision_related_publications_names <- paste(vision_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            vision_related_publications <- vision_related_publications + 1
            if(title_available){
              vision_related_publications_names <- paste(vision_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              vision_related_publications_names <- paste(vision_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }
        } else if (dim(result_disability_hearing)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {


          # compute all ratios
          # ratioHearingVision <- dim(result_disability_hearing)[1] / dim(result_disability_vision)[1]
          # ratioHearingCognitive <- dim(result_disability_hearing)[1] / dim(result_disability_cognitive)[1]
          # ratioHearingPsychological <- dim(result_disability_hearing)[1] / dim(result_disability_psychological)[1]
          # ratioHearingMobility <- dim(result_disability_hearing)[1] / dim(result_disability_mobility)[1]
          # ratioHearingOlder <- dim(result_disability_hearing)[1] / dim(result_disability_older)[1]
          # ratioHearingGeneral <- dim(result_disability_hearing)[1] / dim(result_disability_general)[1]


          secondHighest <- order(dim(result_disability_vision)[1], dim(result_disability_cognitive)[1], dim(result_disability_psychological)[1], dim(result_disability_mobility)[1], dim(result_disability_older)[1], dim(result_disability_general)[1])
          ratioHearingsecondHighest <- dim(result_disability_hearing)[1] / secondHighest


          # if(ratioHearingVision > MINIMUM_RATIO & ratioHearingCognitive > MINIMUM_RATIO & ratioHearingPsychological > MINIMUM_RATIO & ratioHearingMobility > MINIMUM_RATIO & ratioHearingOlder > MINIMUM_RATIO & ratioHearingGeneral > MINIMUM_RATIO){
          if (ratioHearingsecondHighest > MINIMUM_RATIO | ratioHearingsecondHighest == Inf) {
            hearing_related_publications <- hearing_related_publications + 1
            
            if(title_available){
              hearing_related_publications_names <- paste(hearing_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              hearing_related_publications_names <- paste(hearing_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            hearing_related_publications <- hearing_related_publications + 1
            if(title_available){
              hearing_related_publications_names <- paste(hearing_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              hearing_related_publications_names <- paste(hearing_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        } else if (dim(result_disability_cognitive)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {


          # compute all ratios
          # ratioCognitiveHearing <- dim(result_disability_cognitive)[1] / dim(result_disability_hearing)[1]
          # ratioCognitiveVision <- dim(result_disability_cognitive)[1] / dim(result_disability_vision)[1]
          # ratioCognitivePsychological <- dim(result_disability_cognitive)[1] / dim(result_disability_psychological)[1]
          # ratioCognitiveMobility <- dim(result_disability_cognitive)[1] / dim(result_disability_mobility)[1]
          # ratioCognitiveOlder <- dim(result_disability_cognitive)[1] / dim(result_disability_older)[1]
          # ratioCognitiveGeneral <- dim(result_disability_cognitive)[1] / dim(result_disability_general)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_vision)[1], dim(result_disability_psychological)[1], dim(result_disability_mobility)[1], dim(result_disability_older)[1], dim(result_disability_general)[1])
          ratioCognitivesecondHighest <- dim(result_disability_cognitive)[1] / secondHighest



          # if(ratioCognitiveHearing > MINIMUM_RATIO & ratioCognitiveVision > MINIMUM_RATIO & ratioCognitivePsychological > MINIMUM_RATIO & ratioCognitiveMobility > MINIMUM_RATIO & ratioCognitiveOlder > MINIMUM_RATIO & ratioCognitiveGeneral > MINIMUM_RATIO){
          if (ratioCognitivesecondHighest > MINIMUM_RATIO | ratioCognitivesecondHighest == Inf) {
            cognitive_related_publications <- cognitive_related_publications + 1
            
            if(title_available){
              cognitive_related_publications_names <- paste(cognitive_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              cognitive_related_publications_names <- paste(cognitive_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            cognitive_related_publications <- cognitive_related_publications + 1
            
            if(title_available){
              cognitive_related_publications_names <- paste(cognitive_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              cognitive_related_publications_names <- paste(cognitive_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        } else if (dim(result_disability_psychological)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {




          # compute all ratios
          # ratioPsychologicalHearing <- dim(result_disability_psychological)[1] / dim(result_disability_hearing)[1]
          # ratioPsychologicalCognitive <- dim(result_disability_psychological)[1] / dim(result_disability_cognitive)[1]
          # ratioPsychologicalVision <- dim(result_disability_psychological)[1] / dim(result_disability_vision)[1]
          # ratioPsychologicalMobility <- dim(result_disability_psychological)[1] / dim(result_disability_mobility)[1]
          # ratioPsychologicalOlder <- dim(result_disability_psychological)[1] / dim(result_disability_older)[1]
          # ratioPsychologicalGeneral <- dim(result_disability_psychological)[1] / dim(result_disability_general)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_cognitive)[1], dim(result_disability_vision)[1], dim(result_disability_mobility)[1], dim(result_disability_older)[1], dim(result_disability_general)[1])
          ratioPsychologicalsecondHighest <- dim(result_disability_psychological)[1] / secondHighest



          # now check that all others are below threshold
          # if(ratioPsychologicalHearing > MINIMUM_RATIO & ratioPsychologicalCognitive > MINIMUM_RATIO & ratioPsychologicalVision > MINIMUM_RATIO & ratioPsychologicalMobility > MINIMUM_RATIO & ratioPsychologicalOlder > MINIMUM_RATIO & ratioPsychologicalGeneral > MINIMUM_RATIO){
          if (ratioPsychologicalsecondHighest > MINIMUM_RATIO | ratioPsychologicalsecondHighest == Inf) {
            psychological_related_publications <- psychological_related_publications + 1
            
            if(title_available){
              psychological_related_publications_names <- paste(psychological_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              psychological_related_publications_names <- paste(psychological_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            psychological_related_publications <- psychological_related_publications + 1
            if(title_available){
              psychological_related_publications_names <- paste(psychological_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              psychological_related_publications_names <- paste(psychological_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        } else if (dim(result_disability_mobility)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {

          # compute all ratios
          # ratioMobilityHearing <- dim(result_disability_mobility)[1] / dim(result_disability_hearing)[1]
          # ratioMobilityCognitive <- dim(result_disability_mobility)[1] / dim(result_disability_cognitive)[1]
          # ratioMobilityPsychological <- dim(result_disability_mobility)[1] / dim(result_disability_psychological)[1]
          # ratioMobilityVision <- dim(result_disability_mobility)[1] / dim(result_disability_vision)[1]
          # ratioMobilityOlder <- dim(result_disability_mobility)[1] / dim(result_disability_older)[1]
          # ratioMobilityGeneral <- dim(result_disability_mobility)[1] / dim(result_disability_general)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_cognitive)[1], dim(result_disability_psychological)[1], dim(result_disability_vision)[1], dim(result_disability_older)[1], dim(result_disability_general)[1])
          ratioMobilitysecondHighest <- dim(result_disability_mobility)[1] / secondHighest



          # if(ratioMobilityHearing > MINIMUM_RATIO & ratioMobilityCognitive > MINIMUM_RATIO & ratioMobilityPsychological > MINIMUM_RATIO & ratioMobilityVision > MINIMUM_RATIO & ratioMobilityOlder > MINIMUM_RATIO & ratioMobilityGeneral > MINIMUM_RATIO){
          if (ratioMobilitysecondHighest > MINIMUM_RATIO | ratioMobilitysecondHighest == Inf) {
            mobility_related_publications <- mobility_related_publications + 1
            if(title_available){
              mobility_related_publications_names <- paste(mobility_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              mobility_related_publications_names <- paste(mobility_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            mobility_related_publications <- mobility_related_publications + 1
            if(title_available){
              mobility_related_publications_names <- paste(mobility_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              mobility_related_publications_names <- paste(mobility_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        } else if (dim(result_disability_older)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {



          # compute all ratios
          # ratioOlderHearing <- dim(result_disability_older)[1] / dim(result_disability_hearing)[1]
          # ratioOlderCognitive <- dim(result_disability_older)[1] / dim(result_disability_cognitive)[1]
          # ratioOlderPsychological <- dim(result_disability_older)[1] / dim(result_disability_psychological)[1]
          # ratioOlderMobility <- dim(result_disability_older)[1] / dim(result_disability_mobility)[1]
          # ratioOlderVision <- dim(result_disability_older)[1] / dim(result_disability_vision)[1]
          # ratioOlderGeneral <- dim(result_disability_older)[1] / dim(result_disability_general)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_cognitive)[1], dim(result_disability_psychological)[1], dim(result_disability_mobility)[1], dim(result_disability_vision)[1], dim(result_disability_general)[1])
          ratioOlderecondHighest <- dim(result_disability_older)[1] / secondHighest



          # if(ratioOlderHearing > MINIMUM_RATIO & ratioOlderCognitive > MINIMUM_RATIO & ratioOlderPsychological > MINIMUM_RATIO & ratioOlderMobility > MINIMUM_RATIO & ratioOlderVision > MINIMUM_RATIO & ratioOlderGeneral > MINIMUM_RATIO){
          if (ratioOlderecondHighest > MINIMUM_RATIO | ratioOlderecondHighest == Inf) {
            elderly_related_publications <- elderly_related_publications + 1
            if(title_available){
              elderly_related_publications_names <- paste(elderly_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              elderly_related_publications_names <- paste(elderly_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_general)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            elderly_related_publications <- elderly_related_publications + 1
            if(title_available){
              elderly_related_publications_names <- paste(elderly_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              elderly_related_publications_names <- paste(elderly_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        } else if (dim(result_disability_general)[1] > MINIMUM_OCCURRENCES_FOR_ACCEPTANCE) {


          # compute all ratios
          # ratioGeneralHearing <- dim(result_disability_general)[1] / dim(result_disability_hearing)[1]
          # ratioGeneralCognitive <- dim(result_disability_general)[1] / dim(result_disability_cognitive)[1]
          # ratioGeneralPsychological <- dim(result_disability_general)[1] / dim(result_disability_psychological)[1]
          # ratioGeneralMobility <- dim(result_disability_general)[1] / dim(result_disability_mobility)[1]
          # ratioGeneralOlder <- dim(result_disability_general)[1] / dim(result_disability_older)[1]
          # ratioGeneralVision <- dim(result_disability_general)[1] / dim(result_disability_vision)[1]

          secondHighest <- order(dim(result_disability_hearing)[1], dim(result_disability_cognitive)[1], dim(result_disability_psychological)[1], dim(result_disability_mobility)[1], dim(result_disability_older)[1], dim(result_disability_vision)[1])
          ratioGeneralsecondHighest <- dim(result_disability_general)[1] / secondHighest


          # if(ratioGeneralHearing > MINIMUM_RATIO & ratioGeneralCognitive > MINIMUM_RATIO & ratioGeneralPsychological > MINIMUM_RATIO & ratioGeneralMobility > MINIMUM_RATIO & ratioGeneralOlder > MINIMUM_RATIO & ratioGeneralVision > MINIMUM_RATIO){
          if (ratioGeneralsecondHighest > MINIMUM_RATIO | ratioGeneralsecondHighest == Inf) {
            general_related_publications <- general_related_publications + 1
            if(title_available){
              general_related_publications_names <- paste(general_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              general_related_publications_names <- paste(general_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            # here we go to next one as this is already categorized
            next
          }

          # now check that all others are below threshold
          else if ((dim(result_disability_vision)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_cognitive)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_psychological)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_mobility)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_hearing)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL) | (dim(result_disability_older)[1] > MAXIMUM_OCCURRENCES_FOR_MANUAL)) {
            message("                      Error for paper, manual adjustment needed: ")
            message("                      ", files[i])
            # message("                      ", metadata$title))
            number_of_problems <- number_of_problems + 1
          } else {
            # otherwise add one to the year and the correct column
            general_related_publications <- general_related_publications + 1
            if(title_available){
              general_related_publications_names <- paste(general_related_publications_names, pdf_title, sep = ";\n\n")
            }else{
              general_related_publications_names <- paste(general_related_publications_names, substr(files[i], 1, nchar(files[i]) - 4), sep = ";\n\n")
            }
            next
          }
        }
      }


      # if we come to this line, then this paper could not be categorized!
      number_of_not_categorized <- number_of_not_categorized + 1
      message("Could not categorize: ", substr(files[i], 1, nchar(files[i]) - 4))

      # we want to know the number of keywords for every non-categorized work
      if (TRUE) {
        # No rows
        # This data frame has columns but no observations.

        # [1] = rows
        # [2] = columns
        if (dim(result_disability_vision)[1] > 0) {
          # a term was found!
          message("Vision disability term was found. Number:", dim(result_disability_vision)[1])
          # message(head(result_disability_vision))
        }

        if (dim(result_disability_hearing)[1] > 0) {
          # a term was found!
          message("Hearing disability term was found. Number:", dim(result_disability_hearing)[1])
          # message(head(result_disability_hearing))
        }

        if (dim(result_disability_cognitive)[1] > 0) {
          # a term was found!
          message("Cognitive disability term was found. Number:", dim(result_disability_cognitive)[1])
          # message(head(result_disability_cognitive))
        }

        if (dim(result_disability_psychological)[1] > 0) {
          # a term was found!
          message("Psychological disability term was found. Number:", dim(result_disability_psychological)[1])
          # message(head(result_disability_psychological))
        }

        if (dim(result_disability_mobility)[1] > 0) {
          # a term was found!
          message("Mobility disability term was found. Number:", dim(result_disability_mobility)[1])
          # message(head(result_disability_mobility))
        }


        if (dim(result_disability_older)[1] > 0) {
          # a term was found!
          message("Elderly disability term was found. Number:", dim(result_disability_older)[1])
          # message(head(result_disability_older))
        }

        if (dim(result_disability_general)[1] > 0) {
          # a term was found in TITLE!
          message("General disability term was found. Number:", dim(result_disability_general)[1])
          # message(head(result_disability_general))
        }
      }

      # create row for all found keywords
      df_one_year_uncategorized <- data.frame(
        Conference = substring(conference_directories[d], 3),
        Vision = dim(result_disability_vision)[1],
        Hearing = dim(result_disability_hearing)[1],
        Cognitive = dim(result_disability_cognitive)[1],
        Psychological = dim(result_disability_psychological)[1],
        Mobility = dim(result_disability_mobility)[1],
        General = dim(result_disability_general)[1],
        Elderly = dim(result_disability_older)[1],
        stringsAsFactors = FALSE
      )

      # string these together
      df_occurrences_un_categorized <- rbind(df_occurrences_un_categorized, df_one_year_uncategorized)


      #
      if (accessibility_related_occurrences < 20) {
        unrelated_work <- unrelated_work + 1
      } else {
        potentially_related_work <- potentially_related_work + 1
      }

      # message('-------------------------')
    }


    # now put all the variables in the right column
    # as we start in 2009
    row_for_df <- year - 2008

    message("Making row for year: ", year)

    # adjust YEAR as defined by the name of the folder
    # [row][column]
    df_conference[row_for_df, 1] <- as.character(year)
    df_conference[row_for_df, 2] <- as.integer(vision_related_publications)
    df_conference[row_for_df, 3] <- as.integer(hearing_related_publications)
    df_conference[row_for_df, 4] <- as.integer(cognitive_related_publications)
    df_conference[row_for_df, 5] <- as.integer(psychological_related_publications)
    df_conference[row_for_df, 6] <- as.integer(mobility_related_publications)
    df_conference[row_for_df, 7] <- as.integer(general_related_publications)
    df_conference[row_for_df, 8] <- as.integer(elderly_related_publications)
    
    
    # adjust YEAR as defined by the name of the folder
    # [row][column]
    df_conference_names[row_for_df, 1] <- as.character(year)
    df_conference_names[row_for_df, 2] <- as.character(vision_related_publications_names)
    df_conference_names[row_for_df, 3] <- as.character(hearing_related_publications_names)
    df_conference_names[row_for_df, 4] <- as.character(cognitive_related_publications_names)
    df_conference_names[row_for_df, 5] <- as.character(psychological_related_publications_names)
    df_conference_names[row_for_df, 6] <- as.character(mobility_related_publications_names)
    df_conference_names[row_for_df, 7] <- as.character(general_related_publications_names)
    df_conference_names[row_for_df, 8] <- as.character(elderly_related_publications_names)

    # go up a directory to directory with the years
    setwd("..")
  }

  df_conference$Overall <- rowSums(df_conference[, c(2:8)])
  df_conference_names$Overall <- df_conference$Overall

  # column_sums
  df_conference[row_for_df + 1, 1] <- as.character("Combined")
  df_conference[row_for_df + 1, c(2:9)] <- colSums(df_conference[, c(2:9)], na.rm = TRUE)
  message("Making formattable table")

  # now make a nice coloured table from it
  format <- formattable(df_conference, list(area(col = 2:8) ~ color_tile("grey", "green")))
  format_names <- formattable(df_conference_names, list(area(col = 2:8) ~ color_tile("grey", "green")))
  
  
  #export_formattable(format, paste0(substring(conference_directories[d], 3), "_table.png"))
  #export_formattable(format_names, paste0(substring(conference_directories[d], 3), "_names_table.png"))

  # package DT needed
  datatable_conference <- as.datatable(format)
  DT::saveWidget(datatable_conference, paste0(substring(conference_directories[d], 3), "_table.html"))
  
  datatable_conference_names <- as.datatable(format_names)
  DT::saveWidget(datatable_conference_names, paste0(substring(conference_directories[d], 3), "_names_table.html"))
  

  # go up a directory
  setwd("..")
}

message("Analysis complete")
# 4 = "complete"
beep(sound = 4, expr = NULL)

message("In total, this many problems occurred: ", number_of_problems)
message("In total, this many papers could not be categorized: ", number_of_not_categorized)

message("In total, this many papers are probably unrelated: ", unrelated_work)
message("In total, this many papers are maybe related: ", potentially_related_work)

message("In total, this many papers were categorized by title: ", categorized_by_title)
message("In total, this many papers were categorized by keywords: ", categorized_by_keywords)


message(warnings())
sink()

df2 <- as.data.frame(df_occurrences)

df2$secondMax <- NULL
df2$thirdMax <- NULL
df2$fourthMax <- NULL
df2$fifthMax <- NULL
df2$sixthMax <- NULL

df2$max <- NULL
df2$min <- NULL

# now find the max and second most ... per paper
# this was used to assess the criteria for the categorization
# 1 indexes rows
df2$max <- apply(df2[, c(2:8)], 1, max)
df2$min <- apply(df2[, c(2:8)], 1, min)
df2$secondMax <- apply(df2[, c(2:8)], 1, function(x) x[maxn(2)(x)])
df2$thirdMax <- apply(df2[, c(2:8)], 1, function(x) x[maxn(3)(x)])
df2$fourthMax <- apply(df2[, c(2:8)], 1, function(x) x[maxn(4)(x)])
df2$fifthMax <- apply(df2[, c(2:8)], 1, function(x) x[maxn(5)(x)])
df2$sixthMax <- apply(df2[, c(2:8)], 1, function(x) x[maxn(6)(x)])

df2$Overall <- rowSums(df2[, c(2:8)])

df2$RatioMaxSecondMax <- df2$max / df2$secondMax

mean(df2$max)
sd(df2$max)
quantile(df2$max, 0.25)
quantile(df2$max, 0.1)


ggplot(df2, aes(x = max)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  scale_x_continuous(limits = c(00, 100))

ggsave("../Scripts/max_distribution_accessibility_all.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



mean(df2$secondMax)
sd(df2$secondMax)
quantile(df2$secondMax, 0.25)
quantile(df2$secondMax, 0.75)

ggplot(df2, aes(x = secondMax)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(secondMax, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(secondMax, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(secondMax, 0.75)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  ggtitle("Second most common occurrences") +
  scale_x_continuous(limits = c(00, 30))

## TODO we saved these in a directory called Scripts
ggsave("../Scripts/second_max_distribution_accessibility_all.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mean(df2$thirdMax)
mean(df2$fourthMax)


mean(df2$Overall)
sd(df2$Overall)
quantile(df2$Overall, 0.25)
quantile(df2$Overall, 0.05)


ggplot(df2, aes(x = Overall)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(Overall, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(Overall, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(Overall, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(Overall, 0.05)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  ggtitle("Overall occurrences") +
  scale_x_continuous(limits = c(00, 150))

ggsave("../Scripts/overall_distribution_accessibility_all.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)








### only look at accessibility-related venues:

accessDf <- subset(df2, df2$Conference == "W4A" | df2$Conference == "ASSETS" | df2$Conference == "TACCESS")


mean(accessDf$max)
sd(accessDf$max)
quantile(accessDf$max, 0.25)
quantile(accessDf$max, 0.1)


mean(accessDf$secondMax)
sd(accessDf$secondMax)
quantile(accessDf$secondMax, 0.25)
quantile(accessDf$secondMax, 0.50)
quantile(accessDf$secondMax, 0.75)

mean(accessDf$Overall)
sd(accessDf$Overall)
quantile(accessDf$Overall, 0.25)
quantile(accessDf$Overall, 0.05)



ggplot(accessDf, aes(x = Overall)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  )

ggsave("../Scripts/overall_distribution_accessibility_only_accessibility.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



ggplot(accessDf, aes(x = max)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  )

ggsave("../Scripts/max_distribution_accessibility_only_accessibility.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




ggplot(accessDf, aes(x = secondMax)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  )

ggsave("../Scripts/second_max_distribution_accessibility_only_accessibility.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)






### only for those that are not categorizable




df_uncategorized <- as.data.frame(df_occurrences_un_categorized)

df_uncategorized$secondMax <- NULL
df_uncategorized$thirdMax <- NULL
df_uncategorized$fourthMax <- NULL
df_uncategorized$fifthMax <- NULL
df_uncategorized$sixthMax <- NULL

df_uncategorized$max <- NULL
df_uncategorized$min <- NULL

# now find the max and second most ... per paper
# this was used to assess the criteria for the categorization
# 1 indexes rows
df_uncategorized$max <- apply(df_uncategorized[, c(2:8)], 1, max)
df_uncategorized$min <- apply(df_uncategorized[, c(2:8)], 1, min)
df_uncategorized$secondMax <- apply(df_uncategorized[, c(2:8)], 1, function(x) x[maxn(2)(x)])
df_uncategorized$thirdMax <- apply(df_uncategorized[, c(2:8)], 1, function(x) x[maxn(3)(x)])
df_uncategorized$fourthMax <- apply(df_uncategorized[, c(2:8)], 1, function(x) x[maxn(4)(x)])
df_uncategorized$fifthMax <- apply(df_uncategorized[, c(2:8)], 1, function(x) x[maxn(5)(x)])
df_uncategorized$sixthMax <- apply(df_uncategorized[, c(2:8)], 1, function(x) x[maxn(6)(x)])

df_uncategorized$Overall <- rowSums(df_uncategorized[, c(2:8)])

df_uncategorized$RatioMaxSecondMax <- df_uncategorized$max / df_uncategorized$secondMax

# get number of uncategorized paper
nrow(df_uncategorized)

mean(df_uncategorized$max)
sd(df_uncategorized$max)
quantile(df_uncategorized$max, 0.25)
quantile(df_uncategorized$max, 0.1)


ggplot(df_uncategorized, aes(x = max)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  )

ggsave("../Scripts/max_distribution_accessibility_all_only_uncategorized.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



mean(df_uncategorized$secondMax)
sd(df_uncategorized$secondMax)
quantile(df_uncategorized$secondMax, 0.25)
quantile(df_uncategorized$secondMax, 0.75)

ggplot(df_uncategorized, aes(x = secondMax)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(secondMax, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(secondMax, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(secondMax, 0.75)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  ggtitle("Second most common occurrences")

ggsave("../Scripts/second_max_distribution_accessibility_all_only_uncategorized.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)







ggplot(df_uncategorized, aes(x = Overall)) +
  geom_histogram(aes(y = ..density..), # the histogram will display "density" on its y-axis
    binwidth = .5, colour = "grey", fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6655") +
  geom_vline(aes(xintercept = mean(max, na.rm = T)),
    colour = "red", linetype = "longdash", size = .8
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.1)),
    colour = "blue", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.25)),
    colour = "green", linetype = "longdash", size = .5
  ) +
  geom_vline(aes(xintercept = quantile(max, 0.75)),
    colour = "green", linetype = "longdash", size = .5
  )

ggsave("../Scripts/overall_distribution_accessibility_all_only_uncategorized.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)








mean(subset(df_uncategorized$max, df_uncategorized$Conference == "W4A"))
sd(subset(df_uncategorized$max, df_uncategorized$Conference == "W4A"))

mean(subset(df_uncategorized$secondMax, df_uncategorized$Conference == "W4A"))
sd(subset(df_uncategorized$secondMax, df_uncategorized$Conference == "W4A"))

quantile(subset(df_uncategorized$max, df_uncategorized$Conference == "W4A"), 0.99)



w4a <- subset(df_uncategorized, df_uncategorized$Conference == "W4A")
nrow(w4a)

taccess <- subset(df_uncategorized, df_uncategorized$Conference == "TACCESS")
nrow(taccess)


assets <- subset(df_uncategorized, df_uncategorized$Conference == "ASSETS")
nrow(assets)


uist <- subset(df_uncategorized, df_uncategorized$Conference == "UIST")
nrow(uist)


chi <- subset(df_uncategorized, df_uncategorized$Conference == "CHI")
nrow(chi)

autoui <- subset(df_uncategorized, df_uncategorized$Conference == "AutoUI")
nrow(autoui)


potentially_related_work
