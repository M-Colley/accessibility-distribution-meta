# a function that returns the position of n-th largest
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

read_Metadata <- function(pdf_file) {
  out <- tryCatch(
    {
      metadata <- extract_metadata(pdf_file)

      return(metadata)
    },
    error = function(cond) {
      message(paste0("Title does not seem to exist in PDF:", pdf_file))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return("")
    },
    warning = function(cond) {
      message(paste0("Title caused a warning in PDF:", pdf_file))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return("")
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      # message(paste("Processed PDF:", pdf_file))
      # message("Some other message at the end")
    }
  )
  return(out)
}


export_formattable <- function(f, file, width = "120%", height = NULL,
                               background = "white", delay = 0.3) {
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
    file = file,
    selector = ".formattable_widget",
    delay = delay
  )
}

customGreen0 <- "#DeF7E9"
customGreen <- "#71CA97"
customRed <- "#ff7f7f"


# Change to false to avoid printing
message_all_results <- FALSE
MINIMUM_OCCURRENCES_FOR_ACCEPTANCE <- 23
MAXIMUM_OCCURRENCES_FOR_MANUAL <- 14
MINIMUM_RATIO <- 2.0


mywidth <- 0.4
pdfwidth <- 9
pdfheight <- 4.5
pdfsquare <- 6
myfontsize <- 30

my_positiondodge <- position_dodge(0.5)

scale_fill_colors <- c("#E69F00", "#999999", "#56B4E9")
scale_fill_colors2 <- c("#7fcdbb", "#edf8b1", "#2c7fb8")
scale_fill_colors3 <- c("#a6bddb", "#ece2f0", "#1c9099")
scale_fill_colors4 <- c("#0EE87C", "#999999", "#148DFF")

scale_fill_colors_all_visualizations <- c("#a3a3a3", "#575757", "#faea75", "#ffe000", "#c6ff85", "#88ff00", "#7cf8fc", "#00f7ff", "#809dff", "#003aff", "#f98fff", "#f100ff")
scale_fill_colors_with_visualizations <- c("#575757", "#ffe000", "#88ff00", "#00f7ff", "#003aff", "#f100ff")
scale_fill_colors_without_visualizations <- c("#a3a3a3", "#faea75", "#c6ff85", "#7cf8fc", "#809dff", "#f98fff")
scale_fill_colors_combined_visualizations <- c("#a3a3a3", "#faea75", "#c6ff85", "#7cf8fc", "#809dff", "#f98fff")



#
# Define keywords to search for
#

# disability
# 25-39% of adults with cerebral palsy also have some visual impairment Krigger, K. 2006. Cerebral Palsy: An overview. American Family Physician 73(1), American Academy of Family Physicians, 91-100. Retrieved May 2013 at www.aafp.org/afp
keyword_disability_vision <- c(
  "Braille-Reading", "Impaired Colour Vision", "Tactile Media", "Impaired Colour Vision", "functional vision", "blindness", "Vision Disability", "visually impaired", "visual impairment", "Visual Impairments",
  "visually", "impaired vision", "braille", "visually handicapped", "low vision", "low-vision", "blind", "visually-impaired", "blind-folded", "People with Vision Impairments", "Accessible Image", "accessible EPUB3", "non-visual interaction",
  "partially sighted", "visual disability", "visually challenged", "poor eyesight", "near-blind", "dim-sighted", "visual-to-auditory", "Visual Disabilities", "cane", "image text alternatives", "Aural Navigation", "image text alternative",
  "visual handicap", "shortsighted", "Technology-Mediated Sight", "VIP", "tunnel vision simulation", "Screen-Free", "Screenless", "Auditory Interaction", "Synesthesia vision", "Audio-Tactile Web Browsing", "colour vision deficiency", "Auditory Graphing Software",
  "Color Differentiation", "Color vision", "screen-reader", "Screen Reader", "tactile graphics", "haptic graphics", "Mixed Visual Abilities", "Visual Accessibility", "Foresee", "Tesseract OCR", "Accessible Map Visualization", "Tangram", "Refractive error", "ocular aberration"
)

keyword_disability_hearing <- c(
  "Hard-of-Hearing", "American Sign Language", "Sign Language", "subtitle", "Mobile Sign Language", "Hearing Disability", "deaf", "hearing loss", "impaired hearing", "hearing impaired", "Hearing Disabilities",
  "hearing-impaired", "deafness", "hard of hearing", "impaired hearing", "hearing disorder", "hypoacusis", "hearing difficulties", "ASL", "real-time captioning",
  "hearing problem", "loss of hearing", "Lip-reading", "Hearing Aid", "Cochlear Implants", "Speechreading", "divergent hearing", "transcription"
)

keyword_disability_cognitive <- c(
  "Inclusive Education", "Inclusion and Education", "Speech-Language Therapy", "intellectual disabilities", "cognitive impairment", "cognitively impaired", "Developmental Disabilities", "Developmental Disability", "Brain Injury", "Mental Ill-health", "learning disabilities", "learning disability", "Developmental Disabilities", "Mental Disabilities", "Mental Disability", "Cognitive Disabilities", "Cognitive Disability", "Cognitively Impaired", "Cognitive or Learning Disabilities", "dyslexia", "cognitive disorder", "cognitive impairment",
  "learning difficulty", "learning difficulties", "learning-disabled", "intellectual disability", "intellectual", "Cognitive Assistance", "text simplification",
  "dyslexic", "learning disorder", "down syndrome", "speech impairment", "non-speaking", "nonspeaking", "speech-language therapy", "Complex Communication Needs", "Limited Communication", "SimpleNLG", "Italian Dysarthric Speech", "Dysarthria", "stroke", "Apoplexy", "Brain Disability", "Brain Disabilities", "brain", "Acquired Brain Injury", "Traumatic Brain Injury", "brain disease", "encaphalopathy",
  "brain problem", "brain condition", "brain disorder", "alzheimer", "Parkinson", "with PD", "Amnesia", "dementia", "verbal dyspraxia", "apraxia", "childhood apraxia of speech"
)

keyword_disability_psychological <- c(
  "Psychiatric test", "Borderline", "Dialectical Behavioral", "Psychological Disorders", "schizophrenia", "individuals with SZ", "neurotypical", "personality", "psychological problem",
  "psychological distress", "psychological disturbance", "mental disorder", "mental health", " mental problems",
  "mental illness", "mental disturbance", "folie", "psychic disorder", "neurosis", "psychological condition",
  "eating disorder", "anxiety disorder", "Invisible Disability", "Invisible Disabilities", "adhd", "epilepsy",
  "chronic pain", "chronic fatigue", "chronic dizziness",
  "anxiety disorder", "allergy", "arachnoiditis", "asperger", "asthma",
  "autism", "with ASD", "bipolar disorder", "charcot-marie-tooth", "circadian rhythm",
  "coeliac", "crohn", "depression", "diabetes", "ehlers",
  "endometreosis", "fetal alchohol", "Fibromyalgia", "magraine",
  "multiple sclerosis", "narcolepsy", "repetitive stress",
  "scleroderma", "aphasia", "AAC", "Autistic Spectrum Disorder", "Childhood psychosis"
)
keyword_disability_mobility <- c(
  "Spinal Cord Disability", "Poliomyelitis", "Sidewalk Accessibility", "gait", "Mobility Analysis", "motor impairment", "motor disability", "motor disabilities", "wheelchair", "wheelchair-based", "hand tremor", "tremor", "Cerebral palsy", "Muscular dystrophy", "carpal tunnel", "arthritis", "Dexterity", "Mobility and Physical Impairments", "mobility impairment", "physical handicap", "physically disabled", "physically handicapped",
  "physical inability", "Neuromuscular", "hemiparesis", "Variable Pointing Performance", "gaze-control", "Upper Limb Rehabilitation", "Friedreich", "ataxia", "wearable", "physical condition", "tetraplegic", "motor-impaired", "motor impaired", "limited motor abilities", "physical impairment", "physically disabled"
)



keyword_disability_general <- c(
  "universal access", "accessibility barriers", "universally accessible", "With Disabilities", "with disability", "Accessible Local Government", "Web Design Guidelines", "Teaching Accessibility",
  "Accessible learning", "Accessible Video Player", "Accessible OzPlayer", "Accessibility Guidelines", "Disabled", "Accessible Statistics", "Inclusive Web", "accessibility education",
  "accessibility policies", "website accessibility", "web accessibility", "website accessibility", "Social Connectedness", "digital accessibility", "social accessibility", "App Accessibility", "Chronic Conditions", "Diverse Needs",
  "Cancer", "Printing Assistive Technology", "in Accessibility Research", "Assistive Technology", "Disability and Technology ", "General Accessibility", "Impairment Simulation", "Environmental Accessibility",
  "Special Education", "special support needs", "special needs", "Assistive Services", "Situational impairment", "accessible PDF", "STEM Accessibility", "Web Content Modality", "Capti ESL Assistant", "disability studies", "Assisted Living", "Living", "for the disabled", "Accessible Interactive Simulations", "Accessibility Evaluation", "Accessibility Policies", "Situational Impairment", "Accessibility Feature"
)

keyword_disability_older <- c("Osteoporosis", "older adult", "Ageing", "Aging", "Elderly", "care-giving environment", "Elder", "Senior-Friendly", "senior persona", "Elder Connectedness", "older", "mature", "senior", "elder", "older person", "alzheimer", "seniors", "social isolation", "loneliness", "retirement residence", "Independent Living")









################################################################################## IRRELEVANT

# keyword_method_phone = c('phone interview', 'telephone interview', 'phone conversation', 'telephone interviewing', 'telephone survey')
# keyword_method_focus = c('focus group', 'target group', 'target audience', 'discussion group', 'target group', 'task force',
#                         'group discussion')
# keyword_method_pd = c('participatory design')
# keyword_method_field_study = c('field study', 'field studies', 'ground survey', 'field experiment', 'field trial', 'field investigation',
#                               'fieldstudy', 'field based research', 'investigation in the field')
# Phone Interviews
# Focus Group
# Participatory Design
# Ethnographic Field Studies
# Usability Lab Studies
# Card sorting
# Desirability Studies
# Customer Feedback via email
# Message Board Mining
# Diary / Camera Study
# Usability Benchmarking
# Eye-tracking
# Online User Experience Assessment
# Email Survey
# Intercept Survey
# A/B(Live) Testing
# Data Mining/Analysis
