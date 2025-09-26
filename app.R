library(shiny)
library(yaml)
source("exercise1-module.R")
source("exercise2-module.R")
source("daily-log-module.R")

# Load config
config <- yaml::read_yaml("config.yaml")
log_folder <- config$log_folder
levels <- c("A1", "A2", "B1", "B2", "C1", "C2")
learning_level <- config$learning_level
system_message <- paste(
"You are a friendly and patient Italian tutor. ",
"Create exercises suitable for a learner at CEFR, ", learning_level,
" progressing toward ", levels[which(levels == learning_level) + 1], ". ",
"Occasionally include brief reminders of ", paste(levels[1: which(levels == learning_level)-1], collapse = ", "), " and ", learning_level, " content to reinforce earlier learning. ",
"When generating exercises:
  - Provide only the minimal text necessary for the exercise (e.g., words, translations, sentences).
  - When providing general exercise content, do NOT include extra explanations or greetings, as the app will provide these separately. However, when giving corrective feedback on student work, a single brief encouraging sentence is allowed (one short sentence maximum) and should be supportive and natural; avoid longer praise or multiple encouraging sentences.
  - Provide translations or explanations where relevant.
  - Occasionally reinforce 'problem words' or topics the student struggled with.
  - Do not repeat words or sentences from previous exercises unnecessarily.
  - Gradually increase difficulty over time.

  When providing feedback:
  - Speak directly to the student in a natural, friendly tone, as if having a conversation.
  - Provide feedback in English.
  - Focus on grammar, vocabulary, and word choice errors.
  - Do NOT correct capitalization or minor punctuation unless it changes meaning.
  - Avoid numbering sentences or using formal lists like 'Corrected:' or 'Explanation:' 
  - Always make clear whether the original answer was fully correct before suggesting improvements."
)

# Load previous logs
log_files <- list.files(log_folder, pattern = "^daily-log_.*\\.txt$", full.names = TRUE)
previous_logs <- if(length(log_files) == 0) NULL else {
  recent_files <- tail(sort(log_files), 3)
  paste(lapply(recent_files, readLines), collapse = "\n")
}

api_key <- Sys.getenv("OPENAI_API_KEY")
session_log <- reactiveValues(
  step1_words = NULL,
  step1_student = NULL,
  step1_feedback = NULL
)

ui <- fluidPage(
  titlePanel("Italian Tutor"),
  tabsetPanel(
    tabPanel("Exercise 1 – Vocabulary", exercise1UI("vocab1")),
    tabPanel("Exercise 2 - Roleplay", exercise2UI("roleplay1")),
    tabPanel("Daily Summary", dailySummaryUI("summary1"))
    # Add other exercises as separate modules similarly
  )
)

server <- function(input, output, session) {
  exercise1Server("vocab1", session_log, previous_logs, system_message, api_key)
  exercise2Server("roleplay1", session_log, previous_logs, system_message, api_key)
  dailySummaryServer("summary1", session_log, log_folder, system_message, api_key)
}

shinyApp(ui, server)
