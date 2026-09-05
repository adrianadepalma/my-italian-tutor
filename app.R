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
generation_model <- config$generation_model
feedback_model <- config$feedback_model
summary_model <- config$summary_model
max_turns <- config$max_turns

system_message <- paste(
  "You are a friendly and patient Italian tutor.

The student is currently learning Italian at CEFR level",
  learning_level,
  "and is progressing toward",
  levels[which(levels == learning_level) + 1],
  ".

Teaching principles:
\n- Create exercises primarily at the student's current CEFR level.
\n- Gradually increase difficulty over time.
\n- Occasionally include review material from earlier CEFR levels.
\n- Occasionally reinforce vocabulary, grammar, or expressions the student has previously struggled with.
\n- Balance new learning with revision.
\n- Avoid unnecessary repetition of recent exercises.
\n- Prioritise communication and comprehension over perfection.

Current level focus:
\n- Most content should be suitable for",
  learning_level,
  ".
\n- When useful, briefly revisit material from",
  ifelse(
    learning_level == "A1",
    "None",
    paste(levels[1:(which(levels == learning_level) - 1)], collapse = ", ")
  ),
  "to reinforce earlier learning.

General content rules:
\n- Keep exercises concise.
\n- Provide only the information needed for the exercise.
\n- Do not include greetings or unnecessary introductions.
\n- Provide translations only when relevant.
"
)

# Load previous logs
log_files <- list.files(
  log_folder,
  pattern = "^daily-log_.*\\.txt$",
  full.names = TRUE
)
previous_logs <- if (length(log_files) == 0) {
  NULL
} else {
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
  exercise1Server(
    "vocab1",
    session_log,
    previous_logs,
    system_message,
    api_key,
    generation_model,
    feedback_model,
    learning_level
  )

  exercise2Server(
    "roleplay1",
    session_log,
    previous_logs,
    system_message,
    api_key,
    max_turns,
    generation_model,
    feedback_model,
    learning_level
  )

  dailySummaryServer(
    "summary1",
    session_log,
    log_folder,
    system_message,
    api_key,
    summary_model
  )
}

shinyApp(ui, server)
