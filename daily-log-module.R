library(shiny)
library(httr)
library(jsonlite)

dailySummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("generate_log"), "Generate Daily Session Log"),
    hr(),
    verbatimTextOutput(ns("daily_log_out")),
    hr(),
    actionButton(ns("save_to_file"), "Save Daily Summary to Text File")
  )
}

dailySummaryServer <- function(id, session_log, log_folder, system_message, api_key) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    daily_log_text <- reactiveVal("")

    call_openai <- function(prompt) {
      body <- list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(role = "system", content = system_message),
          list(role = "user", content = as.character(prompt))
        ),
        temperature = 0.7,
        max_tokens = 500
      )

      res <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE)
      )

      if (httr::status_code(res) >= 400) stop("OpenAI API error: ", httr::status_code(res))
      txt <- httr::content(res, "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
      paste(parsed$choices$message.content, collapse = "\n")
    }

    observeEvent(input$generate_log, {

      # Defensive: handle empty or non-list session_log
      if (is.null(session_log) || length(reactiveValuesToList(session_log)) == 0) {
        daily_log_text("No exercises recorded yet.")
        return()
      }

      # --- Build full session log dynamically ---
      local_log <- paste0("# Session Log – ", Sys.Date(), "\n\n")

      log_list <- reactiveValuesToList(session_log)
      has_content <- FALSE
      for (step_name in names(log_list)) {
        step_data <- log_list[[step_name]]
        local_log <- paste0(local_log, "Step: ", step_name, "\n")

        # If step_data is not a list, just print its value
        if (!is.list(step_data)) {
          if (!is.null(step_data) && nzchar(as.character(step_data))) has_content <- TRUE
          local_log <- paste0(local_log, as.character(step_data), "\n\n")
          next
        }

        # Vocabulary exercises (robust to missing fields)
        if (!is.null(step_data$words)) {
          has_content <- TRUE
          local_log <- paste0(local_log, "Vocabulary:\n", paste(step_data$words, collapse="\n"), "\n")
        }
        if (!is.null(step_data$student)) {
          has_content <- TRUE
          local_log <- paste0(local_log, "Student sentences:\n", paste(step_data$student, collapse="\n"), "\n")
        }
        if (!is.null(step_data$feedback)) {
          has_content <- TRUE
          local_log <- paste0(local_log, "Tutor feedback:\n", paste(step_data$feedback, collapse="\n"), "\n")
        }

        # Roleplay exercises (robust to missing fields)
        if (!is.null(step_data$conversations) && length(step_data$conversations) > 0) {
          for (conv in step_data$conversations) {
            # Defensive: check for required fields and type
            if (!is.list(conv)) {
              if (!is.null(conv) && nzchar(as.character(conv))) has_content <- TRUE
              local_log <- paste0(local_log, as.character(conv), "\n")
              next
            }
            scenario <- if (!is.null(conv$scenario)) conv$scenario else "(no scenario)"
            student_role <- if (!is.null(conv$student_role)) conv$student_role else "You"
            assistant_role <- if (!is.null(conv$assistant_role)) conv$assistant_role else "Assistant"
            local_log <- paste0(
              local_log,
              "Scenario: ", scenario, "\n",
              "Roles: Student = ", student_role, ", Assistant = ", assistant_role, "\n"
            )
            if (!is.null(conv$turns) && is.data.frame(conv$turns) && nrow(conv$turns) > 0) {
              has_content <- TRUE
              turns_text <- apply(conv$turns, 1, function(row) {
                paste0("You: ", row["student"], "\n", assistant_role, ": ", row["assistant"])
              })
              local_log <- paste0(local_log, paste(turns_text, collapse="\n"), "\n")
            }
          }
        }

        local_log <- paste0(local_log, "\n") # spacing between steps
      }

      # If no content, do not call OpenAI, just show message
      if (!has_content) {
        daily_log_text("No exercises recorded yet.")
        return()
      }

      # --- Prompt OpenAI for summary ---
      prompt_summary <- paste(
        "Based on all exercises below, write a brief daily summary for the learner in English, including:\n",
        "- Strengths\n- Weaknesses\n- Next Focus\n\n",
        "Content:\n", local_log
      )

      summary_text <- tryCatch(
        call_openai(prompt_summary),
        error = function(e) paste("OpenAI API error:", e$message)
      )

      full_log <- paste(local_log, "Summary\n", summary_text)
      daily_log_text(full_log)
    })

    # --- Save to file ---
    observeEvent(input$save_to_file, {
      if (is.null(daily_log_text()) || daily_log_text() == "") {
        showNotification("No daily log to save!", type = "error")
        return()
      }

      if (!dir.exists(log_folder)) dir.create(log_folder, recursive = TRUE)

      timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
      log_file <- file.path(log_folder, paste0("daily-log_", timestamp, ".txt"))
      writeLines(daily_log_text(), log_file)
      showNotification(paste("Daily log saved to", log_file))
    })

    output$daily_log_out <- renderText({ daily_log_text() })
  })
}
