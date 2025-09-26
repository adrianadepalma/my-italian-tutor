library(shiny)

exercise1UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("exercise1"), "Generate Vocabulary Exercise"),
    hr(),
    h4("Instructions & Vocabulary:"),
    verbatimTextOutput(ns("exercise_out")),
    hr(),
    h4("Write your sentences:"),
    uiOutput(ns("user_inputs")),
    actionButton(ns("submit_resp"), "Submit All Sentences"),
    hr(),
    h4("Tutor Feedback:"),
    verbatimTextOutput(ns("feedback_out"))
  )
}

exercise1Server <- function(id, session_log, previous_logs, system_message, api_key) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    gpt_text <- reactiveVal("")
    feedback_text <- reactiveVal("")
    gpt_generated_words <- reactiveVal(NULL)
    num_words <- reactiveVal(3)

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

    # Generate exercise
    observeEvent(input$exercise1, {
      instructions <- paste(
        "Here are three Italian words for you to practice.",
        "For each word, write one original sentence in Italian using the word."
      )

      prompt <- if(!is.null(previous_logs)){
        paste("Based on previous logs:\n", previous_logs,
              "\nGenerate 3 new Italian words or phrases with English translation.")
      } else {
        "Generate 3 Italian words or phrases with English translation."
      }

      gpt_generated <- tryCatch(
        call_openai(prompt),
        error = function(e) paste("API error:", e$message)
      )

      gpt_generated_words(gpt_generated)
      session_log$step1_words <- gpt_generated
      combined_text <- paste(instructions, "\n\n", gpt_generated)
      gpt_text(combined_text)
      feedback_text("")

      output$user_inputs <- renderUI({
        lapply(1:num_words(), function(i) {
          textInput(ns(paste0("resp_", i)), paste("Sentence", i), value = "")
        })
      })
    })

    # Submit responses and get feedback
    observeEvent(input$submit_resp, {
      sentences <- sapply(1:num_words(), function(i) input[[paste0("resp_", i)]])
      if (any(nchar(trimws(sentences)) == 0)) {
        feedback_text("Please fill in all sentence boxes before submitting.")
        return()
      }

      session_log$step1_student <- paste(seq_along(sentences), ")", sentences, collapse = "\n")

      prompt_feedback <- paste(
        "You are a friendly Italian tutor. The student has completed the following vocabulary exercise:\n",
        session_log$step1_words,
        "\n\nStudent sentences:\n",
        session_log$step1_student,
        "\n\nPlease review each sentence one at a time. For each sentence, follow this concise three-part pattern:",
        "- Start with a single short, supportive phrase (do not use the words 'Incorrect:' or 'Correct:'), for example: 'Nice try —', 'Good attempt —', 'This is correct.'",
        "- If correction is needed, provide the corrected Italian sentence on its own line. If the sentence is already correct, optionally show a slightly more natural alternative but do not replace a correct sentence unless it improves naturalness.",
        "- Add a single brief explanation in English that explains the grammar/vocabulary issue (one or two short sentences max), integrated naturally with the feedback.",
        "Keep the tone friendly and encouraging, avoid long praise or lecture-style explanations, focus only on grammar/vocabulary/word choice, and do NOT correct capitalization or minor punctuation unless it changes meaning.",
        "Do not use numbered lists or labels; write each sentence's feedback as a short conversational paragraph."
      )

      feedback <- tryCatch(
        call_openai(prompt_feedback),
        error = function(e) paste("API error:", e$message)
      )

      session_log$step1_feedback <- feedback
      feedback_text(feedback)
    })

    output$exercise_out <- renderText({ gpt_text() })
    output$feedback_out <- renderText({ feedback_text() })
  })
}
