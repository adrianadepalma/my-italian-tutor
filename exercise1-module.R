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

exercise1Server <- function(
  id,
  session_log,
  previous_logs,
  system_message,
  api_key,
  generation_model,
  feedback_model,
  learning_level
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    gpt_text <- reactiveVal("")
    feedback_text <- reactiveVal("")
    gpt_generated_words <- reactiveVal(NULL)
    num_words <- reactiveVal(3)

    call_openai <- function(prompt, model) {
      body <- list(
        model = model,
        messages = list(
          list(role = "system", content = system_message),
          list(role = "user", content = as.character(prompt))
        ) #,
        #max_completion_tokens = 500
      )

      res <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE)
      )

      if (httr::status_code(res) >= 400) {
        err <- httr::content(
          res,
          "text",
          encoding = "UTF-8"
        )

        print(err)

        stop(err)
      }

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

      prompt <- if (!is.null(previous_logs)) {
        paste(
          "Based on the learner's previous logs:\n",
          previous_logs,

          "\n\nGenerate exactly 3 Italian vocabulary items.",
          "\nItems may be words, short phrases, or useful expressions.",

          "\nMost should target CEFR",
          learning_level,
          ".",

          "\nOccasionally include one item that revises earlier grammar or vocabulary.",
          "\nPrefer areas where the learner has previously struggled.",

          "\nProvide English translations.",
          "\nReturn only the vocabulary list."
        )
      } else {
        paste(
          "Generate exactly 3 Italian vocabulary items.",
          "\nItems may be words, short phrases, or useful expressions.",
          "\nProvide English translations.",
          "\nReturn only the vocabulary list."
        )
      }

      gpt_generated <- tryCatch(
        call_openai(prompt, model = generation_model),
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
      sentences <- sapply(1:num_words(), function(i) {
        input[[paste0("resp_", i)]]
      })
      if (any(nchar(trimws(sentences)) == 0)) {
        feedback_text("Please fill in all sentence boxes before submitting.")
        return()
      }

      session_log$step1_student <- paste(
        seq_along(sentences),
        ")",
        sentences,
        collapse = "\n"
      )

      prompt_feedback <- paste(
        "The student completed a vocabulary exercise.",

        "\n\nIMPORTANT:",
        "\n- All feedback must be written in English.",
        "\n- Italian should only appear when quoting the student's sentence or showing a correction.",
        "\n- Your role is to help the student improve, not to grade them.",

        "\n\nVocabulary provided:\n",
        session_log$step1_words,

        "\n\nStudent sentences:\n",
        session_log$step1_student,

        "\n\nReview each sentence separately.",

        "\n\nEvaluation priorities:",
        "\n1. Did the student successfully communicate their intended meaning?",
        "\n2. Is the grammar correct?",
        "\n3. Is the vocabulary appropriate?",
        "\n4. Does the sentence sound natural in Italian?",

        "\n\nIgnore completely:",
        "\n- capitalization",
        "\n- punctuation",
        "\n- formatting",

        "\n\nOnly mention spelling if:",
        "\n- it affects understanding, or",
        "\n- it is a recurring pattern worth practising.",

        "\n\nImportant rules:",
        "\n- Never invent mistakes.",
        "\n- Never provide generic grammar advice.",
        "\n- Never search for tiny mistakes when the sentence works.",
        "\n- Prioritise communication over perfection.",
        "\n- If a sentence is correct, stop looking for problems.",

        "\n\nFor each sentence:",

        "\n- Start by saying whether the meaning was communicated successfully.",
        "\n- Focus on the single most useful improvement.",
        "\n- Explain the issue in plain English.",

        "\n\nWhen a correction is needed:",
        "\n- Quote the relevant Italian.",
        "\n- Give the corrected Italian.",
        "\n- Explain briefly in English why the change helps.",

        "\n\nWhen a sentence is already correct:",
        "\n- Explicitly say it is correct.",
        "\n- Do not invent corrections.",
        "\n- Optionally suggest a more natural alternative if it is genuinely more idiomatic.",
        "\n- Make clear that the original sentence was already correct.",

        "\n\nStyle:",
        "\n- Sound like a supportive tutor having a conversation.",
        "\n- Keep feedback brief.",
        "\n- Use short paragraphs.",
        "\n- Separate feedback for each sentence with a blank line.",
        "\n- Do not use labels such as 'Correct', 'Incorrect', 'Explanation' or 'Correction'.",
        "\n- Use at most one short encouraging sentence."
      )
      feedback <- tryCatch(
        call_openai(prompt_feedback, model = feedback_model),
        error = function(e) paste("API error:", e$message)
      )

      session_log$step1_feedback <- feedback
      feedback_text(feedback)
    })

    output$exercise_out <- renderText({
      gpt_text()
    })
    output$feedback_out <- renderText({
      feedback_text()
    })
  })
}
