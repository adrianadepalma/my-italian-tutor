library(shiny)
library(httr)
library(jsonlite)

# UI
exercise2UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("exercise2"), "Start Roleplay Scenario"),
    actionButton(ns("end_roleplay"), "End Roleplay"),
    hr(),
    h4("Conversation:"),
    verbatimTextOutput(ns("roleplay_text_out")),
    hr(),
    h4("Your Response:"),
    textAreaInput(
      ns("roleplay_resp"),
      label = NULL,
      placeholder = "Write your response here...", rows = 5),
    actionButton(ns("submit_roleplay"), "Submit Response"),
    hr(),
    h4("Tutor Feedback:"),
    verbatimTextOutput(ns("roleplay_feedback_out"))
  )
}

exercise2Server <- function(id, session_log, previous_logs, system_message, api_key, max_turns = 3) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive values ---
    scenario <- reactiveVal("")
    student_role_name <- reactiveVal("")
    assistant_role_name <- reactiveVal("")
    conversation <- reactiveVal(list())
    roleplay_text <- reactiveVal("")
    roleplay_feedback <- reactiveVal("")
    turn_count <- reactiveVal(0)
    ended <- reactiveVal(FALSE)

    # --- API call helper ---
    call_openai <- function(messages) {
      body <- list(
        model = "gpt-3.5-turbo",
        messages = messages,
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

    # --- Start roleplay ---
    observeEvent(input$exercise2, {
      turn_count(0)
      ended(FALSE)
      roleplay_text("")
      roleplay_feedback("")
      scenario("")
      student_role_name("")
      assistant_role_name("")

      # Step 1: Pick scenario
      scenario_prompt <- paste(
        "Provide a short, real-life scenario suitable for a B1-B2 Italian learner.",
        "Only describe the situation. Do NOT include instructions, suggested sentences, or vocabulary guidance."
      )

      scenario_text <- tryCatch(
        call_openai(list(
          list(role = "system", content = system_message),
          list(role = "user", content = scenario_prompt)
        )),
        error = function(e) paste("API error:", e$message)
      )
      scenario(scenario_text)

      # Step 2: Assign concise role labels using JSON
      roles_prompt <- paste(
        "Given the scenario:\n", scenario_text, "\n",
        "Provide concise, human-friendly role labels as JSON:\n",
        "{\n  \"student_role\": \"...\",\n  \"assistant_role\": \"...\"\n}"
      )

      roles_text <- tryCatch(
        call_openai(list(
          list(role = "system", content = system_message),
          list(role = "user", content = roles_prompt)
        )),
        error = function(e) paste("API error:", e$message)
      )

      # Parse JSON roles safely
      roles_json <- tryCatch({
        jsonlite::fromJSON(roles_text)
      }, error = function(e) {
        list(student_role = "You", assistant_role = "Assistant")
      })
      student_role_name(roles_json$student_role)
      assistant_role_name(roles_json$assistant_role)

      # Step 3: Generate assistant's first line
      first_line_prompt <- paste(
        "You are the assistant/character in the following scenario. ",
        "Provide ONLY the first line in Italian that the assistant would say to start the conversation. ",
        "Do NOT include any student's line, labels, or explanations. ",
        "Do not answer as if the student has already spoken.\n\n",
        "Scenario:\n", scenario_text, "\n",
        "Roles:\n", roles_text
      )

      assistant_first_line <- tryCatch(
        call_openai(list(
          list(role = "system", content = system_message),
          list(role = "user", content = first_line_prompt)
        )),
        error = function(e) paste("API error:", e$message)
      )
      assistant_first_line <- trimws(assistant_first_line)

      # Store conversation
      conv <- list(
        list(role = "system", content = system_message),
        list(role = "assistant", content = assistant_first_line)
      )
      conversation(conv)

      # Personable intro
      intro_text <- paste0(
        "Scenario: ", scenario_text, "\n\n",
        "Here are our roles for this exercise:\n",
        student_role_name(), " will by played by you\n",
        assistant_role_name(), " will be played by me.\n\n",
        "Ok, I'll start: "
      )

      # Display intro + first line
      roleplay_text(paste0(intro_text, assistant_first_line))
      updateTextAreaInput(session, "roleplay_resp", value = "")

      # Initialize structured session log for this scenario
      if (is.null(session_log$step2)) session_log$step2 <- list(conversations = list())
      session_log$step2$conversations[[length(session_log$step2$conversations) + 1]] <- list(
        scenario = scenario_text,
        student_role = student_role_name(),
        assistant_role = assistant_role_name(),
        turns = data.frame(student = character(), assistant = character(), stringsAsFactors = FALSE)
      )
    })

    # --- Submit student response ---
    observeEvent(input$submit_roleplay, {
      if (ended()) return()
      student_resp <- input$roleplay_resp
      if (nchar(trimws(student_resp)) == 0) {
        roleplay_feedback("Please write a response before submitting.")
        return()
      }
      if (turn_count() >= max_turns) {
        roleplay_feedback("Maximum number of turns reached.")
        ended(TRUE)
        return()
      }

      conv <- conversation()
      conv <- append(conv, list(list(role = "user", content = student_resp)))

      conversation_so_far <- paste(sapply(conv[-1], function(x) {
        paste0(ifelse(x$role == "assistant", assistant_role_name(), "You"), ": ", x$content)
      }), collapse = "\n")

      next_turn_prompt <- paste(
        "You are the assistant/character in the following scenario. ",
        "Continue the conversation in Italian based on the scenario and previous messages. ",
        "Your replies should be natural prompts or questions that move the interaction forward, ",
        "based on the situation and what the student just said. ",
        "Do NOT use generic filler phrases like 'Che ne dici?' or 'Che ne pensi?'. ",
        "Do NOT correct or comment on the student's mistakes. Only respond in character.\n\n",
        "Conversation so far:\n", conversation_so_far
      )

      assistant_reply <- tryCatch(
        call_openai(append(conv, list(list(role = "user", content = next_turn_prompt)))),
        error = function(e) paste("API error:", e$message)
      )
      assistant_reply <- trimws(assistant_reply)
      conv <- append(conv, list(list(role = "assistant", content = assistant_reply)))
      conversation(conv)

      roleplay_text(paste0(roleplay_text(), "\n\nYou: ", student_resp, "\n", assistant_role_name(), ": ", assistant_reply))
      roleplay_feedback(assistant_reply)
      turn_count(turn_count() + 1)
      updateTextAreaInput(session, "roleplay_resp", value = "")

      # --- Log this turn in structured session_log ---
      current_conv <- session_log$step2$conversations[[length(session_log$step2$conversations)]]
      current_conv$turns <- rbind(
        current_conv$turns,
        data.frame(student = student_resp, assistant = assistant_reply, stringsAsFactors = FALSE)
      )
      session_log$step2$conversations[[length(session_log$step2$conversations)]] <- current_conv

      if (turn_count() >= max_turns) {
        roleplay_feedback(paste(roleplay_feedback(), "\n\nMaximum number of turns reached. Roleplay ended."))
        ended(TRUE)

        # --- Generate feedback using OpenAI ---
        conversation_history <- paste(sapply(conversation()[-1], function(x) {
          paste0(ifelse(x$role == "assistant", assistant_role_name(), "You"), ": ", x$content)
        }), collapse = "\n")
        feedback_prompt <- paste(
          "You are an expert Italian language tutor. Based on the following roleplay conversation, provide concise, constructive feedback in English for the student (B1-B2 level).",
          "Focus on communication, grammar, vocabulary, and naturalness. Mention strengths and suggest 1-2 areas for improvement. Do NOT translate or correct every sentence.\n\n",
          "Conversation:\n", conversation_history
        )
        feedback_text <- tryCatch(
          call_openai(list(
            list(role = "system", content = system_message),
            list(role = "user", content = feedback_prompt)
          )),
          error = function(e) paste("API error:", e$message)
        )
        roleplay_feedback(paste(roleplay_feedback(), "\n\nFeedback:\n", feedback_text))
      }
    })

    # --- End conversation manually ---
    observeEvent(input$end_roleplay, {
      ended(TRUE)
      roleplay_feedback("Roleplay ended by user.")

      # --- Generate feedback using OpenAI ---
      conversation_history <- paste(sapply(conversation()[-1], function(x) {
        paste0(ifelse(x$role == "assistant", assistant_role_name(), "You"), ": ", x$content)
      }), collapse = "\n")
      feedback_prompt <- paste(
        "You are an expert Italian language tutor. Based on the following roleplay conversation, provide concise, constructive feedback in English for the student.",
        "Focus on communication, grammar, vocabulary, and naturalness. Mention strengths and suggest 1-2 areas for improvement. Do NOT translate or correct every sentence.\n\n",
        "Conversation:\n", conversation_history
      )
      feedback_text <- tryCatch(
        call_openai(list(
          list(role = "system", content = system_message),
          list(role = "user", content = feedback_prompt)
        )),
        error = function(e) paste("API error:", e$message)
      )
      roleplay_feedback(paste(roleplay_feedback(), "\n\nFeedback:\n", feedback_text))
    })

    output$roleplay_text_out <- renderText({ roleplay_text() })
    output$roleplay_feedback_out <- renderText({ roleplay_feedback() })
  })
}
