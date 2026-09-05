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
      placeholder = "Write your response here...",
      rows = 5
    ),
    actionButton(ns("submit_roleplay"), "Submit Response"),
    hr(),
    h4("Tutor Feedback:"),
    verbatimTextOutput(ns("roleplay_feedback_out"))
  )
}

exercise2Server <- function(
  id,
  session_log,
  previous_logs,
  system_message,
  api_key,
  max_turns,
  generation_model,
  feedback_model,
  learning_level
) {
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
    call_openai <- function(messages, model) {
      body <- list(
        model = model,
        messages = messages
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
        "Generate a realistic everyday conversation scenario for an Italian learner.",
        "Suitable for CEFR",
        learning_level,
        ".",
        "The scenario description must be written in English.",
        "Keep it brief (1-2 sentences).",
        "The student should immediately understand the situation.",
        "Return only the scenario description in English."
      )

      scenario_text <- tryCatch(
        call_openai(
          list(
            list(role = "system", content = system_message),
            list(role = "user", content = scenario_prompt)
          ),
          model = generation_model
        ),
        error = function(e) paste("API error:", e$message)
      )
      scenario(scenario_text)

      # Step 2: Assign concise role labels using JSON
      roles_prompt <- paste(
        "The scenario description is for an English-speaking learner.",

        "\n\nScenario:\n",
        scenario_text,

        "\n\nReturn role names in ENGLISH only.",

        "\n\nExamples:",
        "\nCustomer",
        "\nShop Assistant",
        "\nHotel Receptionist",
        "\nTourist",
        "\nFriend",
        "\nWaiter",
        "\nDoctor",
        "\nPatient",

        "\n\nDo NOT use Italian role names.",
        "\nDo NOT translate the scenario.",
        "\nDo NOT add explanations.",

        "\n\nReturn valid JSON only:",
        "\n{",
        "\n  \"student_role\": \"English role name\",",
        "\n  \"assistant_role\": \"English role name\"",
        "\n}"
      )

      roles_text <- tryCatch(
        call_openai(
          list(
            list(role = "system", content = system_message),
            list(role = "user", content = roles_prompt)
          ),
          model = generation_model
        ),
        error = function(e) paste("API error:", e$message)
      )

      # Parse JSON roles safely
      roles_json <- tryCatch(
        {
          jsonlite::fromJSON(roles_text)
        },
        error = function(e) {
          list(student_role = "You", assistant_role = "Assistant")
        }
      )
      student_role_name(roles_json$student_role)
      assistant_role_name(roles_json$assistant_role)

      # Step 3: Generate assistant's first line
      first_line_prompt <- paste(
        "\n- You are playing the assistant role in the scenario below.",
        "\n- Start the conversation naturally in Italian.",
        "\n- Use language appropriate for CEFR",
        learning_level,
        ".",
        "\n- Return only one opening message.",
        "\n- Do not provide explanations.",
        "\n- Do not provide corrections.",
        "\n- Do not describe the scenario.",
        "\n\nScenario:\n",
        scenario_text,
        "\n\nRoles:\n",
        roles_text
      )

      assistant_first_line <- tryCatch(
        call_openai(
          list(
            list(role = "system", content = system_message),
            list(role = "user", content = first_line_prompt)
          ),
          model = generation_model
        ),
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
        "Scenario: ",
        scenario_text,
        "\n\n",
        "Here are our roles for this exercise:\n",
        student_role_name(),
        " will be played by you\n",
        assistant_role_name(),
        " will be played by me.\n\n",
        "Ok, I'll start: "
      )

      # Display intro + first line
      roleplay_text(paste0(intro_text, assistant_first_line))
      updateTextAreaInput(session, "roleplay_resp", value = "")

      # Initialize structured session log for this scenario
      if (is.null(session_log$step2)) {
        session_log$step2 <- list(conversations = list())
      }
      session_log$step2$conversations[[
        length(session_log$step2$conversations) + 1
      ]] <- list(
        scenario = scenario_text,
        student_role = student_role_name(),
        assistant_role = assistant_role_name(),
        turns = data.frame(
          student = character(),
          assistant = character(),
          stringsAsFactors = FALSE
        )
      )
    })

    # --- Submit student response ---
    observeEvent(input$submit_roleplay, {
      if (ended()) {
        return()
      }
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

      conversation_so_far <- paste(
        sapply(conv[-1], function(x) {
          paste0(
            ifelse(x$role == "assistant", assistant_role_name(), "You"),
            ": ",
            x$content
          )
        }),
        collapse = "\n"
      )

      next_turn_prompt <- paste(
        "\n- Remain fully in character.",
        "\n- Reply naturally in Italian.",
        "\n- Build on what the student has said.",
        "\n- Ask follow-up questions when appropriate.",
        "\n- Move the conversation forward naturally.",
        "\n- Do not teach.",
        "\n- Do not correct mistakes.",
        "\n- Do not mention grammar.",
        "\n- Do not evaluate the student's language.",
        "\n- Do not repeatedly use generic prompts such as:",
        "\n  'Che ne pensi?'",
        "\n  'Che ne dici?'",
        "\n\nConversation so far:\n",
        conversation_so_far
      )

      assistant_reply <- tryCatch(
        call_openai(
          append(
            conv,
            list(list(role = "user", content = next_turn_prompt))
          ),
          model = generation_model
        ),
        error = function(e) paste("API error:", e$message)
      )
      assistant_reply <- trimws(assistant_reply)
      conv <- append(
        conv,
        list(list(role = "assistant", content = assistant_reply))
      )
      conversation(conv)

      roleplay_text(paste0(
        roleplay_text(),
        "\n\nYou: ",
        student_resp,
        "\n",
        assistant_role_name(),
        ": ",
        assistant_reply
      ))
      roleplay_feedback("")
      turn_count(turn_count() + 1)
      updateTextAreaInput(session, "roleplay_resp", value = "")

      # --- Log this turn in structured session_log ---
      current_conv <- session_log$step2$conversations[[length(
        session_log$step2$conversations
      )]]
      current_conv$turns <- rbind(
        current_conv$turns,
        data.frame(
          student = student_resp,
          assistant = assistant_reply,
          stringsAsFactors = FALSE
        )
      )
      session_log$step2$conversations[[length(
        session_log$step2$conversations
      )]] <- current_conv

      if (turn_count() >= max_turns) {
        roleplay_feedback(paste(
          roleplay_feedback(),
          "\n\nMaximum number of turns reached. Roleplay ended."
        ))
        ended(TRUE)

        # --- Generate feedback using OpenAI ---
        conversation_history <- paste(
          sapply(conversation()[-1], function(x) {
            paste0(
              ifelse(x$role == "assistant", assistant_role_name(), "You"),
              ": ",
              x$content
            )
          }),
          collapse = "\n"
        )

        feedback_prompt <- paste(
          "You are an experienced Italian language tutor.",

          "\n\nIMPORTANT LANGUAGE RULES:",
          "\n- All feedback must be written in English.",
          "\n- All explanations must be written in English.",
          "\n- Do not provide feedback in Italian.",
          "\n- Italian should only appear when quoting or discussing the student's language.",
          "\n- The learner understands English and wants feedback in English.",

          "\n\nYour goal is to help the student improve, not to grade them.",

          "\n\nReview the student's Italian during the roleplay.",

          "\n\nEvaluation principles:",
          "\n- Prioritise successful communication over grammatical perfection.",
          "\n- Focus on recurring patterns rather than isolated mistakes.",
          "\n- Ignore minor punctuation and capitalisation issues.",
          "\n- Never invent mistakes.",
          "\n- Only comment on issues that actually appear in the student's language.",
          "\n- If communication was successful, explicitly say so.",
          "\n- Do not look for small mistakes simply to provide criticism.",

          "\n\nProvide:",

          "\n\nStrengths:",
          "\n- Identify 2-3 things the student did well.",
          "\n- Focus on communication, vocabulary, grammar, or naturalness.",

          "\n\nMost useful improvement:",
          "\n- Identify the single most important area for improvement.",
          "\n- Focus on one pattern rather than multiple small issues.",

          "\n\nNext practice suggestion:",
          "\n- Suggest one practical thing to practise next.",

          "\n\nExample style:",
          "\nCommunication was successful throughout the conversation.",
          "\nYou used appropriate vocabulary when talking about food.",
          "\nOne area to improve is asking questions more naturally.",
          "\nPractise using question forms in everyday conversations.",

          "\n\nStyle:",
          "\n- Keep feedback concise.",
          "\n- Keep feedback supportive.",
          "\n- Write natural tutor-style comments, not exam-style marking.",
          "\n- Do not use labels such as 'Correct', 'Incorrect', 'Grade', or 'Score'.",

          "\n\nConversation:\n",
          conversation_history
        )

        feedback_text <- tryCatch(
          call_openai(
            list(
              list(role = "system", content = system_message),
              list(role = "user", content = feedback_prompt)
            ),
            model = feedback_model
          ),
          error = function(e) paste("API error:", e$message)
        )
        roleplay_feedback(paste(
          roleplay_feedback(),
          "\n\nFeedback:\n",
          feedback_text
        ))
      }
    })

    # --- End conversation manually ---
    observeEvent(input$end_roleplay, {
      ended(TRUE)
      roleplay_feedback("Roleplay ended by user.")

      # --- Generate feedback using OpenAI ---
      conversation_history <- paste(
        sapply(conversation()[-1], function(x) {
          paste0(
            ifelse(x$role == "assistant", assistant_role_name(), "You"),
            ": ",
            x$content
          )
        }),
        collapse = "\n"
      )

      feedback_prompt <- paste(
        "You are an experienced Italian language tutor.",
        "Review the student's Italian during the roleplay.",

        "\n\nEvaluation principles:",
        "\n- Prioritise successful communication over grammatical perfection.",
        "\n- Focus on recurring patterns rather than isolated mistakes.",
        "\n- Ignore minor punctuation and capitalisation issues.",
        "\n- Never invent mistakes.",
        "\n- Only comment on issues that actually appear in the student's language.",

        "\n\nProvide:",

        "\nStrengths:",
        "\n- Identify 2-3 things the student did well.",

        "\n\nArea for improvement:",
        "\n- Identify the single most important improvement area.",

        "\n\nNext step:",
        "\n- Suggest one practical thing to practise next.",

        "\n\nIf communication was successful, explicitly say so.",

        "\n\nKeep feedback concise and encouraging.",

        "\n\nConversation:\n",
        conversation_history
      )

      feedback_text <- tryCatch(
        call_openai(
          list(
            list(role = "system", content = system_message),
            list(role = "user", content = feedback_prompt)
          ),
          model = feedback_model
        ),
        error = function(e) paste("API error:", e$message)
      )
      roleplay_feedback(paste(
        roleplay_feedback(),
        "\n\nFeedback:\n",
        feedback_text
      ))
    })

    output$roleplay_text_out <- renderText({
      roleplay_text()
    })
    output$roleplay_feedback_out <- renderText({
      roleplay_feedback()
    })
  })
}
