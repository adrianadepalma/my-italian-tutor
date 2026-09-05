# Italian Tutor (Shiny App)

This is a personal Italian language learning app built with R and Shiny. It provides interactive exercises for vocabulary, roleplay scenarios, and daily summaries, leveraging OpenAI's GPT models for dynamic content and feedback.


## Planned Exercises

1. **Step 1 – Vocabulary:**
   - Get 3 new Italian words or phrases, each with English translation.
   - You write one original sentence per word; the AI corrects and suggests improvements.

2. **Step 2 – Speaking Roleplay:**
   - Short real-life scenario (ordering, shopping, traveling, café, etc.).
   - You respond as the student; the AI corrects mistakes and suggests more natural phrasing.
   - Occasionally reuses your previous “struggle words.”

3. **Step 3 – Writing Exercise:**
   - Receive a short prompt (3–5 sentences).
   - You write in Italian; the AI corrects grammar, vocabulary, and style, and provides natural alternatives.

4. **Step 4 – Grammar Focus:**
   - Focus on one grammar point at your level.
   - Brief explanation with 2–3 examples.
   - You write 1–2 sentences using it; the AI corrects your sentences.

5. **Step 5 – Reflection:**
   - 1–2 reflection questions in Italian (e.g., “Quale parola hai imparato oggi?”).
   - You answer; the AI corrects your response.

## Features & Roadmap

**Implemented:**
- Step 1 – Vocabulary: Practice and receive feedback on new Italian words/phrases.
- Step 2 – Speaking Roleplay: Engage in realistic conversation scenarios with AI, with personalized tutor feedback.
- Daily Summary: Log your learning progress and review recent activity.
- Adaptive Content: Exercises and feedback are tailored to your CEFR level and learning history.

**Planned:**
- Step 2 (Roleplay) Speech-to-Text: Planned update to allow speech input, making this a true speaking exercise rather than just writing.
- Step 3 – Writing Exercise: Write a short text in Italian and receive grammar, vocabulary, and style corrections with natural alternatives.
- Step 4 – Grammar Focus: Practice a specific grammar point with brief explanation, examples, and correction of your sentences.
- Step 5 – Reflection: Answer reflection questions in Italian and get corrections.
- Weekly Summary: Generate a structured weekly review using your daily logs and ChatGPT, including:
   - Progress summary (vocabulary, grammar, speaking, writing, reflection)
   - Recurring mistakes with corrections and explanations
   - Anki-ready CSV vocabulary (Italian, English, Example Sentence)
   - Next week’s goals (3–5 specific, achievable goals)
   - Theme for next week (e.g., café, shopping, travel)


## Requirements
- R (>= 4.0)
- [Shiny](https://shiny.rstudio.com/)
- [httr](https://cran.r-project.org/web/packages/httr/index.html)
- [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
- [yaml](https://cran.r-project.org/web/packages/yaml/index.html)
- An OpenAI API key (set as the `OPENAI_API_KEY` environment variable)

## Setup
1. Clone or download this repository.
2. Install required R packages:
   ```r
   install.packages(c("shiny", "httr", "jsonlite", "yaml"))
   ```
3. Add your OpenAI API key to your environment (e.g., in `.Renviron`):
   ```
   OPENAI_API_KEY=sk-...
   ```
4. Edit `config.yaml` to set your learning level and log folder.
5. Run the app:
   ```r
   shiny::runApp()
   ```

## Notes
- This app is for personal use and was developed with the assistance of GitHub Copilot and ChatGPT.
- All data is stored locally; no user data is shared.
- The app is designed for self-study and experimentation with AI-powered language learning.

