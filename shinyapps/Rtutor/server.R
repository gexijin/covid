library(openai)
library(tidyverse)
library(shiny)
library(tippy)
library(gridExtra)


###################################################################
# Server
###################################################################

server <- function(input, output, session) {

  # load demo data when clicked
  observe({
    req(input$demo_prompt)
    req(input$select_data)
    if(input$select_data == "mpg" && input$demo_prompt != demos[1]) {
      updateTextInput(
        session,
        "input_text",
        value = input$demo_prompt
      )
    } else { # if not mpg data, reset
      updateTextInput(
        session,
        "input_text",
        value = "",
        placeholder =
"Clearly state the desired statistical analysis in plain English. See examples above.

To see alternative solutions, try again with the same request.

The generated code only works correctly some of the times."
      )
    }
  })

  observeEvent(input$user_file, {
    updateSelectInput(
      session,
      "select_data",
      selected = uploaded_data
    )
  }, ignoreInit = TRUE, once = TRUE)

  observe({
    if (input$submit_button > 0) {
      updateTextInput(session, "submit_button", label = "Re-submit")
    }
  })

  user_data <- reactive({
    req(input$user_file)
    in_file <- input$user_file
    in_file <- in_file$datapath
    req(!is.null(in_file))

    isolate({
      # Excel file ---------------
      if(grepl("xls$|xlsx$", in_file, ignore.case = TRUE)) {
        df <- readxl::read_excel(in_file)
        df <- as.data.frame(df)
      } else {
        #CSV --------------------
        df <- read.csv(in_file)
        # Tab-delimented file ----------
        if (ncol(df) == 2) {
          df <- read.table(
            in_file,
            sep = "\t",
            header = TRUE
          )
        }
      }
      return(df)
    })
  })

  output$data_upload_ui <- renderUI({

    # Hide this input box after the first run.
    req(input$submit_button == 0)

    fileInput(
      inputId = "user_file",
      label = "Upload a file",
      accept = c(
        "text/csv",
        "text/comma-separated-values",
        "text/tab-separated-values",
        "text/plain",
        ".csv",
        ".tsv",
        ".txt",
        ".xls",
        ".xlsx"
      )
    )
  })

  output$demo_data_ui <- renderUI({

    # Hide this input box after the first run.
    req(input$submit_button == 0)

    selectInput(
      inputId = "select_data",
      label = "Dataset",
      choices = datasets,
      selected = "mpg",
      multiple = FALSE,
      selectize = FALSE
    )

  })

  output$prompt_ui <- renderUI({

    req(input$select_data)

    # hide after data is uploaded
    req(is.null(input$user_file))

    if(input$select_data == "mpg") {
      selectInput(
        inputId = "demo_prompt",
        choices = demos,
        label = NULL
      )
    } else {
      return(NULL)
    }
  })

  openAI_prompt <- reactive({
    req(input$submit_button)
    req(input$select_data)
    prep_input(input$input_text, input$select_data)
  })

  openAI_response <- reactive({

    req(input$submit_button)
    req(input$select_data)

    isolate({  # so that it will not responde to text, until submitted
      req(input$input_text)
      prepared_request <- openAI_prompt()
      req(prepared_request)

      withProgress(message = "One sec ...", value = 0, {
        incProgress(0.7)

        start_time <- Sys.time()
        # Send to openAI
        response <- create_completion(
          engine_id = language_model,
          prompt = prepared_request,
          openai_api_key = Sys.getenv("OPEN_API_KEY"),
          max_tokens = 500
        )
        api_time <- difftime(
          Sys.time(),
          start_time,
          units = "secs"
        )[[1]]

        #issue: check status

        cmd <- clean_cmd(response$choices[1, 1], input$select_data)

        return(
          list(
            cmd = cmd,
            response = response,
            time = round(api_time, 0)
          )
        )
      })
    })
  })

  output$openAI <- renderText({
    req(openAI_response()$cmd)
    res <- openAI_response()$response$choices[1, 1]
    # Replace multiple newlines with just one.
    res <- gsub("\n+", "\n", res)
    # Replace emplty lines,  [ ]{0, }--> zero or more space
    res <- gsub("^[ ]{0, }\n", "", res)
    res <- gsub("```", "", res)

  })

  output$usage <- renderText({
    req(openAI_response()$cmd)
    usage <- openAI_response()$response$usage

    paste0(
      "Tokens used: Input=",
      usage$prompt_tokens,
      ", Output=",
      usage$completion_tokens,
      ". \nCost=$", 
      sprintf("%f", usage$completion_tokens * 2e-5),
      "\nAPI time: ", 
      openAI_response()$time,
      " seconds."
    )

  })

  # stores the results after running the generated code.
  run_result <- reactive({
    req(openAI_response()$cmd)

    tryCatch(
      eval(parse(text = openAI_response()$cmd)),
      error = function(e) {
        return(
          list(
            value = -1,
            message = capture.output(print(e$message)),
            error_status = TRUE
          )
        )
      }
    )
  })

  output$result_plot <- renderPlot({
    req(openAI_response()$cmd)
    req(run_result())

    # if error, dummy plot with message
    if(code_error()) {
      grid::grid.newpage()
          grid::grid.text(
            paste(
              "Error: ",
              run_result()$message,
              "\nPlease try again by click Re-submit."
            ),
            x = 0.5,
            y = 0.85,
            gp = grid::gpar(
              col = "red",
              fontsize = 15
            )
          )
    } else {
      run_result() # show plot
    }
  })

  output$result_text <- renderText({
    req(openAI_response()$cmd)
    req(run_result())

    # if error, the returned list has two elements.
    if (code_error()) {
      paste(
        "Error: ",
        run_result()$message,
        "\n\nPlease try again by click Re-submit."
      )
    } else {
      res <- capture.output(run_result())
      return(paste(res, collapse = "\n"))
    }
  })

  # Error when run the generated code?
  code_error <- reactive({
    req(!is.null(run_result()))
    req(input$submit_button)
    req(openAI_response()$cmd)

    error_status <- FALSE

    # if error returns true, otherwise 
    #  that slot does not exist, returning false.
    # or be NULL
    error_status <- tryCatch(
      !is.null(run_result()$error_status),
      error = function(e) {
        return(TRUE)
      }
    )
    return(error_status)
  })

  output$results_ui <- renderUI({
    req(openAI_response()$cmd)

    # if the prompt include the "plot", generate a plot.
    # otherwise run statistical analysis.
    if(sum(grepl("plot|Plot|PLOT", openAI_response()$cmd)) > 0) {
      plotOutput("result_plot")
    } else {
      verbatimTextOutput("result_text")
    }
  })

  output$data_table <- renderTable({
    req(input$select_data)
    if(input$select_data == uploaded_data) {
      eval(parse(text = paste0("user_data()[1:20, ]")))
    } else {
      eval(parse(text = paste0(input$select_data, "[1:20, ]")))
    }
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )



  output$session_info <- renderUI({
    i <- c("<br><h4>R session info: </h4>")
    i <- c(i, capture.output(sessionInfo()))
    HTML(paste(i, collapse = "<br/>"))
  })

 # Defining & initializing the reactiveValues object
  Rmd_total <- reactiveValues(code = "")

  observeEvent(input$submit_button, {
    Rmd_total$code <- paste0(Rmd_total$code, Rmd_chuck())
  })

  # Markdown chuck for the current request
  Rmd_chuck <- reactive({

    req(openAI_response()$cmd)
    req(openAI_prompt())

    # User request----------------------
    Rmd_script  <- paste0(
      "\n\n### ",
      paste(
        openAI_prompt(),
        collapse = "\n"
      ),
      "\n\n"
    )

    # R Markdown code chuck----------------------
    #if error when running the code, do not run
    if (code_error()) {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R, eval = FALSE}\n"
      )
    } else {
      Rmd_script <- paste0(
        Rmd_script,
        "```{R}\n"
      )
    }

    # if uploaded, remove the line: df <- user_data()
    cmd <- openAI_response()$cmd
    if(input$select_data == uploaded_data) {
      cmd <- cmd[-1]
    }

    # Add R code
    Rmd_script <- paste0(
      Rmd_script,
      paste(
        cmd,
        collapse = "\n"
      ),
      "\n```\n"
    )

    return(Rmd_script)


  })

output$rmd_chuck_output <- renderText({
  req(Rmd_chuck())
  #Rmd_chuck()
  Rmd_total$code
})

  # Markdown report
  output$Rmd_source <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor.Rmd",
    content = function(file) {
      Rmd_script <- paste0(
        "---\n",
        "title: \"Report\"\n",
        "author: \"RTutor, Powered by ChatGPT\"\n",
        "date: \"",
        date(), "\"\n",
        "output: html_document\n",
        "---\n",
        Rmd_total$code
      )
      writeLines(Rmd_script, file)
    }
  )

  # Markdown report
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "RTutor_report.html",
    content = function(file) {
      withProgress(message = "Generating Report ...", {
        incProgress(0.2)

        tempReport <- file.path(tempdir(), "report.Rmd")
        # tempReport
        tempReport <- gsub("\\", "/", tempReport, fixed = TRUE)

        req(openAI_response()$cmd)
        req(openAI_prompt())

        #RMarkdown file's Header
        Rmd_script <- paste0(
          "---\n",
          "title: \"Report\"\n",
          "author: \"RTutor, Powered by ChatGPT\"\n",
          "date: \"",
          date(), "\"\n",
          "output: html_document\n",
          "params:\n",
          "  df:\n",
          "printcode:\n",
          "  label: \"Display Code\"\n",
          "  value: TRUE\n",
          "  input: checkbox\n",
          "---\n"
        )

        Rmd_script <- paste0(
          Rmd_script,
          # Get the data from the params list-----------
          "\n\n```{R, echo = FALSE}\n",
          "df <- params$df\n",
          "\n```\n",
          "\n\n### "
        )

        # R Markdown code chuck----------------------

        # Add R code
        Rmd_script <- paste(
          Rmd_script,
          Rmd_total$code
        )

        write(
          Rmd_script,
          file = tempReport,
          append = FALSE
        )

        # Set up parameters to pass to Rmd document
        params <- list(
          df = iris #dummy
        )

        # if uploaded, use that data
        req(input$select_data)
        if(input$select_data == uploaded_data) {
          params <- list(
            df = user_data()
          )
        }

        req(params)
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = tempReport, # markdown_location,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      })
    }
  )
}

# Run the application
# shiny::runApp("app.R")
