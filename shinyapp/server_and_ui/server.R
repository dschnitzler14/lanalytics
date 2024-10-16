library(shiny)
library(tidyverse)
library(stringr)
library(ggrepel)
library(eRm)
library(shinydlplot)
library(zip)
library(ggplotify)

# general functions ----
## generic download handler for figures
# createDownloadHandler <- function(plot_expr, filename_prefix = "plot", width = 8, height = 6) {
#   downloadHandler(
#     filename = function() {
#       paste0(filename_prefix, ".png")
#     },
#     content = function(file) {
#       ggsave(file, plot = plot_expr(), width = width, height = height)
#     }
#   )
# }
createDownloadHandler <- function(plot_expr, filename_prefix = "plot", width = 8, height = 6) {
  downloadHandler(
    filename = function() {
      if (is.function(filename_prefix)) {
        return(paste0(filename_prefix(), ".png")) 
      } else {
        return(paste0(filename_prefix, ".png")) 
      }
    },
    content = function(file) {
      ggsave(file, plot = plot_expr(), width = width, height = height)
    }
  )
}


# f2 input --------------------------------------------------------
read_lc <- function(file){
  quiz_sheet <- readr::read_csv(file) %>% 
    setNames(tolower(names(.)))
  
  if(!("email address" %in% names(quiz_sheet))){
    stop("Error: The file does not contain a column called \"email address\"")
  }
  
  quiz_sheet <-  quiz_sheet %>% 
    dplyr::filter(stringr::str_detect(`email address`, "@")) %>% 
    dplyr::mutate(`email address` = as.character(`email address`))
  
  quiz_long <- lapply(c("responded at", "score"), function(df_subset){
    selected_cols <- c("email address",
                       names(quiz_sheet)[stringr::str_detect(names(quiz_sheet), df_subset)])
    
    quiz_long_sheet <- quiz_sheet[names(quiz_sheet) %in% selected_cols] %>% 
      tidyr::gather(question, value, 
                    -c(`email address`)) %>% 
      dplyr::mutate(question = str_extract(tolower(question), "item[0-9]*"),
                    question = str_replace(question, "item", "")) %>% 
      filter(!is.na(value))
    
    names(quiz_long_sheet)[which(names(quiz_long_sheet) == "value")] <- df_subset
    quiz_long_sheet
  }) 
  
  quiz_long <- purrr::reduce(quiz_long, left_join) %>% 
    dplyr::mutate(quiz = file,
                  `responded at` = parse_datetime(as.character(`responded at`)))
  class(quiz_long) <- c("quiz", class(quiz_long))
  quiz_long %>% 
    mutate(score = as.integer(score))
}
add_times <- function(course){
  course <- course %>% 
    dplyr::group_by(quiz, `email address`) %>%
    dplyr::arrange(quiz, `email address`, `responded at`) %>% 
    dplyr::mutate(`order answer` = 1:n(),
                  `time per question` = `responded at` - lag(`responded at`),
                  question = as.numeric(question))
}
# f5 IRT-NO-discrim -----------------------------------------------
plot_jointICC <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  plotjointICC(model, xlim = c(-8, 5))
}  
plot_personItem <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  plotPImap(model)
}  
plot_personParameter <- function(quiz_object){
  data_tibble <- quiz_object %>% 
    ungroup() %>% 
    dplyr::select(`email address`, question, score) %>% 
    tidyr::spread(question, score, fill = 0) %>% 
    dplyr::select(-`email address`) %>% 
    stats::setNames(paste("item", names(.))) %>% 
    purrr::map_if(is.character, as.numeric) %>% 
    tibble::as_tibble() %>% 
    purrr::discard(~sum(.)<03) %>% 
    purrr::discard(~sum(.)>97) %>% as.matrix()
  
  model <- RM(data_tibble)
  pp <- person.parameter(model)
  plot(pp)
} 
# f6 display ------------------------------------------------------
plot_hist <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = `total score` *10)) +
    geom_histogram(binwidth = 10, alpha = .7, 
                   color = "dodgerblue4", fill = "dodgerblue4") +
    labs(x = "Total score (max score = 100)", 
         y = "Count") +
    facet_wrap(~quiz, ncol = 2)
}
plot_boxplots <- function(quiz_object){
  summarized_data <- quiz_object %>%  
    dplyr::group_by(`email address`, `quiz`) %>% 
    dplyr::summarise(`total score` = sum(as.numeric(score)/n()*10, na.rm = T)) 
  
  summarized_data %>% 
    ggplot(aes(x = factor(`quiz`), 
               y = `total score`*10)) +
    geom_boxplot() +
    labs(x = "Quiz name", 
         y = "Total score (max score = 100)")
}
plot_order <- function(quiz_object){
  df_left <- tibble(`Tercil per time temp` = c(1,2,3), 
                    `Tercil per time` = c("First Tercil (Fastest)", 
                                          "Second Tercil", 
                                          "Third Tercil (Slowest)"))
  quiz_object %>% 
    dplyr::group_by(`question`, `quiz`) %>% 
    dplyr::mutate(`time per question` = as.numeric(`time per question`),
                  `Tercil per time temp` = ntile(`time per question`, 3)) %>% 
    left_join(df_left) %>% 
    dplyr::mutate(temp = sum(!is.na(`Tercil per time`)),
                  `Tercil per time` = if_else(temp > 10, as.character(`Tercil per time`), "NA")) %>% 
    dplyr::filter(!is.na(`Tercil per time`), `Tercil per time`!= "NA") %>% 
    dplyr::group_by(`question`, `Tercil per time`, quiz) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = factor(`question`), 
                        y = `mean score` * 100, 
                        group = `Tercil per time`, 
                        color = `Tercil per time`,
                        label = round(`mean score` * 100, 0))) +
    ggplot2::facet_wrap(~`Tercil per time`, ncol = 1) +
    geom_line() +
    geom_point() +
    labs(x = "Question in the quiz",
         y = "Average score per tercil (max score = 100)") +
    geom_label(alpha = .7) +
    facet_wrap(~`quiz`, ncol = 1)
} 
plot_easiness_time <- function(quiz_object){
  quiz_object %>%
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>% 
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    ggplot2::ggplot(aes(x = as.numeric(`mean time`), 
                        y = `mean score`* 100, 
                        label = paste0("q", `question`))) +
    ggplot2::geom_point()+
    ggplot2::geom_smooth(method = "lm") +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)", 
         x = "Average time taken to answer each item (in seconds)") +
    facet_wrap(~quiz, ncol = 1)
}
plot_guessers <- function(quiz_object){
  thresholds_df <- quiz_object %>% 
    dplyr::group_by(quiz) %>% 
    mutate(question = as.numeric(question)) %>% 
    dplyr::filter(question %in% c(10, 5, 6)) %>% 
    dplyr::group_by(`email address`, quiz) %>% 
    dplyr::summarise(threshold = min(`time per question`, na.rm = T),
                     threshold = ifelse(threshold == Inf, NA, threshold),
                     threshold = ifelse((is.na(threshold) || threshold > 20), 20, threshold)) 
  guessing <- quiz_object %>% 
    dplyr::left_join(thresholds_df) %>% 
    dplyr::mutate(guessing = ifelse((!is.na(`time per question`) & `time per question` < threshold), 
                                    ifelse(score == 1, 1, -1), 0)) %>% 
    dplyr::arrange(`time per question`) 
  
  guessing %>% 
    dplyr::select(`email address`, guessing, question) %>% 
    dplyr::filter(guessing != 0) %>%
    ggplot2::ggplot(aes(x = `email address`, 
                        y = factor(question), 
                        color = factor(guessing))) +
    ggplot2::geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust = 1)) +
    labs(x = "Email address", y = "Question number (Item)", color = "Guessing score")+
    facet_wrap(~quiz, ncol = 1)
}
plot_etl <- function(quiz_object, challengeLevel, item = "MCM.2014.item", rating = "Rating.HB"){
  homo_quiz_object <- quiz_object %>% 
    dplyr::mutate(score = as.numeric(score)) %>% 
    dplyr::group_by(quiz, question) %>% 
    dplyr::summarise(`mean score` = mean(score, na.rm = T),
                     `mean time` = median(`time per question`, na.rm = T)) %>%
    dplyr::filter(`mean score` > 0, 
                  `mean time` < 600) %>% 
    dplyr::mutate(question_id = paste0("Q4_", "q", question)) %>% 
    dplyr::select(question_id, `mean time`, `mean score`)
  
  homo_challenge_level <- challengeLevel %>% 
    dplyr::select(all_of(c(item, rating))) %>% 
    setNames(c("question_id", "rating"))
  
  rating_df <- data.frame(rating = factor(c(1,2,3)), 
                          rating2 = factor(c("Low cognitive level", 
                                             "Medium cognitive level", 
                                             "High cognitive level")))
  
  homo_quiz_object %>% 
    dplyr::left_join(homo_challenge_level) %>% 
    dplyr::mutate(`mean time` = as.numeric(`mean time`),
                  rating = factor(rating)) %>% 
    left_join(rating_df) %>% 
    dplyr::select(-rating) %>% 
    ggplot2::ggplot(aes(x = `mean time`, 
                        y = `mean score` *100, 
                        color = rating2, 
                        label = question_id)) +
    ggplot2::geom_point() +
    ggrepel::geom_label_repel() +
    labs(y = "Average score per item (max score = 100)",
         x = "Average time taken to answer each item (in seconds)",
         color = "Cognitive level") +
    facet_wrap(~quiz, ncol = 1)
}
plot_group <- function(quiz_object, email_filtered, df_exam = NULL){
  if(is.null(df_exam)){
    df_exam = tibble(`email address` = quiz_object$`email address` %>% unique, 
                     `final exam` = rep(NA, 100))
  }
  quiz_object %>% 
    left_join(df_exam) %>% 
    group_by(`email address`, quiz, `final exam`) %>% 
    dplyr::summarise(`mean score` = round(100*mean(as.numeric(score), na.rm = T), 0)) %>% 
    filter(`email address` %in% email_filtered) %>%
    ggplot(aes(x = quiz, 
               y = `mean score`, 
               group = `email address`)) +
    geom_line() + 
    geom_point() +
    geom_hline(aes(yintercept = `final exam`), color = "red") + 
    facet_wrap(~`email address`, ncol = 2)
}

# s0 server ------------------------------------------------------------------
server <- function(input, output, session) {
  q <- observe({
    if (input$quit_button == 1) stopApp()
  })
  # s1 instructions ------------------------------------------------------------------
  # s2 input ------------------------------------------------------------------
  infile_quiz <- eventReactive(input$upload_quiz_dataset, {
    validate(need(input$file1, message = "Please add a quiz dataset"))  
    infile_quiz <- input$file1
    infile_quiz
  })
  infile_cognitive <- eventReactive(input$upload_cognitive_dataset, {
    validate(need(input$file2, message = "Please select a quiz"))
    infile_cognitive <- input$file2
    infile_cognitive
  })
  infile_finalexam <- eventReactive(input$upload_finalexam_dataset, {
    validate(need(input$file3, message = "Please add the final exam"))
    infile_finalexam <- input$file3
    infile_finalexam
  })
  quiz_values <- reactiveValues(df_data = NULL)
  cognitive_values <- reactiveValues(df_data = NULL)
  finalexam_values <- reactiveValues(df_data = NULL)
  observeEvent(input$remove_quiz_dataset, {
    quiz_values$df_data <- NULL
  })
  observeEvent(input$upload_quiz_dataset, {
    xx <- lapply(1:nrow(infile_quiz()), function(num){
      add_times(read_lc(infile_quiz()$datapath[num]) %>% 
                  mutate(quiz = infile_quiz()$name[num]))
    })
    xx_temp = bind_rows(xx)
    temp <- is.null(quiz_values$df_data)
    if(temp){
      quiz_values$df_data = xx_temp
    }else{
      quiz_values$df_data = unique(bind_rows(quiz_values$df_data, xx_temp))
    }
  })
  observeEvent(input$remove_cognitive_dataset, {
    cognitive_values$df_data <- NULL
  })
  observeEvent(input$upload_cognitive_dataset, {
    cognitive_values$df_data <- read.csv(infile_cognitive()$datapath) %>% 
      mutate(file = infile_cognitive()$name[1])
  })
  observeEvent(input$remove_finalexam_dataset, {
    finalexam_values$df_data <- NULL
  })
  observeEvent(input$upload_finalexam_dataset, {
    finalexam_values$df_data <- read_csv(infile_finalexam()$datapath) %>% 
      mutate(file = infile_finalexam()$name[1])
  })
  
  df_quiz <- reactive(quiz_values$df_data)
  df_cognitive <- reactive(cognitive_values$df_data)
  df_finalexam <- reactive(finalexam_values$df_data)
  
  output$names_quiz <- renderTable({
    if((df_quiz()$quiz %>% length())>0){
      df_quiz()$quiz %>% unique %>% data.frame() %>% setNames("Uploaded files")}
  })
  output$names_cognitive <- renderTable({
    if((df_cognitive()$file %>% unique %>% length())>0){
      df_cognitive()$file %>% unique %>% data.frame() %>% setNames("Uploaded cognitive file")}
  })
  output$names_finalexam <- renderTable({
    if((df_finalexam()$file %>% unique %>% length())>0){
      df_finalexam()$file %>% unique %>% data.frame() %>% setNames("Uploaded final exam file")}
  })
  
  # s3 display ------------------------------------------------------------------
  output$choose_cognitive_item <- renderUI({
    validate(need((df_cognitive() %>% data.frame() %>% names)[1], "Introduce a valid file."))
    column_names <- names(df_cognitive())
    radioButtons("choose_cognitive_item", "Choose the quiz-item column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = column_names[1])
  })
  output$choose_cognitive_rating <- renderUI({
    validate(need((df_cognitive() %>% data.frame() %>% names)[1], "Introduce a valid file."))
    column_names <- names(df_cognitive())
    radioButtons("choose_cognitive_rating", "Choose cognitive level column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = column_names[2])
  })
  output$quiz_dataset <- DT::renderDataTable({
    DT::datatable(df_quiz(),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
  output$cognitive_dataset <- DT::renderDataTable({
    req(input$choose_cognitive_item)
    validate(need(nrow(df_cognitive())>0, message = "Please select a cognitive file"))
    DT::datatable(df_cognitive() %>% 
                    dplyr::select(input$choose_cognitive_item,
                                  input$choose_cognitive_rating),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
  output$finalexam_dataset <- DT::renderDataTable({
    validate(need(nrow(df_finalexam())>0, message = "Please select a final exam file"))
    DT::datatable(df_finalexam(),
                  extensions = 'Responsive',
                  options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ))
  })
  
  # s5 IRT-NO-disc ------------------------------------------------------------------
 
  filtered_rasch_data <- reactive({
    validate(need(input$choose_files_5_1, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_5_1)
    }
  })
  
  output$plot_jointICC <- renderPlot({
    req(filtered_rasch_data())
    plot_jointICC(filtered_rasch_data())
  })
  
 
  output$downloadPlot_rasch <- downloadHandler(
    filename = function() {
      quiz_number <- strsplit(input$choose_files_5_1, "\\.")[[1]][1]
      paste0(quiz_number, "_ICC.png") 
    },
    
    content = function(file) {
      png(file, width = 800, height = 600)
      plot_jointICC(filtered_rasch_data())
      dev.off()
    }
  )
  
  filtered_personItemRasch <- reactive({
    validate(need(input$choose_files_5_2, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_5_2)
    }
  })
  
  output$plot_personItem <- renderPlot({
    req(filtered_personItemRasch())
    plot_personItem(filtered_personItemRasch())
  })
 

  output$downloadPlot_personItemRasch <- downloadHandler(
    filename = function() {
      quiz_number <- strsplit(input$choose_files_5_2, "\\.")[[1]][1]
      paste0(quiz_number, "_Person_Item_Map.png") 
    },
    
    content = function(file) {
      png(file, width = 800, height = 600)
      plot_personItem(filtered_personItemRasch()) 
      dev.off()
    }
  )

  
  filtered_personParameter <- reactive({
    validate(need(input$choose_files_5_3, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_5_3)
    }
  })
  
  output$plot_personParameter <- renderPlot({
    req(filtered_personParameter())
    plot_personParameter(filtered_personParameter())
  })
  

  output$downloadPlot_personParameter <- downloadHandler(
    filename = function() {
      quiz_number <- strsplit(input$choose_files_5_3, "\\.")[[1]][1]
      paste0(quiz_number, "_Person_Parameter_Map.png") 
    },
    
    content = function(file) {
      png(file, width = 800, height = 600)
      plot_personParameter(filtered_personParameter()) 
      dev.off()
    }
  )
  # s6 analysis ------------------------------------------------------------------
  filtered_guesser_data <- reactive({
    validate(need(input$choose_files_6_1, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_6_1)
    }
  })
  
  output$plot_guessers <- renderPlot({
    req(filtered_guesser_data())
    plot_guessers(filtered_guesser_data())
  })
  
  output$downloadPlot_guesser <- createDownloadHandler(
    plot_expr = reactive(plot_guessers(filtered_guesser_data())),  
    filename_prefix = "analysis_guesser_plot",               
    width = 10,
    height = 6
  )
  
  output$plot_order <- renderPlot({
    req(filtered_guesser_data())
    plot_order(filtered_guesser_data())
  })
  
  output$downloadPlot_order <- createDownloadHandler(
    plot_expr = reactive(plot_order(filtered_guesser_data())),  
    filename_prefix = "analysis_order_plot",               
    width = 10,
    height = 6
  )

    output$download_all_group_plots <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "_all_plots_group_analysis.zip", sep = "")
    },
    content = function(file) {
      temp_dir <- tempdir()
      files <- c()
      plot_files <- list(
        list(expr = plot_guessers(filtered_guesser_data()),
             filename = "plot_guesser.png"),
        list(expr = plot_order(filtered_guesser_data()),
             filename = "plot_order.png")
      )
      
      for (plot_info in plot_files) {
        file_path <- file.path(temp_dir, plot_info$filename)
        ggsave(file_path, plot = plot_info$expr, width = 8, height = 6)
        files <- c(files, file_path)
      }
      
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"
  )
  
  filtered_data <- reactive({
    validate(need(input$choose_files_6_2, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_6_2)
    }
  })
  
  output$plot_hist <- renderPlot({
    req(filtered_data())
    plot_hist(filtered_data())
  })
  
  output$downloadPlot_hist <- createDownloadHandler(
    plot_expr = reactive(plot_hist(filtered_data())),  
    filename_prefix = "analysis_per_quiz_histogram",               
    width = 8,
    height = 6
  )

  output$plot_boxplots <- renderPlot({
    req(filtered_data())
    plot_boxplots(filtered_data())
  })
  
  output$downloadPlot_boxplot <- createDownloadHandler(
    plot_expr = reactive(plot_boxplots(filtered_data())),
    filename_prefix = "analysis_per_quiz_boxplot",
    width = 8,
    height = 6
  )
 
  output$plot_et <- renderPlot({
    req(filtered_data())
    plot_easiness_time(filtered_data())
  })
  
  output$downloadPlot_et <- createDownloadHandler(
    plot_expr = reactive(plot_easiness_time(filtered_data())),
    filename_prefix = "analysis_per_quiz_ET",
    width = 8,
    height = 6
  )
  
  cognitive_data <- reactive({
    validate(need(nrow(df_cognitive()) > 0, message = "Please select a cognitive file"))
    df_cognitive()
  })
  
  output$plot_etl <- renderPlot({
    req(filtered_data(), cognitive_data())
    plot_etl(filtered_data(), cognitive_data(), item = input$choose_cognitive_item, rating = input$choose_cognitive_rating)
    
  })

  output$downloadPlot_etl <- createDownloadHandler(
    plot_expr = reactive(plot_etl(filtered_data(), cognitive_data(), item = input$choose_cognitive_item, rating = input$choose_cognitive_rating)),
    filename_prefix = "analysis_per_quiz_ETL",
    width = 8,
    height = 6
  )
  
  group_data <- reactive({
    validate(need(input$choose_files_6_3, message = "Please select a quiz"))
    
    if (exists("df_quiz")) {
      df_quiz() %>%
        filter(quiz %in% input$choose_files_6_3)
    }
  })
  
  output$plot_group_tot <- renderPlot({
    req(group_data())
    
    if (exists("df_finalexam")) {
      plot_group(group_data(), input$email_filtered, df_exam = df_finalexam())
    } else {
      plot_group(group_data(), input$email_filtered)
    }
  })
  
  
  extracted_email <- reactive({
    email_prefix <- strsplit(input$email_filtered, "@")[[1]][1]
    paste0(email_prefix, "_analysis_per_quiz_group")
  })
  
  output$downloadPlot_group_tot <- createDownloadHandler(
    plot_expr = reactive({
      if (exists("df_finalexam")) {
        plot_group(group_data(), input$email_filtered, df_exam = df_finalexam())
      } else {
        plot_group(group_data(), input$email_filtered)
      }
    }),
    
    filename_prefix = extracted_email,
    
    width = 8,
    height = 6
  )
  
  output$download_all_quiz_plots <- downloadHandler(
    filename = function() {
      paste("all_plots_quiz_analysis", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_dir <- tempdir()
      
      files <- c()
      
      plot_files <- list(
        list(expr = plot_etl(filtered_data(), cognitive_data(), input$choose_cognitive_item, input$choose_cognitive_rating),
             filename = "plot_etl.png"),
        list(expr = plot_hist(filtered_data()),
             filename = "plot_hist.png"),
        list(expr = plot_boxplots(filtered_data()),
             filename = "plot_boxplots.png"),
        list(expr = plot_easiness_time(filtered_data()),
             filename = "plot_easiness_time.png"),
      )
      
      for (plot_info in plot_files) {
        file_path <- file.path(temp_dir, plot_info$filename)
        ggsave(file_path, plot = plot_info$expr, width = 8, height = 6)
        files <- c(files, file_path)
      }
      
      zip::zipr(zipfile = file, files = files)
    },
    contentType = "application/zip"  # Correct content type for a zip file
  )
  
  # s-final ------------------------------------------------------------------
  output$choose_files_5_1 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_1", "Choose files", 
                 choices  = c(df_quiz()$quiz %>% unique),
                 selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_5_2 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_2", "Choose files", 
                 choices  = c(df_quiz()$quiz %>% unique),
                 selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_5_3 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    radioButtons("choose_files_5_3", "Choose files", 
                 choices  = c(df_quiz()$quiz %>% unique),
                 selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_6_1 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_1", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])
  })
  output$choose_files_6_2 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_2", "Choose files",
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique)[1])

  })


  output$choose_files_6_3 <- renderUI({
    validate(need((df_quiz()$quiz %>% unique)[1], "Introduce a quiz datafile"))
    checkboxGroupInput("choose_files_6_3", "Choose files", 
                       choices  = c(df_quiz()$quiz %>% unique),
                       selected = c(df_quiz()$quiz %>% unique))
  })
  output$email_filtered <- renderUI({
    validate(need((df_quiz()$`email address` %>% unique)[1], "Introduce a quiz datafile"))
    selectInput("email_filtered", "Choose email", 
                choices  = c(df_quiz()$`email address` %>% unique),
                selected = c(df_quiz()$`email address` %>% unique)[1])
  })
} # end server
