## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggrepel)
library(eRm)
library(rsconnect)

## lanalytics ##
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
                  "responded at" = parse_datetime(x = "responded at"))
  class(quiz_long) <- c("quizz", class(quiz_long))
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
    ggplot(aes(x = `total score`*10)) +
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
    dplyr::group_by(question, quiz) %>% 
    dplyr::mutate(`time per question` = as.numeric(`time per question`),
                  `Tercil per time temp` = ntile(`time per question`, 3)) %>% 
    left_join(df_left) %>% 
    dplyr::mutate(temp = sum(!is.na(`Tercil per time`)),
                  `Tercil per time` = if_else(temp > 10, as.character(`Tercil per time`), "NA")) %>% 
    dplyr::filter(!is.na(`Tercil per time`), `Tercil per time`!= "NA") %>% 
    dplyr::group_by(question, `Tercil per time`, quiz) %>% 
    dplyr::summarise(`mean score` = mean(as.numeric(score), na.rm = T)) %>% 
    dplyr::filter(`mean score`>.05) %>% 
    ggplot2::ggplot(aes(x = factor(question), 
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
    facet_wrap(~quiz, ncol = 1)
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
    dplyr::select(item, rating) %>% 
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

# u0 interface --------------------------------------------------------
header <- dashboardHeader(title = span("Learning Analytics Dashboard", 
                                       img(src="logo.png", width = 60)), 
                          titleWidth=360)
sidebar <- dashboardSidebar(sidebarMenu(
  br(),
  menuItem("Instructions", tabName = "1_instructions", icon = icon("info")),
  menuItem("Import quizzes", tabName = "2_input", icon = icon("th")),
  menuItem("Display quizzes", tabName = "3_display", icon = icon("eye")),
  menuItem("Data analysis", tabName = "6_analysis", icon = icon("line-chart"),
           menuSubItem("Individual analysis", tabName = "6_3_grupal", icon = icon("dashboard")),
           menuSubItem("Group analysis", tabName = "6_1_individual", icon = icon("dashboard")),
           menuSubItem("Quiz analysis", tabName = "6_2_quiz", icon = icon("dashboard"))
  ),
  menuItem("IRT: eRm package", tabName = "5_irt", icon = icon("book"),
           menuSubItem("Item Characteristic Curves", tabName = "5_1_irt", icon = icon("desktop")),
           menuSubItem("Person-Item Map", tabName = "5_2_irt", icon = icon("desktop")),
           menuSubItem("Person parameters", tabName = "5_3_irt", icon = icon("desktop"))
  ),
  br(),
  actionButton("quit_button", "Exit", icon("sign-out")),
  helpText("\t Press 'Exit' to quit app")
), width = 200)

body <- dashboardBody(
  tabItems(
    # u1 instructions --------------------------------------------------------
    tabItem(tabName = "1_instructions", 
            fluidPage(
              titlePanel(strong("Welcome to the lanalytics dashboard!")),
              br(),
              br(),
              sidebarLayout(
                sidebarPanel(
                  h2("Introduction"),
                  p("This dashboard is an interface of the ", 
                    strong("eRm"), " and the ", 
                    strong("lanalytics"), " packages, which provide useful functions to 
                    analyze online quizzes. The first is available in the CRAN, 
                    and the second is available in a GitHub repository."),
                  br(),
                  p("The ", strong("eRm"),
                    " package is an important package in the psychometric section of the 
                    CRAN and can be installed with the following command: "),
                  br(),
                  p(code("install_packages('eRm')")),
                  p("On the other hand, the lanalytics package contain some useful statistical analysis 
                   that can be used for online quizzes. This package can be installed with the following command: ",
                    code("install_github('savrgg/lanalytics')")),
                  br(),
                  p("For an introduction and examples, please visit the ", 
                    a("lanalytics homepage.", href = "https://savrgg.github.io/lanalytics/"))
                ),
                mainPanel(
                  h1("Instructions"),
                  p("The Learning Analytics dashboard consist of four tabs, the first two are
                     used to import data and display the datasets. The consequtive tabs are used 
                    for analysis and graphs given by the eRm and the lanalytics packages.
                    To get started follow this instructions:"), 
                  br(),
                  p("1) Import your quizzes datafiles in the ", strong("Import quizzes"), 
                    " tab and press the upload button."),
                  p("2) (Optionally) in the ", strong("Import quizzes"), 
                    " tab import a file containing the cognitive level of each question of 
                    each quiz and press the upload button."),
                  p("3) (Optionally) in the ", strong("Import quizzes"), 
                    " tab import a file containing the final exam grade for each student 
                    and press the upload button."),
                  p("4) Go to the ", strong("Display quizzes"), 
                    " tab and check that the data files are imported correctly."),
                  p("5) (Optionally) If you uploaded a cognitive level file, select in the ",
                    strong("Display quizzes"), " tab which colum correspond to the item and
                    which to the cognitive level."),
                  p("6) Go to the ", strong("Data Analysis"), 
                    " tab and for each subtab select the quiz to analyze."),
                  p("7) Go to the ", strong("IRT: eRm package"), 
                    " tab and for each subtab select the quiz to analyze."),
                  br()
                )
              )
            )
    ),
    # u2 input --------------------------------------------------------
    tabItem(tabName = "2_input",
            fluidRow(
              tabBox(
                title = "Import quiz file", width = 6, 
                id = "2_tabset_1", height = "400px",
                tabPanel("csv file",
                         br(),
                         p("Select a *.csv file that contains two columns per item. The first indicating its ", em("score"), 
                           " and the second indicating its ", em("answering time") ,". In addition, the file should contain an 
                           ID column called ", em("email address"), ". This file can be exported from the Learning Catalytics software."),
                         br(),
                         fileInput('file1', 'Select file:',
                                   accept = c('.csv'),
                                   multiple = TRUE
                         ),
                         p("Now click ", strong("Add quiz dataset"), " to correctly upload one or more files 
                    or press ", strong("Remove cognitive dataset"), " to delete them all."),
                         column(3, 
                                actionButton(inputId = "upload_quiz_dataset", 
                                             label = "Add quiz dataset")
                         ),
                         column(3, offset = 3,
                                actionButton(inputId = "remove_quiz_dataset", 
                                             label = "Remove quiz datasets")
                         )
                )
              ),
              tabBox(
                title = "(Optional) Import other files", width = 6,
                id = "2_tabset_2", height = "400px",
                tabPanel("Cognitive file", 
                         br(),
                         p("Select a *.csv file that contains the cognitive level of each item. This file must contain
                           two columns, one indicating the quiz and the item number in the format Q1_q3 
                          (Quiz 1, question 3) and the other indicating the cognitive level in the scale 
                           1-3 (1=low, 3=high)."),
                         br(),
                         fileInput('file2', 'Select file:',
                                   accept = c('.csv')
                         ),
                         p("Now click ", strong("Upload cognitive dataset"), " to correctly upload file 
                           or press ", strong("Remove cognitive dataset"), " to delete the file."),
                         column(3, 
                                actionButton(inputId = "upload_cognitive_dataset", 
                                             label = "Upload cognitive dataset")
                         ),
                         column(3, offset = 3,
                                actionButton(inputId = "remove_cognitive_dataset", 
                                             label = "Remove congnitive datasets")
                         )
                ),
                tabPanel("Final Exam", 
                         br(),
                         p("Select a *.csv file that contains the final grade for each student. This file must 
                           contain two columns, one indicating the student and the other the grade"),
                         br(),
                         fileInput('file3', 'Select file:',
                                   accept = c('.csv')
                         ),
                         p("Now click ", strong("Upload final exam data"), " to correctly upload file 
                           or press ", strong("Remove final exam data"), " to delete the file."),
                         column(3, 
                                actionButton(inputId = "upload_finalexam_dataset", 
                                             label = "Upload final exam data")
                         ),
                         column(3, offset = 3,
                                actionButton(inputId = "remove_finalexam_dataset", 
                                             label = "Remove final exam data")
                         )
                )
              )
            ),
            fluidRow(
              box(title = "Uploaded quiz files:", status = "primary", width = 6,
                  collapsible = TRUE,
                  tableOutput('names_quiz')
              ),
              box(title = "Other files uploaded:", status = "primary", width = 6,
                  collapsible = TRUE,
                  tableOutput('names_cognitive'),
                  tableOutput('names_finalexam')
              )
            )
    ),
    # u3 display --------------------------------------------------------
    tabItem(tabName = "3_display", 
            fluidRow(
              br(),
              h1("Display of the imported datasets"),
              br(),
              box(title = "Quiz dataset:", status = "primary", width = 12,
                  collapsible = TRUE,
                  br(),
                  p("The quizzes files that you imported are shown in ", 
                    strong('long format'), ". That means that each row represent one answer 
                    per item per quiz per student:"),
                  br(),
                  DT::dataTableOutput('quiz_dataset')
              )
            ), 
            fluidRow(
              br(),
              box(title = "Select column of the item:", 
                  br(),
                  p("If you import a cognitive file, this box will contain the names of the 
                    columns of the file. Please select the name of the column that corresponds to 
                    the quiz and the item number (in the format Q1_q2 for Quiz 1, question 2):"),
                  br(),
                  status = "primary", width = 6,
                  collapsible = TRUE,
                  uiOutput('choose_cognitive_item')
              ),
              box(title = "Select column of the rating:", 
                  br(),
                  p("If you import a cognitive file, this box will contain the names of the 
                    columns of the file. Please select the name of the column that corresponds to 
                    the cognitive rating:"),
                  br(),
                  status = "primary", width = 6,
                  collapsible = TRUE,
                  uiOutput('choose_cognitive_rating')
              )
            ),
            fluidRow(
              box(title = "Cognitive levels dataset:", status = "primary", width = 12,
                  br(),
                  p("The cognitive columns that you selected are the following: "),
                  br(),
                  collapsible = TRUE,
                  DT::dataTableOutput('cognitive_dataset')
              )
            ),
            fluidRow(
              box(title = "Final exam dataset:", status = "primary", width = 12,
                  br(),
                  p("The final exam file uploaded is the following: "),
                  br(),
                  collapsible = TRUE,
                  DT::dataTableOutput('finalexam_dataset')
              )
            )
    ),
    # u5 IRT-NO-discrim --------------------------------------------------------
    tabItem(tabName = "5_irt"),
    tabItem(tabName = "5_1_irt",
            fluidRow(
              br(),
              titlePanel(strong("Rasch model - Item Characteristic Curve")),
              p(),
              box(title = "Select quizzes to analize:", status = "info", width = 12,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_1")
              )
            ),
            fluidRow(
              br(),
              p("In the Rasch model, two sets of parameters are calculated, 
                the first set contains one parameter per student and can be explained as the student ability, 
                the second set contains one parameter per item and can be explained as the item difficulty.
                In the ICC plot, a general ability `latent trait` is inferred and is plotted versus the probability of getting correct one item. This is useful for the instructors because
                they can assess the item difficulty in terms of the student's abilities. Each curve in the following
                plot represent one item"),
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_jointICC")
              )
            )
    ),
    tabItem(tabName = "5_2_irt",
            fluidRow(
              br(),
              titlePanel(strong("Rasch model - Person Item map")),
              p("In this plot the histogram of the values of the estimated person parameters is 
                displayed in the top of the plot. Also, the values of the item parameters are displayed
                in the bottom part of the plot. This plot is useful because the instructor can assess
                which items were too easy compared to the student's abilities and then they can try to
                use this information to manage the difficulty of the next quizzes."),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_2")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personItem")
              )
            )
    ),
    tabItem(tabName = "5_3_irt",
            fluidRow(
              br(),
              titlePanel(strong("Rasch model - Person parameter map")),
              p("In this plot the relationship for the person parameter with the raw score 
                  (the total number of correct answers per student) is displayed. This way, we can
                see that a monotone relationship is desired. (a bigger ability should be related to a larger raw score)"),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_3")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personParameter")
              )
            )),
    # u6 analysis --------------------------------------------------------
    tabItem(tabName = "6_analysis"),
    tabItem(tabName = "6_1_individual",
            fluidRow(
              br(),
              h1("Analysis per group"),
              h4("In this tab you can see statistical summaries of the performance of your
                group."),
              br(),
              box(title = "Select quizzes to analize:", status = "info", width = 12,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_1")
              )
            ),
            fluidRow(
              box(title = "Guessers plot", status = "primary", width = 12,
                  br(),
                  p("Sometimes when a student answers a question very fast (or under a threshold), the probability that he get wrong or just 
                    guessed the answer is high. In this plot the questions that are answered in less than 20 seconds (or below an individual threshold) 
                    are shown. 1 means that he answer the question correctly and -1 means that he answer the question incorrectly. Questions that take more 
                    time than the threshold are not shown"),
                  collapsible = TRUE,
                  plotOutput("plot_guessers")
              )
            ),
            fluidRow(
              box(title = "Order plot", status = "primary", width = 12,
                  br(),
                  p("This plot is useful to find the relation between the time per question and the obtained correct answers. It is important to
                    get an idea of how many time each question takes to get a better idea of the design of the quizzes and exams. Some topics take more
                    time and a little bit of more time can have a consequence of a better grade."),
                  collapsible = TRUE,
                  plotOutput("plot_order")
              )
            )
    ),
    tabItem(tabName = "6_2_quiz",
            fluidRow(
              br(),
              h1("Analysis per quiz"),
              h4("In this tab you can see statistical summaries of the performance per quiz"),
              br(),
              titlePanel(strong("Quiz")),
              box(title = "Select quizzes to analize:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_2")
              )
            ),
            fluidRow(
              box(title = "Histogram", status = "primary", width = 12,
                  br(),
                  p("This is an histogram of the final grades in the selected quiz. Ideally, most of the students should be in the middle of the 
                    plot."),
                  collapsible = TRUE,
                  plotOutput("plot_hist")
              )
            ),
            fluidRow(
              box(title = "Boxplot", status = "primary", width = 12,
                  br(),
                  p("This is a Boxplot of the grades of each quiz. This is a non-parametric graph that shows the distribution of the grades.
                    It is useful to easily see the dispersion, skweness and outliers. The box contains 50% of the data (the contained between the first 
                    and the third quartil). Also, the bold line represent the median of the data. The whiskers are useful to see the variability above 
                    and below these quartiles."),
                  collapsible = TRUE,
                  plotOutput("plot_boxplots")
              )
            ),
            fluidRow(
              box(title = "Easiness-time (ET-plot)", status = "primary", width = 6,
                  br(),
                  p("The Easiness-time plot shows the relation between the spent time in each question versus the average grade. 
                    Sometimes easy questions can be answered correctly in just some seconds, while difficult questions should take more time.
                    In this plot, we can visualize this relationship."),
                  collapsible = TRUE,
                  plotOutput("plot_et")
              ),
              box(title = "ETL", status = "primary", width = 6,
                  br(),
                  p("The Easiness-Time-Level plot is like the ET-plot, but taking in consideration the cognitive level of the questions.
                      Usually the question with higher cognitive levels take more time and are more difficult, while the low cognitive level
                      questions are easier and may be answered in less time. In this plot we can observe if this idea holds."),
                  collapsible = TRUE,
                  plotOutput("plot_etl")
              )
            )
    ),
    tabItem(tabName = "6_3_grupal",
            fluidRow(
              h1("Analysis per student."),
              h4("In this tab you can see statistical summaries of the performance per student"),
              box(title = "Quizzes to analize:", status = "info", width = 6,
                  br(),
                  p("Please select one or more quizzes to show the information."),
                  collapsible = TRUE,
                  uiOutput("choose_files_6_3")
              ),
              box(title = "Select email to filter:", status = "info", width = 6,
                  collapsible = TRUE,
                  uiOutput("email_filtered")
              )
            ),
            fluidRow(
              box(title = "Group history", status = "primary", width = 12,
                  br(),
                  p("In this plot you can select a student and see its performance in all the quizzes. 
                    Also, if the grade of the final exam is provided, a red line will be show in the plot."),
                  collapsible = TRUE,
                  plotOutput("plot_group_tot")
              )
            )
    )
  ))

# u-final --------------------------------------------------------
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "black"
)


# s0 server ------------------------------------------------------------------
server <- function(input, output) {
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
    validate(need(input$file2, message = "Please select a quizz"))
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
    radioButtons("choose_cognitive_item", "Choose the quiz-item column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = c(df_cognitive() %>% data.frame() %>% names)[1])
  })
  output$choose_cognitive_rating <- renderUI({
    validate(need((df_cognitive() %>% data.frame() %>% names)[1], "Introduce a valid file."))
    radioButtons("choose_cognitive_rating", "Choose cognitive level column", 
                 choices  = c(df_cognitive() %>% data.frame() %>% names),
                 selected = c(df_cognitive() %>% data.frame() %>% names)[1])
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
  output$plot_jointICC <- renderPlot({
    validate(need(input$choose_files_5_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_jointICC(df_quiz() %>% filter(quiz %in% input$choose_files_5_1))
    }
  })
  output$plot_personItem <- renderPlot({
    validate(need(input$choose_files_5_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_personItem(df_quiz() %>% filter(quiz %in% input$choose_files_5_2))
    }
  })
  output$plot_personParameter <- renderPlot({
    validate(need(input$choose_files_5_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_personParameter(df_quiz() %>% filter(quiz %in% input$choose_files_5_3))
    }
  })
  # s6 analysis ------------------------------------------------------------------
  output$plot_guessers <- renderPlot({
    validate(need(input$choose_files_6_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_guessers(df_quiz() %>% filter(quiz %in% input$choose_files_6_1))  
    }
  })
  output$plot_order <- renderPlot({
    validate(need(input$choose_files_6_1, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_order(df_quiz() %>% filter(quiz %in% input$choose_files_6_1))  
    }
  })
  output$plot_hist <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_hist(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_boxplots <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_boxplots(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_et <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      plot_easiness_time(df_quiz() %>% filter(quiz %in% input$choose_files_6_2))  
    }
  })
  output$plot_etl <- renderPlot({
    validate(need(input$choose_files_6_2, message = "Please select a quizz"))
    validate(need(nrow(df_cognitive())>0, message = "Please select a cognitive file"))
    if(exists("df_quiz")){
      input$newplot
      plot_etl(df_quiz() %>% filter(quiz %in% input$choose_files_6_2), 
               df_cognitive(), 
               item = input$choose_cognitive_item, 
               rating = input$choose_cognitive_rating)  
    }
  })
  output$plot_group_tot <- renderPlot({
    validate(need(input$choose_files_6_3, message = "Please select a quizz"))
    if(exists("df_quiz")){
      input$newplot
      if(exists("df_finalexam")){
        plot_group(df_quiz() %>% filter(quiz %in% input$choose_files_6_3), input$email_filtered, 
                   df_exam = df_finalexam())  
      }else{
        plot_group(df_quiz() %>% filter(quiz %in% input$choose_files_6_3), input$email_filtered)
      }
    }
  })
  
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


shinyApp(ui, server)