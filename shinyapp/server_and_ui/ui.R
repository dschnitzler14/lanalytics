library(shiny)
library(shinydashboard)
library(shinydlplot)

# u0 interface --------------------------------------------------------
## header -----
header <- dashboardHeader(title = span("Learning Analytics Dashboard", 
                                       img(src="logo.png", width = 60)), 
                          titleWidth=360)

## sidebar -----
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

## body -----
body <- dashboardBody(
  tabItems(
    ### u1 instructions --------------------------------------------------------
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
                  p("The lanalytics package contains some useful statistical analysis 
                   that can be used for online quizzes. This package can be installed with the following command: ",
                    code("install_github('savrgg/lanalytics')")),
                  br(),
                  p("For an introduction and examples, please visit the ", 
                    a("lanalytics homepage.", href = "https://savrgg.github.io/lanalytics/"))
                ),
                mainPanel(
                  h1("Instructions"),
                  p("The Learning Analytics dashboard consists of four tabs, the first two are
                     used to import data and display the datasets. The consecutive tabs are used 
                    for analysis and graphs given by the eRm and the lanalytics packages.
                    To get started follow these instructions:"), 
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
                  p("5) (Optionally) If you uploaded a cognitive level file, select below which column correspond to the item and
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
    ### u2 input --------------------------------------------------------
    tabItem(tabName = "2_input",
            fluidRow(
              tabBox(
                title = "Import quiz file", width = 6, 
                id = "2_tabset_1", height = "400px",
                tabPanel("csv file",
                         br(),
                         p("Select a *.csv file that contains two columns per item. The first indicating its ", em("score"), 
                           " and the second indicating its ",em("answering time"),". In addition, the file should contain an 
                           ID column called ",em("email address"),". This file can be exported from the Learning Catalytics software."),
                         br(),
                         fileInput('file1', 'Select file:',
                                   accept = c('.csv'),
                                   multiple = TRUE
                         ),
                         p("Now click ", strong("Add quiz dataset"), " to correctly upload one or more files 
                    or click ", strong("Remove quiz dataset"), " to delete them all."),
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
                         markdown("Select a *.csv file that contains the cognitive level of each item. This file must contain
                           two columns, one indicating the quiz and the item number in the format Q1_q3 
                          (Quiz 1, question 3) and the other indicating the cognitive level on a scale 
                           1-3 (1=low, 3=high). **Use the radio buttons below to select the correct column names**"),
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
                         ),
                                
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
                ),
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
            ),
            fluidRow(
              box(title = "Select Columns of Cognitive quiz", status = "primary", width = 12,
                  collapsible = FALSE,
                  uiOutput("choose_cognitive_item"),
                  uiOutput("choose_cognitive_rating")
              ),
            ),
            
    ),
    ### u3 display --------------------------------------------------------
    tabItem(tabName = "3_display", 
            fluidRow(
              br(),
              h1("Display of the imported datasets"),
              br(),
              box(title = "Quiz dataset:", status = "primary", width = 12,
                  collapsible = TRUE,
                  br(),
                  p("The quiz files that you imported are shown in ", 
                    strong('long format'), ". That means that each row represent one answer 
                    per item per quiz per student:"),
                  br(),
                  DT::dataTableOutput('quiz_dataset')
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
    ### u5 IRT-NO-discrim --------------------------------------------------------
    tabItem(tabName = "5_irt"),
    tabItem(tabName = "5_1_irt",
            fluidRow(
              br(),
              titlePanel(strong("Rasch model - Item Characteristic Curve")),
              p(),
              box(title = "Select quizzes to analyze:", status = "info", width = 12,
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
                  plotOutput("plot_jointICC"),
                  downloadButton("downloadPlot_rasch", 'Download Plot')
                  
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
              box(title = "Select quizzes to analyze:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_2")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personItem"),
                  downloadButton("downloadPlot_personItemRasch", 'Download Plot')
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
              box(title = "Select quizzes to analyze:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_5_3")
              )
            ),
            fluidRow(
              box(title = "Rasch model", status = "primary", width = 12,
                  collapsible = TRUE,
                  plotOutput("plot_personParameter"),
                  downloadButton("downloadPlot_personParameter", 'Download Plot')
                  
              )
            )),
    ### u6 analysis --------------------------------------------------------
    tabItem(tabName = "6_analysis"),
    tabItem(tabName = "6_1_individual",
            fluidRow(
              br(),
              h1("Analysis per group"),
              h4("In this tab you can see statistical summaries of the performance of your
                group."),
              br(),
              box(title = "Select quizzes to analyze:", status = "info", width = 12,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_1")
              )
            ),
            fluidRow(
              box(title = "Guessers plot", status = "primary", width = 12,
                  br(),
                  p("Sometimes when a student answers a question very quickly (or under a threshold time), the probability that the incorrect answer was chosen or just 
                    guessed the answer is high. In this plot, the questions that were answered in less than 20 seconds (or below an individual threshold) 
                    are shown. 1 means that the answer to the question was correct and -1 means that the answer to the question was incorrect. Questions that took more 
                    time than the threshold are not shown here."),
                  collapsible = TRUE,
                  plotOutput("plot_guessers"),
                  downloadButton("downloadPlot_guesser", 'Download Plot')
                  
              )
            ),
            fluidRow(
              box(title = "Order plot", status = "primary", width = 12,
                  br(),
                  p("This plot is useful to find the relationship between the time per question and the obtained correct answers. It is important to understand how long each question takes, in order to understand the design of the quizzes and exams. Some topics take more time, as such, more answering time can improve the resulting grade."),
                  collapsible = TRUE,
                  plotOutput("plot_order"),
                  downloadButton("downloadPlot_order", 'Download Plot')
                  
              ),
              box(title = "Download all group data", status = "primary", width = 12,
                  downloadButton("download_all_group_plots", "Download All Group Plots")
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
              box(title = "Select quizzes to analyze:", status = "info", width = 10,
                  collapsible = TRUE,
                  uiOutput("choose_files_6_2")
              )
            ),
            fluidRow(
              box(title = "Histogram", status = "primary", width = 12,
                  br(),
                  p("This is a histogram of the final grades in the selected quiz. Ideally, most of the students should be in the middle of the 
                    plot."),
                  collapsible = TRUE,
                  plotOutput("plot_hist"),
                  downloadButton("downloadPlot_hist", 'Download Plot')
              )
            ),
            fluidRow(
              box(title = "Boxplot", status = "primary", width = 12,
                  br(),
                  p("This is a boxplot of the grades of each quiz. This is a non-parametric graph that shows the distribution of the grades.
                    It is useful to easily see the dispersion, skewness, and outliers. The box contains 50% of the data (the data contained between the first 
                    and the third quartile). The bold line represents the median of the data. The whiskers are useful to see the variability above 
                    and below these quartiles."),
                  collapsible = TRUE,
                  plotOutput("plot_boxplots"),
                  downloadButton("downloadPlot_boxplot", 'Download Plot')
              )
            ),
            fluidRow(
              box(title = "Easiness-time (ET-plot)", status = "primary", width = 6,
                  br(),
                  p("The Easiness-time (ET) plot shows the relationship between the spent time on each question versus the average grade. 
                    Sometimes easy questions can be answered correctly in seconds, while difficult questions should take more time.
                    In this plot, we can visualize this relationship."),
                  collapsible = TRUE,
                  plotOutput("plot_et"),
                  downloadButton("downloadPlot_et", 'Download Plot')
                  
              ),
              box(title = "ETL", status = "primary", width = 6,
                  br(),
                  p("The Easiness-Time-Level plot is like the ET-plot, but taking into consideration the cognitive level of the questions.
                      Usually the questions with higher cognitive levels take more time and are more difficult, while the low cognitive level
                      questions are easier and may be answered in less time. In this plot, we can observe if this idea holds."),
                  collapsible = TRUE,
                  plotOutput("plot_etl"),
                  downloadButton("downloadPlot_etl", 'Download Plot')
                  
              ),
              box(title = "Download all data", status = "primary", width = 12,
                  downloadButton("download_all_quiz_plots", "Download All Plots")
              )
            )
    ),
    tabItem(tabName = "6_3_grupal",
            fluidRow(
              h1("Analysis per student."),
              h4("In this tab you can see statistical summaries of the performances per student"),
              box(title = "Quizzes to analyze:", status = "info", width = 6,
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
                  p("In this plot you can select a student and see their performance in all the quizzes. 
                    If the grade of the final exam has been provided, a red line will be shown in the plot."),
                  collapsible = TRUE,
                  plotOutput("plot_group_tot"),
                  downloadButton("downloadPlot_group_tot", 'Download Plot')
              ),
              
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
