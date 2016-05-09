# for a multifile system with a global file use below line
#source('global.R', local = TRUE)

## reset .httr_oauth from current directory to force google login every refresh
# token <- gs_auth(new_user = TRUE)

## TODO Style Guide: ---------------------------------- ##
# Based on Hadley Wickham style guide in Advanced R
## variables are lower case with an underscore to seperate words
# roleList should be role_list; projectName ==> project_name
# files appear to have the same naming schema
## function names have initial capital letter and are written in CamelCase
# saveData() => SaveData(); loadData() => LoadData(); digestBoxes => DigestBoxes()
# use cmd-f to find and replace so you can change the function names
# and change their spelling where they are used at the same time


library(shiny)
library(shinydashboard)
library(googlesheets)
library(DT)
suppressMessages(library(dplyr))
library(shinyjs)

# only for apply()
library(plyr)

## Global

## Global Options
# commenting bc googlesheet documentation says to place this in the server.R file
# options("googlesheets.webapp.client_secret" = "9CxTmfVIljTHSmjz8IfjoOIx")
# options("googlesheets.webapp.redirect_uri" = "https://irjerad.shinyapps.io/Weekly-Status-Report/")
# options("googlesheets.webapp.client_id" = "575772486822-gfrlu7mocg3roq58rtgrsp8taq1tn0hd.apps.googleusercontent.com")
## Global Variables

# define roles
roleList <- list("Account Manager", "Project Manager",
                 "Technical Lead", "Quality Assurance")
# define project names
projectNames <- list("HMH", "LAC", "SVB", "Empower", "Ebay", "Geico", "Weekly Status Report")
## Global Functions

# name of google sheet being used
## Practice = 'practiceWSR' ; real one = 'weeklyStatusReportData'
table <- "weeklyStatusReportData"

# a function to append data to the bottom row of google sheet 'sheet'
saveData <- function(data) {
  # get google sheet
  sheet <- gs_title(table)
  # add new row
  gs_add_row(sheet, input = data)
}

# a function to load data from the google sheet 'sheet' and return a csv
loadData <- function() {
  # get google sheet
  sheet <- gs_title(table)
  # read the data
  gs_read_csv(sheet)
}

# removes leading zero from month and day digits in order to match stings with google sheets format
removeLeadZero <- function(x) {
  # remove any leading month zero
  y <- gsub("^0", "\\1", x)
  # remove any leading day zero; '/' needs to be replaced
  gsub("/0", "/", y)
  
  ## TODO:
  # Consider making no input and just using today's date
  # since this function should only be used on Sys.Date() input
}

# checks if inputDate is today; and thus should be used for digest
isToday <- function(inputDate) {
  removeLeadZero(format(Sys.Date(), "%m/%d/%Y")) == inputDate
}

# subset gsTBL to only todays values based on timeStamp value
today <- function(gsTBL) {
  # subset input gsTBL only returning today's inputs
  gsTBL[isToday(gsTBL$timeStamp),]
}

# selects a color based on the rating
ratingPic <- function(rating) {
  if (is.null(rating))
    return(NULL)
  
  if (rating == "Green") {
    return(list(
      src = "images/green.png",
      contentType = "image/png",
      alt = "Green"
    ))
  } else if (rating == "Yellow") {
    return(list(
      src = "images/yellow.png",
      filetype = "image/png",
      alt = "Yellow"
    ))
  } else if (rating == "Red") {
    return(list(
      src = "images/red.png",
      filetype = "image/png",
      alt = "Red"
    ))}
}

##=================== server.R / ui.R functions ==============## 
pasteDigest <- function(x) {
  paste(
    # paste desired digest info
    "<pre style=\"background-color:", x$rating,"\">",
    "<b>Project:</b>", x$projectName,
    "<b>Role:</b>", x$role,
    "<b>Rating:</b>", x$rating,
    "<b>One Line:</b>", x$oneLiner,
    "</br>",
    # link to image in www folder by <img src='fileName.jpg' />
    # paste0("<img src='half", x$rating, ".png' />"),
    "<b>Summary:</b>", x$summary,
    "</pre>",
    "<hr>", sep = " "
  )}


# rendering function for digest
digest <- function() {
  renderUI({
    # grab table from google sheets
    tbl <- loadData()
    # grab table of today's entries
    todayTBL <- today(tbl)
    
    # send todayTBL data frame and return an array with HTML text for team summaries
    markupArray <- daply(todayTBL, 1, function(x) pasteDigest(x))
    
    HTML(markupArray)
  })
}

## --- Digest boxes organized by team --- ##
teamBoxes <- function (aTodayList) {
  
  box(
    width = 12,
    title = aTodayList$projectName[[1]],
    HTML(
      if (sum(aTodayList$role == "Account Manager")) {
        AM <- aTodayList[aTodayList$role == "Account Manager", ]
        paste("<div class='col-sm-2'>",
              paste0("<img src='half", AM$rating, ".png' />"),
              "</div>", "<div class='col-sm-10'>",
              "<b>", AM$role, ":</b>", AM$user, "<br>",
              "<b>Overview:</b>", AM$oneLiner, "<br>", "</br>",
              "</div>"
        )
      } else {
        paste("<b style='color:red;'>The Account Manager of",
              aTodayList$projectName[[1]], "</b>",
              "<b>has not submitted their report yet</b>", "<br>"
        )
      },
      "<br>",
      if (sum(aTodayList$role == "Project Manager")) {
        PM <- aTodayList[aTodayList$role == "Project Manager", ]
        paste("<div class='col-sm-2'>",
              paste0("<img src='half", PM$rating, ".png' />"),
              "</div>", "<div class='col-sm-10'>",
              "<b>", PM$role, ":</b>", PM$user, "<br>",
              "<b>Overview:</b>", PM$oneLiner, "<br>", "</br>",
              "</div>"
        )
      } else {
        paste("<b style='color:red;'>The Project Manager of",
              aTodayList$projectName[[1]], "</b>",
              "<b>has not submitted their report yet</b>", "<br>"
        )
      },
      "<br>",
      if (sum(aTodayList$role == "Technical Lead")) {
        TL <- aTodayList[aTodayList$role == "Technical Lead", ]
        paste("<div class='col-sm-2'>",
              paste0("<img src='half", TL$rating, ".png' />"),
              "</div>", "<div class='col-sm-10'>",
              "<b>", TL$role, ":</b>", TL$user, "<br>",
              "<b>Overview:</b>", TL$oneLiner, "<br>", "</br>",
              "</div>"
        )
      } else {
        paste("<b style='color:red;'>The Technical Lead of",
              aTodayList$projectName[[1]], "</b>",
              "<b>has not submitted their report yet</b>", "<br>"
        )
      },
      "<br>",
      if (sum(aTodayList$role == "Quality Assurance")) {
        QA <- aTodayList[aTodayList$role == "Quality Assurance", ]
        paste("<div class='col-sm-2'>",
              paste0("<img src='half", QA$rating, ".png' />"),
              "</div>", "<div class='col-sm-10'>",
              "<b>", QA$role, ":</b>", QA$user, "<br>",
              "<b>Overview:</b>", QA$oneLiner, "<br>", "</br>",
              "</div>"
        )
      } else {
        paste("<b style='color:red;'>The Quality Assurance of",
              aTodayList$projectName[[1]], "</b>",
              "<b>has not submitted their report yet</b>", "<br>"
        )
      }
    )
  )
  
}

# takes google sheet input, extracts todays values, iterates through each project
weeklyView <- function() {
  renderUI({
    tbl <- loadData()
    # filter just todays values
    todayTBL <- today(tbl)
    # organize by project
    byProject <- lapply(projectNames, function(x){
      filter(todayTBL, projectName == x)
    })
    # remove empty values for each group
    grouped <- byProject[sapply(byProject, function(y) {dim(y)[1] > 0})]
    view <- lapply(grouped, function(x) teamBoxes(x))
    view
  })
}

header <- dashboardHeader(title = "Weekly Status Reports",
                          uiOutput("loginButton")
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Input Your Rating", tabName = "inputRating", icon = icon("edit")),
    menuItem("This Week", tabName = "thisWeek", icon = icon("group")),
    menuItem("Team Ratings", tabName = "teamRatings", icon = icon("group")),
    menuItem("Raw Data", tabName = "rawData", icon = icon("table"))
  )
)

body <- dashboardBody(
  # enable javascript in UI
  shinyjs::useShinyjs(),
  tabItems(
    tabItem("teamRatings",
            fluidRow(
              tabBox(
                id = "teamDigests", title = "Team Digests", width = 12,
                tabPanel(
                  title = "This Week",
                  uiOutput("weekly")
                ),
                tabPanel(
                  title = "Digests",
                  htmlOutput("gDigest")
                ),
                tabPanel(
                  title = "Weekly Status Report OverView", 
                  "These are the teams that have submited their reports",
                  div(
                    # Another way is to use the builder functions; see ?tags.
                    "Here is",
                    tags$span(      # Creates an HTML span.
                      class="foo",  # Any named args become attributes.
                      
                      tags$strong("another"),  # Unnamed args become children of the tag.
                      "way"
                    ),
                    "to do it."
                  )
                )
              )
            )
    ),
    tabItem("inputRating",
            # label form for reset functionality
            div(id = "form",
                fluidRow(
                  box(title = "Project Status", width = 12,
                      # User input for Project Name and Date
                      column(width = 4,
                             selectInput(inputId = "projectName", label = "Project Name:",
                                         choices = projectNames),
                             dateInput(inputId = "date", label = "Date:", format = "m-d-yyyy")
                      ),
                      # User input for Role and Rating color
                      column(width = 4,
                             selectInput(inputId = "role", label = "Your Role:", 
                                         choices = roleList
                             ),
                             selectInput(inputId = "rating", label = "Your Rating:", 
                                         choices = list("Green",
                                                        "Yellow",
                                                        "Red")
                             )
                      ),
                      # Render Color circle image for rating
                      column(width = 2, offset = 1,
                             imageOutput("ratingImg", height = "auto")
                      ),
                      fluidRow(
                        column(width = 12, offset = 0,
                               textInput(inputId = "oneLiner", width = "100%",
                                         label = "One Liner:",
                                         placeholder = "One line that says it all...")
                        )
                      )
                  )
                ),
                # Text input for project summary
                fluidRow(
                  box(title = "Project Summary", width = 12,
                      div(id = "summary1",
                          # use tag style width = 100% for mobile responsiveness
                          tags$style(type="text/css", "textarea {width:100%}"),
                          tags$textarea(id="summary", rows=8, cols="100%",
                                        placeholder = "This week in our project...")
                      ),
                      actionButton(inputId = "submit", label = "Submit")
                  )
                )
            )
    ),
    tabItem("rawData",
            # show practice weekly status report
            DT::dataTableOutput("WSRtbl")
    ),
    tabItem("thisWeek",
            # show this weeks submissions by project
            uiOutput("weekly2")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  ## pop up
  observe({
    shinyjs::info("Please be sure to authorize the App with your Google account before submitting this form")
  })
  ## setting googlesheet OAuth options
  options("googlesheets.webapp.client_secret" = "9CxTmfVIljTHSmjz8IfjoOIx")
  options("googlesheets.webapp.redirect_uri" = "https://irjerad.shinyapps.io/Weekly-Status-Report/")
  options("googlesheets.webapp.client_id" = "575772486822-gfrlu7mocg3roq58rtgrsp8taq1tn0hd.apps.googleusercontent.com")
  
  # get digest info from googlesheet
  output$gDigest <- digest()
  
  # get Weekly info
  output$weekly <- weeklyView()
  
  # make unique name for copy
  output$weekly2 <- weeklyView()
  
  # rating sends colored circle png based on rating response 
  output$ratingImg <- renderImage({
    # render rating color pic with global.R function
    ratingPic(input$rating)
  }, deleteFile = FALSE)
  
  # Display current State of sheet data
  output$WSRtbl <- renderDataTable({
    # grab table from google sheets
    tbl <- loadData()
    # render table as server output to be used in the ui
    tbl
  })
  
  # Add new row based on user input after pressing submit
  observeEvent(input$submit, {
    
    # cast inpust$date as character or all following input will look for date objects
    date <- as.character(input$date)
    data = c(date, input$projectName, input$role,input$rating,
             input$summary, input$oneLiner, gs_user()$displayName)
    # update google sheet
    saveData(data)
    
    # # grab table from google sheets
    tbl <- loadData()
    # update raw data view
    output$WSRtbl <- renderDataTable(tbl)
    
    # Rerender gDigest each time new input is created
    output$gDigest <- digest()
    ## Do the same for weekly and weekly2 view
    output$weekly <- weeklyView()
    output$weekly2 <- weeklyView()
    
  })
  
  ## reset form fields
  observeEvent(input$submit, {
    shinyjs::reset("form")
  })
  
  ## disable Submit button unless required fields are satisfied
  observe({
    toggleState("submit",
                !is.null(c(input$oneLiner, input$summary)) &&
                  input$oneLiner != "" &&
                  input$summary != "")
  })
  
  ## Get auth code from return URL
  access_token  <- reactive({
    ## gets all the parameters in the URL. The auth code should be one of them.
    pars <- parseQueryString(session$clientData$url_search)
    
    if(length(pars$code) > 0) {
      ## extract the authorization code
      gs_webapp_get_token(auth_code = pars$code)
    } else {
      NULL
    }
  })
  
  ## Make a button to link to Google auth screen
  ## If auth_code is returned then don't show login button
  output$loginButton <- renderUI({
    if(is.null(isolate(access_token()))) {
      actionButton("loginButton",
                   label = a("Authorize App",
                             href = gs_webapp_auth_url()))
    } else {
      return()
    }
  })
  
}

shinyApp(ui, server)