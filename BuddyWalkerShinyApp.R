# This is the version created on 8/28/2018
# This Version created by Katie Harding
# 
#Goals: Create an app that can track individual buddy walks among coworkers randomly assigned to teams.
# Group walks (more than 2 people) are assigned different point values

# Inputs:
# 1) employee list
#    I took a list of Muppets from https://en.wikipedia.org/wiki/List_of_Sesame_Street_Muppets
#    I edited it to remove some Muppets I was unfamiliar with, and ended up with 108 muppets

# 2) Team List.  
# for my purposes, I divided the group in to 10 teams (8 leftover people randomly assigned to teams)

# Outputs
# 1) list of submitted individual walks
# 2) list of group walks
# 3) each submission as a placeholder

#This is modified from this page.  It's an example that is a lot less complicated!
#https://stackoverflow.com/questions/27854248/r-shiny-how-to-write-a-csv-reactively-including-a-actionbutton
# 

# Steps: 1) divide the groups into teams, pick team colors
#        2) look for walks already taken place (I start each group with one walk)
#        3) graph those walks
#        4) provide drop-down menus for data needed to be collected
#        5) when submit button clicked, update graphs and update files 

#####################################################################

# First for working on the R drive
Saved.Data.Folder = "C:\\Documents\\R_Code\\ShinyWorkingFiles\\BuddyWalker"
setwd(Saved.Data.Folder)

#####################################################################
# Load the Libraries
library(shiny)
library(ggplot2)
library(pbapply)
#####################################################################


# Steps: 1) divide the groups into teams, pick team colors
# note teams will be randomly assigned each time this code is run, so only run once!!
MuppetData = read.csv("MuppetList.csv", header = TRUE, sep = ",") # get the file
MuppetVector = MuppetData$Muppet
RandomMuppetList = sample(MuppetVector, replace = FALSE) # randomly sample from the file
team_list = paste("Team", formatC(seq(1, 10), width = 2, flag = 0), sep = "_") # create the team list (1 to 100)
Team = c(rep(team_list,10), sample(team_list, 8)) # 8 extra team members added to team list
FinalData = data.frame(RandomMuppetList, Team) # make dataframe of list
head(FinalData)
FinalDataOrdered = FinalData[order(FinalData$Team, FinalData$RandomMuppetList),] # order the dataframe
head(FinalDataOrdered)
write.csv(FinalDataOrdered, "Muppet_Team_List.csv") # save file for distribution to teams

# colors picked for diversity and distinguishability for colorblind people
TwelveColors = c("purple3","mediumpurple1",  "turquoise2", "lightskyblue2",  
           "lightgoldenrod", "goldenrod2", "darkseagreen2", "seagreen3",
           "lightpink2", "hotpink4",  "red", "black")



ui = fluidPage(
  pageWithSidebar(
    headerPanel('Buddy Walk Reporting'),
    
    sidebarPanel(
      dateInput('date1',
                label = 'Walk Date: yyyy-mm-dd',
                value = Sys.Date()
      ),
      selectizeInput('TEAM', label = h5('Select Your Team'), 
                     choices = sort(team_list), 
                     options = list(
                       placeholder = 'Team...',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      br(),
      h4( p("Enter your walk buddy")),
      selectizeInput('Person1', label = h5('First Person'), 
                     choices = sort(MuppetVector),  
                     options = list(
                       create = TRUE,
                       placeholder = 'Select your name',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      selectizeInput('Person2', label = h5('Second Person'), 
                     choices = sort(MuppetVector),  
                     options = list(
                       create = TRUE,
                       placeholder = 'Select your walk buddy',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      br(),
      h4( p("OR")),
      selectizeInput('PeopleCount', label = h5('How many people (Group Walks)'), 
                     choices = sort(2:10),  
                     options = list(
                       create = TRUE,
                       placeholder = 'Number of people',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      br(),
      p("Enter your walk by pushing this button"),  
      actionButton("generateButton","Submit")), # end the sidebarPanel
    # Main panel for displaying outputs ----
    mainPanel(
      #h3(uiOutput("MainAction")),
      tabsetPanel(type = "tabs",
                  tabPanel("Graph", uiOutput("MainAction"),
                           plotOutput("TimePlot"),
                           plotOutput("GroupPlot"),
                           tableOutput("test1") # I don't know why this is needed, but it breaks when removed
                  ))
    )
  ))

server = function(input, output) {
  WalkerData = reactive({
    if(input$generateButton == 0){
      files = list.files(Saved.Data.Folder,recursive = TRUE, pattern = "*_SubmittedFile.csv")
      number1 = length(files)
      total.submitted.data = do.call("rbind", pblapply(files, function(x) read.csv(x)))
      suppressWarnings(total.submitted.data[is.na(total.submitted.data)] <- 2)
      # Separate out Group walks from Two Walks
      TwoWalks = total.submitted.data[total.submitted.data$PeopleCount <3, ]
      TwoWalksCopy = TwoWalks
      TwoWalksCopy = TwoWalksCopy[,c(1,3,2,4,5)]
      names(TwoWalksCopy) = c("Team", "Walker1", "Walker2", "DateCompleted", "PeopleCount")
      TwoWalks2 = rbind(TwoWalks, TwoWalksCopy)
      TwoWalks1 = unique(TwoWalks2) # remove duplicates
      TwoWalks1
      }
    isolate({ 
      input$generateButton
      files = list.files(Saved.Data.Folder,recursive = TRUE, pattern = "*_SubmittedFile.csv")
      number1 = length(files)
      total.submitted.data = do.call("rbind", pblapply(files, function(x) read.csv(x)))
      suppressWarnings(total.submitted.data[is.na(total.submitted.data)] <- 2)
      # Separate out Group walks from Two Walks
      TwoWalks = total.submitted.data[total.submitted.data$PeopleCount <3, ]
      TwoWalksCopy = TwoWalks
      TwoWalksCopy = TwoWalksCopy[,c(1,3,2,4,5)]
      names(TwoWalksCopy) = c("Team", "Walker1", "Walker2", "DateCompleted", "PeopleCount")
      TwoWalks2 = rbind(TwoWalks, TwoWalksCopy)
      TwoWalks1 = unique(TwoWalks2) # remove duplicates
      write.csv(TwoWalks1,"SubmittedResults.csv")
      TwoWalks1
    })
  })
  GroupData <- reactive({
    if(input$generateButton == 0){
      files = list.files(Saved.Data.Folder,recursive = TRUE, pattern = "*_SubmittedFile.csv")
      number1 = length(files)
      total.submitted.data = do.call("rbind", pblapply(files, function(x) read.csv(x)))
      suppressWarnings(total.submitted.data[is.na(total.submitted.data)] <- 2)
      # Separate out Group walks from Two Walks
      GroupWalks = total.submitted.data[total.submitted.data$PeopleCount >2, ]
      GroupWalks1 = unique(GroupWalks) # remove duplicates
      GroupWalks1
    }
    isolate({ 
      input$generateButton
      files = list.files(Saved.Data.Folder,recursive = TRUE, pattern = "*_SubmittedFile.csv")
      number1 = length(files)
      total.submitted.data = do.call("rbind", pblapply(files, function(x) read.csv(x)))
      suppressWarnings(total.submitted.data[is.na(total.submitted.data)] <- 2)
      # Separate out Group walks from Two Walks
      GroupWalks = total.submitted.data[total.submitted.data$PeopleCount >2, ]
      GroupWalks1 = unique(GroupWalks) # remove duplicates
      write.csv(GroupWalks1,"SubmittedGroups.csv")
      GroupWalks1
    })
  })
  WalkerInput <- reactive({
    if(input$generateButton == 0){return()}
    isolate({ 
      input$generateButton
      test_data = data.frame( "Team" = input$TEAM, "Walker1" = input$Person1, 
                              "Walker2" = input$Person2, "DateCompleted" = as.character(input$date1),
                              "PeopleCount" = input$PeopleCount )
      test_data
    })
    write.csv(test_data,paste(format(Sys.time(), "%b_%d_%Y_%H_%M_%S"),"SubmittedFile.csv", sep = "_"),row.names = FALSE)
  })
  # Do not remove this line, necessary to run
  output$test1 <- renderTable({WalkerInput()})
  
  output$TimePlot <- renderPlot({
    ggplot(WalkerData(), aes(x = Team,  fill = Team))+
      geom_bar(width = resolution(as.numeric(WalkerData()$Team)),
               colour = "black") +
      theme(text = element_text(size = 20),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_manual(values = TwelveColors)+
      coord_flip()+
      ggtitle("Total Buddy Walks per Team")
    
  })
  output$GroupPlot <- renderPlot({
    ggplot(GroupData(), aes(x = Team, y = PeopleCount, fill = Team))+
      geom_bar(width = resolution(GroupData()$PeopleCount),
               colour = "black", stat = "identity") +
      theme(text = element_text(size = 20),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_manual(values = TwelveColors)+
      coord_flip()+
      ggtitle("Total Group Walks per Team")
    
  })
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$generateButton == 0) 
      return(
        list(
          h3("Welcome to Buddy Walker!")
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$generateButton >0)  
      return(
        list(
          h3("I hope you enjoyed your walk!")
          
        )
      )    
  })
}
shinyApp(ui, server)
