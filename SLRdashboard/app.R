#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(rsconnect)
library(viridis)

DataSLR23 <- read_delim("DataSLR23.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
DataSLR23$title <-as.character(DataSLR23$title)
Encoding(DataSLR23$title) <- "UTF-8"

DataSLR23["numbergroup"][DataSLR23["numbergroup"] == "Nov/20"] <- "11-20"
DataSLR23["numbergroup"][DataSLR23["numbergroup"] == "01/Oct"] <- "1-10"
DataSLR23["age_group"][DataSLR23["age_group"] == "07/Sep"] <- "7-9"
DataSLR23["age_group"][DataSLR23["age_group"] == "10/Dec"] <- "10-12"
DataSLR23["age_bn"][DataSLR23["age_bn"] == "n"] <- "narrow"
DataSLR23$setting_recoded[DataSLR23["id"] == "42"] <- "lab"
DataSLR23["setting_recoded"][DataSLR23["setting_recoded"] == "therapy"] <- "therapy space"
DataSLR23$typical[grepl("utis", DataSLR23$typical)] <- "ASD"
DataSLR23$typical[grepl("speech", DataSLR23$typical)] <- "speech impairment"
DataSLR23$typical[grepl("language", DataSLR23$typical)] <- "speech impairment"
DataSLR23$typical[grepl("Language", DataSLR23$typical)] <- "speech impairment"
DataSLR23$typical[grepl("cerebral", DataSLR23$typical)] <- "speech impairment"
DataSLR23["venue_acc"][DataSLR23["venue_acc"] == "hci"] <- "HCI"

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "CAs & Children"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Quick Overview", tabName = "Overview", icon = icon("eye")),
      menuItem("Robots", tabName = "Robots", icon = icon("robot")),
      menuItem("Setting", tabName = "Setting", icon = icon("school")),
      menuItem("Robot-child relationship", tabName = "Footing", icon = icon("hands-holding-child")),
      menuItem("Autonomy", tabName = "Autonomy", icon = icon("hat-wizard")),
      menuItem("Speech", tabName = "Speech", icon = icon("comment")),
      menuItem("Community", tabName = "Community", icon = icon("people-group")),
      menuItem("Inclusivity", tabName = "Inclusivity", icon = icon("handshake-angle")),
      menuItem("Exploration", tabName = "Explore", icon = icon("chart-column")),
      menuItem("Data View", tabName = "Table", icon = icon("table-cells")),
      menuItem("Data Info", tabName = "Info", icon = icon("info")),
      menuItem("About the author", tabName = "About", icon = icon("user")),
      #dateRangeInput("YearSelect", "Year filter", start = "2000", end = "2023", min = "2000", max = "2023", format = "yyyy", startview = "year", separator = " - "),
      selectInput("TypeSelect", "Agent type filter", c("All CAs" = "All", "Robot" = "Robot", "Virtual Agent" = "Virtual Agent", "Voice Assistant" = "Voice Assistant", "Smart Speaker" = "Smart Speaker")),
      selectInput("GoalSelect", "Goal domain filter", c("All domains" = "All", "Education" = "Education", "Edutainment" = "Edutainment", "Entertainment" = "Entertainment", "Healthcare" = "Healthcare", "Generic" = "Generic", "Other" = "Other")),
      selectInput("RoleSelect", "Agent's role filter", c("All roles" = "All", "Higher" = "higher", "Equal" = "equal", "Lower" = "lower", "Unclear" = "unclear")),
      a(actionButton(inputId = "contact_admin", label = "Contact Admin", icon = icon("envelope")), href="mailto:p.c.velner@utwente.nl", target="_blank")
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Overview",
        fluidRow(
          infoBoxOutput("ibox_overview"),
        ),
        span("This dashboard shows the results of a systematic literature review on "), strong("conversational agents for children"), span("."), br(),
        span("It is advised to view this dashboard on a computer screen. Below you find some generic information about the dataset. On the left, you can click to the different topics in the sidebar to check out the results. In 'Exploration' you can also explore the data yourself by plotting your own barchart. You can also find some filters in the sidebar to specify your interest. Plots will change accordingly and at the top of every page is a counter that shows the number of papers currently plotted. The csv file containing the full dataset can be downloaded"), a("here", href="https://ellavelner.nl/slr_data", target="_blank"), span(". For an explanation of the annotations, see "), a("this document", href="https://docs.google.com/document/d/1MAHX8ivwKQDOCPc3QWJvCb3-T46NpzfK_MC1BR91qUc/edit?usp=sharing", target="_blank"), span("."), p(" "),
        span("If you have any questions or comments about this dashboard, feel free to reach out via "), a("email", href="mailto:p.c.velner@utwente.nl", target="_blank"), span("."), p(" "),
        fluidRow(
          box(plotOutput("type", height = 250)),

          box(plotOutput("domain", height = 250)),
        ),
        fluidRow(
          box(plotOutput("age", height = 250)),
          
          box(plotOutput("venue", height = 250))
        )
      ),

      # Second tab content
      tabItem(tabName = "Robots",
        fluidRow(
          infoBoxOutput("ibox_robots"),
        ),
        h2("Most research involved physical robots"),
        p("Researchers often do research with a physical robot. Rarely has research involved smart speakers or voice assistants, it seems like these CAs have only caught interest over the most recent years. The large numbers of robots in this dataset could be due to multiple reasons. One, researchers might prefer a physical embodied agent over other, more abstract, agents, since physical agents might be more relatable to children. Second, robots might require less technical skills for scientists to work with, since they often are readily available at their lab. Third, the databases we used (ACM and Scopus) might be biased toward robots."), 
        p("Smart speakers and voice assistants are breaking ground in the real world over the last few years, appearing more in homes, and being used on phones on a daily basis. These developments are picked up on by researchers in the last years."),
        p("Researchers mostly study agents in education, entertainment and some healthcare applications. When the agent is envisioned for education, this almost always is a robot. Entertainment and healthcare agents are also often such a physical entity, but also appear as a virtual agent every now and then. The focus on education and entertainment is an obvious choice with this target group, trying to teach children and create fun experiences for them."),
        fluidRow(
          box(plotOutput("typegoal", height = 250)),

          box(plotOutput("typeyear", height = 250))
        )
      ),
      # Third tab content
      tabItem(tabName = "Setting",
        fluidRow(
          infoBoxOutput("ibox_setting"),
        ),
        h2("Research was mostly conducted at schools or in the lab"),
        p("These agents were mostly set up in a lab or at a school to run the user study. A lab is often preferred by researchers to have control over the space, however, it lacks ecological validity. A school is often a convenient setting, since it is a natural hub of children. Note that this setting is not specified to whether this was done in a classroom or in a separate room, where the latter is similar to conducting the study in a lab. When we compare the setting to the goal domain, we would expect that educational agents and a school setting would highly overlap. However, the school is also used for other applications, which could be due to convenience sampling. Entertainment agents are often studied in the lab, but also have some studies in a public space, in an attempt to test the agent in a real-life setting with many unpredictabilities. When an agent is tested in a therapeutic setting, this was an agent that was intended for healthcare applications (e.g., for studying children with ASD, speech therapy, or diabetes interventions)."),
        fluidRow(
          box(plotOutput("setting", height = 250)),

          box(plotOutput("goalsetting", height = 250))
        )
      ),
      # Fourth tab content
      tabItem(tabName = "Footing",
        fluidRow(
          infoBoxOutput("ibox_footing"),
        ),
        h2("Conversational agent was often set on equal footing with the child"),
        p("During the interaction, the agent has a certain role it takes, compared to the child. For instance, the researchers can study an agent, that teaches the child a new language. In this case the robot would be set on a higher footing to the child. However, sometimes a researcher wants the child to think of the agent as a peer or companion. In that case, the agent is set on an equal footing to the child. Lastly, the agent can take the role of assistant, in which case the agent has a lower footing to the child. Researchers are mostly studying CAs that are on an equal footing with the child, presenting the agent as a companion for the child. This is especially popular in entertainment, where the agent is presented as a game buddy. A higher footing is mostly popular among educational agents, trying to teach the children something. However, there also seems to be a rise in agents presenting themselves as an assistant to the child. This could be due to the movement on children's rights, giving the children more autonomy in the interaction."),
        fluidRow(
          box(plotOutput("rolegoal", height = 250)),

          box(plotOutput("roleyear", height = 250)),
        ),
        p("Presenting the agent on a lower footing (e.g., toy, student) seems to be more popular among younger children, possibly because this group is more often entertained with agents than other age groups (with the exception of the broad age group, where the age of children differed more than three years)."),
        fluidRow(
          box(plotOutput("agerole", height = 250)),
          
          box(plotOutput("agedomain", height = 250))
        )
      ),
      # Fifth tab content
      tabItem(tabName = "Autonomy",
        fluidRow(
          infoBoxOutput("ibox_auton"),
        ),
        h2("The conversational agent is often wizarded"),
        p("Next, we look at how the spoken interactions in the user studies were achieved. First, half of the interactions were wizarded, meaning that at least the speech recognition was replaced by a human behind the scenes. Over the years, especially in the last four years, more agents were being deployed autonomously in research, reaching more realistic interactions."),
        fluidRow(
          box(plotOutput("auton", height = 250)),

          box(plotOutput("autonyear", height = 250)),
        ),
        h2("The child could often speak freely in the conversation"),
        p("On the other side of the interaction, the child could often speak freely, both with an autonomous as with a wizarded agent. Educational interactions most often allowed for free speech. Although interactions that were meant for entertainment also sometimes allowed for the child to speak freely, this is the domain that often opted for only allowing limited commands in the interaction, such as 'Go' and 'Turn left' in certain games. This takes away some of the processing needed for automatic speech recognition, since it only needs to understand these commands. However, note that half of the interactions that only allowed certain commands or phrases from the child to be recognized, were still wizarded. Therefore, the researchers might simply have opted for a simpler interactions."),
        fluidRow(
          box(plotOutput("dofauton", height = 250)),
          
          box(plotOutput("dofgoal", height = 250))
        )
      ),
      # Sixth tab content
      tabItem(tabName = "Speech",
        fluidRow(
          infoBoxOutput("ibox_speech"),
        ),
        h2("Speech modules were rarely tailored to children"),
        p("Looking at the speech modules that perform the automatic recognition and generation of speech in the agent, we see little explicit adaptation. Eight studies have explicitly reported an adapted or child-specific recognition module. Surprisingly, this adaptation often happens with broad age groups, while it might be easier to adapt to a narrower age group, since it is more targeted. The big pillar of wizarded interactions make the automatic recognition module often irrelevant. On the synthesis side, adaptation occurs slightly more often (18 studies), but still mostly with broad age groups. Note that often authors do not mention anything ('Not specified') about possibly adapting these modules, sometimes even completely omitting the speech modules."),
        fluidRow(
          box(plotOutput("asrage", height = 250)),

          box(plotOutput("ttsage", height = 250)),
        ),
        p("When the agent needs to interact with children who are developmentally somewhat atypical, the recognition module is either wizarded or not mentioned. This, when this module needs extra attention when there is such a developmental deficit. A wizarded interaction is likely chosen often to avoid having to deal with difficult adaptations. Looking at the generation, most often it is not mentioned whether the atypical children talk with a tailored agent."),
        fluidRow(
          box(plotOutput("asrtd", height = 250)),
          
          box(plotOutput("ttstd", height = 250))
        )
      ),
      # Seventh tab content
      tabItem(tabName = "Community",
        fluidRow(
          infoBoxOutput("ibox_venue"),
        ),
        h2("Little research from the speech community"),
        p("Most of the research comes from the HRI (human-robot interaction), CCI (child-computer interaction) and the more generic HCI (human-computer interaction) communities. Unfortunately, it seems the speech community is not that involved yet in studying conversational agents for children, according to this data. Since speech is such a big aspect of developing and researching conversational agents, it could be beneficial to reach out to this community more. The lack of speech expertise could also be a possible explanation for not adapting the speech modules to children."),
        plotOutput("venueyear", height = 250)
      ),
      # Eight tab content
      tabItem(tabName = "Inclusivity",
        fluidRow(
          infoBoxOutput("ibox_method"),
        ),
        h2("Small samples, broad age groups, and a western point of view"),
        p("We see some inclusivity issues in the research from the dataset. First, most research involves relatively small sample sizes, over 50% of the studies involving less than 40 participants. This is most likely due to the challenge of recruiting child participants, especially when working in the healthcare domain. Of course, when the research is more qualitative, one does not need a large sample size. But most of these studies want to find an effect that occurs with children talking with a conversational agent, which are typically quantitative studies. If one wants to be able to derive representative conclusions from their data, one needs a representative sample, and a small sample is less likely to be representative."),
        p("Second, the development of the children is often omitted. It could be that researchers find it unnecessary to report, because they don't want to discriminate, but it is not helpful to simply discard this information, when it might influence the interaction. For example, children with some form of autism often can thrive in regular schools, but still have different ways of interacting with others. Therefore, taking a sample out of a classroom and not taking these differences into account, might bury important and interesting findings."),
        fluidRow(
          box(plotOutput("number", height = 250)),
          
          box(plotOutput("typical", height = 250)),
        ),
        p("Third, children develop at a different pace. However, it is widely accepted that generally children follow somewhat the same path and can be classified in age groups of two to three years. These groups can be considered then on the same level in their development, and for instance go to the same year in school. In the data we see, though, that research often focuses on broad age groups, ranging more than three years. When zooming further, we see that quite often this age range is even more than five years. It makes the reader wonder how tailored the agent can be to the child, when they are in such different stages of their development."),
        fluidRow(
          box(plotOutput("agegroup", height = 250)),
          
          box(plotOutput("agerange", height = 250)),
        ),
        p("Fourth, the research mostly seems to come from the western part of the Global North (Europe and North America). This uncovers a wider known issue in academia, where the Global South is severely underrepresented due to inequality in opportunity. We need to put serious effort in finding ways to include South America and Africa in our community, since without these communities we will never have inclusive and representative data."),
        p("Lastly, agents interact with children in a certain language. Often, this language is not specified. This could be due to the fact that authors find this deducible from where the research is coming from, or they simply forgot to mention this. However, this poses several issues. One, it makes the research harder to reproduce. Second, since we are dealing with conversational agents, the language of the interaction is important to know, because this affects the speech modules of the agent. Speech modules of underrepresented languages might need more tweaking and adaptation to function properly in the study's context. If the language is English, Spanish or Mandarin, we can expect a near-perfect speech recognition module these days, since large companies that lead these developments, like Google and Amazon, focus on the most spoken languages in the world to reach the widest audience possible. If the language is not English, it is often a European language. This makes us wonder, how accessible are speech modules, and thus conversational agents, to communities that speak minority languages?"),
        fluidRow(
          box(plotOutput("origin", height = 250)),
          
          box(plotOutput("language", height = 250))
        )
      ),
      # Ninth tab content
      tabItem(tabName = "Explore",
        fluidRow(
          infoBoxOutput("ibox_explore"),
        ),
        h2("Build your own plot to explore the data"),
        p("This is your space. Explore the data by plotting your points of interest in a bar chart. If you want to only plot one variable, choose the same variable for the y-axis."),
        fluidRow(
          box(selectInput("explorex", "Choose variable for x-axis", choices=c(colnames(DataSLR23[, c("venue", "year", "country", "region", "type", "name", "domain", "role_hier", "setting_recoded", "age_group", "numbergroup", "typical", "research_purpose", "auton", "asr_child", "tts_child", "dof_recode", "language", "language_English")])), selected = "domain")),
          box(selectInput("explorey", "Choose a variable for y-axis", choices=c(colnames(DataSLR23[, c("venue", "year", "country", "region", "type", "name", "domain", "role_hier", "setting_recoded", "age_group", "numbergroup", "typical", "research_purpose", "auton", "asr_child", "tts_child", "dof_recode", "language", "language_English")])), selected = "type")),
          actionButton("make_plot", "See the plot"),
        ),
        fluidRow(
          box(plotOutput("explore", height = 250))
        )
      ),
      #Tenth tab content
      tabItem(tabName = "Table",
        h2("View the dataset"),
        p("Here you can view the data behind the plots. You can filter at the top of each column. (filters in the sidebar don't work for the table)"),
        p(""), 
        span("The csv file containing the full dataset can be downloaded"), a("here", href="https://ellavelner.nl/slr_data", target="_blank"), span("."),
        fluidRow(
          column(12,
          dataTableOutput("tableExplore")
          )
        )      
      ),
      #Eleventh tab content
      tabItem(tabName = "Info",
          h2("Some info about how we collected the data"),
              column(width=6,
                  fluidRow(
                        box(width=12,
                        span("In total, 468 papers were collected using the search strings below in the ACM Digital Library and the Scopus database. After screening and selection according to our selection criteria, a total of 81 papers has been annotated and included in this dashboard. For an explanation of the annotations, see "), a("this document", href="https://docs.google.com/document/d/1MAHX8ivwKQDOCPc3QWJvCb3-T46NpzfK_MC1BR91qUc/edit?usp=sharing", target="_blank"), span("."),
                        span("The csv file containing the full dataset can be downloaded"), a("here", href="https://ellavelner.nl/slr_data", target="_blank"), span(".")
                        )
                  ),
                  fluidRow(
                        box(width=12,
                          h4("ACM search string:"),
                          p(tags$p("[[Publication Title: child*] OR [Abstract: child*]] AND [[Abstract: 'spoken dialog system'] OR [Abstract: 'spoken dialogue system'] OR [Abstract: 'voice assistant'] OR [Abstract: robot*] OR [Abstract: agent]] AND [[Abstract: talk*] OR [Abstract: speech] OR [Abstract: speak*]] AND [Publication Date: (01/01/2001 TO 12/31/2023)]", style = "font-family: Courier New;"))
                          )
                  ),
                  fluidRow(
                        box(width=12,
                          h4("Scopus search string:"),
                          p(tags$p("TITLE-ABS ( child*  AND  ( spoken  AND  dialog*  AND  system )  OR  ( voice  AND  assistant )  OR  ( ( robot*  OR  agent )  AND  ( talk*  OR  speech  OR  speak* ) ) )  AND  PUBYEAR  >  2000  AND  PUBYEAR  <  2024 AND  SRCTITLE ( human*  OR  child*  OR  *speech*  OR  agent*  OR  robot*  OR  personalization  OR  dialogue )", style = "font-family: Courier New;"))
                        )
                  )
              ),
              column(width=6,
                  box(width=NULL,
                      imageOutput("inclusion", height= 800)
                  )
              )
        ),
      #Twelfth tab content
      tabItem(tabName = "About",
              h2("About the author"),
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  # CREATE FILTER
  
  output$inclusion <- renderImage({
      return(list(src = "inclusionall.png",contentType = "image/png",alt = "Inclusion process using the PRISMA framework", height=800))
  }, deleteFile = FALSE)
  
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }

  TestFilter <- reactive({
    req(input$TypeSelect)
    req(input$GoalSelect)
    req(input$RoleSelect)
    if (input$TypeSelect == "All" & input$GoalSelect == "All" & input$RoleSelect == "All")  {
      df <- DataSLR23
    }
    else if (input$TypeSelect != "All" & input$GoalSelect == "All" & input$RoleSelect == "All")  {
      df <- DataSLR23 %>% filter(type %in% input$TypeSelect)
    }
    else if (input$TypeSelect == "All" & input$GoalSelect != "All" & input$RoleSelect == "All")  {
      df <- DataSLR23 %>% filter(domain %in% input$GoalSelect)
    }
    else if (input$TypeSelect == "All" & input$GoalSelect == "All" & input$RoleSelect != "All")  {
      df <- DataSLR23 %>% filter(role_hier %in% input$RoleSelect)
    }
    else if (input$TypeSelect != "All" & input$GoalSelect != "All" & input$RoleSelect == "All")  {
      df <- DataSLR23 %>% filter(
        conditional(input$TypeSelect != "", type %in% input$TypeSelect),
        conditional(input$GoalSelect != "", domain %in% input$GoalSelect)
      )
    }
    else if (input$TypeSelect != "All" & input$GoalSelect == "All" & input$RoleSelect != "All")  {
      df <- DataSLR23 %>% filter(
        conditional(input$TypeSelect != "", type %in% input$TypeSelect),
        conditional(input$RoleSelect != "", role_hier %in% input$RoleSelect)
      )
    }
    else if (input$TypeSelect == "All" & input$GoalSelect != "All" & input$RoleSelect != "All")  {
      df <- DataSLR23 %>% filter(
        conditional(input$GoalSelect != "", domain %in% input$GoalSelect),
        conditional(input$RoleSelect != "", role_hier %in% input$RoleSelect)
      )
    }
    else {
      df <- DataSLR23 %>% filter(
        conditional(input$TypeSelect != "", type %in% input$TypeSelect),
        conditional(input$GoalSelect != "", domain %in% input$GoalSelect),
        conditional(input$RoleSelect != "", role_hier %in% input$RoleSelect)
      )
    }
  })
  
  TableExplore <- DataSLR23[, c("title", "venue", "year", "country", "region", "language", "type", "name", "domain", "role_hier", "setting_recoded", "age", "age_group", "numbergroup", "typical", "research_purpose", "auton", "asr_child", "tts_child", "dof_recode", "url")]

  output$ibox_overview <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_robots <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_setting <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_footing <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_auton <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_speech <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_venue <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_method <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  output$ibox_explore <- renderInfoBox({
    infoBox(color = "purple",
      "Number of papers plotted",
      nrow(TestFilter()), 
      icon = icon("database")
    )
  })
  
  v <- reactiveValues(plot = NULL)
 
  #change order of axis
  numberorder <- c('1-10', '11-20', '21-30', '31-40', '41-50', '51-60', '61-70', '71-80', '81-90', '91-100', '>100', 'Not specified')
  typeorder <- c('Robot', 'Virtual Agent', 'Voice Assistant', 'Smart Speaker')
  domainorder <- c('Education', 'Entertainment', 'Edutainment', 'Healthcare', 'Generic', 'Other')
  settingorder <- c('home', 'lab', 'online', 'public space', 'school', 'therapy space', 'other', 'Not specified')
  ageorder <- c('young', '7-9', '10-12', 'broad', 'Not specified')
  agebnorder <- c('narrow', 'broad', 'wide', 'Not specified')
  autonorder <- c('Fully autonomous', 'Wizarded', 'Combination', 'Not specified')
  asrorder <- c('Yes', 'Partially', 'No', 'Other', 'Wizarded', 'Not relevant', 'Not specified')
  typicalorder <- c('ASD', 'speech impairment', 'diabetes', 'TD', 'Not specified')
  ttsorder <- c('Yes', 'Partially', 'No', 'Not relevant', 'Not specified')
  venueorder <- c('CCI', 'robot/agent', 'HCI', 'speech', 'CUI', 'misc')
  regionorder <- c('North America', 'Europe', 'Central Asia', 'East Asia', 'Southeast Asia', 'Oceania', 'Combination')
  langorder <- c('Dutch', 'English', 'English, Spanish', 'French', 'German', 'Greek', 'Italian', 'Japanese', 'Kazakh, Russian', 'Korean', 'Mandarin', 'Spanish', 'Swedish', 'Not specified')
  langengorder <- c('English', 'Other', 'Not specified')
  
  #plots
  observeEvent(input$make_plot, {
    v$plot <- ggplot(TestFilter(), aes(x=.data[[input$explorex]],fill=.data[[input$explorey]])) + geom_bar() + scale_fill_viridis_d(option="H") + ggtitle(paste(input$explorex, " x ", input$explorey))
  },ignoreInit = FALSE)
  
  output$type <- renderPlot({
    ggplot(TestFilter(), aes(x = type, fill=type)) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Type of agent in the interaction") + xlab("Type")
  })
  output$domain <- renderPlot({
    ggplot(TestFilter(), aes(x = domain, fill=domain)) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Goal domain of the interaction") + xlab("Goal domain")
  })
  output$age <- renderPlot({
    ggplot(TestFilter(), aes(x = age_bn, fill=age_bn)) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Age range of children in the research") + xlab("Age range")
  })
  output$venue <- renderPlot({
    ggplot(TestFilter(), aes(x = venue_acc, fill=venue_acc)) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Venue where research was published") + xlab("Publication venue")
  })
  output$typegoal <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(type, level = typeorder), fill = factor(domain, domainorder))) + geom_bar(show.legend = TRUE) + ggtitle("Type of agent per goal domain") + scale_fill_viridis_d(option="H") + xlab("Type") + labs(fill = "Goal domain")
  })
  output$typeyear <- renderPlot({
    ggplot(TestFilter(), aes(x = year, fill=factor(type, level = typeorder))) + geom_bar( show.legend = TRUE) + ggtitle("Types of agents researched over the years") + scale_fill_viridis_d(option="H") + xlab("Year") + labs(fill="Type")
  })
  output$setting <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(setting_recoded, level=settingorder), fill=factor(setting_recoded, level = settingorder))) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Setting of the interaction") + xlab("Setting")
  })
  output$goalsetting <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(domain, domainorder), fill=factor(setting_recoded, level = settingorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Ecological validity of the setting") + xlab("Goal domain") + labs(fill="Setting")
  })
  output$rolegoal <- renderPlot({
    ggplot(TestFilter(), aes(x = role_hier, fill=factor(domain, domainorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Role of the agent per goal domain") + xlab("Role") + labs(fill="Goal domain")
  })
  output$roleyear <- renderPlot({
    ggplot(TestFilter(), aes(x = year, fill=role_hier)) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Role of the agent over the years") + xlab("Year") + labs(fill="Role")
  })
  output$agerole <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(age_group, level = ageorder), fill=role_hier)) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Role of the agent per age group") + xlab("Age group") + labs(fill="Role")
  })
  output$agedomain <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(age_group, level = ageorder), fill=factor(domain, domainorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Goal domain per age group") + xlab("Age group") + labs(fill="Goal domain")
  })
  output$auton <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(auton, level=autonorder), fill=factor(auton, level = autonorder))) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Autonomy of the agent during the interaction") + xlab("Autonomy")
  })
  output$autonyear <- renderPlot({
    ggplot(TestFilter(), aes(x = year, fill=factor(auton, level = autonorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Autonomy of the agent over the years") + xlab("Year") + labs(fill="Autonomy")
  })
  output$dofauton <- renderPlot({
    ggplot(TestFilter(), aes(x = dof_recode, fill=factor(auton, level = autonorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Degrees of freedom of the child vs. the autonomy of the agent") + xlab("Degrees of freedom") + labs(fill="Autonomy")
  })
  output$dofgoal <- renderPlot({
    ggplot(TestFilter(), aes(x = dof_recode, fill=factor(domain, domainorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Degrees of freedom of the child per goal domain") + xlab("Degrees of freedom") + labs(fill="Goal domain")
  })
  output$asrage <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(asr_child, asrorder), fill=factor(age_group, level = ageorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Recognition module adaptation for age groups") + xlab("Speech recognition") + labs(fill="Age group")
  })
  output$asrtd <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(asr_child, asrorder), fill=factor(typical, typicalorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Recognition module adaptation for developmental deficits") + xlab("Speech recognition") + labs(fill="Development of children")
  })
  output$ttsage <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(tts_child, ttsorder), fill=factor(age_group, ageorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Generation module adaptation for age groups") + xlab("Speech generation") + labs(fill="Age group")
  })
  output$ttstd <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(tts_child, ttsorder), fill=factor(typical, typicalorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Generation module adaptation for developmental deficits") + xlab("Speech generation") + labs(fill = "Development of children")
  })
  output$venueyear <- renderPlot({
    ggplot(TestFilter(), aes(x = year, fill=factor(venue_acc, venueorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Publication venues of the research over the years") + xlab("Year") + labs(fill = "Publication venue")
  })
  output$number <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(numbergroup, level = numberorder), fill=factor(domain, domainorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Number of child participants in the study") + xlab("Sample size") + labs(fill = "Goal domain")
  })
  output$agegroup <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(age_group, ageorder), fill=factor(age_group, ageorder))) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Age of child participants in the study") + xlab("Age group")
  })
  output$typical <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(typical, typicalorder), fill=factor(typical, typicalorder))) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Development of child participants in the study") + xlab("Development of children")
  })
  output$agerange <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(age_bn, agebnorder), fill=factor(typical, typicalorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Age range of child participants in the study", subtitle = "(wide > 5, broad > 3)") + xlab("Age range") + labs(fill = "Development of children")
  })
  output$origin <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(region, regionorder), fill=factor(language, langorder))) + geom_bar( show.legend = TRUE) + scale_fill_viridis_d(option="H") + ggtitle("Origin region of research") + xlab("Research region") + labs(fill = "Language")
  })
  output$language <- renderPlot({
    ggplot(TestFilter(), aes(x = factor(language_English, langengorder), fill=factor(language_English, langengorder))) + geom_bar( show.legend = FALSE) + scale_fill_viridis_d(option="H") + ggtitle("Language of the interaction") + xlab("Language")
  })
  output$tableExplore <- renderDT(
    TableExplore,
    filter = "top",
    options = list(
      sDom  = "ltipr",
      columns = list(
        list(title="id"),
        list(title="Title"),
        list(title="Venue"),
        list(title="Year"),
        list(title="Country"),
        list(title="Region"),
        list(title="Language"),
        list(title="CA type"),
        list(title="CA name"),
        list(title="Goal domain"),
        list(title="Role of agent"),
        list(title="Setting of study"),
        list(title="Age of children"),
        list(title="Age group"),
        list(title="Number of participants"),
        list(title="Development of children"),
        list(title="Purpose of research"),
        list(title="Autonomy of CA"),
        list(title="ASR adaptation"),
        list(title="TTS adaptation"),
        list(title="Degrees of freedom of child"),
        list(title="URL to paper")
      ),
    scrollX = TRUE))
  
  output$explore <- renderPlot({
    v$plot
  })
}

shinyApp(ui, server)
