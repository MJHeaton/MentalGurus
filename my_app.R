###################
### Application ###
###################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(shiny)
source("score_function.R")



### Building App
# UI # 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Personality Classification"),
  
  # Inputs ----
  fluidRow(
    # Extraversion column ----
    column(3,h4("Extraversion"),
           numericInput(inputId = "ext1",
                        label = "EXT1: I am the life of the party.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext2",
                        label = "EXT2: I don't talk a lot.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext3",
                        label = "EXT3: I feel comfortable around people.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext4",
                        label = "EXT4: I keep in the background.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext5",
                        label = "EXT5: I start conversations.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext6",
                        label = "EXT6: I have little to say.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext7",
                        label = "EXT7: I talk to a lot of different people at parties.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext8",
                        label = "EXT8: I don't like to draw attention to myself.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext9",
                        label = "EXT9: I don't mind being the center of attention",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "ext10",
                        label = "EXT10: I am quiet around strangers.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px")
    ),
    
    # Neuroticism column ----
    column(3,h4("Neuroticism"), #style = "color: #0ACD0F",
           numericInput(inputId = "est1",
                        label = "EST1: I get stressed out easily.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est2",
                        label = "EST2: I am relaxed most of the time.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est3",
                        label = "EST3: I worry about things.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est4",
                        label = "EST4: I seldom feel blue.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est5",
                        label = "EST5: I am easily disturbed.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est6",
                        label = "EST6: I get upset easily.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est7",
                        label = "EST7: I change my mood a lot.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est8",
                        label = "EST8: I have frequent mood swings.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est9",
                        label = "EST9: I get irritated easily.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "est10",
                        label = "EST10: I often feel blue.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px")
    ),
    
    # Agreeableness column ----
    column(3,h4("Agreeableness"),
           numericInput(inputId = "agr1",
                        label = "AGR1: I feel little concern for others.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr2",
                        label = "AGR2: I am interested in people.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr3",
                        label = "AGR3: I insult people.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr4",
                        label = "AGR4: I sympathize with others' feelings.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr5",
                        label = "AGR5: I am not interested in other people's problems.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr6",
                        label = "AGR6: I have a soft heart.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr7",
                        label = "AGR7: I am not really interested in others.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr8",
                        label = "AGR8: I take time out for others.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr9",
                        label = "AGR9: I feel others' emotions.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "agr10",
                        label = "AGR10: I make people feel at ease.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px")
    ),
    
    # Conscientiousness column ----
    column(3,h4("Conscientiousness"),
           numericInput(inputId = "csn1",
                        label = "CSN1: I am always prepared.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn2",
                        label = "CSN2: I leave my belongings around.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn3",
                        label = "CSN3: I pay attention to details.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn4",
                        label = "CSN4: I make a mess of things.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn5",
                        label = "CSN5: I get chores done right away.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn6",
                        label = "CSN6: I often forget to put things back in their proper place.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn7",
                        label = "CSN7: I like order.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn8",
                        label = "CSN8: I shirk my duties.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn9",
                        label = "CSN9: I follow a schedule.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "csn10",
                        label = "CSN10: I am exacting in my work.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px")
    ),
    
    # Openness column ----
    column(3,h4("Openness"),
           numericInput(inputId = "opn1",
                        label = "OPN1: I have a rich vocabulary.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn2",
                        label = "OPN2: I have difficulty understanding abstract ideas.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn3",
                        label = "OPN3: I have a vivid imagination.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn4",
                        label = "OPN4: I am not interested in abstract ideas.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn5",
                        label = "OPN5: I have excellent ideas.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn6",
                        label = "OPN6: I do not have a good imagination.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn7",
                        label = "OPN7: I am quick to understand things.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn8",
                        label = "OPN8: I use difficult words.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn9",
                        label = "OPN9: I spend time reflecting on things.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px"),
           numericInput(inputId = "opn10",
                        label = "OPN10: I am full of ideas.",
                        min = 1,
                        max = 5,
                        value = 3,
                        step=1, width = "400px")
    ),
    # End fluidrow ----  
  ),
  
  actionButton("run", "Run", icon("paper-plane"), 
               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  actionButton("rand", "Random", icon("random"), 
               style="color: #fff; background-color: #0ACD0F; border-color: #09900C"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("Table", tableOutput(outputId = "probTable")),
      tabPanel("Plot", plotOutput(outputId = "probPlot")),
      tabPanel("Plot2", plotOutput(outputId = "compPers")),
      tabPanel("Scores", tableOutput(outputId = "compScores"))
    )
  )
  # mainPanel()
  # tableOutput(outputId = "probTable")
  # Output: Personality Probs ----
  # plotOutput(outputId = "clustprobs"),
  
  # Output: Personality Comparisons ----
  # plotOutput(outputId = "personalitycomp")
  
)


# SERVER #
server <- function(input, output, session) {
  # updatePage <- reactive({
  #   list(input$rand, input$run)
  # })
  observeEvent(input$rand,
               {
                 ##### Draw Random Observation from Data set #####
                 draw <- as.numeric(sumRes2[sample(1:nrow(sumRes2),1),])
                 
                 ##### Update Input Values #####
                 updateNumericInput(session, inputId = "ext1", value = draw[1])
                 updateNumericInput(session, inputId = "ext2", value = draw[2])
                 updateNumericInput(session, inputId = "ext3", value = draw[3])
                 updateNumericInput(session, inputId = "ext4", value = draw[4])
                 updateNumericInput(session, inputId = "ext5", value = draw[5])
                 updateNumericInput(session, inputId = "ext6", value = draw[6])
                 updateNumericInput(session, inputId = "ext7", value = draw[7])
                 updateNumericInput(session, inputId = "ext8", value = draw[8])
                 updateNumericInput(session, inputId = "ext9", value = draw[9])
                 updateNumericInput(session, inputId = "ext10", value = draw[10])
                 updateNumericInput(session, inputId = "est1", value = draw[11])
                 updateNumericInput(session, inputId = "est2", value = draw[12])
                 updateNumericInput(session, inputId = "est3", value = draw[13])
                 updateNumericInput(session, inputId = "est4", value = draw[14])
                 updateNumericInput(session, inputId = "est5", value = draw[15])
                 updateNumericInput(session, inputId = "est6", value = draw[16])
                 updateNumericInput(session, inputId = "est7", value = draw[17])
                 updateNumericInput(session, inputId = "est8", value = draw[18])
                 updateNumericInput(session, inputId = "est9", value = draw[19])
                 updateNumericInput(session, inputId = "est10", value = draw[20])
                 updateNumericInput(session, inputId = "agr1", value = draw[21])
                 updateNumericInput(session, inputId = "agr2", value = draw[22])
                 updateNumericInput(session, inputId = "agr3", value = draw[23])
                 updateNumericInput(session, inputId = "agr4", value = draw[24])
                 updateNumericInput(session, inputId = "agr5", value = draw[25])
                 updateNumericInput(session, inputId = "agr6", value = draw[26])
                 updateNumericInput(session, inputId = "agr7", value = draw[27])
                 updateNumericInput(session, inputId = "agr8", value = draw[28])
                 updateNumericInput(session, inputId = "agr9", value = draw[29])
                 updateNumericInput(session, inputId = "agr10", value = draw[30])
                 updateNumericInput(session, inputId = "csn1", value = draw[31])
                 updateNumericInput(session, inputId = "csn2", value = draw[32])
                 updateNumericInput(session, inputId = "csn3", value = draw[33])
                 updateNumericInput(session, inputId = "csn4", value = draw[34])
                 updateNumericInput(session, inputId = "csn5", value = draw[35])
                 updateNumericInput(session, inputId = "csn6", value = draw[36])
                 updateNumericInput(session, inputId = "csn7", value = draw[37])
                 updateNumericInput(session, inputId = "csn8", value = draw[38])
                 updateNumericInput(session, inputId = "csn9", value = draw[39])
                 updateNumericInput(session, inputId = "csn10", value = draw[40])
                 updateNumericInput(session, inputId = "opn1", value = draw[41])
                 updateNumericInput(session, inputId = "opn2", value = draw[42])
                 updateNumericInput(session, inputId = "opn3", value = draw[43])
                 updateNumericInput(session, inputId = "opn4", value = draw[44])
                 updateNumericInput(session, inputId = "opn5", value = draw[45])
                 updateNumericInput(session, inputId = "opn6", value = draw[46])
                 updateNumericInput(session, inputId = "opn7", value = draw[47])
                 updateNumericInput(session, inputId = "opn8", value = draw[48])
                 updateNumericInput(session, inputId = "opn9", value = draw[49])
                 updateNumericInput(session, inputId = "opn10", value = draw[50])
                 ##### Finish Updating Values #####
               })
  observe({
    input$rand
    input$run
    resp <- c(input$ext1,input$ext2,input$ext3,input$ext4,input$ext5,input$ext6,input$ext7,input$ext8,input$ext9,input$ext10,
              input$est1,input$est2,input$est3,input$est4,input$est5,input$est6,input$est7,input$est8,input$est9,input$est10,
              input$agr1,input$agr2,input$agr3,input$agr4,input$agr5,input$agr6,input$agr7,input$agr8,input$agr9,input$agr10,
              input$csn1,input$csn2,input$csn3,input$csn4,input$csn5,input$csn6,input$csn7,input$csn8,input$csn9,input$csn10,
              input$opn1,input$opn2,input$opn3,input$opn4,input$opn5,input$opn6,input$opn7,input$opn8,input$opn9,input$opn10)
    
    ##### Calculating Probabilities #####
    probs <- get_match(resp)
    probdf1 <- cbind("Personality" = 1:16, "Probability Match" = probs)
    probdf <- as.data.frame(cbind("Personality" = 1:16, "Group" = rep(1:4, each = 4), "Prob" = probs))
    prob_plot <- probdf %>% ggplot(mapping = aes(x = Personality, y = Prob, fill = as.factor(Group))) + geom_col() + 
      labs(y = "Probability Match", title = "Personality Match") +
      scale_x_continuous(breaks = 1:16, labels = 1:16) + 
      theme(legend.position = "None", plot.title = element_text(hjust = 0.5))
    
    ##### Composite Scores #####
    comp <- as.data.frame(cbind("Construct" = c("EXT", "EST", "AGR", "CSN", "OPN"), "Composite Score" = composite_scores(resp)))
    
    ##### Comparing Most Similar Personalitites #####
    temp.vec <- probs %>% sort(decreasing = TRUE) %>% cumsum() 
    select.index <- which(temp.vec > .9)[1]
    selected <- c(as.numeric(names(temp.vec)[1:select.index]))#,"Individual")
    ind.score <- as.data.frame(cbind("cluster" = 0,"group" = 0,"construct" = c("EXT", "EST", "AGR", "CSN", "OPN"),"value" = composite_scores(resp)))
    # selected <- 5:8
    compPersdf <- comp_centers %>% rbind(ind.score) %>% mutate(value = as.numeric(value))
    compPersdf$construct <- factor(compPersdf$construct, levels = c("EXT", "EST", "AGR", "CSN", "OPN"))
    compPersdf$cluster <- factor(compPersdf$cluster, levels = 0:16)
    compPers <-  compPersdf %>% filter(cluster %in% selected) %>% 
      ggplot(mapping = aes(x = construct, y = value, color = (cluster))) + 
      geom_line(aes(group = cluster), size = 1.25) + geom_point(size = 2) + 
      geom_line(data = compPersdf %>% filter(cluster == 0), aes(x = construct, y = value, group = group), size = 2, color = "black") + 
      geom_point(data = compPersdf %>% filter(cluster == 0), aes(x = construct, y = value),size = 3, color = "black") +
      labs(y = "Composite Score", x = "Construct", title = "Most Similar Personalities", color = "Personality") + 
      scale_y_continuous(limits = c(10,50)) + 
      theme( plot.title = element_text(hjust = 0.5))
    
    
    output$compScores <- renderTable(comp)
    output$compPers <- renderPlot(compPers)
    output$probTable <- renderTable(probdf1)
    output$probPlot <- renderPlot(prob_plot)
  })
  # dat <- eventReactive({input$run
  #                       input$rand},
  #                      {
  #   resp <- c(input$ext1,input$ext2,input$ext3,input$ext4,input$ext5,input$ext6,input$ext7,input$ext8,input$ext9,input$ext10,
  #             input$est1,input$est2,input$est3,input$est4,input$est5,input$est6,input$est7,input$est8,input$est9,input$est10,
  #             input$agr1,input$agr2,input$agr3,input$agr4,input$agr5,input$agr6,input$agr7,input$agr8,input$agr9,input$agr10,
  #             input$csn1,input$csn2,input$csn3,input$csn4,input$csn5,input$csn6,input$csn7,input$csn8,input$csn9,input$csn10,
  #             input$opn1,input$opn2,input$opn3,input$opn4,input$opn5,input$opn6,input$opn7,input$opn8,input$opn9,input$opn10)
  #   probs <- get_match(resp)
  #   cbind("Personality" = 1:16, "Probability Match" = probs)
  #                      })
  # dat2 <- eventReactive({input$run
  #                         input$rand},
  #                       {
  #                         resp <- c(input$ext1,input$ext2,input$ext3,input$ext4,input$ext5,input$ext6,input$ext7,input$ext8,input$ext9,input$ext10,
  #                                   input$est1,input$est2,input$est3,input$est4,input$est5,input$est6,input$est7,input$est8,input$est9,input$est10,
  #                                   input$agr1,input$agr2,input$agr3,input$agr4,input$agr5,input$agr6,input$agr7,input$agr8,input$agr9,input$agr10,
  #                                   input$csn1,input$csn2,input$csn3,input$csn4,input$csn5,input$csn6,input$csn7,input$csn8,input$csn9,input$csn10,
  #                                   input$opn1,input$opn2,input$opn3,input$opn4,input$opn5,input$opn6,input$opn7,input$opn8,input$opn9,input$opn10)
  #                         probs <- get_match(resp)
  #                         probdf <- as.data.frame(cbind("Personality" = 1:16, "Group" = rep(1:4, each = 4), "Prob" = probs))
  #                         probdf %>% ggplot(mapping = aes(x = Personality, y = Prob, fill = as.factor(Group))) + geom_col() + 
  #                           labs(y = "Probability Match", title = "Personality Match") +
  #                           scale_x_continuous(breaks = 1:16, labels = 1:16) + 
  #                           theme(legend.position = "None", plot.title = element_text(hjust = 0.5))  
  #                         # graph_match(get_match(resp))
  #                         
  #                       })
  # 
  # output$probTable <- renderTable({
  #   dat()
  # })
  # output$probPlot <- renderPlot({
  #   dat2()
  # })


}

### Running App
shinyApp(ui, server)























ggplot() + geom_line(data = ind.score, aes(x = construct, y = value, group = group), size = 1.25, color = "black") + geom_point(data = ind.score, aes(x = construct, y = value),size = 2) + 
  labs(y = "Composite Score", x = "Construct", title = "Most Similar Personalities") + 
  theme(legend.position = "None", plot.title = element_text(hjust = 0.5))







# scores3 <- rowSums(matrix(scores2, byrow = TRUE, nrow = 5))
# std_scores <- (scores3 - scaling_means)/scaling_sd
# probs <- c()
# for(i in 1:16) {
#   probs <- append(probs, dmvnorm(std_scores, mus[i,], cov.list[[i]]) * prob.weight[i])
# }
# final_probs <- setNames(probs / sum(probs),1:16)
