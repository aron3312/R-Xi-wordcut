#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(jiebaR)
library(dplyr)
library(RWeka)
library(tidyr)
library(ggraph)
library(igraph)
library(networkD3)
library(ggplot2)
library(stringi)
setwd('D:/worddemo')
options(stringsAsFactors = F)
cutter = worker(dict = "jieba.dict.utf8", hmm = "hmm_model.utf8", user = "user.dict.utf8",
                idf = "idf.utf8", stop_word ="stop_words.utf8",type="tag")
# Define UI for application that draws a histogram
# article data and recommand data(collaborative filter)


b$content <- enc2native(b$content)
b$title <- enc2native(b$title)
ui <- bootstrapPage(
  tags$head(tags$script(src="action.js")),
  #tags$style("section {height: 100%}",
  #           "html, body {height: 100%}"
  #           ),
  includeCSS("styles.css"),
   titlePanel(h1("JiebaR Word Demo",align="center")),
  tags$div(id="banner",class="center chinesefont",tags$div(class="headline",h2("每日一習語錄"))),
  tags$div(id="banner2",class="center chinesefont",tags$div(class="headline")),
  #tags$section(id="section01",class="demo",
  #tags$h1("Scroll Down Button #1"),
  #tags$a(href="#section02","Scroll")
  #),
  #tags$section(id="section02",class="demo",
  #             tags$h1("Scroll Down Button #2"),
  #             tags$a(href="#section04","Scroll")
  #), 
  #tags$section(id="section03",class="demo",
  #             tags$h1("Scroll Down Button #3"),
  #             tags$a(href="#section04","Scroll")
  #),
  #tags$section(id="section04",class="demo",
  #             tags$h1("Scroll Down Button #3"),
  #             tags$a(href="#section04","Scroll")
  #),
  #tags$img(src="01.gif"),
  tags$div(id="body_content",class = "jumbotron",tags$div(class="container",tags$div(class="chinesefont",h2("每日一習語錄"),  uiOutput('art_all'),
                                 actionButton("action", label = "分析", class="btn btn-success"),
                                 navbarPage("分析結果",navbarMenu(
                                   "綜合分析",tabPanel("簡單描述",h3("總文字數："),h4(textOutput("x1")),h3("總斷詞數："),h4(textOutput("x2"))),tabPanel("兩字以下常見詞",
                                                                                                                                      tableOutput('table1')),
                                   tabPanel("兩字以上常見詞",
                                            tableOutput('table2'))
                                 ),tabPanel("n-gram網絡視覺化",
                                            forceNetworkOutput("force")),tabPanel("文字統計圖",plotOutput("wordfreq"))
                                 
                                 )
  ))
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table1 <- renderTable({
    input$action
    word_list <- cutter <= input$caption
    word_list <- word_list[grep('n',names(word_list))]
    word_list <- table(word_list)
    word_list <- word_list[nchar(names(word_list))<3]
    word_list <- word_list[order(word_list,decreasing = T)]
    word_list})
  output$table2 <- renderTable({
    input$action
    word_list <- cutter <= input$caption
    word_list <- word_list[grep('n',names(word_list))]
    word_list <- table(word_list)
    word_list <- word_list[nchar(names(word_list))>2]
    word_list <- word_list[order(word_list,decreasing = T)]
    word_list})

  output$x1<- renderText({
    input$action
    nchar(input$caption)
  })
  output$x2<- renderText({
    input$action
    length(unlist(cutter <= input$caption))
  })
  output$art_all <- renderUI({
    sample_num <- sample(1:nrow(b),1)
    recommand <- x_similarity[,colnames(x_similarity)==sample_num]
    names(recommand) <- rownames(x_similarity)
    recommand <- recommand[!recommand==1]
    re <- names(recommand)[order(recommand,decreasing = T)]
    re <- as.numeric(re)
    tagList(
      h3(sprintf("標題：%s",b$title[sample_num])),
      h3(sprintf("日期：%s",b$date[sample_num])),
      textAreaInput("caption", "文章(字)輸入區",b$content[sample_num], width = "1000px",height="600px"),
      h2("相似推薦："),
      tags$section(class="links",
      tags$nav(class="link-effect-13",id="link-effect-13",
      tags$li(tags$a(tags$span("data-hover"=b$title[re[1]],b$title[re[1]]),href=b$href[re[1]])),
      tags$li(tags$a(tags$span("data-hover"=b$title[re[2]],b$title[re[2]]),href=b$href[re[2]])),
      tags$li(tags$a(tags$span("data-hover"=b$title[re[3]],b$title[re[3]]),href=b$href[re[3]]))
      )
      )
      )  
  })

  
  # output$ngram <- renderPlot({
  #   word_list <- lapply(input$caption,function(x){tryCatch({cutter[x]}, error=function(err){})})
  #   count2 <- NGramTokenizer(sapply(word_list, function(x){tryCatch({paste(x, collapse = " ")}, error=function(err){})}), Weka_control(min =2, max =2))%>%
  #     table()%>%
  #     as.data.frame()
  #   colnames(count2)[1] <- 'word'
  #   count2$word <- as.character(count2$word)
  #   
  #   count2 <- count2[nchar(count2$word)>3,]
  #   
  #   
  #   bigrams_separated <- count2 %>%
  #     separate(word, c("word1", "word2"), sep = " ")
  #   
  #   
  #   bigram_graph <- bigrams_separated %>%
  #     filter(Freq > 0) %>%
  #     graph_from_data_frame()
  #   set.seed(2016)
  #   
  #   a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  #   ggraph(bigram_graph, layout = "fr") +
  #     geom_edge_link( aes(edge_alpha = abs(Freq)),show.legend = FALSE,
  #                     arrow = a, end_cap = circle(.07, 'inches')) +
  #     geom_node_point(color = "lightblue", size = 5) +
  #     geom_node_text(aes(label = name), vjust = 1, hjust = 1) 
  # })
  output$force <- renderForceNetwork({
    input$action
    word_list <- cutter <= input$caption
    word_list <- word_list[grep('n',names(word_list))]
    count2 <- NGramTokenizer(paste(word_list,collapse = " "), Weka_control(min =2, max =2))%>%
      table()%>%
      as.data.frame()
    colnames(count2)[1] <- 'word'
    count2$word <- as.character(count2$word)
    
    count2 <- count2[nchar(count2$word)>3,]
    
    
    bigrams_separated <- count2 %>%
      separate(word, c("word1", "word2"), sep = " ")
    
    simpleNetwork(bigrams_separated[,1:2],fontSize = 20,linkColour = "black", nodeColour = "red")
  })
  output$wordfreq <- renderPlot({
    input$action
    word_list <- data.frame(table(cutter <= input$caption))
    word_list <- word_list[order(word_list$Freq,decreasing = T),][1:20,]
    word_list$color <- NA
    word_list$color[which.max(word_list$Freq)] <- "red"
    word_list$color[is.na(word_list$color)] <- "grey"
    b <- ggplot(data = word_list, mapping = aes(x = Var1, y = Freq)) + geom_bar(stat = 'identity', position = 'dodge',fill=word_list$color)+ labs(title="詞頻率圖")  +xlab("詞")+ ylab("頻率")+coord_flip()
    b
    })
}
# Run the application 
shinyApp(ui = ui, server = server)


