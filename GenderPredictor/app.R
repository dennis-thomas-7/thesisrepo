

library(shiny)
library(shinythemes)


#Loading birth data and storing in a temp variable

birth_data <- readr::read_csv('~/VSA01.20220412164432.csv')
temp_birth_data<-birth_data

#Loading boy names data and storing in a temp variable

boys_names_data<-readr::read_csv('~/VSA50.20220412164235.csv')
temp_boys_data<-boys_names_data

#Loading girl names data and storing in a temp variable

girls_names_data<-readr::read_csv('~/VSA60.20220412164418.csv')
temp_girls_data<-girls_names_data

# Function for displaying probability based on Name

prob_name <- function(df_boys,df_girls,given_name){
    
    df_boys<-df_boys %>% filter(Names == given_name, STATISTIC == "VSA50C01") %>% drop_na(VALUE)
    p1<-sum(df_boys$VALUE)
    df_girls<-df_girls %>% filter(Names == given_name, STATISTIC == "VSA60C01") %>% drop_na(VALUE)
    p2<-sum(df_girls$VALUE)
    
    if(p1 != 0 && p2 != 0){
        
        prob_boy<- round(p1/(p1+p2),digits = 2)
        prob_girl<- round(p2/(p1+p2),digits = 2)
        results <- c(a = prob_boy, b = prob_girl)
        
    }else if(p1 == 0 && p2 != 0){
        
        results<- (paste("The given name has only entries in girls data suggesting", given_name, "is a girl's name."))
        
    }else if(p1 != 0 && p2 == 0){
        
        results<- (paste("The given name has only entries in boys data suggesting", given_name, "is a boy's name."))
        
    }else{
        
        results<- (paste(" No entries for",given_name,"found in boy's and girl's data. "))
        
        
    }
    
    return(results)  
}

# Function for displaying results

prob_results <- function(result){
    
    if(is.na(result["a"])){
        res<-result
    }else{
        boy_prob<-result["a"]
        girl_prob<-result["b"]
        res<-paste("Probability of boy name :", boy_prob ,"|| Probability of girl name :", girl_prob )
    }
    return(res)
}

# Function for displaying graph

prob_graph <- function(df_boys,df_girls,given_name){
    
    df_boys_graph<-df_boys %>% filter(Names == given_name, STATISTIC == "VSA50C01") %>% drop_na(VALUE)
    df_girls_graph<-df_girls %>% filter(Names == given_name, STATISTIC == "VSA60C01") %>% drop_na(VALUE)
    
    if(nrow(df_boys_graph)==0 || nrow(df_girls_graph)==0){
        
        print("No results")
        
    }else{
        
        res_graph<- ggplot() +
            # blue plot
            geom_point(data=df_boys_graph, aes(x=Year, y=VALUE,
                                               colour="darkblue"), fill="blue", size=1.5) +
            # pink plot
            geom_point(data=df_girls_graph, aes(x=Year, y=VALUE,
                                                colour="deeppink"), fill="deeppink", size=1.5) + 
            scale_x_continuous(breaks = scales::breaks_width(4)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5,size = 12))+
            scale_color_manual(labels =  c(paste("Boy's with the name:",given_name), paste("Girl's with the name:",given_name)), values = c("blue", "deeppink") )
    }
    
}

# Function for displaying probability based on Name and Year of Birth

prob_name_year <- function(df_boys,df_girls,given_name,year){
    
    df_boys<-df_boys %>% filter(Names == given_name, STATISTIC == "VSA50C01", Year == year) %>% drop_na(VALUE)
    p1_year<-sum(df_boys$VALUE)
    df_girls<-df_girls %>% filter(Names == given_name, STATISTIC == "VSA60C01", Year == year) %>% drop_na(VALUE)
    p2_year<-sum(df_girls$VALUE)
    
    if(p1_year != 0 && p2_year != 0){
        
        prob_boy_year<- round(p1_year/(p1_year+p2_year),digits = 2)
        prob_girl_year<- round(p2_year/(p1_year+p2_year),digits = 2)
        results_year <- c(a = prob_boy_year, b = prob_girl_year)
        
    }else if(p1_year == 0 && p2_year != 0){
        
        results_year <- (paste("The given name has only entries in girls data suggesting", given_name, "is a girl's name."))
        
    }else if(p1_year != 0 && p2_year == 0){
        
        results_year <- (paste("The given name has only entries in boys data suggesting", given_name, "is a boy's name."))
        
    }else{
        
        results_year <- (paste(" No entries for",given_name,"found in boy's and girl's data. "))
        
    }
    return(results_year)  
}

prob_year_results <- function(result){
    if(is.na(result["a"])){
        res<-result
    }else{
        boy_prob<-result["a"]
        girl_prob<-result["b"]
        res<-paste("Probability of boy name :", boy_prob ,"|| Probability of girl name :", girl_prob)
    }
    return(res)
}



linebreaks <- function(n){HTML(strrep(br(), n))}

firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

# UI for the application

ui<- fluidPage(theme=shinytheme("flatly"),navbarPage("Gender Predictor",
                                                     tabPanel("Name",
                                                              sidebarPanel("Gender based on name: ",linebreaks(2),
                                                                           textInput(inputId="nameId", label="Enter Name", value = "", width = NULL, placeholder = NULL),
                                                                           linebreaks(1),
                                                                           actionButton("submitBtn", label = "Submit")),
                                                              
                                                              mainPanel(h1(strong("Prediction of Names: "), align="center", style="font-size:30px;"),linebreaks(2),span(textOutput("result"), style="font-size:20px;", align="center"),linebreaks(2),span(plotOutput("plot_result"), style="font-size:20px;", align="center"))),
                                                     
                                                     
                                                     tabPanel("Name & Year Of Birth",
                                                              sidebarPanel("Gender based on name and year of birth: ",linebreaks(2),
                                                                           textInput(inputId="nameYearId", label="Enter Name", value = "", width = NULL, placeholder = NULL),
                                                                           textInput(inputId="yearId", label="Enter Year", value = "", width = NULL, placeholder = NULL),
                                                                           linebreaks(1),
                                                                           actionButton("submitYearBtn", label = "Submit")),
                                                              
                                                              mainPanel(h1(strong("Prediction of Names: "), align="center", style="font-size:30px;"),linebreaks(2),span(textOutput("resultyear"), style="font-size:20px;", align="center")))))

# Server for the application

server <- function(input, output){
    
    
    pasted_values <- 
        eventReactive(
            input$submitBtn,
            {
                input<- firstup(input$nameId)
                new_result<-prob_name(temp_boys_data,temp_girls_data,input)
                paste(prob_results(new_result))
                
            }
        )
    
    pasted_graph <- 
        eventReactive(
            input$submitBtn,
            {
                input<- firstup(input$nameId)
                new_graph<-prob_graph(temp_boys_data,temp_girls_data,input)
                new_graph
                
            }
        )
    
    pasted_year_values <- 
        eventReactive(
            input$submitYearBtn,
            {
                input_year_name<- firstup(input$nameYearId)
                year<- input$yearId
                new_year_result<-prob_name_year(temp_boys_data,temp_girls_data,input_year_name,year)
                paste(prob_year_results(new_year_result))
            }
        )
    
    output$result = renderText({  pasted_values() })
    
    output$plot_result = renderPlot({  pasted_graph() })
    
    output$resultyear = renderText({  pasted_year_values() })
    
}

# Run the application 

shinyApp(ui = ui, server = server)
