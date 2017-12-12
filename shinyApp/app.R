

# Source all global variables, packages, functions, data and tables here
source('global.R')

## UI ####
ui <- navbarPage("",
                 # name of the navigation bar menu
                 navbarMenu("Global Virtual Water Flow",
                            # names of the pages under this navigation bar menu
                            tabPanel("Top and bottom countries",
                                     sidebarLayout(
                                       # set layout of the input parameters on the sidebar panel
                                       sidebarPanel(
                                         radioButtons("watermetric", "Select Metric:", c("Virtual water import"="import", "Virtual water export"="export",
                                                                                         "Net virtual water import" = "net")),
                                         radioButtons("countries", "Order of Countries:", c("Descending"="top", "Ascending"="bottom")),
                                         checkboxGroupInput("producttype", "Related Products:", choices = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial"),
                                                            selected = c("Crop"="crop", "Animal"="animal", "Industrial"="industrial")),
                                         checkboxGroupInput("watertype", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey"),
                                                            selected = c("Green"="green", "Blue"="blue", "Grey"="grey"))
                                       ),
                                       # set format of the main panel
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Top and bottom countries"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable")),
                                           tabPanel("Plot", br(), br(), plotlyOutput("topbottomPlot", height = "600px", width = "1000px"))
                                         )
                                       )
                                     )
                            ),
                            tabPanel("Global distribution",
                                     sidebarLayout(
                                       # set layout of the input parameters on the sidebar panel
                                       sidebarPanel(
                                         radioButtons("watermetric_distr", "Select Metric:",
                                                      c("Virtual water import"="import", "Virtual water export"="export",
                                                        "Net virtual water import" = "net")),
                                         radioButtons("watertype_dist", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey", "All" = "red"))
                                       ),
                                       # set format of the main panel
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Global distribution"),
                                                         style = "success",
                                                         ""),
                                         plotlyOutput("distributionPlot", height = "600px", width = "1100px")
                                       )
                                     )
                            ),
                            tabPanel("Ranking per country",
                                     sidebarLayout(
                                       # set layout of the input parameters on the sidebar panel
                                       sidebarPanel(
                                         textInput("country_input", "Enter a country name:", value = "USA")
                                       ),
                                       # set format of the main panel
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Ranking per country"),
                                                         style = "success",
                                                         ""),
                                         dataTableOutput("rankingPercentageTable")
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Per Capita Analysis",
                            tabPanel("Top and bottom countries",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("countries_percapita", "Order of Countries:", c("Descending"="top", "Ascending"="bottom")),
                                         checkboxGroupInput("producttype_percapita", "Related Products:", choices = c("Agricultural"="agricultural", "Industrial"="industrial", "Domestic"="domestic"),
                                                            selected = c("Agricultural"="agricultural", "Industrial"="industrial", "Domestic"="domestic")),
                                         checkboxGroupInput("watertype_percapita", "Water Types:", choices = c("Green"="green", "Blue"="blue", "Grey"="grey"),
                                                            selected = c("Green"="green", "Blue"="blue", "Grey"="grey"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Per Capita WF: top and bottom countries"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable_percapita")),
                                           tabPanel("Plot", br(), br(), plotlyOutput("topbottomPlot_percapita", height = "600px", width = "1100px")
                                           )
                                         )
                                       )
                                     )
                                     
                            ),
                            tabPanel("Global distribution",
                                     sidebarLayout(
                                       sidebarPanel(
                                         radioButtons("countries_percapita_change", "Order of Countries:", c("Descending"="top", "Ascending"="bottom"))
                                       ),
                                       mainPanel(
                                         bsCollapsePanel(title = div(icon("check-square-o"), "Per Capita WF: p"),
                                                         style = "success",
                                                         ""),
                                         tabsetPanel(
                                           tabPanel("Table", dataTableOutput("topbottomTable_percapita_change")),
                                           tabPanel("Plot", br(), br(), plotlyOutput("topbottomTable_percapita_change_plot", height = "600px", width = "1000px"))
                                         )
                                       )
                                     )
                            )
                 ),
                 tags$style(type = 'text/css',
                            '.navbar { font-family: Arial; font-size: 19px; }',
                            '.navbar-dropdown { font-family: Arial; font-size: 29px; }')
)



## Server ####
server <- function(input, output, session) {
  output$summary <- renderPrint({
    summary(cars)
  })
  
  # reference tables for parameter names
  VW_name_match <- data.frame(
    short = c("import", "export", "net"),
    name = c("Virtual water import", "Virtual water export", "Net virtual water import")
  )
  
  color_name_match <- data.frame(
    short = c("blue", "green", "grey", "red"),
    name = c("Blue only", "Green only", "Grey only", "All water types")
  )
  
  ## "Top and bottom countries" page under "Global Virtual Water Flow" tab
  # compute the underlying dataframe for data table and plotly object based on the user input parameters
  topbottom <- reactive({
    if(input$countries == "top"){
      df = (dfWaterFlow %>%
              filter(action %in% input$watermetric) %>%
              filter(product_type %in% input$producttype) %>%
              filter(water_type %in% input$watertype) %>%
              group_by(country) %>%
              summarize(water.footPrint = sum(amount)) %>%
              select(country, water.footPrint) %>%
              arrange(desc(water.footPrint)) %>%
              mutate(percentage = water.footPrint/sum(water.footPrint)) %>%
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = format(round(percentage*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = paste0(percentage, "%"))) %>%
        data.frame(stringsAsFactors = FALSE)
    } else {
      df = (dfWaterFlow %>%
              filter(action %in% input$watermetric) %>%
              filter(product_type %in% input$producttype) %>%
              filter(water_type %in% input$watertype) %>%
              group_by(country) %>%
              summarize(water.footPrint = sum(amount)) %>%
              select(country, water.footPrint) %>%
              arrange(water.footPrint) %>%
              mutate(percentage = water.footPrint/sum(water.footPrint)) %>%
              mutate(water.footPrint = format(as.numeric(water.footPrint), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = format(round(percentage*100, 2), nsmall = 2, big.mark=",", scientific = FALSE)) %>%
              mutate(percentage = paste0(percentage, "%"))) %>%
        data.frame(stringsAsFactors = FALSE)
    }
    names(df) = c("Country", "Virtual Water (Millions of m3)", "Percentage of World Total")
    return(df)
  })
  
  # set rendering format for the datatable
  output$topbottomTable <- renderDataTable({
    if(input$watermetric != "net"){
      datatable(topbottom(), options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20)))
    } else {
      datatable(topbottom()[, 1:2], options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20)))
    }
    
  })
  
  # prepare the plotly object based on the dataframe
  output$topbottomPlot <- renderPlotly({
    plotdata = topbottom()[1:100, ]
    plotdata$Country <- factor(plotdata$Country, levels = topbottom()[1:100, ]$Country)
    plotdata = plotdata %>%
      mutate(`Virtual Water (Millions of m3)` = as.numeric(gsub(",", "", `Virtual Water (Millions of m3)`))
      )
    plot_ly(plotdata, x = ~Country, y = ~`Virtual Water (Millions of m3)`, type = 'bar',
            hoverinfo = 'text',
            text = ~paste('Water:', prettyNum(`Virtual Water (Millions of m3)`,big.mark=",",scientific=FALSE), '<br>Country:', Country)
    ) %>%
      layout(yaxis = list(title = 'Virtual Water (Millions of m³)', range = c(0, max(plotdata[,2]))),
             margin= list(b = 150, r = 80, l = 100),
             title = paste0(VW_name_match[VW_name_match$short==input$watermetric,]$name, ', ', as.character(input$countries), ' 100 countries'),
             xaxis = list(title = 'Country', tickangle = 45)
      )
  })
  
  
  ## "Global Distribution" page under "Global Virtual Water Flow" tab
  # compute the underlying dataframe for plotly object based on the user input parameters
  distributionTable <- reactive({
    if(input$watermetric_distr == "net"){
      if(input$watertype_dist != "red"){
        df =  dfWaterFlow %>%
          filter(action %in% c('net')) %>%
          filter(water_type %in% input$watertype_dist) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }else{
        df =  dfWaterFlow %>%
          filter(action %in% c('net')) %>%
          filter(water_type %in% c('green', 'blue', 'grey')) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }
      
    } else {
      if(input$watertype_dist != "red"){
        df =  dfWaterFlow %>%
          filter(action == input$watermetric_distr) %>%
          filter(water_type %in% input$watertype_dist) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }else{
        df =  dfWaterFlow %>%
          filter(action == input$watermetric_distr) %>%
          filter(water_type %in% c('green', 'blue', 'grey')) %>%
          group_by(country) %>%
          summarize(water.footPrint = sum(amount))
      }
      
      
    }
    df = left_join(df, country_code, by = "country")
    df[df$country == "USA",]$code = "USA"
    return(df)
  })
  
  # prepare the plotly object based on the dataframe
  output$distributionPlot <- renderPlotly({
    plotdata = distributionTable()
    l <- list(color = toRGB("grey"), width = 0.5)
    names(plotdata) = c("Country", "Water", "Code")
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(plotdata) %>%
      add_trace(z = ~Water, color = ~Water,
                colors = paste0(tools::toTitleCase(input$watertype_dist), 's'),
                locations = ~Code, marker = list(line = l),
                hovertext = 'text',
                text = ~paste('Water:', prettyNum(Water,big.mark=",",digit=0,scientific=FALSE), '<br>Country:', Country)
      ) %>%
      colorbar(title = 'in Millions of m3') %>%
      layout(title = paste0(VW_name_match[VW_name_match$short==input$watermetric_distr,]$name, ', ',
                            color_name_match[color_name_match$short==input$watertype_dist,]$name),
             geo = g)
  })
  
  
  ## "Ranking per country" page under "Global Virtual Water Flow" tab
  # compute the underlying dataframe for datatable object based on the user input parameters
  rankingPercentage <- reactive({
    df =  dfWaterFlow %>%
      filter(action %in% c("import", "export")) %>%
      unite(temp1, action, product_type) %>%
      unite(temp, temp1, water_type) %>%
      spread(temp, amount) %>%
      mutate(total_import_green = import_crop_green + import_animal_green)  %>%
      mutate(total_import_blue = import_crop_blue + import_animal_blue + import_industrial_blue)  %>%
      mutate(total_import_grey = import_crop_grey + import_animal_grey + import_industrial_grey)  %>%
      mutate(total_export_green = export_crop_green + export_animal_green)  %>%
      mutate(total_export_blue = export_crop_blue + export_animal_blue + export_industrial_blue)  %>%
      mutate(total_export_grey = export_crop_grey + export_animal_grey + export_industrial_grey)  %>%
      mutate(net_green = total_import_green - total_export_green)  %>%
      mutate(net_blue = total_import_blue - total_export_blue)  %>%
      mutate(net_grey = total_import_grey - total_export_grey)  %>%
      mutate(percent_export_animal_blue = export_animal_blue/sum(export_animal_blue))  %>%
      mutate(percent_export_animal_green = export_animal_green/sum(export_animal_green))  %>%
      mutate(percent_export_animal_grey = export_animal_grey/sum(export_animal_grey))  %>%
      mutate(percent_export_crop_blue = export_crop_blue/sum(export_crop_blue))  %>%
      mutate(percent_export_crop_green = export_crop_green/sum(export_crop_green))  %>%
      mutate(percent_export_crop_grey = export_crop_grey/sum(export_crop_grey))  %>%
      mutate(percent_export_industrial_blue = export_industrial_blue/sum(export_industrial_blue))  %>%
      mutate(percent_export_industrial_grey = export_industrial_grey/sum(export_industrial_grey))  %>%
      mutate(percent_import_animal_blue = import_animal_blue/sum(import_animal_blue))  %>%
      mutate(percent_import_animal_green = import_animal_green/sum(import_animal_green))  %>%
      mutate(percent_import_animal_grey = import_animal_grey/sum(import_animal_grey))  %>%
      mutate(percent_import_crop_blue = import_crop_blue/sum(import_crop_blue))  %>%
      mutate(percent_import_crop_green = import_crop_green/sum(import_crop_green))  %>%
      mutate(percent_import_crop_grey = import_crop_grey/sum(import_crop_grey))  %>%
      mutate(percent_import_industrial_blue = import_industrial_blue/sum(import_industrial_blue))  %>%
      mutate(percent_import_industrial_grey = import_industrial_grey/sum(import_industrial_grey))  %>%
      mutate(percent_total_import_green = total_import_green/sum(total_import_green))  %>%
      mutate(percent_total_import_blue = total_import_blue/sum(total_import_blue))  %>%
      mutate(percent_total_import_grey = total_import_grey/sum(total_import_grey))  %>%
      mutate(percent_total_export_green = total_export_green/sum(total_export_green))  %>%
      mutate(percent_total_export_blue = total_export_blue/sum(total_export_blue))  %>%
      mutate(percent_total_export_grey = total_export_grey/sum(total_export_grey))  %>%
      mutate(percent_net_green = net_green/sum(net_green))  %>%
      mutate(percent_net_blue = net_blue/sum(net_blue))  %>%
      mutate(percent_net_grey = net_grey/sum(net_grey))  %>%
      arrange(desc(export_animal_blue))  %>%
      mutate(position_export_animal_blue = row_number())  %>%
      arrange(desc(export_animal_green))  %>%
      mutate(position_export_animal_green = row_number())  %>%
      arrange(desc(export_animal_grey))  %>%
      mutate(position_export_animal_grey = row_number())  %>%
      arrange(desc(export_crop_blue))  %>%
      mutate(position_export_crop_blue = row_number())  %>%
      arrange(desc(export_crop_green))  %>%
      mutate(position_export_crop_green = row_number())  %>%
      arrange(desc(export_crop_grey))  %>%
      mutate(position_export_crop_grey = row_number())  %>%
      arrange(desc(export_industrial_blue))  %>%
      mutate(position_export_industrial_blue = row_number())  %>%
      arrange(desc(export_industrial_grey))  %>%
      mutate(position_export_industrial_grey = row_number())  %>%
      arrange(desc(import_animal_blue))  %>%
      mutate(position_import_animal_blue = row_number())  %>%
      arrange(desc(import_animal_green))  %>%
      mutate(position_import_animal_green = row_number())  %>%
      arrange(desc(import_animal_grey))  %>%
      mutate(position_import_animal_grey = row_number())  %>%
      arrange(desc(import_crop_blue))  %>%
      mutate(position_import_crop_blue = row_number())  %>%
      arrange(desc(import_crop_green))  %>%
      mutate(position_import_crop_green = row_number())  %>%
      arrange(desc(import_crop_grey))  %>%
      mutate(position_import_crop_grey = row_number())  %>%
      arrange(desc(import_industrial_blue))  %>%
      mutate(position_import_industrial_blue = row_number())  %>%
      arrange(desc(import_industrial_grey))  %>%
      mutate(position_import_industrial_grey = row_number())  %>%
      arrange(desc(total_import_green))  %>%
      mutate(position_total_import_green = row_number())  %>%
      arrange(desc(total_import_blue))  %>%
      mutate(position_total_import_blue = row_number())  %>%
      arrange(desc(total_import_grey))  %>%
      mutate(position_total_import_grey = row_number())  %>%
      arrange(desc(total_export_green))  %>%
      mutate(position_total_export_green = row_number())  %>%
      arrange(desc(total_export_blue))  %>%
      mutate(position_total_export_blue = row_number())  %>%
      arrange(desc(total_export_grey))  %>%
      mutate(position_total_export_grey = row_number())  %>%
      arrange(desc(net_green))  %>%
      mutate(position_net_green = row_number())  %>%
      arrange(desc(net_blue))  %>%
      mutate(position_net_blue = row_number())  %>%
      arrange(desc(net_grey))  %>%
      mutate(position_net_grey = row_number()) %>%
      filter(country %in% input$country_input)
    
    final_df = data.frame(Metric = c("Export Animal Blue", "Export Animal Green", "Export Animal Grey", "Export Crop Blue", "Export Crop Green",
                                     "Export Crop Grey",  "Export Industrial Blue", "Export Industrial Grey", "Import Animal Blue",
                                     "Import Animal Green", "Import Animal Grey", "Import Crop Blue", "Import Crop Green", "Import Crop Grey",
                                     "Import Industrial Blue", "Import Industrial Grey", "Total Import Green", "Total Import Blue", "Total Import Grey",
                                     "Total Export Green", "Total Export Blue", "Total Export Grey", "Net Green", "Net Blue", "Net Grey"),
                          Amount = as.numeric(df[1, 2:26]),
                          Percentage= c(as.numeric(df[1, 27:48]), NA, NA, NA),
                          Ranking = as.numeric(df[1, 52:76]))
    final_df = final_df %>%
      mutate(Percentage = format(round(Percentage*100, 2), big.mark=",", scientific = FALSE)) %>%
      mutate(Amount = format(as.numeric(Amount), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
      mutate(Percentage = paste0(Percentage, "%"))
    final_df[final_df$Metric %in% c("Net Green", "Net Blue", "Net Grey"), ]$Percentage = "NA"
    names(final_df) = c("Metric", "Virtual Water (Millions of m3)", "Percentage of World Total", "World Ranking")
    
    return(final_df)
  })
  
  # set rendering format for the datatable object
  output$rankingPercentageTable <- renderDataTable({
    datatable(rankingPercentage(), options = list(
      pageLength = 25,
      lengthMenu = c(25, 50)))
  })
  
  
  
  
  
  ############################################################################################################################## 
  topbottom_percapita <- reactive({
    if(input$countries_percapita == "top"){
      df = (dfWFPC %>%
              filter(product_type %in% input$producttype_percapita) %>%
              filter(water_type %in% input$watertype_percapita) %>%
              group_by(country, population) %>%
              summarize(per_capita = sum(amount)) %>%
              select(country, population, per_capita) %>%
              arrange(desc(per_capita)) %>%
              mutate(population = format(as.numeric(population*1000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(per_capita = format(as.numeric(per_capita), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)))
    } else {
      df = (dfWFPC %>%
              filter(product_type %in% input$producttype_percapita) %>%
              filter(water_type %in% input$watertype_percapita) %>%
              group_by(country, population) %>%
              summarize(per_capita = sum(amount)) %>%
              select(country, population, per_capita) %>%
              arrange(per_capita) %>%
              mutate(population = format(as.numeric(population*1000), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)) %>%
              mutate(per_capita = format(as.numeric(per_capita), nsmall=0, digits = 0, big.mark=",", scientific = FALSE)))
    }
    return(df)
  })
  
  
  output$topbottomTable_percapita <- renderDataTable({
    df = topbottom_percapita()[, 1:3]
    names(df) = c("Country", "Population", "WF per capita (m3/cap)")
    datatable(df, options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20)))
  })
  
  # prepare the plotly object based on the dataframe
  output$topbottomPlot_percapita <- renderPlotly({
    plotdata = topbottom_percapita()[1:100, c(1, 3)]
    plotdata = plotdata %>%
      mutate(per_capita = as.numeric(gsub(",", "", per_capita))
      )
    plotdata$country <- factor(plotdata$country, levels = topbottom_percapita()[1:100, ]$country)
    
    plot_ly(plotdata, x = ~country, y = ~per_capita, type = 'bar',
            hoverinfo = 'text',
            text = ~paste('WF Per Capita:', prettyNum(per_capita,big.mark=",",scientific=FALSE), '<br>Country:', country)
    ) %>%
      layout(yaxis = list(title = 'WF Per Capita (m³/cap)', range = c(0, max(plotdata[,2]))),
             margin= list(b = 150, r = 80, l = 100),
             title = paste0('WF Per Capita', ', ', as.character(input$countries_percapita), ' 100 countries'),
             xaxis = list(title = 'Country', tickangle = 45)
      )
  })
}

## Run the application ####
shinyApp(ui = ui, server = server)