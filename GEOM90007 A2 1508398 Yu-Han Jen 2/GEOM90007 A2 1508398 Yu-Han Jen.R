library(shiny)
library(ggplot2)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(tidyr)
library(DT)
library(bslib)


# Load data
data <- read.csv("natural_disasters.csv", header = TRUE)

# Preprocess data, replace comma
colnames(data) <- tolower(gsub("\\.", " ", colnames(data)))

# Extract columns related to economic losses
damage_columns <- grep("total economic damages from", colnames(data), ignore.case = TRUE, value = TRUE)
economic_loss_columns <- c("country name", "year", damage_columns)
economic_loss_data <- data[, economic_loss_columns]

# get disaster
colnames(economic_loss_data) <- gsub("total economic damages from ", "", colnames(economic_loss_data))

# Convert data to new format
economic_loss_data_new <- economic_loss_data %>%
  pivot_longer(cols = -c(`country name`, year),
               names_to = "Disaster_Type",
               values_to = "Economic_Loss")

# No warning due to have already check, warning now is unrelated
options(warn = -1)

# Map country name to the leaflet
additional_name_corrections <- c(
  "Bolivia Plurinational State of" = "Bolivia",
  "Bosnia and Herzegovina" = "Bosnia and Herz.",
  "British Virgin Islands" = "British Virgin Is.",
  "Brunei Darussalam" = "Brunei",
  "Cabo Verde" = "Cape Verde",
  "Cote d'Ivoire" = "Ivory Coast",
  "Democratic Republic of Congo" = "Dem. Rep. Congo",
  "Dominican Republic" = "Dominican Rep.",
  "Iran Islamic Republic of" = "Iran",
  "Lao People's Democratic Republic" = "Laos",
  "Marshall Islands" = "Marshall Is.",
  "Micronesia (country)" = "Micronesia",
  "Republic of Korea" = "South Korea",
  "Russian Federation" = "Russia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "United States" = "United States of America",
  "Venezuela Bolivarian Republic of" = "Venezuela",
  "Viet Nam" = "Vietnam"
)
#add into it
economic_loss_data_new$`country name` <- recode(
  economic_loss_data_new$`country name`,
  !!!additional_name_corrections
)

# Remove non-country regions from the main dataset
regions_to_exclude <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania", "World",
                        "Lower-middle-income countries", "Low-income countries",
                        "Upper-middle-income countries", "High-income countries",
                        "European Union (27)")

economic_loss_data_new <- economic_loss_data_new %>%
  filter(!`country name` %in% regions_to_exclude)

# Additional preprocessing for the treemap
economic_loss_data_new_clean <- economic_loss_data_new %>%
  filter(!grepl("as a share of gdp", tolower(trimws(Disaster_Type))))

# Preprocessing for the Reconstruction Costs navbar
reconstruct_data <- data %>%
  select(`country name`, year, `reconstruction costs from disasters`)

# Rename columns for consistency
colnames(reconstruct_data) <- c("Country", "Year", "Reconstruction_Costs")

# Refer tp United Union website
state_mapping <- list(
  Africa = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
             "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", 
             "Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
             "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", 
             "Guinea-Bissau", "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", 
             "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", 
             "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", 
             "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", 
             "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", 
             "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"),
  
  Asia = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", 
           "Bhutan", "Brunei", "Cambodia", "China", "Georgia", "India", "Indonesia", 
           "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
           "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
           "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", 
           "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", 
           "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", 
           "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", 
           "Yemen"),
  
  Europe = c("Albania", "Andorra", "Austria", "Belarus", 
             "Belgium", "Bosnia and Herz.", "Bulgaria", "Croatia", "Cyprus", 
             "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", 
             "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
             "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", 
             "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
             "Romania", "Russia", "San Marino", "Serbia", "Slovakia", "Slovenia", 
             "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican City"),
  
  North_America = c(
    "Canada", "United States of America", "Belize", "Costa Rica", "El Salvador", "Guatemala", 
    "Honduras", "Mexico", "Nicaragua", "Panama", "Antigua and Barbuda", "Bahamas", 
    "Barbados", "Cuba", "Dominica", "Dominican Rep.", "Grenada", "Haiti", 
    "Jamaica", "St. Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines", 
    "Trinidad and Tobago"
  ),
  
  South_America = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                    "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"),
  
  Oceania = c("Australia", "Fiji", "Kiribati", "Marshall Is.", "Micronesia", 
              "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", 
              "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")
)

# Create a reverse mapping for easy lookup
country_to_continent <- data.frame(
  Country = unlist(state_mapping),
  Continent = rep(names(state_mapping), times = sapply(state_mapping, length)),
  stringsAsFactors = FALSE
)

# Merge with the main data
reconstruct_data <- merge(reconstruct_data, country_to_continent, by = "Country", all.x = TRUE)

# Aggregate data by continent for reconstruction costs
continent_aggregated <- reconstruct_data %>%
  group_by(Continent, Year) %>%
  summarise(total_reconstruction_costs = sum(Reconstruction_Costs, na.rm = TRUE), .groups = 'drop')

final_data <- bind_rows(
  continent_aggregated,
  reconstruct_data %>%
    filter(Country %in% c("Africa", "Asia", "Europe", "Oceania")) %>%
    select(Continent = Country, Year, total_reconstruction_costs = Reconstruction_Costs)
) %>%
  filter(!is.na(total_reconstruction_costs))

# Exclude unwanted area, due to some is not country
entries_to_exclude <- c("World", "Asia", "Lower-middle-income countries", "Low-income countries",
                        "North America", "Upper-middle-income countries", "High-income countries",
                        "European Union (27)", "Europe", "South America")

# Load world map with geometry for visualization
world <- ne_countries(scale = "medium", returnclass = "sf")

#Ui part
ui <- navbarPage(
  title = "Natural Disasters Economic Dashboard",
  theme = bs_theme(
    bg = "black",
    fg = "white",
    primary = "#66B2FF",
    base_font = font_google("Roboto"),
    code_font = font_google("Fira Code")
  ),
  header = tagList(
    #HTML part
    tags$head(
      tags$style(HTML("
        h1, h2, h3, h4, h5, h6, .navbar-brand, .nav-link {
          color: #66B2FF;
          font-size: 24px;
          font-weight: bold;
        }
        .navbar .navbar-nav > li > a:hover, .navbar .navbar-nav > li > a:focus {
          background-color: #66B2FF;
          color: white;
        }
      "))
    )
  ),
  
  # Economic Loss Map 
  tabPanel(
    "Economic Loss Map",
    fluidPage(
      fluidRow(
        column(12, 
               div(style = "font-size: 24px; font-weight: bold; color: #66B2FF;", 
                   uiOutput("totalLoss"), 
                   align = "center")
        )
      ),
      fluidRow(
        column(7,
               leafletOutput("economicLossMap", width = "100%", height = "80vh")
        ),
        column(5,
               plotlyOutput("line_chart", width = "100%", height = "80vh")
        )
      ),
      absolutePanel(
        top = 100, left = 50, draggable = TRUE,
        selectInput("disasterType", "Select Disaster Type:",
                    choices = c(
                      "All Disaster", 
                      "drought", 
                      "earthquakes", 
                      "disasters", 
                      "volcanic activity", 
                      "floods", 
                      "mass movements", 
                      "storms", 
                      "wildfires", 
                      "extreme temperatures"
                    )),
        style = "background-color: rgba(51, 51, 51, 0.8); padding: 10px; border-radius: 8px; color: #66B2FF;"
      ),
      absolutePanel(
        top = 180, left = 50, draggable = TRUE,
        actionButton("clear_selection", "Clear Selection", class = "btn-primary"),
        style = "background-color: rgba(51, 51, 51, 0.8); padding: 10px; border-radius: 8px; color: black;"
      )
    )
  ),
  
  # Second navbar: Reconstruction Costs by Continent, use lollipop char to have inovation design
  tabPanel(
    "Reconstruction Costs",
    sidebarLayout(
      sidebarPanel(
        style = "background-color: #222222;",
        selectInput("year", "Select Year:", 
                    choices = sort(unique(final_data$Year)), 
                    selected = max(final_data$Year))
      ),
      mainPanel(
        fluidRow(
          column(12,
                 plotlyOutput("lollipop_chart", height = "50vh")
          )
        ),
        fluidRow(
          column(12,
                 DTOutput("country_table")
          )
        )
      )
    )
  ),
  
  # Third navbar: Economic Loss Distribution (Treemap)
  tabPanel(
    "Economic Loss Distribution",
    fluidPage(
      fluidRow(
        column(9,  # Treemap taking 75% of the screen
               plotlyOutput("economicLossTreemap", height = "70vh", width = "100%")
        ),
        column(3,  # Sidebar on the right
               sidebarPanel(
                 selectInput("treemap_disasterType", "Select Disaster Type:",
                             choices = c("drought", "earthquakes", "floods", "storms", 
                                         "extreme temperatures", "landslides", "volcanic activity", "wildfires")),
                 sliderTextInput(
                   "yearRange", 
                   "Select Year:",
                   choices = sort(unique(economic_loss_data_new_clean$year)),  # Use all available years
                   selected = max(economic_loss_data_new_clean$year), 
                   animate = TRUE
                 ),
                 width = 12
               )
        )
      )
    )
  ),
  
  # Additional navbars under 'More' navbarMenu
  navbarMenu("More",
             tabPanel("Data", 
                      fluidRow(
                        column(12,
                               h2("Data", style = "text-align:center; color: #66B2FF;"),
                               br(),
                               p("The natural disaster data covers events across the world from 1900 to 2024.", style = "font-size: 18px; color: #66B2FF;"),
                               br(),
                               tags$a(href = "https://www.emdat.be", "Download the natural disaster data", style = "color: #66B2FF; font-size: 16px;")
                        )
                      )
             )
  )
)

# Server part
server <- function(input, output, session) {
  # -------- Server logic for the Economic Loss Map tab -------------
  selected_countries <- reactiveValues(countries = list())
  
  # Function to calculate total loss based on the selected disaster type
  total_loss_value <- reactive({
    if (input$disasterType == "All Disaster") {
      sum(economic_loss_data_new$Economic_Loss, na.rm = TRUE)
    } else {
      sum(economic_loss_data_new %>%
            filter(Disaster_Type == input$disasterType) %>%
            pull(Economic_Loss), na.rm = TRUE)
    }
  })
  
  # Display the total loss
  output$totalLoss <- renderUI({
    if (input$disasterType == "All Disaster") {
      loss <- sum(economic_loss_data_new$Economic_Loss, na.rm = TRUE)
      disaster_type <- "Global"
    } else {
      loss <- sum(economic_loss_data_new$Economic_Loss[economic_loss_data_new$Disaster_Type == input$disasterType], na.rm = TRUE)
      disaster_type <- tools::toTitleCase(input$disasterType)
    }
    
    formatted_loss <- paste0(round(loss / 1e9, 1), " Billion $")
    
    HTML(paste("<h3 style='color:#66B2FF;'>Total ", disaster_type, " Economic Loss: ", formatted_loss, "</h3>"))
  })
  
  output$economicLossMap <- renderLeaflet({
    if (input$disasterType == "All Disaster") {
      filtered_data <- economic_loss_data_new
    } else {
      filtered_data <- economic_loss_data_new %>%
        filter(Disaster_Type == input$disasterType)
    }
    
    # Group by country and summarize total economic loss
    filtered_data <- filtered_data %>%
      group_by(`country name`) %>%
      summarise(Total_Economic_Loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
    
    # Calculate global total proportion
    global_total <- sum(filtered_data$Total_Economic_Loss, na.rm = TRUE)
    
    # Calculate proportion each country
    filtered_data <- filtered_data %>%
      mutate(Proportion = (Total_Economic_Loss / global_total) * 100)
    
    # Join with world map data
    world_data <- world %>%
      left_join(filtered_data, by = c("name" = "country name"))
    
    # Remove rows where Total_Economic_Loss is NA
    world_data <- world_data %>%
      filter(!is.na(Total_Economic_Loss) & is.finite(Total_Economic_Loss))
    
    # set bins based on the economic loss values
    bins <- c(0, 1e5, 1e6, 1e7, 5e7, 1e8, 5e8, 1e9, max(world_data$Total_Economic_Loss, na.rm = TRUE))
    
    # Define the color palette
    pal <- colorBin(palette = "Blues", domain = world_data$Total_Economic_Loss, bins = bins, na.color = "gray")
    
    leaflet(world_data) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%  
      addPolygons(
        fillColor = ~pal(Total_Economic_Loss),
        weight = 1,
        color = "white",  
        fillOpacity = 0.7,
        layerId = ~name,
        label = ~paste(
          name, ": $",
          ifelse(Total_Economic_Loss >= 1e9,
                 paste0(round(Total_Economic_Loss / 1e9, 2), " Billion"),
                 ifelse(Total_Economic_Loss >= 1e6,
                        paste0(round(Total_Economic_Loss / 1e6, 2), " Million"),
                        paste0(round(Total_Economic_Loss, 2), " USD")))
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#000000"), 
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = ~Total_Economic_Loss, 
        opacity = 1,
        title = "Total Loss in $", 
        position = "bottomright",
        labFormat = function(type, cuts, p) {
          labels <- sapply(cuts, function(cut) {
            if (cut >= 1e9) {
              paste0(round(cut / 1e9, 1), " Billion ")
            } else if (cut >= 1e6) {
              paste0(round(cut / 1e6, 1), " Million")
            } else if (cut >= 1e3) {
              paste0(round(cut / 1e3, 1), " Thousand")
            } else {
              paste0(cut, " USD")
            }
          })
          return(labels)
        }
      )
  })
  
  observeEvent(input$economicLossMap_shape_click, {
    clicked_country <- input$economicLossMap_shape_click$id
    if (!clicked_country %in% selected_countries$countries) {
      selected_countries$countries <- c(selected_countries$countries, clicked_country)
    }
  })
  
  observeEvent(input$clear_selection, {
    selected_countries$countries <- list()
  })
  
  # line_chart renderPlotly include selected countries
  output$line_chart <- renderPlotly({
    if (length(selected_countries$countries) == 0) {
      # No countries selected
      if (input$disasterType == "All Disaster") {
        data_for_chart <- economic_loss_data_new %>%
          group_by(year) %>%
          summarise(total_loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
        title <- "Total Economic Loss Over Time"
        p <- plot_ly(data_for_chart, x = ~year, y = ~total_loss, type = 'scatter', mode = 'lines+markers') %>%
          layout(title = list(text = title, font = list(color = "#66B2FF"),size=32),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Economic Loss (USD)"),
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 font = list(color = '#66B2FF'))
      } else {
        data_for_chart <- economic_loss_data_new %>%
          filter(Disaster_Type == input$disasterType) %>%
          group_by(year) %>%
          summarise(total_loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
        title <- paste("Total", tools::toTitleCase(input$disasterType), "Economic Loss Over Time")
        p <- plot_ly(data_for_chart, x = ~year, y = ~total_loss, type = 'scatter', mode = 'lines+markers') %>%
          layout(title = title,
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Economic Loss (USD)"),
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 font = list(color = '#66B2FF'))
      }
    } else {
      # Countries selected
      if (input$disasterType == "All Disaster") {
        data_for_chart <- economic_loss_data_new %>%
          filter(`country name` %in% selected_countries$countries) %>%
          group_by(year, `country name`) %>%
          summarise(total_loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
        title <- "Economic Loss Over Time for Selected Countries"
      } else {
        data_for_chart <- economic_loss_data_new %>%
          filter(Disaster_Type == input$disasterType,
                 `country name` %in% selected_countries$countries) %>%
          group_by(year, `country name`) %>%
          summarise(total_loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
        title <- paste(tools::toTitleCase(input$disasterType), "Economic Loss Over Time for Selected Countries")
      }
      # Ensure 'year' is numeric
      data_for_chart$year <- as.numeric(as.character(data_for_chart$year))
      # Arrange data
      data_for_chart <- data_for_chart %>% arrange(`country name`, year)

      p <- plot_ly(data_for_chart, x = ~year, y = ~total_loss, type = 'scatter', mode = 'lines+markers',
                   split = ~`country name`, color = ~`country name`) %>%
        layout(title = title,
               xaxis = list(title = "Year"),
               yaxis = list(title = "Economic Loss ($)"),
               legend = list(title = list(text = "Country")),
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)',
               font = list(color = '#66B2FF'))
    }
    p
  })
  
  # ----------- logic for the Reconstruction Costs navbar -------------
  # Reactive filtered data based on selected year
  filtered_data_recon <- reactive({
    final_data %>%
      filter(Year == input$year)
  })
  
  # Reactive value to store selected continent
  selected_continent <- reactiveVal(NULL)
  
  # selected continent when lollipop chart is clicked
  observeEvent(event_data("plotly_click", source = "lollipop_chart"), {
    click_data <- event_data("plotly_click", source = "lollipop_chart")
    if (!is.null(click_data)) {
      selected_continent(click_data$y)
    }
  })
  
  # Reset selected continent when year is changed
  observeEvent(input$year, {
    selected_continent(NULL)
  })
  
  output$lollipop_chart <- renderPlotly({
    data <- filtered_data_recon()
    
    p <- plot_ly(data, source = "lollipop_chart") %>%
      add_segments(
        x = 0, xend = ~total_reconstruction_costs,
        y = ~Continent, yend = ~Continent,
        line = list(color = '#888888', width = 2)
      ) %>%
      add_trace(
        x = ~total_reconstruction_costs,
        y = ~Continent,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = '#66B2FF', size = 16),
        hovertemplate = "Continent: %{y}<br>Total Reconstruction Costs: %{x:$,.0f}<extra></extra>",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = "Reconstruction Costs by Continent",
          font = list(size = 24, color = '#66B2FF')
        ),
        xaxis = list(
          title = "Total Reconstruction Costs ($)",
          font = list(size = 24, color = '#66B2FF'),
          tickformat = "$,",
          tickfont = list(size = 14),
          titlefont = list(size = 16),
          gridcolor = '#444444'
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 14),
          titlefont = list(size = 16)
        ),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black',
        font = list(color = '#66B2FF'),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
    plotly::event_register(p, 'plotly_click')
    p
  })
  
  # Render the data table with filtered country data
  output$country_table <- renderDT({
    if (is.null(selected_continent())) {
      # default
      filtered_country_data <- reconstruct_data %>%
        filter(Year == input$year) %>%
        filter(!Country %in% entries_to_exclude) %>%
        group_by(Country) %>%
        summarise(total_reconstruction_costs = sum(Reconstruction_Costs, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(total_reconstruction_costs))
    } else {
      # Continent selected, show data for countries in that continent
      filtered_country_data <- reconstruct_data %>%
        filter(Continent == selected_continent(), Year == input$year) %>%
        filter(!Country %in% entries_to_exclude) %>%
        group_by(Country) %>%
        summarise(total_reconstruction_costs = sum(Reconstruction_Costs, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(total_reconstruction_costs))
    }
    #show each country in continent
    datatable(
      filtered_country_data,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        initComplete = JS(
          "function(settings, json) {",
          
          "$(this.api().table().header()).css({'background-color': '#222222', 'color': '#66B2FF'});",
          
          "$(this.api().table().body()).css({'background-color': '#000000', 'color': '#FFFFFF'});",
          "}"
        )
      ),
      colnames = c("Country", "Total Reconstruction Costs"),
      rownames = FALSE,
      class = 'stripe hover'
    ) %>%
      formatCurrency('total_reconstruction_costs', currency = "$", interval = 3, mark = ",", digits = 0)
  })
  
  # (convert to millions/billions)
  format_loss_value <- function(value) {
    if (value >= 1e9) {
      return(paste0(round(value / 1e9, 2), " Billion"))  # Format as billions
    } else if (value >= 1e6) {
      return(paste0(round(value / 1e6, 2), " Million"))  # Format as millions
    } else {
      return(as.character(value))
    }
  }
  
  # Render the treemap
  output$economicLossTreemap <- renderPlotly({
    # Filter data based on inputs
    filtered_data <- economic_loss_data_new_clean %>%
      filter(Disaster_Type == input$treemap_disasterType,
             year == input$yearRange,
             !grepl("world|income|countries|america|asia|europe|union", tolower(`country name`)))
    
    # Check if there's any data if no show no
    if (nrow(filtered_data) == 0) {
      return(plot_ly() %>%
               layout(
                 title = "No Data Available",
                 annotations = list(
                   text = "No data available for the selected filters，please check others.",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 20)
                 ),
                 paper_bgcolor = 'black',
                 plot_bgcolor = 'black',
                 font = list(color = '#FFFFFF')
               )
      )
    }
    
    # Group and summarize data for treemap 
    treemap_data <- filtered_data %>%
      group_by(`country name`) %>%
      summarise(Economic_Loss = sum(Economic_Loss, na.rm = TRUE), .groups = 'drop')
    
    # Calculate the percentage of loss
    total_loss <- sum(treemap_data$Economic_Loss)
    treemap_data$Percentage <- (treemap_data$Economic_Loss / total_loss) * 100
    
    # convert to millions/billions
    treemap_data$Formatted_Loss <- sapply(treemap_data$Economic_Loss, format_loss_value)
    
    # Use a color palette for the treemap and show
    colors <- brewer.pal(n = min(8, nrow(treemap_data)), name = "Set3")
    
    plot_ly(
      data = treemap_data,
      labels = ~`country name`,
      parents = NA,
      values = ~Economic_Loss,
      type = 'treemap',
      textinfo = 'label+text',  # Show both country name and custom text
      text = ~paste("Loss:", Formatted_Loss),  # Show formatted loss directly on treemap
      hoverinfo = 'text',  # Show additional info (like percentage) on hover
      hovertext = ~paste("Country:", `country name`, "<br>Total Loss:", Formatted_Loss, "<br>Percentage:", round(Percentage, 2), "%"),
      marker = list(colors = colors)
    ) %>%
      layout(
        title = paste("Economic Loss Distribution for", input$treemap_disasterType, "in", input$yearRange),
        paper_bgcolor = 'black',
        plot_bgcolor = 'black',
        font = list(color = "#66B2FF")
      )
  })
}

# Run!!
shinyApp(ui = ui, server = server)
