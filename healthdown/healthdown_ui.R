mod_healthdown_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
      class = "top-row",
      box(
        width = 4,
        height = "100px",
        class = "card-hexagon",
        img(src = "assets/images/aspire-logo.jpg"),
        div("Aspire 365 Analytics", class = "card-hexagon-title", style = "color: black;")
      ),
      box(
        width = 8,
        height = "100px",
        fluidRow(
          div(
            class = "spread3evenly",
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("year"),
                label = "Select the Year",
                choices = all_years,
                selected = max(all_years)
              )
            ),
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("prim_var"),
                label = "Select the Primary Variable",
                choices = all_vars,
                selected = all_vars[1]
              )
            ),
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("sec_var"),
                label = "Select the Secondary Variable",
                choices = all_vars,
                selected = all_vars[2]
              )
            ),
            div(
              class = "generateReport",
              label = "Generate Report",
              downloadButton(outputId = ns("report"), 
                             label = "Generate report")
            ),
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 3,
        DT::dataTableOutput(ns("mytable"), height = "50vh")
      ),
      column(
        width = 10,
        fluidRow(
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            actionButton(ns("drill_down"), "Drill Down", icon = icon("arrow-down"), class = "drill-button healthdown-button"),
            actionButton(ns("drill_up"), "Drill Up", icon = icon("arrow-up"), class = "drill-button healthdown-button"),
            leafletOutput(ns("leafdown"), height = "30vh")
          ),
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("bar"), height = "30vh")
          )
        ),
        fluidRow(
          height = "90%",
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("line"), height = "30vh")
          ),
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("scatter"), height = "30vh")
          )
        )
      )
    )
  )
}
