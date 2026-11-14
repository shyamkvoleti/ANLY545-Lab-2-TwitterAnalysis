# Workday-style HR training clone (demo only)
# This app is for classroom/training purposes and does not implement
# enterprise-grade security, compliance, or production workflows.

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)

employees <- tibble::tibble(
  employee_id = sprintf("E%04d", 1:12),
  name = c(
    "Alex Carter", "Jordan Lee", "Priya Nair", "Miguel Santos",
    "Hannah Kim", "Noah Patel", "Olivia Zhang", "Liam Smith",
    "Emma Davis", "Ethan Brown", "Sophia Miller", "Mason Wilson"
  ),
  department = c(
    "Engineering", "Finance", "People Ops", "Engineering",
    "Marketing", "Sales", "Engineering", "Operations",
    "People Ops", "Finance", "Sales", "Marketing"
  ),
  title = c(
    "Software Engineer", "Financial Analyst", "HR Business Partner", "Engineering Manager",
    "Marketing Specialist", "Account Executive", "Data Engineer", "Operations Lead",
    "Recruiter", "Controller", "Sales Manager", "Brand Manager"
  ),
  manager = c(
    "Miguel Santos", "Ethan Brown", "Alex Carter", "-",
    "Mason Wilson", "Sophia Miller", "Miguel Santos", "Alex Carter",
    "Priya Nair", "-", "-", "-"
  ),
  location = c("New York", "Austin", "Chicago", "New York", "Seattle", "Denver", "Austin", "Chicago", "Denver", "New York", "Seattle", "Austin"),
  annual_salary = c(128000, 89000, 97000, 162000, 84000, 98000, 132000, 121000, 82000, 146000, 137000, 109000)
)

candidates <- tibble::tibble(
  candidate = c("Avery Thomas", "Kai Robinson", "Harper Nguyen", "Riley Brooks", "Cameron Flores"),
  role = c("Data Analyst", "Recruiter", "Backend Engineer", "Sales Development Rep", "Payroll Specialist"),
  stage = c("Phone Screen", "Offer", "Panel", "Application", "Panel"),
  recruiter = c("Emma Davis", "Emma Davis", "Sophia Miller", "Emma Davis", "Priya Nair")
)

pto <- tibble::tibble(
  employee = c("Jordan Lee", "Hannah Kim", "Liam Smith", "Olivia Zhang", "Noah Patel", "Emma Davis"),
  type = c("Vacation", "Sick", "Vacation", "Parental", "Vacation", "Sick"),
  start_date = as.Date(c("2026-05-01", "2026-04-08", "2026-06-15", "2026-07-01", "2026-08-10", "2026-04-11")),
  end_date = as.Date(c("2026-05-05", "2026-04-09", "2026-06-19", "2026-09-01", "2026-08-14", "2026-04-12")),
  status = c("Approved", "Approved", "Pending", "Approved", "Pending", "Approved")
)

learning <- tibble::tibble(
  course = c("Manager Essentials", "Anti-Harassment", "Compensation 101", "Interview Calibration", "Data Privacy Basics"),
  owner = c("People Ops", "Compliance", "Finance", "Talent Acquisition", "Legal"),
  due_date = as.Date(c("2026-05-10", "2026-04-20", "2026-05-30", "2026-04-25", "2026-06-05")),
  completion = c(0.61, 0.88, 0.45, 0.72, 0.39)
)

ui <- fluidPage(
  titlePanel("Workday-Style HR Training App (Demo)"),
  tags$p(
    class = "text-muted",
    "Training simulator only. Not affiliated with or a replacement for Workday."
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("dept", "Department Filter", choices = c("All", sort(unique(employees$department))), selected = "All"),
      sliderInput("salary", "Salary Range", min = min(employees$annual_salary), max = max(employees$annual_salary),
                  value = c(min(employees$annual_salary), max(employees$annual_salary)), step = 1000, pre = "$"),
      hr(),
      actionButton("reset", "Reset Filters")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Workforce",
          br(),
          fluidRow(
            column(3, wellPanel(h4("Headcount"), textOutput("kpi_headcount"))),
            column(3, wellPanel(h4("Departments"), textOutput("kpi_departments"))),
            column(3, wellPanel(h4("Avg Salary"), textOutput("kpi_avg_salary"))),
            column(3, wellPanel(h4("Open PTO"), textOutput("kpi_open_pto")))
          ),
          DTOutput("employee_table"),
          plotOutput("dept_plot", height = "300px")
        ),
        tabPanel(
          "Recruiting",
          br(),
          DTOutput("candidate_table"),
          plotOutput("stage_plot", height = "300px")
        ),
        tabPanel(
          "Time Off",
          br(),
          DTOutput("pto_table")
        ),
        tabPanel(
          "Learning",
          br(),
          DTOutput("learning_table"),
          plotOutput("learning_plot", height = "300px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSelectInput(session, "dept", selected = "All")
    updateSliderInput(session, "salary", value = c(min(employees$annual_salary), max(employees$annual_salary)))
  })

  filtered_employees <- reactive({
    out <- employees %>%
      filter(
        annual_salary >= input$salary[1],
        annual_salary <= input$salary[2]
      )

    if (input$dept != "All") {
      out <- out %>% filter(department == input$dept)
    }

    out
  })

  output$kpi_headcount <- renderText(nrow(filtered_employees()))
  output$kpi_departments <- renderText(dplyr::n_distinct(filtered_employees()$department))
  output$kpi_avg_salary <- renderText(scales::dollar(mean(filtered_employees()$annual_salary, na.rm = TRUE)))
  output$kpi_open_pto <- renderText(sum(pto$status == "Pending"))

  output$employee_table <- renderDT({
    datatable(
      filtered_employees(),
      rownames = FALSE,
      options = list(pageLength = 8, autoWidth = TRUE)
    )
  })

  output$dept_plot <- renderPlot({
    filtered_employees() %>%
      count(department) %>%
      ggplot(aes(x = reorder(department, n), y = n, fill = department)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Employees", title = "Headcount by Department") +
      theme_minimal()
  })

  output$candidate_table <- renderDT({
    datatable(candidates, rownames = FALSE, options = list(pageLength = 5))
  })

  output$stage_plot <- renderPlot({
    candidates %>%
      count(stage) %>%
      ggplot(aes(x = stage, y = n, fill = stage)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Stage", y = "Candidates", title = "Recruiting Pipeline") +
      theme_minimal()
  })

  output$pto_table <- renderDT({
    datatable(pto, rownames = FALSE, options = list(pageLength = 6))
  })

  output$learning_table <- renderDT({
    datatable(
      learning %>% mutate(completion = scales::percent(completion)),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
  })

  output$learning_plot <- renderPlot({
    learning %>%
      arrange(completion) %>%
      ggplot(aes(x = reorder(course, completion), y = completion, fill = completion)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = NULL, y = "Completion", title = "Training Completion by Course") +
      theme_minimal()
  })
}

shinyApp(ui, server)
