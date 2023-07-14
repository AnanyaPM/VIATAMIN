# install.packages("shiny")

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(shinyBS)
library(plotly)
library(shinytitle)

extendedCheckboxGroup <- function(..., extensions = list()) {
  cbg <- checkboxGroupInput(...)
  nExtensions <- length(extensions)
  nChoices <- length(cbg$children[[2]]$children[[1]])
  
  if (nExtensions > 0 && nChoices > 0) {
    lapply(1:min(nExtensions, nChoices), function(i) {
      # For each Extension, add the element as a child (to one of the checkboxes)
      cbg$children[[2]]$children[[1]][[i]]$children[[2]] <<- extensions[[i]]
    })
  }
  cbg
}

bsButtonRight <- function(...) {
  btn <- bsButton(...)
  # Directly inject the style into the shiny element.
  btn$attribs$style <- "
  color: white; 
  border-color: white;
  background: none;
  display: inline-block;
  max-width: 600px;"
  btn
}

################# UI

header <- dashboardHeader(title = span("VIATAMIN", 
                                       style = "color: white; font-size: 28px; "),
                          titleWidth = 375)

sidebar <- dashboardSidebar(
  width = 375,
  useShinyjs(),
  
  # sidebar scrolling
  tags$style(
    "#sidebarItemExpanded {
            position:fixed;
            overflow-x: visible;
            overflow-y: auto;
            max-height: 90vh;
            margin-left:10px;
            width:375px;
        }"
  ),
  # Info hover text
  tags$style(HTML("
                .tooltip > .tooltip-inner {
                background-color: white;
                color: black;
                border: 1px black;
                padding: 5px;
                font-size: 12px;
                margin-top: 5px;
                width: 350px;
                }
                ")),
  # Info icons:
  tags$head(
    tags$style(
      HTML("
        .radio-inline {
         margin-right: 20px;
         }
         #MT_level_help {
         display: inline;
         margin: 0px;
         color: white;
         width: 200px;
         }
         #MT_Func_help {
         display: inline;
         margin: 0px;
         color: white;
         width: 350px;
         }
         #cMT_Func_help {
         display: inline;
         margin: 0px;
         color: white;
         width: 350px;
         }
         #int_date_help{
         display: inline;
         margin: 0px;
         color: black;
         width: 350px;
         }
         #cDI_help{
         display: inline;
         margin: 0px;
         color: white;
         width: 350px;
         }
         #ind_ANCunit_help{
         display: inline;
         margin: 0px;
         color: white;
         width: 350px;
         }
         #c_ANCunit_help{
         display: inline;
         margin: 0px;
         color: white;
         width: 350px;
         }
           ")
    )
  ),
  
  # Input: Choose level -----
  radioButtons(inputId = "MT_level", 
               label = HTML("Choose analysis Level", as.character(actionLink(inputId = "MT_level_help", label = "", icon = icon("circle-info")))),
               choices = c("Individual", "Cohort"),
               selected = character(0)),
  bsTooltip(id = "MT_level_help", title="Choose what level of analysis you would like to perform", 
            placement = "right", trigger = "hover"),
  
  # Choosing : Individual -------
  conditionalPanel(
    condition = "input.MT_level == 'Individual'",
    # Input: Choose patient file -----
    fileInput(
      inputId = "ID",
      label = "Patient ID",
      multiple = FALSE,
      accept = c(".csv",".xls", ".xlsx", ".xlsm"),
      width = NULL,
      buttonLabel = "Browse ",
      placeholder = "Please choose a file",
      capture = NULL),
    # conditionalPanel(condition = "output.filesUploaded", 
    # Input: Choose function -----
    extendedCheckboxGroup(inputId = "MT_Func", 
                       label = HTML("Choose Analysis", as.character(actionLink(inputId = "MT_Func_help", label = "", icon = icon("circle-info")))),
                       choices = c("Longitudinal progression" = "Ind_LG",
                                   "Serial Treatment Intensity" = "Ind_csSM",
                                   "Assess Dose decisions" = "ass_dose",
                                   "Assess Hematological Toxicity" = "hemat_tox"),
                       extensions = list(tipify(bsButtonRight("pB1", "?", style = "info", size = "extra-small"),
                                                "Plot the treatment progression (line graph)"),
                                         tipify(bsButtonRight("pB2", "?", style = "inverse", size = "extra-small"),
                                                "Plot the cycle-wise treatment progression dose decisions made by the physician"),
                                         tipify(bsButtonRight("pB3", "?", style = "inverse", size = "extra-small"),
                                                "Assess stop, reduce and increase dose decisions"),
                                         tipify(bsButtonRight("pB4", "?", style = "inverse", size = "extra-small"),
                                                "Assess hematological toxicities observed during treatment")
                                         )),
    bsTooltip(id = "MT_Func_help", title="Choose what kind of analysis you would like to perform",
              placement = "right", trigger = "hover"),
    
    conditionalPanel(condition = "input.MT_Func.includes('Ind_LG') || input.MT_Func.includes('Ind_csSM')", 
    # Provide input ANC parameters ------
    HTML("<br>"),
    h4(strong("Target Absolute Neutrophil Count (ANC)"), align = "center"),
    h4(strong("values"), align = "center"),
    # Choose unit -----
    radioButtons(inputId = "unit",
                 label = HTML("Choose ANC unit", as.character(actionLink(inputId = "ind_ANCunit_help", label = "", icon = icon("circle-info")))),
                 choiceNames = list(HTML("x10<sup>6</sup>/L"),HTML("x10<sup>9</sup>/L")),
                 choiceValues = list("million", "billion"),
                 selected = character(0),
                 inline = TRUE),
    bsTooltip(id = "ind_ANCunit_help", title="eg: \\'700x10<sup>6</sup>/L\\' = \\'700/mm<sup>3</sup>\\' or \\'700/uL\\' or \\'0.7x10<sup>9</sup>/L\\'", 
              placement = "right", trigger = "hover"),
    
    # Provide ANC target range -----
    fluidRow(
      splitLayout(cellWidths = c('50%', '50%'), align = "center", 
                  style = "margin-top: -20px;",
                  numericInput(
                    inputId = "ANC_lower",
                    label = h5("Lower threshold"),
                    value = character(0),
                    width = '100px'
                  ),
                  
                  numericInput(
                    inputId = "ANC_upper",
                    label = h5("Upper Threshold"),
                    value = character(0),
                    width = '100px'
                  )
      ))),
    
    # Submit -----
    actionButton(
      inputId = "go", label = "Analyze"),
    h5("(Analyze once all criteria are selected ", style = "text-align: left; margin-left: 20px"),
    h5(" OR a new function is selected)", style = "text-align: left; margin-left: 20px"),
    HTML("<br>"),
    actionButton("refresh1" ,"Refresh", icon("refresh")),
    h5("(refresh to assess new patient or cohort)", style = "text-align: left; margin-left: 20px")
  ),
  # ),
  
  # Choosing : Cohort ------------
  conditionalPanel(
    condition = "input.MT_level == 'Cohort'",
    # Input: Choose all patient files for analysis -----
    fileInput (
      inputId = "cohort_IDs",
      label = "Patient files",
      multiple = TRUE,
      accept = ".csv",
      width = NULL,
      buttonLabel = "Browse ",
      placeholder = "Select all required files",
      capture = NULL),
    
    # Input: Choose function -----
    extendedCheckboxGroup(inputId = "cMT_Func", 
                          label = HTML("Choose Analysis", as.character(actionLink(inputId = "cMT_Func_help", label = "", icon = icon("circle-info")))),
                       choices = c("Cohort Summary Measures" = "Cohort_SM",
                                   "Compare Cohorts" = "Comp_cohorts",
                                   "Assess Dose decisions" = "ass_dose",
                                   "Assess Hematological Toxicity" = "hemat_tox"),
                       extensions = list(tipify(bsButtonRight("pB5", "?", style = "inverse", size = "extra-small"),
                                                "Scatter plot with summarized ANC and antimetabolite values (across MT) for each patient"),
                                         tipify(bsButtonRight("pB6", "?", style = "inverse", size = "extra-small"),
                                                "Compare treatment intensity of patients before and after a chosen intervention date"),
                                         tipify(bsButtonRight("pB7", "?", style = "inverse", size = "extra-small"),
                                                "Assess stop, reduce and increase dose decisions"),
                                         tipify(bsButtonRight("pB8", "?", style = "inverse", size = "extra-small"),
                                                "Assess hematological toxicities observed during treatment")
                       )),
    bsTooltip(id = "cMT_Func_help", title="Choose what kind of analysis you would like to perform",
              placement = "bottom", trigger = "hover"),
    
    # Summary Measures/Compare Cohorts ----------
    conditionalPanel(condition = "input.cMT_Func.includes('Cohort_SM') || input.cMT_Func.includes('Comp_cohorts')", 
                     # Provide input ANC parameters ------
                     HTML("<br>"),
                     h4(strong("Target Absolute Neutrophil Count (ANC)"), align = "center"),
                     h4(strong("values"), align = "center"),
                     # Choose unit -----
                     radioButtons(inputId = "c_unit",
                                  label = HTML("Choose ANC unit", as.character(actionLink(inputId = "c_ANCunit_help", label = "", icon = icon("circle-info")))),
                                  choiceNames = list(HTML("x10<sup>6</sup>/L"),HTML("x10<sup>9</sup>/L")),
                                  choiceValues = list("million", "billion"),
                                  selected = character(0),
                                  inline = TRUE),
                     bsTooltip(id = "c_ANCunit_help", title="eg: \\'700x10<sup>6</sup>/L\\' = \\'700/mm<sup>3</sup>\\' or \\'700/uL\\' or \\'0.7x10<sup>9</sup>/L\\'", 
                               placement = "right", trigger = "hover"),
                     
                     tags$head(
                       tags$style(
                         ".radio-inline{margin-left:35px;"
                       )
                     ),
                     
                     # Provide ANC target range -----
                     fluidRow(
                       splitLayout(cellWidths = c('50%', '50%'), align = "center", 
                                   style = "margin-top: -20px;",
                                   numericInput(
                                     inputId = "cANC_lower",
                                     label = "Lower threshold",
                                     value = character(0),
                                     width = '100px'),
                                   
                                   numericInput(
                                     inputId = "cANC_upper",
                                     label = "Upper Threshold",
                                     value = character(0),
                                     width = '100px')
                       )),
                     # Provide input PLT values -----
                     # h4(strong("Target Platelet Values"), align = "center"),
     # Provide Dose intensity threshold -----
            sliderInput(inputId = "c_DI",
                        label = HTML("Choose dose intensity threshold (%)", as.character(actionLink(inputId = "cDI_help", label = "", icon = icon("circle-info")))),
                        min = 0,max = 300, value = 0,
                        step = 10, ticks = TRUE),
     bsTooltip(id = "cDI_help", title= "Drug dose intensity percentage to compare which patients are above or below threshold", 
               placement = "top", trigger = "hover"),
    ),
    # Submit -----
    actionButton(
      inputId = "go1", label = "Analyze"),
    h5("(Analyze once all criteria are selected ", style = "text-align: left; margin-left: 20px"),
    h5(" OR a new analysis is selected)", style = "text-align: left; margin-left: 20px"),
    HTML("<br>"),
    actionButton("refresh2" ,"Refresh", icon("refresh")),
    h5("(refresh to assess new cohort)", style = "text-align: left; margin-left: 20px")
    # -----
  ) 
)


body <- dashboardBody(
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        float: right;
        padding: 0 40px;
        text-align: right;
        # font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        overflow: hidden;
        color: white;
      }
    '))),
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> VIsualisation & Analysis Tool in ALL MaINtenance </span>\');
      })
     ')),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
  tags$head(tags$style(HTML('.box { margin-top: 100; }'))),
  tags$head(tags$style(HTML("#PanelID li:has(a[data-value='Help']) {  float:right;}"))),
  tabsetPanel(id = "PanelID", 
              # Get Started -----
              tabPanel("Get Started", 
                       
                       HTML("<br>"),
                       
                       fluidRow(
                         box(title = HTML("<b>Introduction</b>"), 
                             # height = "175px",
                             HTML("<b> BACKGROUND: </b>
                                 <p>Acute lymphoblastic leukemia (ALL) studies across the globe have shown that delivering optimal maintenance therapy (MT) 
                                  is crucial to achieve better treatment outcomes<sup> 1 </sup>. 
                                  Delivering optimal MT includes prescribing maximum tolerated doses of 6-mercaptopurine (6MP) and methotraxate (MTX)
                                  based on patient’s blood counts (absolute neutrophil count (ANC), platelet count (PLT) and hemoglobin (HB)).<br> 
                                  <b>This app was created built upon the methodology and functions developed in the <b>allMT</b> R package <sup> 2,3 </sup>
                                  to analyse and visualize the MT data (ANC/PLT/HB/6MP/MTX) for a single patient or a given cohort at any center</b>.
                                  <br> <br>
                                  <b> VISUAL ANALYSES: </b> <br>
                                  <p> <i>Individual patient: </i> <br> <ol type = 2 style='line-height:1'> 
                                        <li> <u> Longitudinal progression </u> - Single patient MT history </li> <br>
                                        <li> <u>Serial treatment intensity </u> - Cycle (tri-monthly) -specific MT information of a single patient</li> </ol> <br>
                                  <p>  <i> Cohort of patients: </i> <br> <ol type = 2 style='line-height:1'>
                                        <li> <u> Summary Measures </u> - Summarized MT treatment of all patients in given cohort </li> <br>
                                        <li> <u> Compare Cohorts </u> - Comparative representation of patients before and after a seleted date</li> </ol> </p>
                                  <b> STATISTICAL ANALYSES: </b>
                                  <p> <ol type = 2 style='line-height:1'> 
                                      <li> <u>Assess physician's dosing decisions </u> (stop, reduce and increase dose decisions) </li> <br>
                                      <li> <u>Assess (median) time to dose increase </u> for the patient or cohort </li> <br>
                                      <li> <u>Assess hematological toxicities </u> (Neutropenia, Thrombocytopenia, Anaemia) experienced by the patient over the course of MT </li> </ol>
                                  <i>Refer to 'Further Explanation' or 'Video Explanation' for more in-depth help </i>"),
                            collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, width = 9),
                       
                         box(title = "Help Section", status = "success",
                             # height = "175px",
                             br(),
                             uiOutput("start_help"), 
                             br(),
                             uiOutput("start_video"),
                             align = "center",
                             collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, width = 3)
                       ),
                       
                       fluidRow(
                         box(title = "What data can I upload?", status = "primary",
                             HTML("<h5><ul type = 1 style='line-height:1.5'> <ul>
                                  <li> Ensure the column names include (in any order): <b>'Cycle'</b>, &nbsp; <b>'Dates'</b>, &nbsp;
                                       <b> 'Weeks'</b>, &nbsp; <b> 'ANC'</b>, &nbsp; <b> 'PLT'</b>, &nbsp; <b> 'Hb'</b>, &nbsp; <b> 'MP'</b>, &nbsp;
                                       <b>'MP_adj'</b>*, &nbsp; <b> 'MTX' </b>, &nbsp; and &nbsp; <b>'MTX_adj'</b>* </li>
                                  <li> Ensure all values are numeric in nature </li>
                                  <li> Ensure date formats are standard across all uploaded sheets </li>
                                  <li> Ensure files are in .csv or excel (.xls or .xlsx) formats  </li> </ul>
                                  <i> Refer to example below </i>
                                  </h5></ul>"),
                             collapsible = T, collapsed = T, solidHeader = T, width = 9)
                       ),

                       fluidRow(
                         box(title = "Let's talk units", status = "primary",
                             HTML("<h5> Ensure that any blood count input values you provide, match the units in the data files you have uploaded!</h5>"),
                             img(src = 'blood counts.png', width = "100%", height = "150px"),
                             HTML("<h5><ul style='line-height:1.2'>
                                   <u> Absolute Neutrophil Count (ANC) Units : </u>
                                        <li> Choose <b>'x10<sup>6</sup>/L'</b> if your data has ANC units as: '/mm<sup>3</sup>' or '/uL'. &nbsp;
                                                  eg: 1250x10<sup>6</sup>/L' or '1250/mm<sup>3</sup>' or '1250/uL'</li> 
                                        <li> Choose <b>'x10<sup>9</sup>/L'</b> if your data has units as: 'x10<sup>9</sup>/L' eg: 1.250x10<sup>9</sup>/L'.</li><br>
                                   <u>Platelet (PLT) Units</u> : '20,000x10<sup>6</sup>/L' is the same as '20,000/uL' or '20x10<sup>9</sup>/L'
                                  </h5>"),
                             collapsible = T, collapsed = T, solidHeader = T, width = 9)
                         ),
                       
                       fluidRow(
                         box(title = "For example", status = "primary",
                             HTML("</u></h4> <p>(this data has ANC units and PLT units in 'x10<sup>9</sup>/L'; Hemoglobin in g/L; MP and MTX doses in mg)</p>"),
                             # img(src='input_data.png', width = "70%", height = "700px", style="display: block; margin-left: auto; margin-right: auto;")
                             img(src='input_data2.png', width = "70%", height = "700px"),
                             HTML("<br>"),
                             HTML("* <b>MP_adj</b> : Adjusted 6MP dose intensity = (prescribed 6MP dose (mg) / protocol 6MP dose)*100<br>"),
                             HTML("* <b>MTX_adj</b> : Adjusted MTX dose intensity = (prescribed MTX dose (mg) / protocol MTX dose)*100<br>"),
                             HTML("<br>"),
                             HTML("<u>Protocol dose </u> is calculated as the absolute dose (mg) based on the patient's body surface area (BSA)<br>"),
                             HTML("eg: BSA = 0.8m<sup>2</sup>, 6MP dose = 350mg per week<br>"),
                             HTML("Protocol = 60mg/day/m<sup>2</sup><br>"),
                             HTML("Thus, Protocol dose = 60*7*0.8 = 336mg/week<br>"),
                             HTML("Thus MP_adj = 350/336 = 104%<br>"),
                             collapsible = T, collapsed = T, solidHeader = T, width = 9)
                       ),
                       
                       fluidRow(
                         box(title = "What do I need to do?", status = "primary",
                             HTML("<h5> <i> (Each analysis feature will appear as required) </i> </h5>"),
                             HTML("<h5> <ol type = 1 style='line-height:1.5'> <li> Choose level - do you want to do individual analysis or for a group? </li>
                                  <li> Upload data files (refer below for required format, <b>ensure that there is no missing data!)</b> </li>
                                  <li> Choose what analysis you'd like to do </li>
                                  <li> Either fill in required inputs or press Analyze! </li> </ol> </h5>"),
                             # HTML('<iframe width="400" height="270" src="https://www.youtube.com/watch?v=w4u3f7k-klA&ab_channel=MedicalCentric',
                             #      "/embed",'" frameborder="0" allowfullscreen></iframe>'),
                             # HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/w4u3f7k-klA" title="YouTube video player" 
                             #      frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
                             HTML("<br>"), 
                             collapsible = T, collapsed = T, solidHeader = T, width = 9)
                       ),
                       
                       fluidRow(
                         box(title = "References", status = "primary",
                             HTML("[1]. K. Schmiegelow, S. N. Nielsen, T. L. Frandsen, and J. Nersting,
                                  “Mercaptopurine/methotrexate maintenance therapy of childhood acute lymphoblastic leukemia: Clinical facts and fiction,” 
                                  Journal of pediatric hematology/oncology, vol. 36, no. 7, p. 503, 2014.
                                  <br> <br>
                                  
                                  [2]. Mungle, Tushar Dilip. 'Modelling Clinical Decision Processes to Optimise Maintenance Chemotherapy in Children
                                  with Acute Lymphoblastic Leukaemia.' PhD diss., IIT Kharagpur, 2020.
                                  <br> <br>

                                  [3]. Mungle T, Mahadevan A, Krishnan S (2023).
                                  allMT: Acute Lymphoblastic Leukemia Maintenance Therapy Analysis. https://CRAN.R-project.org/package=allMT .
                                  <b> (R Package) </b> "),
                                  collapsible = T, collapsed = T, solidHeader = T, width = 9)
                       ),
                       
                       fluidRow(
                         box(title = "Contact us",
                             HTML("<b>Ananya Mahadevan </b> <i>(Developer)</i><br>
                                   Email : ananyamahadevan97@gmail.com
                                  <br> <br>
                                   <b>Dr. Shekhar Krishnan</b> <br>
                                   Email: shekhar.krishnan@ttcrc.tmckolkata.org"),
                             collapsible = T, collapsed = T, solidHeader = T, width = 9)
                       ),
                       
                       HTML("<p><i> Last updated: 22<sup> nd</sup> June 2023 </i> </p>"),
                       HTML("<p> Developed by Ananya Mahadevan, Tushar Mungle, Dr. Shekhar Krishnan</p>")
                       
              ),
              # ------------
  )
)

ui <- fluidPage(
  title = "VIATAMIN",
  use_shiny_title(),
  # Dashboard ------
  dashboardPage(
    header,
    sidebar,
    body,
    
    mainPanel()
  )
)



