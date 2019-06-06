library(fs)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyBS)
library(frame)
library(tidyverse)
library(extraDistr)
library(plotly)
library(impact)
source("used_functions.R")

ui <- dashboardPage(skin = "red",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu( id = "tabs",
    menuItem("Introduction",tabName = "intro",icon = icon("chalkboard")),
    menuItem("Community description",tabName = "binputs", icon = icon("envira")),
    menuItem("Flammability dynamics ",tabName = "fire_dynm_anls" )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              box( width = 12,
                h3(tags$b("Flammability Dynamics"),align ="middle"),
                p(em("Finds the effects of fire on long-term flammability. 
                  This analysis grows the community, then at each stage you specify, 
                  measures the contextual flammability to show how this varies with age.
                  To model the dynamics resulting from a high severity fire, set ‘age of 
                  the youngest age class’ and ‘age of the trees at the starting point’ as the 
                  same number of years since the fire in question. To model the dynamics resulting from a 
                   low severity fire, set ‘age of the youngest age class’ to the number of years since 
                    the fire in question, 
                    and ‘age of the trees at the starting point’ to the number 
                    of years since the last high severity fire.
                   Results can optionally be saved by selecting a folder that they will be written to."),
                  align ="middle")
              )
              ),
      tabItem( tabName = "binputs",
               tabsetPanel( id = "binputs_tabset",
                            tabPanel( title = "Legacy Parameters ", value = "leg_par_tab",
                                      fluidRow(
                                        box(
                                          p("This file inputs' tab can be applied when the legacy parameters are 
                                            uploaded."),
                                          fileInput("legparms",label = tags$em("Legacy Parameters Files"),accept = "text/txt")
                                          ),
                                        box(
                                          DT::dataTableOutput("leg_parm_tbl")
                                        )
                                      )
                                      ),
                            tabPanel( title = "Site number", value =  "site_nmbr_tab",
                                      box(
                                        fileInput("site_added",label = tags$em("Site"),accept = "text/.csv"),
                                        fileInput("struc_added",label = tags$em("Structure"),accept = "text/.csv"),
                                        fileInput("flora_added",label = tags$em("Flora"),accept = "text/.csv")
                                      ),
                                      box(
                                        fileInput("plnttrts_added",label = tags$em("Plant Traits"), accept = "text/.csv"),
                                        numericInput("rec_to_model","Number of record to be modelled",1),
                                        actionButton("prmbldr_site_addedd",label = tags$em("Build parameters"))
                                      ),
                                      box(
                                        textOutput("prm_build_error_txt"),
                                        bsAlert("prm_build_error"),
                                        DT::dataTableOutput("prm_build_tbl")
                                      )
                            ),
                            tabPanel(title = "Age the community",value = "age_comm_tab",
                                     box(
                                       numericInput("tr_ag_site","Tree age",50, min =0),
                                       numericInput("ag_site","Age",10, min =0),
                                       fileInput("grth_added",label = tags$em("Growth"),accept = "text/.csv"),
                                       fileInput("cover_added",label = tags$em("Cover"),accept = "text/.csv"),
                                       actionButton("grow_forst",label = tags$em("Grow forest"))
                                     ),
                                     box(
                                       textOutput("grow_forst_txt"),
                                       bsAlert("grow_forst_error"),
                                       DT::dataTableOutput("grow_forst_table")
                                     )
                            ),
                            tabPanel( title= "Weather data set", value = "wthr_tab",
                                      box(
                                        fileInput("wthr_added",label = tags$em("Weather"),accept = "text/.csv")
                                      )
                                     )
                              )
                ),
      
      tabItem(
      tabName = "fire_dynm_anls",
      fluidRow(
        box(
          numericInput("yrs_btw_agclss_dynm","Years between age classes",1,min = 0),
          numericInput("yngst_agclss_dynm","Age of the youngest age class",1, min =0),
          numericInput("agclss_stps_dynm","Number of age class steps to take",50,min = 0),
          numericInput("age_trs_strt_pnt_dynm","Age of the trees at the starting point",75,min = 0),
          numericInput("slp_dynm","Slope",0,min = 0,max = 90),
          actionButton("run_dynm_anls",
                       label = "Run", icon = icon("calculator")),
          shinyDirButton("fire_dyn_folder",
                         label = "Select output folder",
                         title = "Please select the output folder")
        ),
        box(
            bsAlert("fire_dyn_error"),
            textOutput("fire_dyn_txt"),
            plotOutput("fire_dynm_plot_1"),
            plotOutput("fire_dynm_plot_2"),
            plotOutput("fire_dynm_plot_3")
        )
      )
    )
  )
  )
)

server <- function(input, output,session){ 

#########  
##site numbers  
#########
  
  in_site <- reactive({
    input$site_added
  })
  
  site_df <- reactive({
    if(!is.null(in_site())){
      temp_upl <- read.csv(in_site()$datapath)
      if(site_check(temp_upl) == "np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  
  ## structure df stored
  in_struc <- reactive({
    input$struc_added
  })
  
  struc_df <- reactive({
    if(!is.null(in_struc())){
      temp_upl <- read.csv(in_struc()$datapath)
      if(stctr_check(temp_upl) == "np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  
  ## flora df stored
  in_flora <- reactive({
    input$flora_added
  })
  
  flora_df <- reactive({
    if(!is.null(in_flora())){
      temp_upl <- read.csv(in_flora()$datapath)
      if(flora_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })  
  
  ## growth df stored
  in_grth <- reactive({
    input$grth_added
  })
  
  grth_df <- reactive({
    if(!is.null(in_grth())){
      temp_upl <- read.csv(in_grth()$datapath)
      if(growth_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })  
  
  ## Cover df stored
  in_cover <- reactive({
    input$cover_added
  })
  
  cover_df <- reactive({
    if(!is.null(in_cover())){
      temp_upl <- read.csv(in_cover()$datapath)
      if(cover_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  ## Weather dataframe
  in_wthr <- reactive({ 
    input$wthr_added
  })
  wthr_df <- reactive({
    if(!is.null(in_wthr())){
      temp_upl <- read.csv(in_wthr()$datapath)
      if(wthr_check(temp_upl) == "np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  
  ##Default parameters species 
  in_species <- reactive({
    input$plnttrts_added
  })
  ## add the data check for spcies in the fire dyn and site anls
  def_param_species_df <-  reactive({
    if(!is.null(in_species())){
      temp_upl <- read.csv(in_species()$datapath)
      if(traits_check(temp_upl)=="np"){
        return(temp_upl)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })

  
  
## base_params status holder. To see whether any basic parameters file 
##(leg file or built pars ds) are uploded for  is uploaded or not?
## it is equal to one if a leg file is uploaded
## it is equal to 2 if a parambuilder is applied
## it is equal to 3 if growth is applied
base_params_stat <- reactiveValues(status = 0)
  
  
#########  
## Legacy file
#########  
  
## legac files 
in_legfile <- reactive({
    input$legparms
})
  
## base_params is the value holder for built parameters
base_params <- reactiveValues(values =  NULL)
  
##writing the legacy file to the base_params Value
observe({
    req(in_legfile())
    base_params$values <- ffm_read_legacy_params(in_legfile()$datapath)
    base_params_stat$status <- 1
})
  
## printing output of the leg file parameters
output$leg_parm_tbl <- DT::renderDataTable({
   DT::datatable(base_params$values)
})
  
## Build inital parameter table  
  
observeEvent(input$prmbldr_site_addedd,{
    req(site_df(),struc_df(),flora_df(),
        def_param_species_df(),
        input$rec_to_model)
  base_params_init <- paramBuilder(
      site_df(),
      struc_df(),
      flora_df(),
      ## removing the first column because it has no name and frame throws error
      def_param_species_df(),
      input$rec_to_model
   )
    base_params_init$value[base_params_init$param == "leafForm"] <- "flat"
    base_params$values <- base_params_init
    base_params_stat$status <- 2
  })  
  ## param build output
  ## Throw error if inputs are not uploaded  
prm_bld_error_output<- eventReactive(input$prmbldr_site_addedd,{
  # Update param file
  ## Vector of input data sources
  ## Too check if all data inputs are NULL then ask upload either leg file of data sets
  ## Too check there is no missing data sets
  if(is.null(site_df())){
    if(is.null(in_site())){
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "You have not uploaded site data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded site does not follow template.", append = TRUE)
    }
  }else if(is.null(struc_df())){
    if(is.null(in_struc())){
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "You have not uploaded structure data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded structure does not follow template.", append = TRUE)
    }
  }else if(is.null(flora_df())){
    if(is.null(in_flora())){
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "You have not uploaded flora data set.", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded flora does not follow template.", append = TRUE)
    }
  }else if(is.null(def_param_species_df())) {
    if(is.null(in_species())){
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "You have not uploaded Traits data set", append = TRUE)
    }else{
      createAlert(session, "prm_build_error", "prmbld_alert", title = "Oops",
                  content = "uploaded species does not follow template.", append = TRUE)
    }
  }else{
    closeAlert(session,"prmbld_alert")
  }
})

  
  ## error holder for the param builder output 
  output$prm_build_error_txt <- renderText({
    ## error output of param builders 
    prm_bld_error_output()
  })
  
  
# param build table output
output$prm_build_tbl <-  DT::renderDataTable({
    DT::datatable(base_params$values,options =  list(searching = FALSE,pageLength = 5))
})
  
##grow forest status
## check whether growth cover is run
grow_forst_stat <-  reactiveValues(status = 0)
  
###Grow forest error
grow_forst_error_output <-  eventReactive(input$grow_forst,{
  if(base_params_stat$status == 0){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You should either uploade a 
                legacy file parameter or build a parameter table",append = TRUE)
  }else if(is.null(cover_df())){
    if(is.null(in_cover())){
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "You have not uploaded the cover data set.",append = TRUE)
    }else{
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "Uploaded cover does not follow templates.",append = TRUE)
    }
  }else if(is.null(grth_df())){
    if(is.null(in_grth())){
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "You have not uploaded the growth data set.",append = TRUE)
    }else{
      createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                  content = "Uploaded growth does not follow templates.",append = TRUE)
    }
  }else if( base_params_stat$status == 0){
    createAlert(session, "grow_forst_error","grow_forst_alert",title = "Oops",
                content = "You need to build parameters or upload a legacy file",append = TRUE)
  }else{
    closeAlert(session,"grow_forst_alert")
  }
})
## aging the community
  
## modifying base parameters
observeEvent(input$grow_forst,{
    req(base_params$values, grth_df(), cover_df(),flora_df())
    # AGE THE STAND
    nTable <- subset(base_params$values, param=="name")
    nSp <- as.numeric(count(nTable))
    strat <- filter(base_params$values, param == "levelName")
    nSt <- as.numeric(count(strat))
    nCanopy <- subset(nTable, stratum==nSt)
    nCsp <- as.numeric(count(nCanopy))
    nLow <- nSp-nCsp
    suspNS <- ""
    # Weight of the O-horizon
    base_params$values <- olsen(base_params$values, grth_df(), input$ag_site)
    
    
    for (stNum in 1:nSt) {
      st <- strat$value[stNum]
      sep <- coverChange(st, input$rec_to_model, cover_df(), flora_df(), input$ag_site)
      base_params$values <- ffm_set_stratum_param(base_params$values, stNum, "plantSeparation", sep)
      spList <- filter(nTable, stratum == stNum)
      n_a <- as.integer(spList$species[1])
      n_b <- as.integer(max(spList$species))
      nSusp <- as.integer((subset(base_params$values, value ==suspNS))$species[1])
      
      if (stNum < nSt) {
        for (spName in n_a:n_b)
          if(spList$value != suspNS){
            current <- growPlants(base_params$values, 
                                  input$rec_to_model, sp = nTable$value[spName], 
                                  stn = stNum, grth_df(), input$ag_site)
            base_params$values <- applyGrowth(base_params$values, nTable$value[spName], current)
          }
      } else{
        for(spName in n_a:n_b){
          current <- growPlants(base_params$values, 
                                input$rec_to_model, nTable$value[spName], 
                                stn = stNum, grth_df(), input$tr_ag_site)
        base_params$values <- applyGrowth(base_params$values, nTable$value[spName], current)
        }
    }
    base_params_stat$status <-  3

}
})
  
  
output$grow_forst_txt <- renderText({
    grow_forst_error_output()
})

output$grow_forst_table <-  DT::renderDataTable({
    DT::datatable(base_params$values,options =  list(searching = FALSE,pageLength = 5))
})
  
#########
#### Fire dynamics analysis  
########
##detrm behvr dirctry
fire_dynm_path_status <-  reactiveValues(dir = 0)
##Roots  of user
roots <- c(HOME = fs::path_home())

shinyDirChoose(input, 'fire_dyn_folder', roots= roots)

## Uploading Det beh path   
fire_dynm_path <- reactive({
  pth <- parseDirPath(roots, input$fire_dyn_folder)
  pth
})

## setting that det beh output is chosen  
observeEvent(input$fire_dyn_folder,{
  fire_dynm_path_status$dir <- 1
})

observe({
  print(fire_dynm_path())
})  


### fire dynamics error

fire_dyn_error_output <- eventReactive(input$run_dynm_anls,{
  if(base_params_stat$status == 0){
    createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                content = "You should either run legacy parameter or build parameters",append = TRUE)
  }else if(is.null(grth_df())){
    if(is.null(in_grth())){
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "You have not uploaded the growth data set",append = TRUE)
    }else{
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "Uploaded growth does not follow template.",append = TRUE)
    }
    
  }else if(is.null(wthr_df())){
    if(is.null(in_wthr())){
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "You have not uploaded the weather data set.",append = TRUE)
    }else{
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "Uploaded weather does not follow template.",append = TRUE)
    }
  }else if(is.null(cover_df())){
    if(is.null(in_cover())){
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "You have not uploaded the cover data set.",append = TRUE)
    }else{
      createAlert(session,"fire_dyn_error","fire_dyn_alert",title = "Oops",
                  content = "Uploaded weather does not follow template.",append = TRUE)
    }
  }else{
    closeAlert(session,"fire_dyn_alert")
  }
  
})

##Printing the error output
output$fire_dyn_txt <- renderText({
  fire_dyn_error_output()
})
 
## Running the fire dynamic analysis 
fire_dynm <- eventReactive(input$run_dynm_anls,{
    req(isolate(base_params$values))
    init <- isolate(base_params$values)
    # Set slope
    base_params_dyn <-
     init %>%
      ffm_set_site_param("slope", input$slp_dynm, "deg")
    
    progress <- shiny::Progress$new()
    progress$set(message = "Running the fire behaviour \n", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    prg_steps <-  input$agclss_stps_dynm
    updateProgress <- function(value = prg_steps, detail = NULL) {
      progress$inc(amount = 1/value, detail = detail)
    }
    
    # Running fire dynamics
    dyn <- fireDynamics(base_params_dyn, wthr_df(), grth_df(), cover_df(),
                        flora_df(), jitters = 10,
                        ageStep = input$yrs_btw_agclss_dynm,
                        firstAge = input$yngst_agclss_dynm,
                        steps = input$agclss_stps_dynm,
                        tAge = input$age_trs_strt_pnt_dynm,
                        l = 0.1,
                        DefaultSpeciesParams = def_param_species_df(), Ms = 0.01, Pm = 1,
                        Mr = 1.001, Hs = 1.25, Hr = 1.42, a = input$rec_to_model
                        , suspNS = "", density = 300,updateProgress = updateProgress)
    # Find growth curve for O_horizon
    ols <- filter(growth, Species == "O_horizon")
    
    # Model accumulation
    dynamics <- dyn%>%
      mutate(O_horizon = ols$max*(1-exp(-ols$rate*age)),
             spread = ifelse(O_horizon > 4, spread, 0),
             fh = fh*spread,
             fl=fl*spread,
             ros_kph=ros_kph*spread,
             Height = Height*spread,
             b1 = b1*spread,
             b2 = b2*spread,
             b3 = b3*spread,
             b3 = b4*spread,
             sc1 = sc1*spread,
             sc2 = sc2*spread,
             sc3 = sc3*spread,
             sc4 = sc4*spread)
    
    
    attack <- dynamics %>%
      mutate(DA = 100-as.numeric(fh <=1.5)*100,
             PA = 100-as.numeric(fh <=3)*100)%>%
      group_by(age) %>%
      summarize_all(mean)
    
    ### Writing the output of fire dynamics analysis
    if(fire_dynm_path_status$dir == 1){
      write.csv(dynamics,paste(fire_dynm_path(),"/Dynamics_flammibility"))
    }
    fnl = list(attack =  attack,dynamics = dynamics)
    return(fnl)
  })

##printing plots
output$fire_dynm_plot_1 <- renderPlotly({
  req(fire_dynm())
  p1 <- plotly::plot_ly() %>% 
    plotly::add_boxplot(x = ~fire_dynm()$dynamics$age,
                        y = ~fire_dynm()$dynamics$fh) %>%     
    layout(
      xaxis = list(title = list(text = "Years since fire",
                                font = list(family ="Arial",size = 10)),
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Flame height (m)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),zeroline =FALSE,
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  p1
})

output$fire_dynm_plot_2 <- renderPlotly({
  req(fire_dynm())
  p2 <-  plotly::plot_ly() %>% 
    plotly::add_boxplot(x = ~fire_dynm()$dynamics$age,
                        y = ~fire_dynm()$dynamics$ros_kph) %>%     
    layout(
      xaxis = list(title = list(text = "Years since fire",
                                font = list(family ="Arial",size = 10)),
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "ROS (km/h)",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),zeroline =FALSE,
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  p2
  
})

output$fire_dynm_plot_3 <- renderPlotly({
  req(fire_dynm())
  aSpline = smooth.spline(fire_dynm()$attack$age, 
                          fire_dynm()$attack$DA, spar=0.5) 
  p3 <- plotly::plot_ly() %>% 
    plotly::add_lines(x = ~fire_dynm()$attack$age,
                      y = ~fire_dynm()$attack$DA) %>% 
    plotly::add_lines(x = ~aSpline$x,aSpline$y) %>% 
    layout( 
      xaxis = list(title = list(text = "Years since fire",
                                font = list(family ="Arial",size = 10)),
                   showgrid = TRUE,dtick = 1,
                   showline = TRUE,linewidth = 3,
                   mirror = "ticks",showticklabels  = FALSE),
      yaxis = list(title = list(text = "Percent occurrence",
                                font = list(family ="Arial",size = 10)),
                   tickfont=list(family = "Arial",size=10),zeroline =FALSE,
                   showgrid = TRUE,showline = TRUE,
                   linewidth = 3,mirror = "ticks"),
      showlegend =FALSE)
  p3
})


}

shinyApp(ui, server)