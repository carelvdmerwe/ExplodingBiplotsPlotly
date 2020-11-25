#Exploding biplots in Plotly 
Install.packagesTriplot <- function()
{
  list.of.packages <-
    c(
      "cluster",
      "RConics",
      "pracma",
      "plotly",
      "MASS",
      "ggplot2",
      "ellipsis",
      "FactoMineR",
      "shinyWidgets",
      "htmlwidgets",
      "GPArotation",
      'mvtnorm',
      'mclust',
      'adegraphics',
      'factoextra',
      'PLSbiplot1',
      'psych',
      'ade4'
    )
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  
  library(cluster)
  library(RConics)
  library(pracma)
  library(MASS)
  library(plotly)
  library(ggplot2)
  library(ellipsis)
  library(FactoMineR)
  library(mclust)
  library(GPArotation)
  library(cluster)
  library(shinyWidgets)
  library(htmlwidgets)
  library(mvtnorm)
  library(adegraphics)
  library('factoextra')
  library('PLSbiplot1')
  library(psych)
  library(ade4)
  
  
  data(iris)
  data(mtcars)
  
  PCAbipl <<- source("source/PCAbipl.R")$value
  drawbipl.bagalpha <<- source("source/drawbipl.bagalpha.R")$value
  PCAbipl.density <<- source("source/PCAbipl.density.R")$value
  drawbipl.bagalpha.density <<- source("source/drawbipl.bagalpha.density.R")$value
  indmat<<- source("source/indmat.R")$value
  Draw.line2<<- source("source/Draw.line2.R")$value
  Draw.onecmline<<- source("source/Draw.onecmline.R")$value
  Plot.marker.new<<- source("source/Plot.marker.new.R")$value
  draw.density.rect<<- source("source/draw.density.rect.R")$value
  blegend.colchar<<- source("source/blegend.colchar.R")$value
  key.R<<- source("source/key.R")$value
  
  exploding_bipl <<- source("source/exploding_bipl.R")$value
  Ocoteadata <<- read.csv("data/Ocotea.data.csv")
  Xclaradata <<- read.csv("data/xclara.csv")
  Copperdata <<- read.csv("data/CopperFrothdata.csv")
  Irisdata  <<-  cbind(iris[, 5], iris[, -5])
  Mtcarsdata <<- cbind(rep(1, dim(mtcars)[1]), mtcars)
  datastart <<- "Iris"
  datain <<- Irisdata
}


########################################################################################################################################

Install.packagesTriplot()

ui <- tagList(navbarPage(
  theme = "yeti",
  "Exploding biplots with density axes in Plotly",
  tabPanel(
    "Interactive Shiny App",
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          'Info',
          p(
            'This Shiny Web Based Application serves as an illustration of the adjustments and enhancements to biplots as proposed by the paper Exploding biplots with density axes in Plotly. '
          ),
          p(
            'All interactions with the biplot conducted on the plot itself, and not via the tabs on the left-hand side, are functionalities available when just making use of Plotly in R and not the Shiny application. '
          ),
          downloadButton("download_plotly_widget", "Download Plotly graph as HTML file")
        ),
        
        tabPanel(
          'Data',
          p(
            'Within the Data tab, the user can choose which dataset they would like to use, whether they would like to see predicted values of specific observations, as well as which eigenvectors must be used as z1 and z2 axes.'
          ),
          
          radioButtons(
            "data",
            "Dataset",
            selected = "Iris",
            inline = TRUE,
            choiceNames = c("Iris", "Xclara", "Ocotea", "Mtcars", "CopperFroth"),
            choiceValues = c("Iris", "Xclara", "Ocotea", "Mtcars", "CopperFroth")
          ),
          checkboxInput("plotobs", value = TRUE, label = "Plot the predicted observation"),
          pickerInput(
            "plotobsnr",
            'Observation to Predict',
            choices = rownames(Irisdata),
            selected = NULL,
            multiple = FALSE
          ),
          pickerInput(
            "evinput",
            "Eigenvectors used for axes z1 and z2",
            choices = 1:4,
            selected = c(1, 2),
            multiple = TRUE,
            options = list(`max-options` = 2)
          )
        ),
        
        
        tabPanel(
          'Axes',
          p(
            'The Axes tab allows the user to choose the initial minimum distance used to translate axes to the edge of the biplot also referred to as D_base. The user can also choose the amount of data to include in the ellipse used to calculate the parameters for axes translations'
          ),
          sliderInput(
            "mld",
            label = "Minimum initial distance to move each axis",
            min = 0,
            max = 10,
            step = 0.1,
            value = 1
          ),
          sliderInput(
            "percentile",
            "Percentage data to include in the ellipse and the convex hull",
            min = 50,
            max = 99,
            value = 99,
            step = 1,
            post = "%"
          )
        ),
        
        tabPanel(
          'Densities',
          p(
            'The Densities tab contains all the options regarding the densities namely: whether the user wants to plot the densities on the axes, whether the densities must be constructed using counts or densities. Counts enables you to see which cluster has more data as the density structures constructed in this way does not rescale each density to sum to 1. Furthermore, the user can choose to inflate (adjust) the size of the density. Finally, the densities can be smoothed using smoothing splines. The user can specify their own preferred smoothing parameter or let the model choose for itself.'
          ),
          checkboxInput(
            "plotdens",
            label = strong("Plot the densities"),
            value = TRUE
          ),
          helpText(
            '(Note that densities should only be plotted if there are enough datapoints to construct proper densities.)'
          ),
          radioButtons(
            "countdens",
            "Construction of densities",
            selected = 1,
            inline = TRUE,
            choiceNames = c('Densities', 'Counts'),
            choiceValues = c(1, 2)
          ),
          helpText(
            '(Selecting counts will allow the user to see how the number of observations per cluster compare to one another.)'
          ),
          sliderInput(
            "densinfl",
            label = "Density inflation factor",
            min = 0,
            max = 3,
            step = 0.01,
            value = 1
          ),
          
          radioButtons(
            "splinesmooth",
            "Make use of spline smoothing",
            selected = 1,
            inline = TRUE,
            choiceNames = c('Yes', 'No'),
            choiceValues = c(1, 2)
          ),
          fluidRow(column(
            1, checkboxInput("ownsmooth", label = "", value = FALSE)
          ),
          column(
            11,
            sliderInput(
              "ownsmoothval",
              min = 0,
              max = 3,
              step = 0.1,
              value = 1,
              label = "Choose smoothing parameter"
            )
          )),
          
        ),
        tabPanel(
          'Plotting Preferences',
          p(
            'The final tab, Plotting Preferences, contains all the additional options to adjust the overall look of the biplot. '
          ),
          checkboxInput(
            "plotellipse",
            label = strong("Plot ellipse"),
            value = FALSE
          ),
          checkboxInput(
            "plothull",
            label = strong("Plot convex hull"),
            value = FALSE
          ),
          checkboxInput(
            "varnames",
            label = strong("Show variable names"),
            value = TRUE
          ),
          checkboxInput(
            "plotqual",
            label = strong("Plot axis quality"),
            value = FALSE
          ),
          checkboxInput(
            "greyscale",
            label = strong("Convert plot to greyscale"),
            value = FALSE
          ),
          sliderInput(
            "namesize",
            "Size of variable names",
            min = 3,
            max = 20,
            value = 16,
            step = 1
          ),
          sliderInput(
            "ticksize",
            "Size of tickmarks",
            min = 3,
            max = 20,
            value = 12,
            step = 1
          ),
          radioButtons(
            "axvals",
            "Axes values",
            selected = 'Original',
            inline = TRUE,
            choiceNames = c('Original', 'Standardised'),
            choiceValues = c('Original', 'Standardised')
          ),
          sliderInput(
            "tickround",
            "Number of decimal places dispalyed for projected value",
            min = 0,
            max = 5,
            value = 3,
            step = 1
          ),
          sliderInput(
            "moreticks",
            "Adjustment factor for the number of tickmarks on each axis",
            min = 0.1,
            max = 10,
            value = 6,
            step = 0.1
          )
        )
      ),
      
      
    ),
    
    
    mainPanel(helpText(" "),
              plotlyOutput(
                "main", height = 900, width = 900
              ))
  ),
  tabPanel('Supplentary Material',
           tabsetPanel(
             tabPanel('UBbipl',tabsetPanel(
               tabPanel("Mtcars",plotOutput(outputId = 'pack1left',height=800,width=800)),
               tabPanel("xClara",plotOutput(outputId = 'pack1right',height=800,width=800)))),
             tabPanel('factoextra',tabsetPanel(
               tabPanel("Mtcars",plotOutput(outputId = 'pack2left',height=800,width=800)),
               tabPanel("xClara",plotOutput(outputId = 'pack2right',height=800,width=800)))),
             tabPanel('PLSbiplot1',tabsetPanel(
               tabPanel("Mtcars",plotOutput(outputId = 'pack3left',height=800,width=800)),
               tabPanel("xClara",plotOutput(outputId = 'pack3right',height=800,width=800)))),
             tabPanel('psych',tabsetPanel(
               tabPanel("Mtcars",plotOutput(outputId = 'pack4left',height=800,width=800)),
               tabPanel("xClara",plotOutput(outputId = 'pack4right',height=800,width=800)))),
             tabPanel('ade4',tabsetPanel(
               tabPanel("Mtcars",plotOutput(outputId = 'pack5left',height=800,width=800)),
               tabPanel("xClara",plotOutput(outputId = 'pack5right',height=800,width=800))))
           ))
))


server <- function(input, output, session)
{
  rv <<- reactiveValues()
  output$main <- renderPlotly({
    #choice <<- input$data
    if (datastart != input$data)
    {
      if (input$data == "Iris")
      {
        datain  <<-  Irisdata
        updateSliderInput(session, "mld", value = 2)
        updateSliderInput(session, "densinfl", value = 0.85)
        updateCheckboxInput(session, "ownsmooth", value = TRUE)
        updateSliderInput(session, "ownsmoothval", value = 0.5)
        updateCheckboxInput(session, "plotdens", value = TRUE)
        updateRadioButtons(session, "countdens", selected=2)
        plotobs_val=TRUE
        
      }
      if (input$data == "Xclara")
      {
        datain  <<-  Xclaradata
        updateSliderInput(session, "mld", value = 1)
        updateSliderInput(session, "densinfl", value = 0.26)
        updateCheckboxInput(session, "ownsmooth", value = TRUE)
        updateSliderInput(session, "ownsmoothval", value = 0.6)
        updateCheckboxInput(session, "plotdens", value = TRUE)
        updateRadioButtons(session, "countdens", selected=2)
        plotobs_val=FALSE
      }
        
      if (input$data == "Ocotea")
      {
        datain  <<-  Ocoteadata
        updateSliderInput(session, "mld", value = 1.4)
        updateSliderInput(session, "densinfl", value = 1)
        updateCheckboxInput(session, "ownsmooth", value = FALSE)
        updateSliderInput(session, "ownsmoothval", value = 1)
        updateCheckboxInput(session, "plotdens", value = FALSE)
        updateRadioButtons(session, "countdens", selected=1)
        plotobs_val=FALSE
      }
        
      if (input$data == "Mtcars")
      {
        datain  <<-  Mtcarsdata
        updateSliderInput(session, "mld", value = 2)
        updateSliderInput(session, "densinfl", value = 1)
        updateCheckboxInput(session, "ownsmooth", value = FALSE)
        updateSliderInput(session, "ownsmoothval", value = 1)
        updateCheckboxInput(session, "plotdens", value = FALSE)
        updateRadioButtons(session, "countdens", selected=1)
        plotobs_val=TRUE
      }
        
      if (input$data == "CopperFroth")
      {
        datain  <<-  Copperdata
        updateSliderInput(session, "mld", value = 1.7)
        updateSliderInput(session, "densinfl", value = 1)
        updateCheckboxInput(session, "ownsmooth", value = TRUE)
        updateSliderInput(session, "ownsmoothval", value = 0.6)
        updateCheckboxInput(session, "plotdens", value = TRUE)
        updateRadioButtons(session, "countdens", selected=2)
        plotobs_val=FALSE
      }
        
      rnames <<- rownames(datain)
      if (is.na(as.numeric(rnames[1])) == TRUE)
        rnames <<- sort(rnames)
      updatePickerInput(session,
                        "evinput",
                        choices = 1:(ncol(datain) - 1),
                        selected = c(1, 2))
      updateCheckboxInput(session, "plotobs", value = plotobs_val)
      updatePickerInput(session,
                        "plotobsnr",
                        choices = rnames,
                        selected = rnames[1])
      
      obsnumplot <<- NULL
      datastart <<- input$data
    }
    
    if (input$plotobs == TRUE)
    {
      obsnumplot <<- input$plotobsnr
    }
    else
    {
      obsnumplot <<- NULL
    }
    
    if (input$ownsmooth == TRUE)
    {
      smoothval <<- input$ownsmoothval
    }
    else
    {
      smoothval <<- NULL
    }
    
    if (length(input$evinput) == 2)
      evininput <<- as.numeric(input$evinput)
    
    rv$plotbiplot <<- exploding_bipl(
      datain[, -1],
      percentile_included = input$percentile /
        100,
      use_convex_hull = input$plothull,
      fact_ = input$densinfl,
      numticksfact = input$moreticks,
      round_ticks_to = input$tickround,
      ticks_in_original_units = (input$axvals ==
                                   "Original"),
      plot_the_dens = input$plotdens,
      move_lines_dist = input$mld,
      name = obsnumplot,
      plot_all_the_data = TRUE,
      tick_size = input$ticksize,
      cluster_var = as.vector(datain[, 1]),
      counts_or_densities = c('densities', 'counts')[as.numeric(input$countdens)],
      smoothing_method = c('smooth.spline', 0)[as.numeric(input$splinesmooth)],
      smooth_par = smoothval,
      draw_ellipse = input$plotellipse,
      variable_name_size = input$namesize,
      var_names = input$varnames,
      gray_scale = input$greyscale,
      EVchoice = as.numeric(evininput),
      plotqual = input$plotqual,
      scaledmat = TRUE
    )
    
    rv$plotbiplot$p
  })
  
  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("Plotly biplot - ", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      saveWidget(as_widget(rv$plotbiplot$p), file, selfcontained = TRUE)
    }
  )
  output$pack1left<-renderPlot(
    PCAbipl(
      Mtcarsdata[,-1],
      colours = 'black',
      pch.samples = 17,
      label.size = 0.8
    ), 
    res = 140
  )
  output$pack1right<-renderPlot(
    {
      PCAbipl.density(
        Xclaradata[,-1],
        G = indmat(Xclaradata[,1]),
        select.origin = FALSE,
        label = FALSE,
        #density.plot = TRUE,
        #pch.means = 21:23,
        specify.classes = 1:3,
        #means.plot=TRUE,
        #specify.ellipses =1:3,
        colours.density = c(gray(0.95), gray(0.6), gray(0.25)),
        draw.densitycontours = TRUE,
        pch.samples = rep('' , 3),
        exp.factor = 1.45
      )
    }, 
    res = 140
  )
  output$pack2left<-renderPlot(
    fviz_pca_biplot(PCA(Mtcarsdata[,-1], graph = FALSE), repel = TRUE), 
    res = 100
  )
  output$pack2right<-renderPlot(
    fviz_pca_biplot(
      PCA(Xclaradata[,-1], graph = FALSE),
      habillage = as.factor(Xclaradata[,1]) ,
      repel = TRUE,
      col.ind = Xclaradata[,1],
      label = 'var',
      addEllipses = TRUE,
      geom.var = c('arrow', 'text'),
    ), 
    res = 100
  )
  output$pack3left<-renderPlot(
    PCA.biplot(scale(Mtcarsdata[,-1]),
               method = mod.PCA,
               ax.tickvec.D = rep(5, length(Mtcarsdata[, 1]))), 
    res = 100
  )
  output$pack3right<-renderPlot(
    PCA.biplot(scale(Xclaradata[,-1]),
               method = mod.PCA,
               ax.tickvec.D = rep(8, length(Xclaradata[, 1]))), 
    res = 100
  )
  
  output$pack4left<-renderPlot(
    biplot.psych(
      fa(Mtcarsdata[,-1], 2, scores = TRUE),
      col = c('black', gray(0.6)),
      main = '',
      cuts = 0.1,
      labels = rownames(Mtcarsdata)
    ), 
    res = 100
  )
  output$pack4right<-renderPlot(
    biplot.psych(
      fa(Xclaradata[,-1], 2, scores = TRUE),
      col = c('black', gray(0.3)),
      smoother = TRUE,
      group = as.integer(Xclaradata[,1]),
      main = '',
      labels = rownames(Xclaradata)
    ), 
    res = 100
  )
  output$pack5left<-renderPlot({
    
    cars.try <<- dudi.pca(Mtcarsdata[,-1],scannf = FALSE, nf = 2)
    scatter(cars.try)
  }, 
  res = 140
    )
  
  output$pack5right<-renderPlot(
    {
    dat.try <<- dudi.pca(Xclaradata[,-1],scannf = FALSE, nf = 2)
    scatter(dat.try, clab.row = 0, posieig = "none")
    }, 
    res = 140
    )
  
  
  
}

shinyApp(ui = ui, server = server)


