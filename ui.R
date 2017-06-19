# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",

  # Application title
  navbarPage("PRIMAL",
    tabPanel("Load data", id="tab1", icon=icon("upload"),
      fluidRow(
        column(4, 
                fileInput('global_file', 'Choose file with all image descriptors', accept=c('text/comma-separated-values', '.csv')),
                fileInput('train_file', 'Choose file with ground-truth data', accept=c('text/comma-separated-values', '.csv')),
                # fileInput('test_file', 'Choose file with testing data', accept=c('text/comma-separated-values', '.csv')),
               checkboxInput('use_example', "Use example data from Atkinson, Lobet et al. 2017", value = F, width = NULL),
                bsButton(inputId = "load_data", type = "action", style="primary", label="Load data",icon("upload")),
               tags$hr(),
               textOutput("text0"),
               tags$head(tags$style("#text0{color: #28aa46;
                                   font-weight: bold;
                                   }"
               )
               ),
               tags$hr(),
               img(src='logo.jpg', align = "left", width="80%")
        ),
        column(7, 
               h4("Training / testing dataset"),
               tags$hr(),
               sliderInput('test_number', 'What proportion of the ground-truth data to use for the training [%]', min=5, max=95, value = 95), 
               textOutput("test_text"),
               tags$head(tags$style("#test_text{color: #28aa46;
                                   font-weight: bold;
                                    }"
               )),
               textOutput("train_text"),
               tags$head(tags$style("#train_text{color: #28aa46;
                                   font-weight: bold;
                                    }"
               )),
               tags$hr(),
               selectInput("to_plot_1", label = "Variable to plot", choices = c("Load datafile")),
               plotOutput("distribution_plot"),
               tags$hr(),
               helpText("Distribution of the ground-truth data for the training (grey shape) and test (red lines) dataset.
                        This allows you to check wether the test data falls corretly within the scope of the training data.")
               
               # DT::dataTableOutput('train_data')
        )#,
        # column(4, 
        #        h4("Testing datatable"),
        #        tags$hr(),
        #        helpText("This table contains the data that will be used for the testing the Random Forest model"),
        #        tags$hr(),
        #        textOutput("test_text"),
        #        tags$head(tags$style("#test_text{color: #28aa46;
        #                            font-weight: bold;
        #                             }"
        #        )),
        #        tags$hr(),
        #        DT::dataTableOutput('test_data')
        # )
      )
    ),
    tabPanel("Train the model", id="tab2", icon = icon('magic'),
        fluidRow(
          column(3, 
                 textOutput("test_text_1"),
                 tags$head(tags$style("#test_text_1{color: #28aa46;
                                      font-weight: bold;
                                      }"
               )),
               textOutput("train_text_1"),
               tags$head(tags$style("#train_text_1{color: #28aa46;
                                    font-weight: bold;
                                    }"
               )),                 
              selectInput("type_to_guess", label="Variables to use in analysis", choices = c("Load datafile"), 
                          selected = NULL, multiple = TRUE, width="100%"),
              sliderInput("vecmodels", "Number of models to try:",min = 5, max = 50, value = c(5,5), step = 5),
              sliderInput("vectrees", "Number of trees in each model:",min = 5, max = 50, value = c(5,5), step = 5),
              bsButton(inputId = "train_primal", type="action", style="success", label="Train PRIMAL",icon("magic")),
              tags$hr(),
              textOutput("text1"),
              tags$head(tags$style("#text1{color: #28aa46;
                                   font-weight: bold;
                                   }"
                         )
              ),
              tags$hr(),
              img(src='logo.jpg', align = "left", width="80%")
          ),
          column(5,
                 selectInput("to_plot", label = "Variable to plot", choices = c("Load datafile")),
                 plotOutput("regression_plot")
          ),
          column(4,
                 DT::dataTableOutput('accuracy_data'),
                 tags$hr(),
                 selectInput("indicator", label = "Indicator to plot", choices = c("R-square"="r2",
                                                                                   "Mean relative error"="rrmse",
                                                                                   "Pearson correlation"="pearson", 
                                                                                   "Spearman Ranked correlation"="spearman")),
                 plotOutput("accuracy_plot")
          )
        )
    ),
    tabPanel("Analyse the full dataset", id="tab3",icon = icon("bug"),
       fluidRow(
         column(3, 
            helpText("If you are satisfied with the output of the training, you can now applied to the rest of the dataset"),
            tags$hr(),
            bsButton(inputId = "run_primal", type= "action", style = "warning", label="Unleash PRIMAL",icon("bug")),
            tags$hr(),
            img(src='logo.jpg', align = "left", width="80%")
         ),
         column(9, 
             h4("Random Forest estimators"),
              tags$hr(),
              helpText("This table contains the data estimated using the Random Forest model"),
              downloadButton('download_model_data', 'Download full table'),
              tags$hr(),
              DT::dataTableOutput('model_data'),
              value=2
         )
       )
    ),
    tabPanel("About", id="tab4", icon=icon("plus-circle"),
      fluidRow(
        column(3),
        column(6,
            h4("What is PRIMAL"),
            helpText("PRIMAL stands for a Pipeline of Root Image analysis using MAchine Learning. In short, the pipeline use Machine Learning techniques (Random Forest in particular), tu streamline the image analysis of large root dataset. PRIMAL needs only a subset of the data to be analysed manually, instead of the full dataset. That subset is then used to train the algorithm behind PRIMAL the predict the parameters of interest, based on automatically acquired descriptors."),
            tags$hr(),
            h4("How to use PRIMAL"),
            helpText("We tried to make PRIMAL as user-friendly as possible. However, if you have any questions regading its use, you can check our webpage."),
            actionButton(inputId='ab1', label="PRIMAL webpage", icon = icon("cogs"), onclick ="window.open('https://plantmodelling.github.io/primal/', '_blank')"),
            tags$hr(),
            h4("How to cite PRIMAL"),
            tags$strong("Combining semi-automated root image analysis techniques with machine learning algorithms to accelerate large scale genetic root studies."),
            helpText("Jonathan A. Atkinson*, Guillaume Lobet*, Manuel Noll, Markus Griffiths, Darren M. Wells"),
            actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('#', '_blank')"),                                              
            tags$hr(),
            h4("Licence"),
            helpText("PRIMAL is released under a GPL licence, which means that redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met: 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.")                    
          )
      )
    )
  )
))
