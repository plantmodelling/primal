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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pipeline of Root Image analysis using MAchine Learning >> PRIMAL"),
  tags$hr(),
  fluidRow(
    column(3, 
           tabsetPanel(
             
             tabPanel("1. Loading",
                      tags$hr(),
                      fileInput('global_file', 'Choose file with all image descriptors', accept=c('text/comma-separated-values', '.csv')),
                      fileInput('train_file', 'Choose file with training data', accept=c('text/comma-separated-values', '.csv')),
                      fileInput('test_file', 'Choose file with testing data', accept=c('text/comma-separated-values', '.csv')),
                      actionButton(inputId = "load_data", label="Load data",icon("upload"),
                                   style="color: #fff; background-color: #28aa46; border-color: #258c3e")

             ),
             tabPanel("2. Training",
                      tags$hr(),
                      selectInput("type_to_guess", label="Variables to use in analysis", choices = c("Load datafile"), 
                                             selected = NULL, multiple = TRUE, width="100%"),
                     sliderInput("vecmodels", "Number of models to try:",min = 5, max = 50, value = c(5,5), step = 5),
                     sliderInput("vectrees", "Number of trees in each model:",min = 5, max = 50, value = c(5,5), step = 5),
                     actionButton(inputId = "train_primal", label="Train PRIMAL",icon("magic"), 
                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                            
             ),
             tabPanel("3. Analysing",
                      tags$hr(),
                      helpText("If ylu are satisfied with the output of the training, you can now applied to the rest of the dataset"),
                      tags$hr(),
                      actionButton(inputId = "run_primal", label="Unleash PRIMAL",icon("resistance"), 
                                   style="color: #fff; background-color: #d9534f; border-color: #d43f3a")
             )
           ),
           tags$hr(),
           img(src='logo.jpg', align = "left", width="80%")
    ),
    column(4,
           selectInput("to_plot", label = "Variable to plot", choices = c("Load datafile")),
           plotOutput("regression_plot"),
           selectInput("indicator", label = "Indicator to plot", choices = c("R-square"="r2",
                                                                           "Mean relative error"="rrmse",
                                                                           "Pearson correlation"="pearson", 
                                                                           "Spearman Ranked correlation"="spearman")),
           plotOutput("accuracy_plot")
    ),
    column(5,
           tabsetPanel(

             tabPanel("Random Forest accuracy",
                      tags$hr(),
                      helpText("This table contains the accuracy parameters for the Random Forest model"),
                      tags$hr(),
                      tableOutput('accuracy_data'),
                      value=2
             ),
             
             tabPanel("Random Forest estimators",
                      tags$hr(),
                      helpText("This table contains the data estimated using the Random Forest model"),
                      downloadButton('download_model_data', 'Download full table'),
                      tags$hr(),
                      tableOutput('model_data'),
                      value=2
             ),
             tabPanel("About",
                      h4("What is PRIMAL"),
                      helpText("PRIMAL is "),
                      tags$hr(),
                      h4("How to use PRIMAL"),
                      helpText("We tried to make PRIMAL as user-friendly as possible. However, if you have any questions regading its use, you can check our webpage."),
                      actionButton(inputId='ab1', label="PRIMAL webpage", icon = icon("cogs"), onclick ="window.open('https://mecharoot.github.io/', '_blank')"),
                      tags$hr(),
                      h4("How to cite PRIMAL"),
                      tags$strong("A novel hydraulic model of plant root cross-sections brings biological information in soil-plant water dynamics A novel hydraulic model of plant root cross-sections brings biological information in soil-plant water dynamics"),
                      helpText("Valentin Couvreur, Marc Faget, Guillaume Lobet, Mathieu Javaux, François Chaumont and Xavier Draye"),
                      actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('#', '_blank')"),                                              
                      tags$hr(),
                      h4("Licence"),
                      helpText("PRIMAL is released under a GPL licence, which means that redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met: 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.")                    
             )
             
           )
    )
  )
))
