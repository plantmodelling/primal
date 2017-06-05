#
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

options(shiny.maxRequestSize=30*1024^2) 

shinyServer(
  
  function(input, output, clientData, session) {  
    
    rs <- reactiveValues(global = NULL, 
                         test = NULL, 
                         train = NULL, 
                         rfmodel = NULL, 
                         var_to_plot = NULL,
                         estimators = NULL,
                         results = NULL,
                         accuracy = NULL)
    
    
    #------------------------------------------------------
    # UPDATE THE DYNAMIC FIELDS
    #------------------------------------------------------
    observe({
      if(is.null(rs$train)){return()}
      vars <- colnames(rs$train)[-1]
      ct_options <- list()
      sel <- input$type_to_guess
      if(length(sel) == 0) sel = vars
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "type_to_guess", choices = ct_options, selected=sel) 
    })    
    
    observe({
      if(is.null(rs$train)){return()}
      sel <- input$to_plot
      if(sel == "") sel = input$type_to_guess[1]
      updateSelectInput(session, "to_plot", choices = input$type_to_guess, selected = sel[1])
    })  
    
    
    
    
    #------------------------------------------------------
    #------------------------------------------------------
    # COMPUTATIONS
    #------------------------------------------------------
    #------------------------------------------------------ 
    
    
    observeEvent(input$load_data, {
      
      #------------------------------------------------------
      # LOAD THE USER DATA
      #------------------------------------------------------
      
      # Load datafiles
      withProgress(message = 'Loading data', {
        
        inGlobal <- input$global_file
        inTest <- input$test_file
        inTrain <- input$train_file

        if(!is.null(inGlobal)) global <- read_csv(inGlobal$datapath)
        if(!is.null(inTest)) test <- read_csv(inTest$datapath)
        if(!is.null(inTrain)) train <- read_csv(inTrain$datapath)

      }) 
      
      if(input$use_example){
        train <- read_csv("www/training_data.csv")
        test <- read_csv("www/test_data.csv")
        global <- read_csv("www/global_estimators.csv")
      }
      
      # Arrange the column names
      colnames(global)[colnames(global) %in% colnames(train)] <- paste0(colnames(global)[colnames(global) %in% colnames(train)],"1")
      colnames(global)[1] <- "id"
      colnames(train)[1] <- "id"
      colnames(test)[1] <- "id"
      
      if(!is.null(train) & !is.null(test) & !is.null(global)){
        rs$train <- train
        rs$test <- test
        rs$global <- global
      }else{
        
      }
    })
    
    
    
    
    # MACHINE LEARNING ANALYSIS
    
    observeEvent(input$train_primal, {
      
      if(is.null(rs$train)){return()}
      
      withProgress(message = 'Training the Trees', {
        vec.models <- seq(from=input$vecmodels[1], to=input$vecmodels[2], by=5)                 # Vector with the number of models to try
        vec.trees <- seq(from=input$vectrees[1], to=input$vectrees[2], by=5)                  # Vector with the number of tree to try in each model
        to_est <- input$type_to_guess         # Vector of parameters to estimate with the machine learning
        
        # Merge the train grond-truth and the train descriptors to perfome the random forest analysis
        id <- colnames(rs$global)[1]
        train <- merge(rs$train, rs$global, by=id)
        test <- rs$global[rs$global[[id]] %in% rs$test[[id]],]
        rs$test <- rs$test[rs$test[[id]] %in% rs$global[[id]],]
        print(dim(test))
        
        # Indices of the descriptors columns to used in the training. We do not take the first one as it contain the image id
        descrs <- colnames(rs$global)[-1]
        descr_ind <- match(descrs, colnames(train))
        to_est_ind <- match(to_est, colnames(train))
        descr_ind <- c(descr_ind, to_est_ind)
        
        models <- GenerateModels(fname = NULL, 
                                 mat.data = train, 
                                 vec.models = vec.models, 
                                 vec.trees = vec.trees, 
                                 vec.f = to_est_ind, 
                                 vec.p = descr_ind)
        
        vec.weights <- rep(1, length(to_est))
        
        model <- SelectModel(models, vec.weights)
        estimators <- PredictRFs(model, test)
        
        
        # Compute accuracy estimator for each variable
        accuracy <- NULL
        for(est in to_est){
          truth <- rs$test[[est]]
          estimation <- estimators[[est]]
          
          rel_diff <- abs((truth - estimation) / truth)
          rel_diff[is.infinite(rel_diff)] <- 0
          rrmse <- mean(rel_diff, na.rm = T)  
          
          accuracy <- rbind(accuracy, tibble(variable = est,
                                             r2 = round(summary(lm(estimation~truth))$r.squared,3),
                                             rrmse = round(rrmse,3),
                                             pearson = round(rcorr(estimation, truth, type = "pearson")[[1]][1,2],3),
                                             spearman = round(rcorr(estimation, truth, type = "spearman")[[1]][1,2],3)))
        }

        rs$estimators <- estimators
        rs$rfmodel <- model
        rs$accuracy <- accuracy
      })
      
    })
    
    
    # APPLYI THE TRAINED RANDOM FOREST ON THE WHOLE DATASET
    
    observeEvent(input$run_primal, {
      if(is.null(rs$rfmodel)){return()}
      
      withProgress(message = 'Using the Trees', {
        results <- PredictRFs(rs$rfmodel, rs$global)
        rs$results <- cbind(rs$global[,1], results)
          
      })
    
    })
    
    #------------------------------------------------------
    #------------------------------------------------------
    # PLOT
    #------------------------------------------------------
    #------------------------------------------------------ 
    
    output$regression_plot <- renderPlot({
      if(is.null(rs$estimators)){return()}
      
      temp <- tibble(x=rs$test[[input$to_plot]], y=rs$estimators[[input$to_plot]])

      pl <- ggplot(temp, aes(x, y)) + 
        geom_point() + 
        stat_smooth(method="lm", se=F) + 
        ylab("Estimation") + 
        xlab("Ground-truth") + 
        coord_fixed() +
        geom_abline(intercept = 0, slope=1, lty=2, colour="red") + 
        ggtitle(input$to_plot) + 
        theme_classic() +
        theme(text=element_text(size=15))
      
      pl
      
    })
    
    
    output$accuracy_plot <- renderPlot({
      if(is.null(rs$accuracy)){return()}
      
      temp <- rs$accuracy
      temp$value <- temp[[input$indicator]]
      
      hl <- 1
      if(input$indicator == "rrmse") hl <- 0
      
      pl <- ggplot(temp, aes(variable,value)) + 
        geom_point(data=temp[temp$variable == input$to_plot,], aes(variable,value), colour="red", size=5) + 
        geom_point(size=2) + 
        ylab("Accuracy estimator") + 
        xlab("") + 
        geom_hline(yintercept = hl, lty=2)+
        ggtitle(input$indicator) + 
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=15))
      pl
      
    })    
    
    
    
    #------------------------------------------------------
    #------------------------------------------------------
    # TABLES
    #------------------------------------------------------
    #------------------------------------------------------ 
    
    
    output$accuracy_data <- renderTable({
      if(is.null(rs$accuracy)){return()}
      temp <- rs$accuracy
      names <- colnames(temp)[-c(1,3)]
      
      for(n in names){
        temp[[n]][temp[[n]] >= 0.7] <- paste0('<div style="background-color: #a3cc9b;"><span>',temp[[n]][temp[[n]] >= 0.7],'</span></div>')
        temp[[n]][temp[[n]] < 0.7 & temp[[n]] >= 0.4] <- paste0('<div style="background-color: #fbfeaa;"><span>',temp[[n]][temp[[n]] < 0.7 & temp[[n]] >= 0.4],'</span></div>')
        temp[[n]][temp[[n]] < 0.4] <- paste0('<div style="background-color: #f3b686;"><span>',temp[[n]][temp[[n]] < 0.4],'</span></div>')
      }
      
      temp$rrmse[temp$rrmse <= 0.4] <- paste0('<div style="background-color: #a3cc9b;"><span>',temp$rrmse[temp$rrmse <= 0.4],'</span></div>')
      temp$rrmse[temp$rrmse > 0.4 & temp$rrmse <= 0.7] <- paste0('<div style="background-color: #fbfeaa;"><span>', temp$rrmse[temp$rrmse > 0.4 & temp$rrmse <= 0.7],'</span></div>')
      temp$rrmse[temp$rrmse > 0.7] <- paste0('<div style="background-color: #f3b686;"><span>', temp$rrmse[temp$rrmse > 0.7],'</span></div>')
      
      print(temp)
      
      temp
      
    }, sanitize.text.function = function(x) x) 
    
    
    
    
    output$model_data <- renderTable({
      if(is.null(rs$results)){return()}
        rs$results
    })
    output$download_model_data <- downloadHandler(
      filename = function() {"primal_results.csv"},
      content = function(file) {
        write.csv(rs$results, file)
      }
    )
  
})
