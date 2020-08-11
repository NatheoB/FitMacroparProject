shinyServer(function(input, output, session) {
    values <- reactiveValues()

    # Sidebar
   observeEvent(input$exitApp, {
       stopApp()
    })


    # Bibliography
    output$pdfviewerBiblio <- renderText({
        return(paste('<iframe style="height:600px; width:100%" src="',
                     "resources/bibliography.pdf",
                     '"></iframe>', sep = ""))
    })


    # App
    observeEvent(input$validateMaxBurden, {
        values$createdData <- data.frame(
            paraPerHost = 0:input$dataMaxBurden,
            freq = rep(as.integer(0), input$dataMaxBurden + 1)
        )
    })

    output$dlCreatedData <- downloadHandler(
        filename = function() {
            paste(values$data.name(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv2(values$createdData, file, row.names = FALSE)
        }
    )

    observe({
        if (!is.null(input$dataCreatedInput)) {
            DF <- hot_to_r(input$dataCreatedInput)
        } else {
            if (is.null(values$createdData)) {
                DF <- data.null
            } else {
                DF <- values$createdData
            }

        }
        values$createdData <- DF
    })

    output$dataCreatedInput <- renderRHandsontable({
        DF <- values$createdData
        if (!is.null(DF)) {
            rhandsontable(DF, height = 200, rowHeaders = NULL) %>%
                hot_col("paraPerHost", readOnly = TRUE) %>%
                hot_context_menu(allowRowEdit = FALSE) %>%
                hot_validate_numeric(cols = 2, min = 0)
        }
    })

    values$data.name <- reactive(ifelse(input$dataName == "", "Data", input$dataName))


        # Chosen dataset
    data.original <- eventReactive(input$submitData, {
        # Create original data
        type <- input$dataChoiceInput
        if (type == "1") {
            return(values$createdData)
        } else if (type == "2") {
            if (is.null(input$dataFileInput)) {
                return(data.null)
            }
            return(read.csv2(input$dataFileInput$datapath))
        }
        read.csv2(
            paste("www/resources/data/", data.names[[as.numeric(input$dataExampleInput)]], ".csv", sep = ""))
    })


        #Create a vector with all sampling from data
    data.sampling <- reactive(rep(data.original()$paraPerHost, data.original()$freq))

        # Create a vector of frequency
    data.frequency <- reactive(data.original()$freq)
    data.sampleSize <- reactive(sum(data.frequency()))


    output$dataTitleOutput.overview <- renderUI({
        if (!is.null(data.original()))
            h2(values$data.name())
    })

        # Output matrix to show
    matrix.output <- reactive(t(as.matrix(data.original())))

    # Table output
    output$table <- renderTable({
        out <- matrix.output()
        out <- isolate(out)
        rownames(out) <- c("Frequency", "No of parasites / host")
        out
    }, rownames = TRUE, colnames = FALSE)

    # Stats output
    output$stats <- renderPrint({
        summary(data.sampling())
    })

    # Save fits
    output$dataTitleOutput.pars <- renderUI({
        helpText(values$data.name())
    })

    observeEvent(input$fit.all, {
        fit.mle.pois <- fitdist(data.sampling(), "pois", method = "mle")
        updateNumericInput(session, "pars.pois.lambda", value = fit.mle.pois$estimate[1][[1]])

        fit.mle.nb <- fitdist(data.sampling(), "nbinom", method = "mle")
        updateNumericInput(session, "pars.nb.size", value = fit.mle.nb$estimate[1][[1]])
        updateNumericInput(session, "pars.nb.mu", value = fit.mle.nb$estimate[2][[1]])

        fit.mle.geom <- optimize(f = NLL.geom, interval = c(0,1), data = data.sampling())
        updateNumericInput(session, "pars.geom.M0", value = fit.mle.geom$minimum)

        estimate.HLmodel <- find.estimate.HLmodel(data.sampling(), input$HLnit)
        updateNumericInput(session, "pars.HLmodel.force", value = estimate.HLmodel["force"][[1]])
        updateNumericInput(session, "pars.HLmodel.mu", value = estimate.HLmodel["mu"][[1]])
        updateNumericInput(session, "pars.HLmodel.gamma", value = estimate.HLmodel["gamma"][[1]])
    })

    output$dlParams <- downloadHandler(
        filename = function() {
            paste(values$data.name(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv2(values$params(), file, row.names = FALSE)
        }
    )

    values$params <- reactive({
        df <- data.frame(
            Models = c("Poisson", "NBD", "NBD", "Geom", "HLmodel", "HLmodel", "HLmodel"),
            Parameters = c("Lambda", "Size", "Mu", "M0", "Force", "Mu", "Gamma"),
            Values = c(input$pars.pois.lambda,
                       input$pars.nb.size, input$pars.nb.mu,
                       input$pars.geom.M0,
                       input$pars.HLmodel.force, input$pars.HLmodel.mu, input$pars.HLmodel.gamma)
        )
        df
    })

    # Fit data button

    observeEvent(input$fit.pois, {
        fit.mle.pois <- fitdist(data.sampling(), "pois", method = "mle")
        updateNumericInput(session, "pars.pois.lambda", value = fit.mle.pois$estimate[1][[1]])
    })

    observeEvent(input$fit.nb, {
        fit.mle.nb <- fitdist(data.sampling(), "nbinom", method = "mle")
        updateNumericInput(session, "pars.nb.size", value = fit.mle.nb$estimate[1][[1]])
        updateNumericInput(session, "pars.nb.mu", value = fit.mle.nb$estimate[2][[1]])
    })

    observeEvent(input$fit.geom, {
        fit.mle.geom <- optimize(f = NLL.geom, interval = c(0,1), data = data.sampling())
        updateNumericInput(session, "pars.geom.M0", value = fit.mle.geom$minimum)
    })

    observeEvent(input$fit.HLmodel, {
        estimate.HLmodel <- find.estimate.HLmodel(data.sampling(), input$HLnit)
        updateNumericInput(session, "pars.HLmodel.force", value = estimate.HLmodel["force"][[1]])
        updateNumericInput(session, "pars.HLmodel.mu", value = estimate.HLmodel["mu"][[1]])
        updateNumericInput(session, "pars.HLmodel.gamma", value = estimate.HLmodel["gamma"][[1]])
    })

    # Max K of histogram
    output$maxK.out <- renderUI({
        sliderInput(
            "maxK.in", "Maximum x-axis",
            min = 0, max = length(data.frequency())-1,
            value = length(data.frequency())-1)
    })

    # Reactive discrete probability distribution and reactive frequencies
    khi2 <- reactiveValues()

    # Create Poisson data
    dpd.pois <- reactive({
        dpois(
            0:(length(data.frequency())-1),
            lambda = input$pars.pois.lambda
        )
    })
    freq.pois <- reactive(dpd.pois() * data.sampleSize())
    gfreq.ind.pois <- reactive(freq.group.index(freq.pois()))
    gfreq.pois <- reactive(freq.group.distrib(
        freq.pois(), gfreq.ind.pois()[1], gfreq.ind.pois()[2]))
    gfreq.data.pois <- reactive(freq.group.distrib(
        data.frequency(), gfreq.ind.pois()[1], gfreq.ind.pois()[2]))

     observe({
        stat <- reactive(chisq.calc.statistic(gfreq.pois(), gfreq.data.pois()))
        df <- reactive(length(gfreq.pois()) - 1 - 1)
        pvalue <- reactive(chisq.calc.pvalue(stat(), df()))
        khi2$pois <- reactive(cbind(stat(), df(), pvalue()))
    })

    # Create NBD data
    dpd.nb <- reactive({
        dnbinom(
            0:(length(data.frequency())-1),
            size = input$pars.nb.size, mu = input$pars.nb.mu
        )
    })
    freq.nb <- reactive(dpd.nb() * data.sampleSize())
    gfreq.ind.nb <- reactive(freq.group.index(freq.nb()))
    gfreq.nb <- reactive(freq.group.distrib(
        freq.nb(), gfreq.ind.nb()[1], gfreq.ind.nb()[2]))
    gfreq.data.nb <- reactive(freq.group.distrib(
        data.frequency(), gfreq.ind.nb()[1], gfreq.ind.nb()[2]))

    observe({
        stat <- reactive(chisq.calc.statistic(gfreq.nb(), gfreq.data.nb()))
        df <- reactive(length(gfreq.nb()) - 2 - 1)
        pvalue <- reactive(chisq.calc.pvalue(stat(), df()))
        khi2$nb <- reactive(cbind(stat(), df(), pvalue()))
    })

    # Create Geom data
    dpd.geom <- reactive({
        M.geom(
            0:(length(data.frequency())-1),
            M0 = input$pars.geom.M0
        )
    })
    freq.geom <- reactive(dpd.geom() * data.sampleSize())
    gfreq.ind.geom <- reactive(freq.group.index(freq.geom()))
    gfreq.geom <- reactive(freq.group.distrib(
        freq.geom(), gfreq.ind.geom()[1], gfreq.ind.geom()[2]))
    gfreq.data.geom <- reactive(freq.group.distrib(
        data.frequency(), gfreq.ind.geom()[1], gfreq.ind.geom()[2]))

    observe({
        stat <- reactive(chisq.calc.statistic(gfreq.geom(), gfreq.data.geom()))
        df <- reactive(length(gfreq.geom()) - 1 - 1)
        pvalue <- reactive(chisq.calc.pvalue(stat(), df()))
        khi2$geom <- reactive(cbind(stat(), df(), pvalue()))
    })



    # Create NBD data
    dpd.HLmodel <- reactive({
        M.HLmodel(
            0:(length(data.frequency())-1),
            pars = c(
                force = input$pars.HLmodel.force,
                mu = input$pars.HLmodel.mu,
                gamma = input$pars.HLmodel.gamma
            )
        )
    })
    freq.HLmodel <- reactive(dpd.HLmodel() * data.sampleSize())
    gfreq.ind.HLmodel <- reactive(freq.group.index(freq.HLmodel()))
    gfreq.HLmodel <- reactive(freq.group.distrib(
        freq.HLmodel(), gfreq.ind.HLmodel()[1], gfreq.ind.HLmodel()[2]))
    gfreq.data.HLmodel <- reactive(freq.group.distrib(
        data.frequency(), gfreq.ind.HLmodel()[1], gfreq.ind.HLmodel()[2]))

    observe({
        stat <- reactive(chisq.calc.statistic(gfreq.HLmodel(), gfreq.data.HLmodel()))
        df <- reactive(length(gfreq.HLmodel()) - 3 - 1)
        pvalue <- reactive(chisq.calc.pvalue(stat(), df()))
        khi2$HLmodel <- reactive(cbind(stat(), df(), pvalue()))
    })


    # Khi2 table
    khi2$data <- reactive(cbind(khi2$pois(), khi2$nb(), khi2$geom(), khi2$HLmodel()))
    output$khi2 <- renderTable({
        mat <- matrix(
                data = khi2$data(),
                byrow = TRUE, nrow = 4,
                dimnames = list(
                    c("Poisson", "Negative binomial", "Geom", "HLmodel"),
                    c("Statistic", "df", "pValue")))
        mat <- as.data.frame(mat)
        mat$df <- as.integer(mat$df)
        values$khi2 <- mat
        mat
    }, rownames = TRUE, digits = 5)

    output$dlKhi2 <- downloadHandler(
        filename = function() {
            paste(values$data.name(), "_khi2.csv", sep = "")
        },
        content = function(file) {
            write.csv2(values$khi2, file, row.names = FALSE)
        }
    )

    # Initialize histogram
    observe({
        if (!is.null(input$maxK.in)) {
            values$plot.y <- rbind(
                freq.pois()[1:(input$maxK.in+1)],
                freq.nb()[1:(input$maxK.in+1)],
                freq.geom()[1:(input$maxK.in+1)],
                freq.HLmodel()[1:(input$maxK.in+1)],
                data.frequency()[1:(input$maxK.in+1)])
        }
    })


    # Plot histrogram
    output$hist <- renderPlot({
        if (length(input$hist.models) != 0 && !is.null(input$maxK.in)) {
            barplot(values$plot.y[as.numeric(input$hist.models),], beside = TRUE,
                    col = plot.cols[as.numeric(input$hist.models)],
                    xlab = "No of parasites / host", ylab = "Frequency",
                    legend.text = plot.grades[as.numeric(input$hist.models)],
                    main = values$data.name())
        }
    })

    # Models comparison
    # AIC
    values$AIC <- reactive({
        c(calc.AIC(dpd.pois()[data.sampling()+1], 1),
          calc.AIC(dpd.nb()[data.sampling()+1], 2),
          calc.AIC(dpd.geom()[data.sampling()+1], 1),
          calc.AIC(dpd.HLmodel()[data.sampling()+1], 3))
    })
    values$AIC.rank <- reactive(as.integer(rank.models(values$AIC())))

    # BIC
    values$BIC <- reactive({
        c(calc.BIC(dpd.pois()[data.sampling()+1], 1, data.sampleSize()),
          calc.BIC(dpd.nb()[data.sampling()+1], 2, data.sampleSize()),
          calc.BIC(dpd.geom()[data.sampling()+1], 1, data.sampleSize()),
          calc.BIC(dpd.HLmodel()[data.sampling()+1], 3, data.sampleSize()))
    })
    values$BIC.rank <- reactive(as.integer(rank.models(values$BIC())))

    # Results

    # Render models comparison
    output$comparison <- renderTable({
        values$comparison <- data.frame(
            AIC = values$AIC(),
            AIC.rank = values$AIC.rank(),
            BIC = values$BIC(),
            BIC.rank = values$BIC.rank(),
            row.names = c("Poisson", "Negative binomial", "Geom", "HLmodel"))
        values$comparison
    }, rownames = TRUE)

    output$dlComp <- downloadHandler(
        filename = function() {
            paste(values$data.name(), "_comparison.csv", sep = "")
        },
        content = function(file) {
            write.csv2(values$comparison, file, row.names = FALSE)
        }
    )

    # Results
    output$data_description <- DT::renderDataTable({
        DT::datatable(data = data.description,
                      options = list(scrollX = TRUE)
        )
    })
    output$data_khi2 <- DT::renderDataTable({
        DT::datatable(data = data.khi2,
                      options = list(scrollX = TRUE)
        )
    })
    output$data_ranking <- DT::renderDataTable({
        DT::datatable(data = data.ranking,
                      options = list(scrollX = TRUE)
        )
    })
    output$data_param <- DT::renderDataTable({
        DT::datatable(data = data.param,
                      options = list(scrollX = TRUE)
        )
    })

    output$dlFullResults <- downloadHandler(
        filename = "all_results.xlsx",
        content = function(file) {
            file.copy("www/resources/results/all_results.xlsx", file)
        }
    )
})
