#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# I put everything into one file for easier deployment
# code is sourced from my main project

# required libraries
library(shiny)
library(ggplot2)
library(waiter)
library(metaheuristicOpt)

################################################################################
# Global variables for app
################################################################################
models = c(
  "Logistic",
  "Log-logistic",
  "Probit",
  "Log-probit",
  "Weibull",
  "Multistage 1",
  "Multistage 2",
  "Multistage 3",
  "Hill",
  "Logistic quadratic",
  "Logistic cubic",
  "4 parameter log-logistic"
  #"Logistic fractional polynomial"
  #"Mixture multistage"
  #"Box-Cox Weibull"
)

objectives = c(
  "D",
  "A"
)

algorithms = c(
  "PSO",
  "DE"
  #"GWO",
  #"HS"
)

bmd_models = c(
  "Logistic",
  "Weibull",
  "Log-logistic",
  "Hill"
)

################################################################################
# UI and server functions for app
################################################################################

ui = fixedPage(
  withMathJax(),
  tabsetPanel(
    selected = "Find optimal designs",
    type = "tabs",
    id = "toptabpanel",
    tabPanel(
      "User manual",

      tags$h2("Overview of the app", style = "text-align:left;"),
      tags$p(
        "This app allows the user to find optimal experimental designs for
          nonlinear models used in toxicology and for estimating the benchmark
          dose. It also provides a tool for
          comparing the efficiency of any two designs. This page explains how to use
          the app and the optimal design theory used to generate and interpret the results."
      ),
      tags$p(
        "An optimal design for a dose-response experiment is a set of doses and
        the proportion of subjects at each dose to efficiently fit a statistical model given a fixed sample size.
        For example, we might want to find the best doses and subject allocation to estimate
        a log-logistic model. The benefit of using an optimal design is the design
        will maximize the information gained from the experiment. This can also
        reduce the cost of the experiment since a well designed experiment with a
        small number of subjects can be more effective than a bad experimental design
        with a large sample size. For more information, see the optimal design reference
        at the bottom of this tab."
      ),
      tags$p(
        "This app uses metaheuristic algorithms, which are a class of
          optimization methods that have been widely used
          to solve difficult problems. They are inspired by natural phenomena and simulate the
          behavior of a group of entities, such as animals, insects, or particles,
          in search of an optimal solution. Two examples of metaheuristic algorithms
          are Particle Swarm Optimization (PSO) and Differential Evolution (DE).
          They are especially useful in optimal design of experiments because they can easily be applied to a wide variety
          of design problems."
      ),
      tags$h3("Design tab", style = "text-align:left;"),
      tags$p(
        "The design tab allows the user to use metaheuristic algorithms to find
          the optimal design for a selected nonlinear model. The sidebar allows
          the user to set several algorithm options. The algorithm selector chooses
          metaheuristic to use to find the design and the iterations and swarm size
          options tell the algorithm how long to run and the diversity of the search.
          Increasing both the number of iterations and the swarm size will make it
          more likely that the optimal design will be found but increases the run
          time. The dose limit option controls the highest dose allowed.
          The limit should be chosen based on background knowledge of the
          experiment. A lower dose limit will limit the search space and make the
          optimal design easier to find. The design points option controls how
          many doses will be allowed in the optimal design. If too few doses are
          specified, then the optimal design might be impossible to find. A general
          rule of thumb is that the number of doses in the optimal design should
          be equal to the number of parameters in the model."
      ),
      tags$p(
        "The main pane allows the user to select the model, parameter values,
          and the design objective. Theta values should be entered separated by
          commas, keeping in mind realistic bounds for the parameters.
          Pressing the find design button will run the selected algorithm
          options on the design problem, display the final design, and display a
          graphical check of optimality.
          If plotted function is less than 0 with equality at the doses in the design,
          then the optimal design has been found. If the design is not optimal,
          try re-running with more iterations and a larger swarm size. A different
          algorithm might also help to find the optimal design. If the plot displays
          a message about a singular information matrix, then there is likely a
          problem with how the design problem is specified. The most common issue
          is that the number of doses is too few. Another common issue is that
          the dose range may be too small for the model in question."
      ),
      tags$p(
        "Some example theta values are shown for each model. These parameter values
        were obtained by fitting the models to data from Table 3 from Haber et. al. (2018).
        Models were fit using R and the EPA's Benchmark Dose Software."
      ),
      tags$h3("Compare tab", style = "text-align:left;"),
      tags$p(
        "The compare tab allows the user to compare two designs to see which
          is more efficient. This is useful when comparing
          a naive or more practical design against the optimal design.
          Design 1 is compared relative to design design 2.
          If design 1 is less efficient, then the efficiency will be less than 1.
          If design 1 is more efficient, then the efficiency will be greater than 1.
          Parameter values, dose levels, and weights should be entered as comma
          separated values."
      ),
      tags$h3("BMD designs tab", style = "text-align:left;"),
      tags$p("This tab allows user to find designs for estimating the benchmark
             dose (BMD). The options here are similar to the other design tab, but
             there are a few features unique to the BMD designs. The BMD is defined
             as the dose associated with a
             pre-specified increase in the probability of toxicity compared to the
             zero dose. There are two
             ways to define this increase in risk. Added risk is defined as
             $$
             r_{add} = P(d) - P(0)
             $$
             and extra risk is defined as
             $$
             r_{extra} = \\frac{P(d) - P(0)}{1 - P(0)}
             $$
             The user can choose which risk definition to use by using the risk
             type selector. It is also required to specify a risk increase between
             0 and 1. Values of 0.05 and 0.10 are common choices."),
      tags$p("The designs found in this tab are compound designs between c and D
             optimality. The compound design is needed because the c-optimal
             design for estimating the BMD is often inadequate for fitting the model.
             For more details, see the section on compound designs in the optimal
             design reference. The user must choose a value between 0 and 1
             for the weighting parameter \\(\\lambda\\). A value of 0 gives the
             D-optimal design while values close to 1 estimate the BMD more efficiently.
             It is recommended to choose a \\(\\lambda\\) that gives at least 80%
             D-efficiency."),

      tags$h2("Optimal design reference", style = "text-align:left;"),
      tags$p(
        "
          Suppose \\(Y\\) is an outcome variable of interest and \\(d\\) is the dosage or
          concentration of the substance under investigation. We
          additionally assume that \\(Y\\) is a binary variable that is equal to one
          if toxicity has occurred and equal to 0 otherwise. We also assume the
          that the dose \\(d\\) is in the range \\([0, D]\\) where \\(D\\) is highest
          experimentally feasible dose. The goal of a dose response experiment
          is to establish a mathematical relationship of the form
$$
P(Y = 1| d) = P(d) = f(d, \\theta) + \\epsilon
$$
where \\(f\\) is a possibly nonlinear function of the dose and parameters \\(\\theta\\).
The error term \\(\\epsilon\\) is assumed to have a normal distribution,
\\(N(0, \\sigma^2)\\). Common forms of \\(f\\) include the exponential and inverse logit functions.

The goal of optimal design is to choose good values for the doses included in the
experiment and the number of subjects to assign to each dose. Let \\(d_i\\) denote the
ith dose in the design and let \\(n_i\\) be the number of subjects assigned to the ith
dose for \\(i = 1, \\dots, k\\).  For theoretical convenience,
we assume a fixed sample size \\(N\\) and work with the proportions assigned to each
dose instead of the raw \\(n_i\\). Define \\(w_i\\) as \\(w_i = n_i/N\\) such that \\(\\sum_i^k w_i = 1\\).
Using this weight-based notation, we can define a \\(k\\) dose design as a probability measure on \\([0, D]\\).
$$
\\xi = \\begin{pmatrix}
d_1, & \\dots, & d_k\\\
w_1, & \\dots, & w_k
\\end{pmatrix}
$$

The design \\(\\xi\\) contains all of the variables that must be
chosen optimally in order to maximize a design objective function.
Objective functions in optimal design are based on the model information matrix,
\\(M(\\xi, \\theta)\\). This information matrix can be thought of as the observed
information matrix if the experiment is run using the design \\(\\xi\\). Note that \\(M(\\xi, \\theta)\\)
also depends on the parameter values of the model, meaning that a prior value for
the parameters is required. This value can come from previous data or theoretical
models. The designs generated are referred to as \\(\\textit{locally optimal}\\) with
respect to \\(\\theta\\), meaning that the design is only optimal if \\(\\theta\\)
is the true value. In light of this fact, sensitivity analyses should be performed
to see how variation in \\(\\theta\\) might affect the design.
"
      ),
      tags$h3("Objective functions", style = "text-align:left;"),
      tags$p("Objective functions for optimal design are chosen to minimize the
          variance of estimated quantities or, equivalently, to maximize the information
          gain of the experiment. For example, a common design strategy is to minimize
$$
\\Psi_D(M) = -\\log |M|.
$$
Minimizing \\(\\Psi_D\\) is equivalent to minimizing the size of the confidence
region of the parameter estimates. A design that minimizes \\(\\Psi_D\\) is called
D-optimal. Another approach is to minimize the objective function
$$
\\Psi_A(M) = \\operatorname{tr} M^{-1}.
$$
which is equivalent to minimizing the sum of the variances of the parameter estimates.
Another useful objective function is
$$
\\Psi_c(M) = c'M^{-1}c
$$
which minimizes the variance of some linear combination
of the parameters. The \\(\\Psi_c\\) objective is useful is because it can be
applied to construct a wide variety of domain specific objectives."
             ),
      tags$h3("Design efficiency", style = "text-align:left;"),
      tags$p("In order to make comparisons between designs, it is useful to consider
 efficiency relative to the optimal design. For example, we may wish to see how a
 naive design compares to the optimal design. In the case of D-optimality,
 let \\(\\xi_D\\) be the D-optimal design. The D-efficiency of a design \\(\\xi\\) is defined as
 $$
 \\operatorname{eff}_D(\\xi) = \\left(\\frac{|\\Psi_D(\\xi)|}{|\\Psi_D(\\xi_D)|}\\right)^{1/p}
 $$
 where \\(p\\) is the number of parameters in the model. Similar efficiency functions
 exist for the other optimality criteria. The design efficiency that takes a value
             between 0 and 1, with values close to 1 signifying the design is close
             to optimal."),
      tags$h3("Equivalence theorem", style = "text-align:left;"),
      tags$p("A final useful tool for optimal design is the equivalence theorem of Kiefer (1960).
 This theorem says that if a design \\(\\xi\\) is optimal, then the directional
 derivative of the design criterion evaluated at \\(\\xi\\) must satisfy an inequality
 for all dose values in the design space with equality attained when the dose is
 in the design. This provides an easy graphical test to check if the design is
 optimal. An optimal design will produce a plot where the plotted function
             curve stays below the y=0 line except at the doses in the design."),
      tags$h3("Compound designs", style = "text-align:left;"),
      tags$p("A major issue with c-optimal designs is that they often result in
             designs that are unable to fit the dose-response model. For example,
             the c-optimal design for estimating the BMD is usually a single dose
             design. A compound design criterion addresses this issue by using a
             weighted combination of c and D-optimality. The compound criterion
             is
             $$
             \\Psi_{CD}(M) = (1-\\lambda)\\log\\left[ \\text{eff}_D (M)\\right] + \\lambda \\log \\left[\\text{eff}_c (M)\\right]
             $$
             The weighting parameter \\(\\lambda\\) ranges between 0 and 1 and
             can be used to set how much the design deviates from the D-optimal
             design. The designs obtained using this criterion are very similar to
             the D-optimal design with the same number of doses, but the designs are
             usually no longer equally weighted. It is recommended to choose
             \\( \\lambda\\) such that the design obtains a pre-specified level
             of D-efficiency.
             ")

    ),
    tabPanel(
      "Find optimal designs",

      tabsetPanel(
        selected = "Design",
        type = "pills",
        id = "designtabpanel",
        tabPanel("Design",
                 # sidebar layout for algorithm options
                 sidebarLayout(
                   sidebarPanel(
                     tags$h3("Algorithm options"),
                     selectInput("algorithm",
                                 "Algorithm",
                                 algorithms),
                     numericInput("iter",
                                  "Iterations",
                                  200,
                                  1,
                                  Inf,
                                  10),
                     numericInput("swarm",
                                  "Swarm size",
                                  30,
                                  10,
                                  Inf,
                                  10),
                     numericInput("bound",
                                  "Dose limit",
                                  30,
                                  0.1,
                                  Inf,
                                  1),
                     numericInput("pts",
                                  "Max design points",
                                  3,
                                  1,
                                  10,
                                  1),
                     numericInput("seed",
                                  "Seed",
                                  155,
                                  1,
                                  Inf,
                                  1),
                     numericInput(
                       "N",
                       "N",
                       min = 1,
                       max = 1000,
                       value = 50
                     )

                   ),
                   mainPanel(
                     "This tab is for finding A and D optimal designs for
                     various dose response models. Prior values for the model
                     parameters (theta) must be supplied. Theta values can be obtained
                     from dedicated model fitting software or
                     from published results. Check that the theta values have been
                     entered correctly by plotting the response. Once the theta values have been entered,
                     the design can be found by using the find design button. Note
                     that these designs are locally optimal with respect to theta
                     and will not be optimal if theta is misspecified. For
                     more information on how to use the other options and tips, refer to
                     the design tab section in the user manual tab.",
                     fluidRow(
                       column(
                         6,
                         selectInput("model_selector", "Model", models)
                       ),
                       column(
                         6,
                         textInput("theta_input", "Theta ( enter values separated by , )"),
                         selectInput("objective", "Objective", objectives)
                       )
                     ),
                     uiOutput("model_formula_display"),
                     uiOutput("example_theta"),
                     actionButton("find", "Find design"),
                     actionButton("plot_response", "Plot response"),
                     checkboxInput("log_dose_check", "Plot mean response on log dose scale"),
                     plotOutput("sens_plot"),
                     waiter::use_waiter(),
                     verbatimTextOutput("design_out"),
                     actionButton("copy_design", "Copy design to compare tab")
                   )
                 )),
        tabPanel(
          "Compare",
          "Compare the efficiency of design 1 relative to the
          design 2. The efficiency is a number between 0 and 1 where
          a value close to 1 means that design 1 is nearly as good as design 2.
          If the efficiency is greater than 1, it implies that design 1 is better
          than design design 2",
          "The doses and weights may be obtained using the
          other tabs and entered as comma separated values. Design 2 may
          also be filled by using the copy button in the design tabs. For more
          information, refer to the compare tab section in the user manual tab.",
          selectInput("model_selector_compare", "Model", models),
          textInput("theta_input_compare", "Theta ( enter values separated by , )"),
          selectInput("objective_compare", "Objective", objectives),
          fluidRow(
            column(
              6,
              textInput("\\xi1_doses", "Design 1 doses"),
              textInput("\\xi1_weights", "Design 1 weights")
            ),
            column(
              6,
              textInput("\\xi2_doses", "Design 2 doses"),
              textInput("\\xi2_weights", "Design 2 weights")
            ),
            actionButton("compute_eff", "Compute efficiency"),
            #textOutput("eff_out"),
            verbatimTextOutput("eff_out")
          )
        ),
        tabPanel("BMD Designs",
                 # sidebar layout for algorithm options
                 sidebarLayout(
                   sidebarPanel(
                     tags$h3("Algorithm options"),
                     selectInput("algorithm_bmd",
                                 "Algorithm",
                                 algorithms),
                     numericInput("iter_bmd",
                                  "Iterations",
                                  200,
                                  1,
                                  Inf,
                                  10),
                     numericInput("swarm_bmd",
                                  "Swarm size",
                                  30,
                                  10,
                                  Inf,
                                  10),
                     numericInput("bound_bmd",
                                  "Dose limit",
                                  30,
                                  0.1,
                                  Inf,
                                  1),
                     numericInput("pts_bmd",
                                  "Max design points",
                                  3,
                                  1,
                                  10,
                                  1),
                     numericInput("seed_bmd",
                                  "Seed",
                                  155,
                                  1,
                                  Inf,
                                  1),
                     numericInput(
                       "N_bmd",
                       "N",
                       min = 1,
                       max = 1000,
                       value = 50
                     )

                   ),
                   mainPanel(
                     "This tab is for finding designs for estimating
                     the benchmark dose (BMD) while efficiently estimating
                     the model parameters. This tab works similarly to the other design
                     tab, but includes several additional inputs. The risk type
                     and a pre-specified level of risk for the BMD must both be set.
                     These are described in more detail in the BMD designs tab
                     section of the user manual. The user must also choose a lambda
                     value to control the trade-off between estimating the BMD
                     and the model parameters. The value of lambda ranges between 0
                     and 1 with values close to 1 placing a higher priority on estimating
                     the BMD. A good starting value for lambda is 0.5. More information
                     and tips can be found in the user manual.",
                     fluidRow(
                       column(
                         6,
                         selectInput("model_selector_bmd", "Model", bmd_models),
                         selectInput("risk_type_selector", "Risk type", c("Added", "Extra")),
                         numericInput("risk", "Risk increase",
                                      value = 0.1, min = .01, max = .99, step = 0.01)
                       ),
                       column(
                         6,
                         textInput("theta_input_bmd", "Theta ( enter values separated by , )"),
                         numericInput("lambda_input", "\\(\\lambda\\) (weight parameter for c vs. D optimality)",
                                      value = 0.5, min = 0, max = 1, step = 0.1)
                       )
                     ),
                     uiOutput("model_formula_display_bmd"),
                     uiOutput("example_theta_bmd"),
                     actionButton("find_bmd", "Find design"),
                     actionButton("plot_response_bmd", "Plot Response"),
                     checkboxInput("log_dose_check_bmd", "Plot mean response on log dose scale"),
                     checkboxInput('compute_Deff', 'Compute D-efficiency'),
                     plotOutput("sens_plot_bmd"),
                     waiter::use_waiter(),
                     verbatimTextOutput("design_out_bmd"),
                     actionButton("copy_design_bmd", "Copy design to compare tab")
                   )
                 ))
      )
    )
  )
)

server = function(input, output, session) {

  # reactive data structure
  values <- reactiveValues()
  values$DT <- data.frame(x = numeric(),
                          y = numeric(),
                          yhat = numeric()
                          #color = factor(),
                          #shape = factor()
  )
  values$eff_crit = c("NA", "?")

  # set up reactive data structure
  # initialize with empty arrays and plots
  values$OD <- list(
    design = numeric(),
    x = numeric(),
    w = numeric(),
    sens_plot = ggplot2::ggplot(),
    msg = character()
  )

  # display model formula
  output$model_formula_display = renderUI({
    p(withMathJax(model_display(input$model_selector)))
  })

  # display of example local parameters
  output$example_theta = renderUI({
    p(withMathJax(display_example_param(input$model_selector)))
  })


  # sensitivity plot
  output$sens_plot = renderPlot({

    # load plot from reactive data
    ggp = values$OD$sens_plot

    # display plot
    ggp
  })

  # action for Find button
  observeEvent(
    input$find,
    {
      # set up loading indicator
      waiter <- waiter::Waiter$new(
        id = "sens_plot",
        html = waiter::spin_terminal(),
        color = "grey"
      )$show()
      waiter$show()
      on.exit(waiter$hide())

      # grab and process theta from raw input
      theta = process_theta(input$theta_input)

      # check model bounds
      model = input$model_selector
      if (check_bounds(model, theta)) {
        # select gradient function
        grad_fun = grad_selector(model)

        # find optimal design
        out = find_design_single(
          grad_fun,
          input$objective,
          theta,
          input$bound,
          input$pts,
          input$algorithm,
          input$swarm,
          input$iter,
          input$seed
        )

        # update reactive data with new design data
        values$OD$msg = ""
        values$OD$design = out$result$result
        values$OD$sens_plot = out$plot
        #values$OD$response_plot = response_plot
        values$OD$val = out$result$optimumValue
      }
      else {
        # show error message in plot
        p = ggplot2::ggplot(mapping = ggplot2::aes()) +
          ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 5, y = 0.5,
                            label = "Theta values out of bound", size = 5)
        values$OD$sens_plot = p
      }
    }
  )

  # plotting dose response function
  observeEvent(
    input$plot_response,
    {
      model = input$model_selector
      theta = process_theta(input$theta_input)
      values$OD$sens_plot = plot_response(model, theta,
                                          input$bound, input$log_dose_check)
    }
  )

  # design output
  output$design_out = renderPrint({

    raw = values$OD$design
    obj_val = values$OD$val[[1]]


    # case if algorithm hasn't run
    if (length(raw) == 0) {
      cat("No design")
    }
    else {
      cat("Objective value:", obj_val, "\n")

      # label and reorder
      l = length(raw)
      x = raw[1:(l/2)]
      w = raw[(l/2 + 1):l]
      cat("Doses:\n", signif(x[order(x)], 3), "\n", sep = " ")
      cat("Weights:\n", round(w[order(x)], 3), "\n", sep = " ")

      # save ordered doses and weights
      values$OD$x = x[order(x)]
      values$OD$w = w[order(x)]

      # give example for a finite sample size
      #n = 50
      n = input$N
      cat('Suggested allocations for N = ', n, ":\n", sep = '')
      #cat(round(w[order(x)] * n), sep = ' ')
      # last sample size N-1 to make allocations sum to N
      ns = round(w[order(x)] * n)[-length(w)]
      cat(c(ns, n - sum(ns)), sep = ' ')


    }
  })

  observeEvent(
    input$copy_design,
    {
      updateTextInput(session, "\\xi2_doses", value = toString(signif(values$OD$x, 3)))
      updateTextInput(session, "\\xi2_weights", value = toString(round(values$OD$w, 3)))
    }
  )
  ##############################################################################
  # compare designs tab
  ##############################################################################

  # run when compare designs button
  observeEvent(
    input$compute_eff,
    {

      d1 = process_theta(input$`\\xi1_doses`)
      d2 = process_theta(input$`\\xi2_doses`)
      w1 = process_theta(input$`\\xi1_weights`)
      w2 = process_theta(input$`\\xi2_weights`)
      theta = process_theta(input$theta_input_compare)
      eff = compute_eff(
        input$model_selector_compare,
        theta,
        input$objective_compare,
        d1,
        d2,
        w1,
        w2
      )

      # save eff to reactive data
      values$eff_crit = c(as.character(eff), input$objective_compare)

    }
  )

  # display design efficiency
  output$eff_out = renderPrint({
    # sprintf("The %s-efficiency of design 1 relative to design 2 is %s",
    #         values$eff_crit[2], values$eff_crit[1])
    cat("Criterion: ", values$eff_crit[2],
        "\nEfficiency: ", values$eff_crit[1], "\n")
  })

  ##############################################################################
  # BMD design tab
  ##############################################################################
  # set up reactive data structure
  # initialize with empty arrays and plots
  values$OD2 <- list(
    design = numeric(),
    x = numeric(),
    w = numeric(),
    sens_plot = ggplot2::ggplot(),
    msg = character(),
    Deff = NA
  )

  # display model formula
  output$model_formula_display_bmd = renderUI({
    p(withMathJax(model_display(input$model_selector_bmd)))
  })

  # example theta values
  output$example_theta_bmd = renderUI({
    p(withMathJax(display_example_param(input$model_selector_bmd)))
  })

  # sensitivity plot
  output$sens_plot_bmd = renderPlot({

    # load plot from reactive data
    ggp = values$OD2$sens_plot

    # display plot
    ggp
  })

  # action for Find button
  observeEvent(
    input$find_bmd,
    {
      # set up loading indicator
      waiter <- waiter::Waiter$new(
        id = "sens_plot_bmd",
        html = waiter::spin_terminal(),
        color = "grey"
      )$show()
      waiter$show()
      on.exit(waiter$hide())

      # grab and process theta from raw input
      theta = process_theta(input$theta_input_bmd)

      # check theta values
      model = input$model_selector_bmd
      if (check_bounds(model, theta)) {
        # select gradient function
        if (model == "Logistic")
          grad_fun = grad.logistic
        else if (model == "Weibull")
          grad_fun = grad.weibull
        else if (model == "Log-logistic")
          grad_fun = grad.loglogistic


        # find optimal design
        out = find_bmd_design(
          model,
          input$lambda_input,
          input$risk,
          input$risk_type_selector,
          theta,
          input$bound_bmd,
          input$pts_bmd,
          input$algorithm_bmd,
          input$swarm_bmd,
          input$iter_bmd,
          input$seed_bmd
        )


        # update reactive data with new design data
        values$OD2$msg = ""
        values$OD2$design = out$result$result
        values$OD2$sens_plot = out$plot
        #values$OD$response_plot = response_plot
        values$OD2$val = out$result$optimumValue

        # compute D-efficiency if box if checked
        if (input$compute_Deff) {
          #browser()
          # find D-optimal design
          # current algorithm choices should be good enough to find the design
          # grab and process theta from raw input
          theta = process_theta(input$theta_input_bmd)

          # check model bounds
          model = input$model_selector_bmd
          if (check_bounds(model, theta)) {
            # select gradient function
            grad_fun = grad_selector(model)

            # find optimal design
            out = find_design_single(
              grad_fun,
              'D',
              theta,
              input$bound_bmd,
              input$pts_bmd,
              input$algorithm_bmd,
              input$swarm_bmd,
              input$iter_bmd,
              input$seed_bmd
            )$result$result

            # process
            l = length(values$OD2$design)
            x = values$OD2$design[1:(l/2)]
            w = values$OD2$design[(l/2 + 1):l]

            l_d = length(out)
            x_d = out[1:(l_d/2)]
            w_d = out[(l_d/2 + 1):l_d]

            # compute D efficiency and save to reactive
            values$OD2$Deff = compute_eff(
              model,
              theta,
              'D',
              d1 = x,
              d2 = x_d,
              w1 = w,
              w2 = w_d
            )
          }
        }
      }
      else {
        # show error message in plot
        p = ggplot2::ggplot(mapping = ggplot2::aes()) +
          ggplot2::theme_bw() +
          ggplot2::annotate("text", x = 5, y = 0.5,
                            label = "Theta values out of bound", size = 5)
        values$OD2$sens_plot = p
      }


    }
  )

  # plotting dose response function
  observeEvent(
    input$plot_response_bmd,
    {
      model = input$model_selector_bmd
      theta = process_theta(input$theta_input_bmd)
      values$OD2$sens_plot = plot_response(model, theta,
                                          input$bound_bmd, input$log_dose_check_bmd)
    }
  )

  # design output
  output$design_out_bmd = renderPrint({

    raw = values$OD2$design
    obj_val = values$OD2$val[[1]]


    # case if algorithm hasn't run
    if (length(raw) == 0) {
      cat("No design")
    }
    else {
      cat("Objective value:", obj_val, "\n")

      # label and reorder
      l = length(raw)
      x = raw[1:(l/2)]
      w = raw[(l/2 + 1):l]
      cat("Doses:\n", signif(x[order(x)], 3), "\n", sep = " ")
      cat("Weights:\n", round(w[order(x)], 3), "\n", sep = " ")

      # save ordered doses and weights
      values$OD2$x = x[order(x)]
      values$OD2$w = w[order(x)]

      # give example for a finite sample size
      n = input$N_bmd
      cat('Suggested allocations for N = ', n, ":\n", sep = '')
      #cat(floor(w[order(x)] * n), sep = ' ')
      # last sample size N-1 to make allocations sum to N
      ns = round(w[order(x)] * n)[-length(w)]
      cat(c(ns, n - sum(ns)), sep = ' ')
      cat("\nD-efficiency:", round(values$OD2$Deff, 2))
    }
  })

  observeEvent(
    input$copy_design_bmd,
    {
      updateTextInput(session, "\\xi2_doses", value = toString(signif(values$OD2$x, 3)))
      updateTextInput(session, "\\xi2_weights", value = toString(round(values$OD2$w, 3)))
    }
  )

}

################################################################################
# Gradient functions, objective functions, and derivatives
################################################################################
grad.logistic = function(x, theta) {

  eta = theta[1] + theta[2] * x
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x)
  return(grad)
}

grad.logistic.quad = function(x, theta) {

  eta = theta[1] + theta[2] * x + theta[3] * x^2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2)
  return(grad)
}

grad.logistic.cubic = function(x, theta) {
  eta = theta[1] + theta[2] * x + theta[3] * x^2 + theta[4] * x^3
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x, x^2, x^3)
  return(grad)
}

# 2nd degree fractional polynomial predictor
grad.logistic.fp = function(x, theta) {

  # theta4 and theta5 are power paramters in this model
  powers = c(0, theta[4], theta[5])

  # x1 is the 2nd term in the polynomial
  x1 = H(2, x, powers)
  x2 = H(3, x, powers)
  eta = theta[1] + theta[2] * x1 + theta[3] * x2
  sigma = exp(eta)/(1 + exp(eta))^2
  grad = sigma * c(1, x1, x2)
  return(grad)

}

# mixture of two multistage models
# Razzaghi (2002) in Envirometrics
grad.mix2 = function(x, theta) {

  # identify parameters
  a = theta[1]
  b = theta[2]
  c = theta[3]
  d = theta[4]
  f = theta[5]
  g = theta[6]

  # gradient components
  d1 = g*exp(-a-b*x-c*x^2) + (1-g)*exp(-a-d*x-f*x^2)
  d2 = g*x*exp(-a-x*(b+c*x))
  d3 = g * x^2 * exp(-a-x*(b+c*x))
  d4 = (1 - g)*x*exp(-a-x*(d+f*x))
  d5 = (1 - g) *x^2 * exp(-a-x*(d+f*x))
  d6 = exp(-a-d*x-f*x^2) - exp(-a-b*x-c*x^2)
  grad = c(d1, d2, d3, d4, d5, d6)
  return(grad)
}

# Box-Cox Weibull model from Backhaus et al (2000)
# P(x) = 1 - exp( -exp(theta1 + theta2 * (x^theta3 - 1)/theta3) )
grad.boxcoxweibull = function(x, theta) {

  # identify parameters
  a = theta[1]
  b = theta[2]
  c = theta[3]

  # gradient components
  d1 = exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)
  d2 = (x^c - 1)*exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)/c
  d3 = (b*x^c * log(x)/c - b*(x^c-1)/c^2) * exp(-exp(a + b*(x^c-1)/c) + a + b*(x^c-1)/c)
  grad = c(d1, d2, d3)
  return(grad)

}

# Weibull model
# using version found in BMDS
# P[dose] = g + (1 - g) * (1 - exp(-b * dose^a))
grad.weibull = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-b * x^a)
  g2 = -b * (g - 1) * x^a * log(x) * exp(-b * x^a)
  g3 = (g - 1) * x^a * (-exp(-b * x^a))
  return(c(g1, g2, g3))
}

# log-logistic model
# using version from BMDS
# P[dose] = g + (1 - g)/(1 + exp(-a - b * Log(dose)))
grad.loglogistic = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 1/(exp(a) * x^b + 1)
  g2 = (1-g)*exp(-a-b*log(x))/(exp(-a-b*log(x)) + 1)^2
  g3 = (1-g)*log(x)*exp(-a-b*log(x))/(exp(-a-b*log(x))+1)^2
  return(c(g1, g2, g3))
}

# Hill model
# P(d) = g + (v - v*g) / (1 + exp(-a-b*log(d)))
grad.hill = function(x, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  g1 = 1 - exp(a) * v * x^b / (exp(a) * x^b + 1)
  g2 = - exp(a) * (g-1) * x^b / (exp(a)*x^b + 1)
  g3 = - exp(a) * (g-1) * v * x^b / (exp(a) * x^b + 1)^2
  g4 = - exp(a)*(g-1)*v*x^b*log(x) / (exp(a) * x^b + 1)^2
  return(c(g1, g2, g3, g4))

}

# multistage 1
# P(d) = g + (1-g)*(1-exp(-a*d))
grad.multi1 = function(x, theta) {

  g = theta[1]
  a = theta[2]

  g1 = exp(-a * x)
  g2 = -(g-1)*x*exp(-a*x)
  return(c(g1, g2))
}

# multistage 2
# P(d) = g + (1-g)*(1-exp(-a*d - b*d^2))
grad.multi2 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-a*x - b*x^2)
  g2 = (1-g)*x*exp(-a*x - b*x^2)
  g3 = (1-g)*x^2*exp(-a*x - b*x^2)
  return(c(g1, g2, g3))
}

# multistage 3
# P(d) = g + (1-g)*(1-exp(-a*d - b*d^2 - c*d^3))
grad.multi3 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]
  c = theta[4]

  g1 = exp(-a*x - b*x^2 - c*x^3)
  g2 = x*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  g3 = x^2*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  g4 = x^3*(1-g)*exp(-a*x - b*x^2 - c*x^3)
  return(c(g1, g2, g3, g4))
}

# probit
# P(d) = phi(a + bx)
grad.probit = function(x, theta) {

  a = theta[1]
  b = theta[2]

  g1 = 1/sqrt(2*pi)*exp(-(a+b*x)^2/2)
  g2 = 1/sqrt(2*pi)*exp(-(a+b*x)^2/2) * x
  return(c(g1, g2))
}

# log probit
# P(d) = g + (1-g) * phi(a + b*log(d))
grad.logprobit = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 1 - pnorm(a + b * log(x))
  g2 = (1-g) * 1/sqrt(2*pi)*exp(-(a+b*log(x))^2/2)
  g3 = (1-g) * 1/sqrt(2*pi)*exp(-(a+b*log(x))^2/2) * log(x)
  return(c(g1, g2, g3))
}

# 4 parameter logistic
# P(x) = g + (c - g)/(1 + exp(-a - b * log(x)))
grad.loglogistic4 = function(x, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]
  c = theta[4]

  g1 = 1 / (exp(a) * x^b + 1)
  g2 = (c - g) * exp(-a-b*log(x)) / (exp(-a-b*log(x)) + 1)^2
  g3 = (c - g) * exp(-a-b*log(x)) * log(x) / (exp(-a-b*log(x)) + 1)^2
  g4 = 1 / (exp(-a) * x^(-b) + 1)
  return(c(g1, g2, g3, g4))
}


################################################################################
# Objective functions
################################################################################
# D optimality
# maximize logdetM
obj.D = function(M, param) {
  suppressWarnings(log(det(M)))
}

# A optimality
# minimize trM^-1
obj.A = function(M, param) {

  # check if matrix is invertible
  if (!checkMinv(M))
    return(-Inf)
  else
    return(-sum(diag(solve(M))))
}

# BMD optimality
obj.bmd = function(M, param) {

  lambda = param[1]
  c = param[-1]
  if (!checkMinv(M))
    return(-Inf)
  else {
    Minv = solve(M)
    Dval = suppressWarnings(log(det(M)))
    Cval = -suppressWarnings(log(t(c) %*% Minv %*% c)) # note the sign flip
    p = length(c)
    return(lambda * Cval + (1 - lambda)/p * Dval)
  }
}

# derivatives of objective functions with respect to information matrix
# matrix singularity is already checked here
# M: information matrix
dPsi.D = function(M, param) {
  Minv = solve(M)
  return(Minv)
}

dPsi.A = function(M, param) {
  Minv = solve(M)
  Minv2 = Minv %*% Minv
  return(Minv2)
}

# compound D and c
# see Atkinson book p389
dPsi.CD = function(M, param) {

  Minv = solve(M)
  p = nrow(M)
  lambda = param[1]
  c = param[-1]
  num = Minv %*% c %*% t(c) %*% Minv
  denom = c(t(c) %*% Minv %*% c)

  return((1 - lambda)/p * Minv + lambda/denom * num)
}

################################################################################
# Main driver functions
################################################################################
find_design_single = function(
    grad_fun,
    obj,
    theta,
    bound,
    pts,
    algorithm,
    swarm,
    iter,
    seed
) {

  # design objective
  if (obj == "D")
    obj_fun = obj.D
  else if (obj == "A")
    obj_fun = obj.A
  else
    stop("Objective not supported")

  # objective function
  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # set up variable bounds
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # find design
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # check optimality
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]

  # collapse doses if weights are small
  x = x[w > 1e-5]
  w = w[w > 1e-5]
  result$result = c(x, w)

  M = M.nonlinear(x, w, theta, grad_fun)
  problem = list(bound = bound, obj = obj, theta = theta)
  p = plot_sens(x, w, problem, M, grad_fun)

  return(list(result = result, plot = p))
}

# function for computing design efficiencies
compute_eff = function(
    model,
    theta,
    objective,
    d1,
    d2,
    w1,
    w2
) {

  # normalize user submitted weights
  w1 = w1/sum(w1)
  w2 = w2/sum(w2)

  # if any of the doses are 0, add a small value
  d1[which(d1==0)] = 1e-3
  d2[which(d2==0)] = 1e-3

  # select gradient function
  grad_fun = grad_selector(model)

  if (objective == "D")
    obj_fun = obj.D
  else if (objective == "A")
    obj_fun = obj.A

  # define objective function
  param = c()
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # compute and return efficiencies
  if (objective == "D")
    eff = (exp(obj_fun_M(c(d1, w1)))/exp(obj_fun_M(c(d2, w2))))^(1/length(theta))
  else if (objective == "A")
    eff = obj_fun_M(c(d2, w2))/obj_fun_M(c(d1, w1))

  return(round(eff, 3))
}

# function for finding BMD designs
# basically a compound optimal design
# lambda is the weight parameter
find_bmd_design = function(
    model,
    lambda,
    risk,
    risk_type,
    theta,
    bound,
    pts,
    algorithm,
    swarm,
    iter,
    seed
) {
  #browser()
  # select gradient functions
  if (model == "Logistic") {
    grad_fun = grad.logistic

    if (risk_type == "Added") {
      bmd_grad = bmdgrad.logistic.add
    }
    else if (risk_type == "Extra")
      bmd_grad = bmdgrad.logistic.extra
  }
  else if (model == "Weibull") {
    grad_fun = grad.weibull

    if (risk_type == "Added") {
      bmd_grad = bmdgrad.weibull.add
    }
    else if (risk_type == "Extra") {
      bmd_grad = bmdgrad.weibull.extra
    }
  }
  else if (model == "Log-logistic") {
    grad_fun = grad.loglogistic

    if (risk_type == "Added") {
      bmd_grad = bmdgrad.loglogistic.add
    }
    else if (risk_type == "Extra") {
      bmd_grad = bmdgrad.loglogistic.extra
    }
  }
  else if (model == "Hill") {
    grad_fun = grad.hill

    if (risk_type == "Added") {
      bmd_grad = bmdgrad.hill.add
    }
    else if (risk_type == "Extra") {
      bmd_grad = bmdgrad.hill.extra
    }
  }


  # objective function
  c = bmd_grad(risk, theta) # compute BMD gradient components
  param = c(lambda, c)
  obj_fun = obj.bmd
  obj_fun_M = obj_fun_factory(grad_fun, obj_fun, theta, param)

  # set up variable bounds
  rangeVar = matrix(c(rep(c(0, bound), pts), rep(c(0,1), pts)), nrow = 2)

  # algorithm options
  control = list(numPopulation = swarm, maxIter = iter)

  # find design
  result = metaheuristicOpt::metaOpt(
    obj_fun_M,
    optimType = "MAX",
    algorithm = algorithm,
    numVar = 2 * pts,
    rangeVar,
    control,
    seed = seed
  )

  # check optimality
  vars = result$result
  x = vars[1:pts]
  w = vars[(pts+1):(2*pts)]

  # collapse doses if weights < 1e-5
  x = x[w > 1e-5]
  w = w[w > 1e-5]
  result$result = c(x, w)

  M = M.nonlinear(x, w, theta, grad_fun)
  problem = list(bound = bound, obj = "bmd", theta = theta, param = param)
  p = plot_sens(x, w, problem, M, grad_fun)

  # compute D-efficiency of design
  #D_eff = compute_eff(model, theta, "bmd", )
  # probably need to make this an option

  return(list(result = result, plot = p))

}
################################################################################
# Core optimal design related functions
################################################################################
# function for constucting objective function for use with algorithms
# M_fun: information matrix function
# obj_fun: objective function of information matrix
# theta: parameter values to pass through to M_fun
# par: other parameters, such as c values for c objective
obj_fun_factory = function(grad_fun, obj_fun, theta, param) {

  # these are used in interface function
  force(grad_fun)
  force(theta)
  force(param)

  # interface called by optimization software
  # return this function
  function(vars, ...) {
    # distinguish between points and weights
    pts = length(vars)/2
    x = vars[1:pts]
    w = vars[(pts+1):(2*pts)]

    # check weight constraint
    s = sum(w, na.rm = T) # na.rm needed to fix if statement error
    if (s > 1) # constraint implementation
      return(-Inf)

    M_fun = M.nonlinear # always using general nonlinear matrix
    obj_value = obj_fun(M_fun(x, w, theta, grad_fun), param)

    # deal with missing
    if (is.na(obj_value))
      return(-Inf)
    else
      return(obj_value)
  }

}


# information matrix
# x: array of design points
# w: array of weights
# theta: array of parameter values
# most general case
# grad_fun: gradient function to use
M.nonlinear = function(x, w, theta, grad_fun) {

  IM = 0
  for (i in 1:length(x)) {
    IM_i = w[i] * grad_fun(x[i], theta) %*% t(grad_fun(x[i],theta))
    IM = IM + IM_i
  }
  IM
}


# plot sensitivity function for a given design
# problem is same list from toxODmeta
# x, w are design point and weight vectors
# M: pre-computed information matrix
# grad_fun: gradient function
plot_sens = function(x, w, problem, M, grad_fun) {

  # x values
  step = problem$bound/1000
  xvals = seq(0, problem$bound, step)

  # select derivative function for sensitivity function
  if (problem$obj == "D") {
    dPsi = dPsi.D
    param = NULL
  }
  else if (problem$obj == "A") {
    dPsi = dPsi.A
    param = NULL
  }
  else if (problem$obj == "bmd") {

    dPsi = dPsi.CD
    param = problem$param
  }
  else {
    # expand this to handle solving design problems with no verification
    #stop("No derivative specified for this objective.")
    # use y=2 to denote missing derivative function
    yvals = rep(2, length(xvals))
  }

  # compute sensitivity function
  # check first if matrix is invertible and then invert
  if (!checkMinv(M)) {
    # using y=1 to denote matrix singularity
    yvals = rep(1, length(xvals))
  }
  else {
    Minv = solve(M)
    yvals = sapply(xvals, sens, grad_fun, dPsi, M, problem$theta, param)
  }



  # plot
  # display message if missing matrix deriv or singular matrix
  if (sum(yvals - 1, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                        label = "Singular information matrix, try increasing max design points.", size = 5)
  }
  else if (sum(yvals - 2, na.rm = T) == 0) {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Equivalence Theorem Check") +
      ggplot2::xlab("x") +
      ggplot2::ylab("ch(x)") +
      ggplot2::annotate("text", x = mean(xvals), y = 0.5,
                        label = "No dPsi defined", size = 5)
  }
  else {
    p = ggplot2::ggplot(mapping = ggplot2::aes(y = yvals, x = xvals)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_hline(yintercept = 0) +
      #ggplot2::geom_point(aes(x = design_points, y = pts_ch), col = "red", size = 3) +
      ggplot2::geom_vline(xintercept = x, color = "red", linetype = "dashed") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Optimality check", subtitle = 'Is the blue line equal to 0 at the selected doses (red lines) and less than 0 everywhere else?') +
      ggplot2::xlab("dose") +
      ggplot2::ylab("directional derivative")
  }


  return(p)
}

# sensitivity function

# general function
# z: independent variable
# grad: gradient function
# dPsi: derivative of the objective function wrt M
# M: information matrix
# theta: model parameters
sens = function(z, grad, dPsi, M, theta, param) {

  dg = grad(z, theta)
  dM = dPsi(M, param)
  y = t(dg) %*% dM %*% dg - sum(diag(M %*% dM))
  return(y)
}

################################################################################
# Other utility functions
################################################################################
# function that displays latex formulas for models in app
model_display = function(model) {

  if (model == "Hill")
    "$$ P(d) = \\theta_1 + \\frac{(\\theta_2 - \\theta_2 \\theta_1)}{1 + \\exp(-\\theta_3 - \\theta_4\\log (d))} $$"
  # else if (model == "Gamma") # don't know how to do this => Elvis' paper reparameterizes
  #   "$$ P(d) = $$"
  else if (model == "Logistic")
    "$$ P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d)} $$"
  else if (model == "Logistic quadratic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2)}$$"
  else if (model == "Logistic cubic")
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3)}$$"
  else if (model == "Logistic fractional polynomial") {
    "$$P(d) = \\frac{1}{1 + \\exp(-\\theta_1 - \\theta_2 d^{\\theta_4} - \\theta_3 d^{\\theta_5})}$$
    \n Note: the exponent parameters are considered known and the design is not
    optimal for estimating these parameters. The optimal design will most likely
    have 3 doses."

  }
  else if (model == "Log-logistic")
    "$$ P(d) = \\theta_1 +  \\frac{1-\\theta_1}{1 + \\exp(-\\theta_2- \\theta_3 \\log d)}$$"
  else if (model == "Log-probit")
    "$$ P(d) = \\theta_1 + (1 - \\theta_1) \\Phi(\\theta_2 + \\theta_3 \\log(d))$$"
  else if (model == "Probit")
    "$$P(d)=\\Phi(\\theta_1 + \\theta_2 d)$$"
  else if (model == "Quantal linear")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_2 d))$$"
  else if (model == "Weibull")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1-\\exp(-\\theta_3 d^{\\theta_2}))$$"
  else if (model == "Mixture multistage")
    "$$P(d) = \\theta_6 \\left[1 - \\exp(-\\theta_1-\\theta_2 d - \\theta_3 d^2) \\right] + (1-\\theta_6)\\left[1 - \\exp(-\\theta_1 - \\theta_4 d - \\theta_5 d^2) \\right]$$"
  else if (model == "Box-Cox Weibull")
    "$$P(d)=1-\\exp \\left[ -\\exp \\left(\\theta_1 + \\theta_2 \\frac{d^{\\theta_3}-1}{\\theta_3}\\right)\\right]$$"
  else if (model == "Hill")
    "$$P(d) = \\theta_1 + \\frac{\\theta_2 - \\theta_2\\theta_1}{1+\\exp(-\\theta_3 - \\theta_4 d)}$$"
  else if (model == "Multistage 1")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d))$$"
  else if (model == "Multistage 2")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2))$$"
  else if (model == "Multistage 3")
    "$$P(d) = \\theta_1 + (1-\\theta_1)(1 - \\exp(-\\theta_2 d - \\theta_3 d^2 - \\theta_4 d^3))$$"
  else if (model == "Probit")
    "$$P(d) = \\phi(\\theta_1 + \\theta_2 d)$$"
  else if (model == "Log-probit")
    "$$P(d) = \\theta_1 + (1-\\theta_1) \\phi(\\theta_2 + \\theta_3 * \\log(d))$$"
  else if (model == "4 parameter log-logistic")
    "$$P(d) = \\theta_1 + \\frac{(\\theta_4 - \\theta_1)}{1 + \\exp(-\\theta_2 - \\theta_3 \\log(d))}$$"
  else
    "Model not supported"

}

# displays example local theta values
# model: string from model selector
# returns: string to be displayed by Shiny ui element
display_example_param = function(model) {

  if (model == "Hill")
    "EX: \\(\\theta\\) = (0.02201, 0.9034, -2.132, 1)"
  else if (model == "Logistic")
    "EX: \\(\\theta\\) = (-1.710, 0.09703)"
  else if (model == "Logistic quadratic")
    "EX: \\(\\theta\\) = (-2.52, 0.26, -0.006)"
  else if (model == "Logistic cubic")
    "EX: \\(\\theta\\) = (-3.91, 1.56, -0.18, 0.004)"
  else if (model == "Logistic fractional polynomial") {
    "EX: \\(\\theta\\) = ()"
  }
  else if (model == "Log-logistic")
    "EX: \\(\\theta\\) = (0.02461, -2.390, 1)"
  else if (model == "Log-probit")
    "EX: \\(\\theta\\) = (0.02051, -1.237, 0.5173)"
  else if (model == "Probit")
    "EX: \\(\\theta\\) = (-1.051, 0.05948)"
  else if (model == "Quantal linear")
    "EX: \\(\\theta\\) = (0.05307, 0.04929)"
  else if (model == "Weibull")
    "EX: \\(\\theta\\) = (0.05307, .99, 0.04929)"
  else if (model == "Multistage 1")
    "EX: \\(\\theta\\) = (0.05307, 0.04929)"
  else if (model == "Multistage 2")
    "EX: \\(\\theta\\) = (0.05307, 0.04929, 0)"
  else if (model == "Multistage 3")
    "EX: \\(\\theta\\) = (0.05307, 0.04929, 0, 0)"
  else if (model == '4 parameter log-logistic')
    'EX: \\(\\theta\\) = (1.01, -2.93, 0.54, 140.09)'
  else
    "EX: \\(\\theta\\) = "


}

# convert raw text input to a vector of parameter values
# pulling this out into its own function because input checking could be complex
# useful in multiple places where there is text input
process_theta = function(text) {
  as.numeric(strsplit(text, ",")[[1]])
}

# function for checking model parameter bounds
# inputs: model type and parameters
# returns true if parameters are within bounds for model
check_bounds = function(model, theta) {

  # being very generous on beta coefficient ranges
  if (model == "Logistic") {
    if (theta[1] < -20 | theta[1] > 20)
      return(FALSE)
    if (theta[2] < -100 | theta[2] > 100)
      return(FALSE)
  }
  else if (model == "Logistic quadratic") {
    if (theta[1] < -20 | theta[1] > 20)
      return(FALSE)
    if (theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if (theta[3] < -100 | theta[3] > 100)
      return(FALSE)
  }
  else if (model == "Logistic cubic") {
    if (theta[1] < -20 | theta[1] > 20)
      return(FALSE)
    if (theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if (theta[3] < -100 | theta[3] > 100)
      return(FALSE)
    if (theta[4] < -100 | theta[4] > 100)
      return(FALSE)
  }
  else if (model == "Logistic fractional polynomial") {
    if (theta[1] < -20 | theta[1] > 20)
      return(FALSE)
    if (theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if (theta[3] < -100 | theta[3] > 100)
      return(FALSE)
  }
  else if (model == "Weibull") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if (theta[2] <= 0 | theta[2] > 20)
      return(FALSE)
    if (theta[3] <= 0 | theta[3] > 100)
      return(FALSE)
  }
  else if (model == "Log-logistic") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if (theta[2] < -20 | theta[2] > 20)
      return(FALSE)
    if (theta[3] <= 0 | theta[3] > 20)
      return(FALSE)
  }
  else if (model == "Mixture multistage") {

    # bounding for regression coef is same as in a multistage
    # being less restrictive than EPA guidelines
    if(theta[1] < -100 | theta[1] > 100)
      return(FALSE)
    if(theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if(theta[3] < -100 | theta[3] > 100)
      return(FALSE)
    if(theta[4] < -100 | theta[4] > 100)
      return(FALSE)
    if(theta[5] < -100 | theta[5] > 100)
      return(FALSE)
    if(theta[6] < 0 | theta[6] > 1)
      return(FALSE)
  }
  else if (model == "Hill") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if (theta[2] < -20 | theta[2] > 20)
      return(FALSE)
    if (theta[3] < -20 | theta[3] > 20)
      return(FALSE)
    if (theta[4] < 0 | theta[4] > 20)
      return(FALSE)
  }
  else if (model == "Multistage 1") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if(theta[2] < -100 | theta[2] > 100)
      return(FALSE)

  }
  else if (model == "Multistage 2") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if(theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if(theta[3] < -100 | theta[3] > 100)
      return(FALSE)
  }
  else if (model == "Multistage 3") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if(theta[2] < -100 | theta[2] > 100)
      return(FALSE)
    if(theta[3] < -100 | theta[3] > 100)
      return(FALSE)
    if(theta[4] < -100 | theta[4] > 100)
      return(FALSE)
  }
  else if (model == "Probit") {
    if (theta[1] < -20 | theta[1] > 20)
      return(FALSE)
    if (theta[2] <= 0 | theta[2] > 20)
      return(FALSE)
  }
  else if (model == "Log-probit") {
    if (theta[1] < 0 | theta[1] >= 1)
      return(FALSE)
    if (theta[2] < -20 | theta[2] > 20)
      return(FALSE)
    if (theta[3] <= 0 | theta[3] > 20)
      return(FALSE)
  }

  return(TRUE)

}

# plotting function for dose response model
# model: string name of dose response model
# theta: vector of model parameter values
# limit: dose limit, will control how much of the dose response function is shown
# log_dose: if true transform the x-axis
# returns: a ggplot of the dose response function
plot_response = function(model, theta, limit, log_dose = F) {

  # generate dose levels
  x = seq(0.01, limit, length.out=100)

  # add additional resolution close to 0 to help with log transform
  x = c(x, seq(.001, .1, length.out=20))

  # compute response using appropriate model function
  if (model == "Logistic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x))
  else if (model == "Log-logistic")
    y = theta[1] + (1-theta[1])/(1+exp(-theta[2]-theta[3]*log(x)))
  else if (model == "Weibull")
    y = theta[1] + (1 - theta[1])*(1 - exp(-theta[3]*x^theta[2]))
  else if (model == "Multistage 1")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x))
  else if (model == "Multistage 2")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x - theta[3]*x^2))
  else if (model == "Multistage 3")
    y = theta[1] + (1-theta[1])*(1-exp(-theta[2]*x - theta[3]*x^2 - theta[4]*x^3))
  else if (model == "Hill")
    y = theta[1] + (theta[2]-theta[2]*theta[1])/(1+exp(-theta[3]-theta[4]*log(x)))
  else if (model == "Logistic quadratic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x - theta[3]*x^2))
  else if (model == "Logistic cubic")
    y = 1/(1 + exp(-theta[1] - theta[2]*x - theta[3]*x^2 - theta[4]*x^3))
  else if (model == "Logistic fractional polynomial") {
    powers = c(0, theta[4], theta[5])
    eta = theta[1] + theta[2]*H(2, x, powers) + theta[4]*H(3, x, powers)
    y = 1/(1+exp(-eta))
  }
  else if (model == "Mixture multistage")
    y = theta[6]*(1-exp(-theta[1]-theta[2]*x-theta[3]*x^2)) + (1-theta[6])*(1-exp(-theta[1]-theta[4]*x - theta[5]*x^2))
  else if (model == "Probit")
    y = pnorm(theta[1] + theta[2]*x)
  else if (model == "Log-probit")
    y = theta[1] + (1-theta[1])*pnorm(theta[2] + theta[3]*log(x))
  else if (model == "4 parameter log-logistic")
    y = theta[1] + (theta[4] - theta[1])/(1 + exp(-theta[2] - theta[3]*log(x)))
  else
    y = x

  # plot
  # scaling dose
  if (log_dose) {
    xlabel = "log dose"
    dose = log(x)
  }
  else {
    xlabel = 'dose'
    dose = x
  }

  p = ggplot2::ggplot(mapping = ggplot2::aes(y = y, x = dose)) +
    ggplot2::geom_line(color = "red") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Dose response") +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab("P(dose)")


  return(p)
}

# checks if information matrix is invertible
# returns 1 if invertible and 0 if not
# can optimize easily for 2 dim
checkMinv = function(M) {

  if (class(try(solve(M),silent=T))[1]!="matrix")
    return(0)
  else
    return(1)
}

# utility functions for fractional polynomials

# Box-Tidwell transformation
bt = function(X, p) {
  if (p != 0)
    return(X^p)
  else if (p == 0)
    return(suppressWarnings(log(X)))
}

# derivative of Box-Tidwell
dbt = function(X, p) {
  if (p != 0)
    return(p * X^(p-1))
  else if (p == 0)
    return(1/X)
}

# H function
# j: index
H = function(j, X, powers) {
  if (j == 1) # base case
    return(1)
  if (powers[j] != powers[j-1])
    return(bt(X, powers[j]))
  else if (powers[j] == powers[j-1])
    return(suppressWarnings(log(X)) * H(j-1, X, powers))
}

# derivative of the H function
dH = function(j, X, powers) {
  if (j == 1) # base case
    return(0)
  if (powers[j] != powers[j-1])
    return(dbt(X, powers[j]))
  else if (powers[j] == powers[j-1])
    return(suppressWarnings(log(X)) * dH(j-1, X, powers) +
             H(j-1, X, powers)/X)
}

# calculates the fractional polynomial for given X, coefficients, powers
# m: degree
fracpoly = function(X, betas, powers, m) {

  y = 0

  for (j in 1:(m+1)) {
    y = y + betas[j] * H(j, X, powers)
  }

  return(y)

}

# model selector
# returns gradient function
grad_selector = function(model) {

  if (model == "Logistic")
    grad_fun = grad.logistic
  else if (model == "Logistic quadratic")
    grad_fun = grad.logistic.quad
  else if (model == "Logistic cubic")
    grad_fun = grad.logistic.cubic
  else if (model == "Logistic fractional polynomial")
    grad_fun = grad.logistic.fp
  else if (model == "Weibull")
    grad_fun = grad.weibull
  else if (model == "Log-logistic")
    grad_fun = grad.loglogistic
  else if (model == "Mixture multistage")
    grad_fun = grad.mix2
  else if (model == "Box-Cox Weibull")
    grad_fun = grad.boxcoxweibull
  else if (model == "Hill")
    grad_fun = grad.hill
  else if (model == "Multistage 1")
    grad_fun = grad.multi1
  else if (model == "Multistage 2")
    grad_fun = grad.multi2
  else if (model == "Multistage 3")
    grad_fun = grad.multi3
  else if (model == "Probit")
    grad_fun = grad.probit
  else if (model == "Log-probit")
    grad_fun = grad.logprobit
  else if (model == "4 parameter log-logistic")
    grad_fun = grad.loglogistic4

  return(grad_fun)
}

################################################################################
# gradient functions for BMD
################################################################################
# refer to the EPA's BMDS user manual for the original formulas
bmdgrad.logistic.add = function(r, theta) {

  beta0 = theta[1]
  beta1 = theta[2]

  g1 = r/(beta1 * (exp(beta0) + r))
  g2 = suppressWarnings(-log(-(exp(beta0) * (r - 1))/(exp(beta0) +r)) / beta1^2)
  return(c(g1, g2))
}

bmdgrad.logistic.extra = function(r, theta) {

  beta0 = theta[1]
  beta1 = theta[2]

  g1 = (exp(beta0) + 1)*r*(exp(beta0)*(r+1)+r-1)/(beta1*(exp(beta0)*r+r-1)*(exp(beta0)*(r+1)+r))
  g2 = suppressWarnings(- log(- (exp(beta0)*(exp(beta0)*r+r-1))/(exp(beta0)*(r+1)+r))/beta1^2)
  return(c(g1, g2))

}

bmdgrad.weibull.add = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = suppressWarnings(r * (-log((g+r-1)/(g-1))/b)^(1/(a-1)) / (a*b*(g-1)*(g+r-1)))
  g2 = suppressWarnings(-(log(-(log((g+r-1)/(g-1)))/b) * (-(log((g+r-1)/(g-1)))/b)^(1/a))/(a^2))
  g3 = suppressWarnings(-(-log((g+r-1)/(g-1))/b)^(1/a)/(a * b))
  return(c(g1, g2, g3))
}

bmdgrad.weibull.extra = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 0
  g2 = suppressWarnings(- log(-log(1-r)/b)*(-log(1-r)/b)^(1/a) / a^2)
  g3 = suppressWarnings(- (-log(1-r)/b)^(1/a) / (a*b))
  return(c(g1, g2, g3))
}

bmdgrad.loglogistic.add = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = exp(-a/b)*(-r/(g+r-1))^(1/(b+1)) / (b*r)
  g2 = -exp(-a/b)*(-r/(g+r-1))^(1/b) / b
  g3 = suppressWarnings(exp(-a/b)*(-r/(g+r-1))^(1/b) * (a - log(-r/(g+r-1))) / b^2)
  return(c(g1, g2, g3))
}

bmdgrad.loglogistic.extra = function(r, theta) {

  g = theta[1]
  a = theta[2]
  b = theta[3]

  g1 = 0
  g2 = suppressWarnings(-exp((log(r/(1-r))-a)/b)/b)
  g3 = suppressWarnings(exp(-a/b) * (r/(1-r))^(1/b) * (a - log(r/(1-r))) / b^2)
  return(c(g1,g2,g3))
}

bmdgrad.hill.add = function(r, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  t1 = exp(-a/b)
  t2 = ((g*r*v-g*v-r+v)/(r-g*r*v))^(-1/b)
  t3 = r*(g*v-1)-g*v+v

  g1 = (v-1)*v*t1*t2/(b*(g*v-1)*t3)
  g2 = -(g-1)*t1*t2/(b*(g*v-1)*t3)
  g3 = -t1*t2/b
  g4 = suppressWarnings(-(-a-log((g*r*v-g*v-r+v)/(r*(1-g*v))))*(exp((-a-log((g*r*v-g*v-r+v)/(r-g*r*v)))/(b)))/(b^2))
  return(c(g1, g2, g3, g4))

}

bmdgrad.hill.extra = function(r, theta) {

  g = theta[1]
  v = theta[2]
  a = theta[3]
  b = theta[4]

  t1 = exp(-a/b)
  t2 = (-(g*v+r-v)/(r))^(1/b)
  t3 = b*((g-1)*v+r)

  g1 = v*t1*t2/t3
  g2 = (g-1)*t1*t2/t3
  g3 = suppressWarnings(-exp((log(-(g*v+r-v)/r)-a)/b)/b)
  g4 = t1*t2 * (a - log(-(g*v+r-v)/(r))) / (b^2)
  return(c(g1,g2,g3,g4))

}



################################################################################
# Run the application
################################################################################
shinyApp(ui = ui, server = server)

