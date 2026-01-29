# ==============================================================================
# MRM-LD Interactive Visualization System (Optimized for Mobile)
# Multiple Response Model with Inter-option Local Dependencies
# 
# Reference: Zhou & Guo (2026). Psychometric Model Framework for Multiple Response Items
# Psychometrika. doi:10.1017/psy.2025.10073
# ==============================================================================

library(shiny)
library(ggplot2)
if (FALSE) {
  library(munsell)
}

# ==============================================================================
# 1. Core Functions (Optimized)
# ==============================================================================

get_all_patterns <- function(O_j) {
  as.matrix(expand.grid(replicate(O_j, 0:1, simplify = FALSE)))
}

filter_patterns <- function(full_patterns, itemtype, Key) {
  row_sums <- rowSums(full_patterns)
  T_correct <- sum(Key)
  keep <- switch(itemtype,
                 "MTF" = rep(TRUE, nrow(full_patterns)),
                 "CMS" = row_sums >= 1,
                 "Select-N" = row_sums == T_correct,
                 rep(TRUE, nrow(full_patterns)))
  full_patterns[keep, , drop = FALSE]
}

calculate_ps_scores <- function(patterns, Key) {
  rowSums(patterns == matrix(Key, nrow(patterns), length(Key), byrow = TRUE))
}

calculate_mrm_ld_prob <- function(theta, a, d, a_star, gamma, Key, patterns) {
  n_theta <- length(theta)
  n_patterns <- nrow(patterns)
  W <- ifelse(Key == 1, 1, -1)
  h_base <- d + W * a_star * gamma
  probs_matrix <- matrix(0, n_patterns, n_theta)
  patterns_a <- as.vector(patterns %*% a)
  patterns_base <- as.vector(patterns %*% h_base)
  for (i in seq_len(n_theta)) {
    log_kernels <- patterns_a * theta[i] + patterns_base
    kernels <- exp(log_kernels - max(log_kernels))
    probs_matrix[, i] <- kernels / sum(kernels)
  }
  pattern_strings <- apply(patterns, 1, paste, collapse = "")
  scores <- calculate_ps_scores(patterns, Key)
  data.frame(Theta = rep(theta, each = n_patterns), Pattern = rep(pattern_strings, n_theta),
             Score = rep(scores, n_theta), Prob = as.vector(probs_matrix))
}

calculate_mrm_info <- function(theta, a, d, a_star, gamma, Key, patterns) {
  n_theta <- length(theta)
  W <- ifelse(Key == 1, 1, -1)
  h_base <- d + W * a_star * gamma
  a_x <- as.vector(patterns %*% a)
  patterns_base <- as.vector(patterns %*% h_base)
  info_vec <- numeric(n_theta)
  for (i in seq_len(n_theta)) {
    log_kernels <- a_x * theta[i] + patterns_base
    kernels <- exp(log_kernels - max(log_kernels))
    p_vec <- kernels / sum(kernels)
    info_vec[i] <- sum(a_x^2 * p_vec) - sum(a_x * p_vec)^2
  }
  info_vec
}

calculate_mrm_info_marginal <- function(theta, a, d, a_star, Key, patterns) {
  g_points <- seq(-3, 3, length.out = 15)
  g_weights <- dnorm(g_points); g_weights <- g_weights / sum(g_weights)
  info_marginal <- numeric(length(theta))
  for (k in seq_along(g_points)) {
    info_marginal <- info_marginal + calculate_mrm_info(theta, a, d, a_star, g_points[k], Key, patterns) * g_weights[k]
  }
  info_marginal
}

# ==============================================================================
# 2. UI Layout (Mobile Optimized)
# ==============================================================================

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.9/MathJax.js?config=TeX-MML-AM_CHTML", defer = NA),
    tags$script(HTML("if(window.MathJax){MathJax.Hub.Config({tex2jax:{inlineMath:[['$','$'],['\\\\(','\\\\)']]}});}")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateStepperButtons', function(data) {
        var minusBtn = document.getElementById('O_j_minus');
        var plusBtn = document.getElementById('O_j_plus');
        if (minusBtn) minusBtn.disabled = data.minusDisabled;
        if (plusBtn) plusBtn.disabled = data.plusDisabled;
      });
    ")),
    tags$style(HTML("
      body { font-size: 15px; }
      .well { background-color: #f8f9fa; border: 1px solid #e9ecef; }
      h4 { color: #2c3e50; font-weight: 600; font-size: 1.25rem; margin-top: 18px; }
      h5 { font-size: 1.15rem; font-weight: 600; color: #2c3e50; }
      .model-intro { background-color: #e8f4f8; border-left: 4px solid #3498db; padding: 15px; margin: 10px 0; border-radius: 4px; font-size: 14px; }
      .formula-box { background-color: #ffffff; border: 1px solid #ddd; padding: 20px; border-radius: 4px; margin: 15px 0; overflow-x: auto; }
      .formula-box .MathJax, .formula-box .MathJax span, .formula-box .help-block { color: #000000 !important; }
      .score-note { background-color: #f5f5f5; border: 1px solid #e0e0e0; padding: 10px 15px; border-radius: 4px; margin: 5px 0; font-size: 13px; color: #555; }
      .symbol-def { padding: 10px 8px; margin: 4px 0; border-bottom: 1px solid #eee; font-size: 14px; line-height: 1.5; }
      .symbol-def:last-child { border-bottom: none; }
      .param-name { font-weight: 600; color: #2c3e50; font-style: italic; }
      .item-type-desc { font-size: 13px; color: #555; margin-top: 5px; padding: 10px; background-color: #f5f5f5; border-radius: 4px; line-height: 1.5; }
      .sidebar-label { font-weight: 600; margin-bottom: 3px; font-size: 14px; }
      .form-group { margin-bottom: 12px; }
      .slider-container { margin-bottom: 15px; }
      .selectize-input, .form-control { font-size: 14px; }
      .param-header-row { display: flex !important; flex-wrap: nowrap !important; align-items: center; margin-bottom: 5px; }
      .param-header-row .param-label-col { flex: 0 0 16.67%; max-width: 16.67%; min-width: 30px; }
      .param-header-row .param-input-col { flex: 0 0 41.67%; max-width: 41.67%; text-align: center; font-weight: bold; font-size: 14px; padding: 0 5px; }
      .param-row { display: flex !important; flex-wrap: nowrap !important; align-items: center; margin-bottom: 8px; }
      .param-row .param-label-col { flex: 0 0 16.67%; max-width: 16.67%; min-width: 30px; padding-top: 8px; font-weight: bold; font-size: 14px; }
      .param-row .param-input-col { flex: 0 0 41.67%; max-width: 41.67%; padding: 0 5px; }
      .param-row .param-input-col .form-group { margin-bottom: 0; }
      @media (max-width: 576px) { .param-header-row .param-input-col, .param-row .param-label-col { font-size: 12px; } .param-row .param-input-col input { font-size: 12px; padding: 4px 6px; } }
      .ld-warning { background-color: #fff3cd; border: 1px solid #ffc107; color: #856404; padding: 10px; border-radius: 4px; margin-bottom: 10px; font-size: 12px; }
      .key-warning { background-color: #f8d7da; border: 1px solid #f5c6cb; color: #721c24; padding: 10px; border-radius: 4px; margin-top: 10px; font-size: 12px; }
      .citation-footer { background-color: #f8f9fa; border-top: 1px solid #dee2e6; padding: 15px 20px; margin-top: 30px; text-align: center; font-size: 13px; color: #6c757d; }
      .citation-footer a { color: #007bff; text-decoration: none; }
      .citation-footer a:hover { text-decoration: underline; }
      
      /* ========== Mobile Optimizations ========== */
      
      /* Option count stepper buttons */
      .stepper-container {
        display: flex;
        align-items: center;
        gap: 4px;
        margin-bottom: 8px;
      }
      .stepper-btn {
        width: 28px;
        height: 28px;
        font-size: 16px;
        font-weight: bold;
        border-radius: 4px;
        border: 1px solid #3498db;
        background-color: #fff;
        color: #3498db;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        touch-action: manipulation;
        -webkit-tap-highlight-color: transparent;
        padding: 0;
        line-height: 1;
      }
      .stepper-btn:hover, .stepper-btn:active {
        background-color: #3498db;
        color: #fff;
      }
      .stepper-btn:disabled {
        opacity: 0.4;
        cursor: not-allowed;
        background-color: #f0f0f0;
        border-color: #ccc;
        color: #999;
      }
      .stepper-value {
        font-size: 16px;
        font-weight: 600;
        min-width: 24px;
        text-align: center;
        color: #2c3e50;
      }
      
      /* Responsive plot container */
      .plot-container {
        width: 100%;
        overflow-x: auto;
        -webkit-overflow-scrolling: touch;
      }
      
      /* Mobile-specific styles */
      @media (max-width: 768px) {
        .sidebar-panel-mobile {
          padding: 10px !important;
        }
        h4 { font-size: 1.1rem; margin-top: 12px; }
        h5 { font-size: 1rem; }
        .model-intro { padding: 10px; font-size: 13px; }
        .item-type-desc { font-size: 12px; padding: 8px; }
        .stepper-btn {
          width: 26px;
          height: 26px;
          font-size: 14px;
        }
        .stepper-value {
          font-size: 14px;
          min-width: 20px;
        }
        .stepper-container {
          gap: 3px;
        }
        /* Stack Item Type and Options on mobile */
        .item-config-row .col-6 {
          flex: 0 0 100%;
          max-width: 100%;
          margin-bottom: 10px;
        }
        
        /* Plot adjustments for mobile */
        .rccc-plot-wrapper {
          min-height: 400px;
        }
      }
      
      /* Very small screens */
      @media (max-width: 480px) {
        body { font-size: 14px; }
        .stepper-container {
          justify-content: center;
        }
      }
      
      /* Portrait mode optimizations for main content */
      @media (max-width: 992px) {
        .col-sm-8, .col-sm-4 {
          flex: 0 0 100%;
          max-width: 100%;
        }
        .formula-box {
          padding: 12px;
        }
        .formula-box .col-md-6 {
          flex: 0 0 100%;
          max-width: 100%;
        }
        .symbol-def {
          font-size: 13px;
          padding: 8px 5px;
        }
      }
      
      /* Sidebar stacking on mobile */
      @media (max-width: 768px) {
        .col-sm-3 {
          flex: 0 0 100%;
          max-width: 100%;
        }
        .col-sm-9 {
          flex: 0 0 100%;
          max-width: 100%;
        }
      }
    "))
  ),
  
  titlePanel(h3("Multiple Response Model - Interactive Visualization System", style = "font-weight: 600; color: #2c3e50;")),
  
  sidebarLayout(
    sidebarPanel(width = 3, class = "sidebar-panel-mobile",
                 div(class = "model-intro",
                     h5("Model Overview", style = "margin-top: 0;"),
                     p("MRM-LD (Multiple Response Model with Inter-option Local Dependencies) is an IRT model framework ",
                       "for Multiple Response (MR) items. Based on the divide-by-total structure, it directly models the probability of each Response Combination (RC) ",
                       "without compressing RC data into scores, and captures inter-option local dependencies within items.")),
                 h4("1. Item Configuration"),
                 fluidRow(class = "item-config-row",
                   column(6, selectInput("itemtype", "Item Type", choices = c("MTF", "CMS", "Select-N"), selected = "MTF")),
                   column(6, 
                          tags$label(HTML("O<sub>j</sub> (Number of Options)"), style = "font-weight: 600; margin-bottom: 5px; display: block; font-size: 13px;"),
                          div(class = "stepper-container",
                              actionButton("O_j_minus", "−", class = "stepper-btn"),
                              div(class = "stepper-value", textOutput("O_j_display", inline = TRUE)),
                              actionButton("O_j_plus", "+", class = "stepper-btn")
                          ),
                          # Hidden numeric input to store the actual value
                          div(style = "display: none;",
                              numericInput("O_j", NULL, value = 4, min = 2, max = 8, step = 1))
                   )),
                 uiOutput("oj_warning_ui"),
                 div(class = "item-type-desc",
                     HTML("<b>MTF</b> = Multiple True-False<br>"),
                     HTML("<b>CMS</b> = Conventional Multiple-Select<br>"),
                     HTML("<b>Select-N</b> = Select exactly N options<br><br>"),
                     HTML("X<sub>j</sub>: MTF = 2<sup>O<sub>j</sub></sup>; CMS = 2<sup>O<sub>j</sub></sup> − 1; "),
                     HTML("Select-N = C(O<sub>j</sub>, T).")),
                 h5("Answer Key", style = "margin-top: 15px;"),
                 uiOutput("key_ui"),
                 uiOutput("key_warning_ui"),
                 hr(),
                 div(style = "display:flex; justify-content:space-between; align-items:center;",
                     h4("2. Option Parameters"),
                     actionButton("random_btn", "Randomize", icon = icon("dice"), class = "btn-info btn-sm")),
                 uiOutput("dynamic_params_ui"),
                 conditionalPanel(condition = "input.itemtype == 'Select-N'",
                                  checkboxInput("normalize_selectn", "Apply sum-to-zero constraint", value = TRUE),
                                  tags$div(style = "font-size: 13px; color: #666;", HTML("For Select-N identifiability: &Sigma;a = 0 and &Sigma;d = 0"))),
                 hr(),
                 h4("3. Local Dependence"),
                 uiOutput("ld_warning_ui"),
                 div(class = "slider-container",
                     tags$div(class = "sidebar-label", HTML("a*<sub>j</sub> (Item-level LD Magnitude)")),
                     sliderInput("a_star", NULL, min = 0, max = 3, value = 0.5, step = 0.1)),
                 div(class = "slider-container",
                     tags$div(class = "sidebar-label", HTML("&gamma;<sub>ij</sub> (Person-Item Specific Factor)")),
                     sliderInput("gamma", NULL, min = -3, max = 3, value = 0, step = 0.1)),
                 checkboxInput("show_marginal", HTML("Integrate over &gamma; (marginal probability)"), value = FALSE),
                 hr(),
                 h4("4. Display Range"),
                 fluidRow(
                   column(6, numericInput("x_min", HTML("&theta; min"), value = -4, step = 0.5)),
                   column(6, numericInput("x_max", HTML("&theta; max"), value = 4, step = 0.5)))
    ),
    
    mainPanel(width = 9,
              fluidRow(
                column(8, 
                       div(class = "plot-container rccc-plot-wrapper",
                           plotOutput("rccc_plot", height = "auto")),
                       div(class = "score-note",
                           tags$b("Note: "), "Response Combinations (RCs) are grouped by Partial Scoring (PS) = number of options matching the answer key. ",
                           "The cumulative curve shows the sum of all RC probabilities at each score level.","1st RC and 2nd RC represent the most and second-most probable RCs for each score group.")),
                column(4, 
                       wellPanel(style = "max-height: 620px; overflow-y: auto; padding: 12px;",
                                 h5(HTML("Design Matrix Z<sub>j</sub>"), style = "margin-top: 0;"),
                                 tags$div(style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                                          HTML("Z<sub>jxo</sub> = 1 if option <i>o</i> is selected in RC <i>x</i>, 0 otherwise.")),
                                 tableOutput("z_matrix_display")))),
              fluidRow(
                column(8, 
                       div(class = "plot-container",
                           plotOutput("info_plot", height = "320px"))),
                column(4, wellPanel(style = "height: 320px; overflow-y: auto;",
                                    h5("Current Item Summary"),
                                    verbatimTextOutput("info_params_display")))),
              div(class = "formula-box",
                  h4("MRM-LD Model Specification", style = "color: #2c3e50; margin-top: 0;"),
                  fluidRow(
                    column(6,
                           h5("Core Formulas", style = "color: #000;"),
                           tags$div(style = "margin-bottom: 15px; color: #000;",
                                    tags$b("Formula (5) - Option Attractiveness:", style = "color: #000;"),
                                    withMathJax(tags$div(style = "color: #000;", "$$h'_{ijo} = a_{jo}\\theta_i + d_{jo} + W_{jo}a^*_j\\gamma_{ij}$$"))),
                           tags$div(style = "margin-bottom: 15px; color: #000;",
                                    tags$b("Formula (6) - Response Combination Probability Function (RCPF):", style = "color: #000;"),
                                    withMathJax(tags$div(style = "color: #000;", "$$P(Y_{ij} = x | \\theta_i) = \\frac{\\exp\\left(\\sum_{o=1}^{O_j} Z_{jxo} h'_{ijo}\\right)}{\\sum_{m=1}^{X_j} \\exp\\left(\\sum_{o=1}^{O_j} Z_{jmo} h'_{ijo}\\right)}$$"))),
                           tags$div(style = "color: #000;",
                                    tags$b("Formula (21) - Item Information Function:", style = "color: #000;"),
                                    withMathJax(tags$div(style = "color: #000;", "$$I(\\theta) = \\sum_{x=1}^{X_j} \\left[\\sum_{o=1}^{O_j} a_{jo} Z_{jxo}\\right]^2 P(x|\\theta) - \\left[\\sum_{x=1}^{X_j} \\sum_{o=1}^{O_j} a_{jo} Z_{jxo} P(x|\\theta)\\right]^2$$")))),
                    column(6,
                           h5("Parameter Definitions", style = "color: #000;"),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>O<sub>j</sub></span> &mdash; Number of options in item <i>j</i>. ")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>X<sub>j</sub></span> &mdash; Number of valid RCs in item <i>j</i>.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>&theta;<sub>i</sub></span> &mdash; Latent trait (ability) of person <i>i</i>.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>a<sub>jo</sub></span> &mdash; Slope (discrimination) parameter for option <i>o</i> in item <i>j</i>. "), HTML("Positive for correct options (attractiveness increases with &theta;); negative for incorrect options.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>d<sub>jo</sub></span> &mdash; Intercept parameter for option <i>o</i> in item <i>j</i>. "), HTML("Baseline attractiveness when &theta;<sub>i</sub> = 0.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>a*<sub>j</sub></span> &mdash; Local dependence magnitude for item <i>j</i>. "), HTML("When a*<sub>j</sub> = 0, MRM-LD reduces to MRM.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>&gamma;<sub>ij</sub></span> &mdash; Person-item specific ability factor. "), HTML("Reflects specific abilities related to the item beyond &theta;.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>W<sub>jo</sub></span> &mdash; Correctness indicator: +1 for pre-defined correct, &minus;1 for pre-defined incorrect options.")),
                           tags$div(class = "symbol-def", HTML("<span class='param-name'>Z<sub>jxo</sub></span> &mdash; Design matrix element: 1 if option <i>o</i> is selected in RC <i>x</i>, 0 otherwise.")))))
    )
  ),
  div(class = "citation-footer",
      tags$b("Cite: "),
      "Zhou, W., & Guo, L. (2026). Psychometric Model Framework for Multiple Response Items. ",
      tags$i("Psychometrika"), ". ",
      tags$a(href = "https://doi.org/10.1017/psy.2025.10073", target = "_blank", "doi:10.1017/psy.2025.10073"))
)

# ==============================================================================
# 3. Server Logic (Optimized with caching)
# ==============================================================================

server <- function(input, output, session) {
  
  cached_patterns <- reactiveVal(NULL)
  cached_O_j <- reactiveVal(NULL)
  
  # Reactive value for O_j to handle stepper buttons
  O_j_val <- reactiveVal(4)
  
  # Display current O_j value
  output$O_j_display <- renderText({
    O_j_val()
  })
  
  # Stepper button handlers
  observeEvent(input$O_j_minus, {
    current <- O_j_val()
    if (current > 2) {
      O_j_val(current - 1)
      updateNumericInput(session, "O_j", value = current - 1)
    }
  })
  
  observeEvent(input$O_j_plus, {
    current <- O_j_val()
    if (current < 8) {
      O_j_val(current + 1)
      updateNumericInput(session, "O_j", value = current + 1)
    }
  })
  
  # Sync O_j_val with input$O_j (for direct input changes)
  observeEvent(input$O_j, {
    if (!is.null(input$O_j) && input$O_j != O_j_val()) {
      O_j_val(min(max(input$O_j, 2), 8))
    }
  })
  
  # Update button disabled states via custom message

  observe({
    current <- O_j_val()
    session$sendCustomMessage("updateStepperButtons", list(
      minusDisabled = current <= 2,
      plusDisabled = current >= 8
    ))
  })
  
  get_patterns <- reactive({
    O_j <- min(O_j_val(), 8)
    if (is.null(cached_O_j()) || cached_O_j() != O_j) {
      cached_O_j(O_j); cached_patterns(get_all_patterns(O_j))
    }
    cached_patterns()
  })
  
  output$oj_warning_ui <- renderUI({
    if (O_j_val() >= 8) div(class = "key-warning", tags$b("Warning: "), HTML("To avoid performance issues, number of options is limited to a maximum of 8."))
  })
  
  
  output$key_ui <- renderUI({
    O_j <- min(O_j_val(), 8)
    checkbox_list <- lapply(1:O_j, function(o) column(3, checkboxInput(paste0("key_bit_", o), paste0("O", o), value = o <= ceiling(O_j/2))))
    fluidRow(do.call(tagList, checkbox_list))
  })
  
  current_key <- reactive({
    O_j <- min(O_j_val(), 8)
    sapply(1:O_j, function(o) { val <- input[[paste0("key_bit_", o)]]; if(is.null(val)) as.numeric(o <= ceiling(O_j/2)) else as.numeric(val) })
  })
  
  key_valid <- reactive({
    req(input$itemtype)
    if (input$itemtype != "Select-N") return(TRUE)
    key_vals <- current_key(); T_correct <- sum(key_vals)
    T_correct > 0 && T_correct < length(key_vals)
  })
  
  output$key_warning_ui <- renderUI({
    req(input$itemtype)
    if (input$itemtype == "Select-N" && !key_valid()) {
      key_vals <- current_key(); T_correct <- sum(key_vals); O_j <- length(key_vals)
      msg <- if (T_correct == 0) "For Select-N items, at least one option must be marked as correct."
      else if (T_correct == O_j) "For Select-N items, at least one option must be marked as incorrect."
      else "Invalid answer key configuration."
      div(class = "key-warning", tags$b("Invalid Key: "), msg)
    }
  })
  
  make_key_watcher <- function(n) {
    observeEvent(input[[paste0("key_bit_", n)]], {
      current_a <- input[[paste0("a_param_", n)]]; key_val <- input[[paste0("key_bit_", n)]]
      if (!is.null(current_a) && !is.null(key_val)) {
        if (key_val && current_a < 0) updateNumericInput(session, paste0("a_param_", n), value = round(abs(current_a), 2))
        else if (!key_val && current_a > 0) updateNumericInput(session, paste0("a_param_", n), value = round(-abs(current_a), 2))
      }
    }, ignoreInit = TRUE)
  }
  lapply(1:8, make_key_watcher)
  
  output$ld_warning_ui <- renderUI({
    req(input$itemtype)
    if (input$itemtype == "Select-N") {
      T_correct <- sum(current_key()); O_j <- min(O_j_val(), 8)
      if (T_correct == 1 || T_correct == (O_j - 1))
        return(div(class = "ld-warning", tags$b("Note: "), HTML("Local Dependence is not applicable for single-choice items (Select-N with T=1 or T=O<sub>j</sub>−1). The LD parameters below will have no effect.")))
    }
    NULL
  })
  
  initial_params <- reactive({
    O_j <- min(O_j_val(), 8); set.seed(123)
    key_vals <- c(rep(1, ceiling(O_j/2)), rep(0, O_j - ceiling(O_j/2)))
    a_vals <- sapply(1:O_j, function(o) { mag <- runif(1, 0.5, 2.5); if(key_vals[o] == 1) mag else -mag })
    list(a = round(a_vals, 2), d = round(runif(O_j, -2, 2), 2))
  })
  
  output$dynamic_params_ui <- renderUI({
    O_j <- min(O_j_val(), 8); init <- initial_params()
    input_list <- lapply(1:O_j, function(o) {
      tags$div(class = "param-row",
               tags$div(class = "param-label-col", paste0("O", o)),
               tags$div(class = "param-input-col", numericInput(paste0("a_param_", o), NULL, init$a[o], step = 0.1)),
               tags$div(class = "param-input-col", numericInput(paste0("d_param_", o), NULL, init$d[o], step = 0.1)))
    })
    tagList(
      tags$div(class = "param-header-row",
               tags$div(class = "param-label-col", ""),
               tags$div(class = "param-input-col", HTML("a<sub>jo</sub>")),
               tags$div(class = "param-input-col", HTML("d<sub>jo</sub>"))),
      do.call(tagList, input_list))
  })
  
  observeEvent(input$random_btn, {
    O_j <- min(O_j_val(), 8); key_vals <- current_key()
    for(o in 1:O_j) {
      mag_a <- runif(1, 0.5, 2.5)
      updateNumericInput(session, paste0("a_param_", o), value = round(if(key_vals[o] == 1) mag_a else -mag_a, 2))
      updateNumericInput(session, paste0("d_param_", o), value = round(runif(1, -2, 2), 2))
    }
    showNotification("Parameters randomized", type = "message")
  })
  
  current_inputs <- reactive({
    O_j <- min(O_j_val(), 8); init <- initial_params(); key_vals <- current_key()
    a_vals <- sapply(1:O_j, function(o) { val <- input[[paste0("a_param_", o)]]; if(is.null(val)) init$a[o] else val })
    d_vals <- sapply(1:O_j, function(o) { val <- input[[paste0("d_param_", o)]]; if(is.null(val)) init$d[o] else val })
    is_norm <- FALSE
    if (input$itemtype == "Select-N" && isTRUE(input$normalize_selectn)) {
      a_vals <- a_vals - mean(a_vals); d_vals <- d_vals - mean(d_vals); is_norm <- TRUE
    }
    list(a = a_vals, d = d_vals, Key = key_vals, is_norm = is_norm)
  })
  
  output$z_matrix_display <- renderTable({
    req(input$itemtype)
    if (input$itemtype == "Select-N" && !key_valid()) return(NULL)
    O_j <- min(O_j_val(), 8); full_pats <- get_patterns()
    valid_pats <- filter_patterns(full_pats, input$itemtype, current_key())
    pattern_strings <- apply(valid_pats, 1, paste, collapse = "")
    num_ones <- rowSums(valid_pats)
    pattern_values <- apply(valid_pats, 1, function(row) sum(row * (2^((O_j-1):0))))
    z_df <- as.data.frame(valid_pats); colnames(z_df) <- paste0("O", 1:O_j)
    z_df$RC <- pattern_strings; z_df$num_ones <- num_ones; z_df$pattern_val <- pattern_values
    z_df <- z_df[order(z_df$num_ones, -z_df$pattern_val), ]
    z_df$num_ones <- NULL; z_df$pattern_val <- NULL
    cbind(x = 1:nrow(z_df), RC = z_df$RC, z_df[, paste0("O", 1:O_j)])
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", align = "c", digits = 0)
  
  plot_data <- reactive({
    if (input$itemtype == "Select-N" && !key_valid()) return(NULL)
    inputs <- current_inputs(); req(input$x_min, input$x_max)
    theta <- seq(input$x_min, input$x_max, length.out = 100)
    valid_pats <- filter_patterns(get_patterns(), input$itemtype, inputs$Key)
    if (isTRUE(input$show_marginal)) {
      g_points <- seq(-3, 3, length.out = 15); g_weights <- dnorm(g_points); g_weights <- g_weights / sum(g_weights)
      df_final <- calculate_mrm_ld_prob(theta, inputs$a, inputs$d, input$a_star, 0, inputs$Key, valid_pats)
      df_final$Prob <- 0
      for (k in seq_along(g_points)) {
        temp <- calculate_mrm_ld_prob(theta, inputs$a, inputs$d, input$a_star, g_points[k], inputs$Key, valid_pats)
        df_final$Prob <- df_final$Prob + temp$Prob * g_weights[k]
      }
      df_final
    } else calculate_mrm_ld_prob(theta, inputs$a, inputs$d, input$a_star, input$gamma, inputs$Key, valid_pats)
  })
  
  # Dynamic plot height based on O_j
  rccc_plot_height <- reactive({
    O_j <- min(O_j_val(), 8)
    # More rows of facets need more height
    n_scores <- O_j + 1
    n_rows <- ceiling(n_scores / 4)  # Assuming ~4 facets per row on mobile
    base_height <- 280
    per_row <- 200
    max(400, min(800, base_height + n_rows * per_row))
  })
  
  output$rccc_plot <- renderPlot({
    df <- plot_data()
    if (is.null(df)) { plot.new(); text(0.5, 0.5, "Please configure a valid Answer Key for Select-N items.\n(Cannot have all options selected or none selected)", cex = 1.2, col = "#721c24"); return() }
    O_j <- min(O_j_val(), 8); is_marginal <- isTRUE(input$show_marginal)
    plot_title <- if(is_marginal) "Response Combination Characteristic Curves (RCCC) — Marginal" else "Response Combination Characteristic Curves (RCCC) — Conditional"
    plot_subtitle <- if(is_marginal) expression("Probabilities integrated over " * gamma * " ~ N(0,1)") else bquote(gamma[ij] == .(input$gamma))
    
    df_sum <- aggregate(Prob ~ Theta + Score, df, sum)
    df_sum$Pattern <- "Cumulative"; df_sum$Category <- "Cumulative"; df_sum$Rank <- 999
    max_probs <- aggregate(Prob ~ Score + Pattern, df, max); names(max_probs)[3] <- "MaxVal"
    max_probs <- max_probs[order(max_probs$Score, -max_probs$MaxVal), ]
    max_probs$Rank <- ave(max_probs$MaxVal, max_probs$Score, FUN = function(x) seq_along(x))
    df <- merge(df, max_probs[, c("Score", "Pattern", "Rank")], by = c("Score", "Pattern"), all.x = TRUE)
    df$Category <- ifelse(df$Rank == 1, "1st RC", ifelse(df$Rank == 2, "2nd RC", "Other"))
    df$Theta <- as.numeric(df$Theta)
    df_sum_filtered <- df_sum[df_sum$Score > 0 & df_sum$Score < O_j, ]
    df_top <- df[df$Rank <= 2, ]
    df_labels <- do.call(rbind, lapply(split(df_top, list(df_top$Score, df_top$Pattern)), function(x) x[which.max(x$Prob), ]))
    
    my_colors <- c("Cumulative" = "#E41A1C", "1st RC" = "#1B9E77", "2nd RC" = "#377EB8", "Other" = "#999999")
    my_linetypes <- c("Cumulative" = "dashed", "1st RC" = "solid", "2nd RC" = "solid", "Other" = "solid")
    my_labels <- c("Cumulative" = "Cumulative", "1st RC" = "1st RC", "2nd RC" = "2nd RC", "Other" = "Other")
    
    # Determine facet layout based on number of scores
    n_scores <- O_j + 1
    n_cols <- if (n_scores <= 3) n_scores else if (n_scores <= 6) 3 else 4
    
    p <- ggplot() +
      geom_line(data = df[df$Category == "Other", ], aes(x = Theta, y = Prob, group = Pattern, color = "Other", linetype = "Other"), linewidth = 0.4, alpha = 0.5) +
      geom_line(data = df[df$Category %in% c("1st RC", "2nd RC"), ], aes(x = Theta, y = Prob, group = Pattern, color = Category, linetype = Category), linewidth = 0.8) +
      geom_text(data = df_labels, aes(x = Theta, y = Prob, label = Pattern, color = Category), vjust = -0.5, size = 2.8, fontface = "bold", show.legend = FALSE) +
      geom_line(data = df_sum_filtered, aes(x = Theta, y = Prob, group = Score, color = "Cumulative", linetype = "Cumulative"), linewidth = 0.9) +
      scale_color_manual(name = NULL, values = my_colors, labels = my_labels, breaks = c("Cumulative", "1st RC", "2nd RC", "Other")) +
      scale_linetype_manual(name = NULL, values = my_linetypes, labels = my_labels, breaks = c("Cumulative", "1st RC", "2nd RC", "Other")) +
      facet_wrap(~Score, labeller = labeller(Score = function(x) paste0("PS=", x)), scales = "fixed", ncol = n_cols) +
      scale_x_continuous(limits = c(input$x_min, input$x_max)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
      coord_cartesian(clip = "on") +
      theme_bw(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.margin = margin(t = 0, b = 5),
        legend.spacing.x = unit(0.3, "cm"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0),
        plot.title = element_text(size = 14, face = "bold", margin = margin(b = 2)),
        plot.subtitle = element_text(size = 12, color = "#555", margin = margin(b = 8)),
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.3, "lines")
      ) +
      guides(
        color = guide_legend(nrow = 1, override.aes = list(linewidth = 1.2)),
        linetype = guide_legend(nrow = 1)
      ) +
      labs(
        x = expression(theta),
        y = expression(P(RC~"|"~theta)),
        title = plot_title,
        subtitle = plot_subtitle
      )
    print(p)
  }, height = function() { rccc_plot_height() })
  
  output$info_plot <- renderPlot({
    if (input$itemtype == "Select-N" && !key_valid()) { plot.new(); text(0.5, 0.5, "Please configure a valid Answer Key", cex = 1.2, col = "#721c24"); return() }
    inputs <- current_inputs(); req(input$x_min, input$x_max)
    theta <- seq(input$x_min, input$x_max, length.out = 100)
    valid_pats <- filter_patterns(get_patterns(), input$itemtype, inputs$Key)
    is_marginal <- isTRUE(input$show_marginal)
    info <- if(is_marginal) calculate_mrm_info_marginal(theta, inputs$a, inputs$d, input$a_star, inputs$Key, valid_pats)
    else calculate_mrm_info(theta, inputs$a, inputs$d, input$a_star, input$gamma, inputs$Key, valid_pats)
    plot_title <- if(is_marginal) "Item Information Function (IIF) — Marginal" else "Item Information Function (IIF) — Conditional"
    plot_subtitle <- if(is_marginal) expression("Integrated over " * gamma * " ~ N(0,1)") else bquote(gamma[ij] == .(input$gamma))
    df_info <- data.frame(Theta = theta, Information = info)
    ggplot(df_info, aes(x = Theta, y = Information)) +
      geom_line(color = "#0072B2", linewidth = 1.2) + geom_area(alpha = 0.2, fill = "#0072B2") +
      theme_bw(base_size = 12) +
      theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10),
            plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12, color = "#555"),
            plot.margin = margin(5, 10, 5, 10)) +
      labs(x = expression(theta), y = expression(I(theta)), title = plot_title, subtitle = plot_subtitle)
  })
  
  output$info_params_display <- renderPrint({
    if (input$itemtype == "Select-N" && !key_valid()) { cat("Invalid Answer Key configuration.\n"); cat("Please select at least one correct and one incorrect option."); return() }
    inputs <- current_inputs(); O_j <- min(O_j_val(), 8)
    valid_pats <- filter_patterns(get_patterns(), input$itemtype, inputs$Key)
    cat("Item Type:", input$itemtype, "\n"); cat("Oj (Options):", O_j, "\n")
    cat("Xj (Valid RCs):", nrow(valid_pats), "\n"); cat("T (Correct):", sum(inputs$Key), "\n\n")
    cat("Answer Key:", paste(inputs$Key, collapse = ""), "\n\n")
    cat("Option Parameters:\n")
    for(o in 1:O_j) cat(sprintf("  O%d: a=%6.2f, d=%6.2f, W=%+d\n", o, inputs$a[o], inputs$d[o], ifelse(inputs$Key[o] == 1, 1, -1)))
    cat("\nLocal Dependence:\n"); cat("  a*j =", input$a_star, "\n")
    if (isTRUE(input$show_marginal)) cat("  gamma: integrated (marginal)\n") else cat("  gamma_ij =", input$gamma, "\n")
    if(inputs$is_norm) cat("\n[Sum-to-zero applied]\n")
  })
}

shinyApp(ui, server)