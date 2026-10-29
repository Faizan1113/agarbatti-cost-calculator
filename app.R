# ======================================================
#  અગરબત્તી કોસ્ટ કેલ્ક્યુલેટર — R Shiny App
#  app.R  |  Single-file Shiny Application
# ======================================================

library(shiny)

# ── Helpers ────────────────────────────────────────────

fmt_inr <- function(x) {
  if (is.na(x) || !is.finite(x)) return("—")
  formatted <- format(round(x, 2), nsmall = 2, big.mark = ",", scientific = FALSE)
  paste0("\u20b9", formatted)   # ₹
}

fmt_kg <- function(x) {
  if (is.na(x) || !is.finite(x)) return("—")
  paste0(format(round(x, 3), nsmall = 3, big.mark = ",", scientific = FALSE), " કિ.ગ્રા.")
}

# ── UI ────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
    tags$meta(name = "description",
              content = "અગરબત્તી ઉત્પાદન ખર્ચ કેલ્ક્યુલેટર"),
    tags$meta(name = "theme-color", content = "#1b5e20"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title", content = "અગરબત્તી કેલ્ક"),
    tags$title("અગરબત્તી કોસ્ટ કેલ્ક્યુલેટર"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('resetInputs', function(defaults) {
        Shiny.setInputValue('perfume_wt',    defaults.perfume_wt,    {priority: 'event'});
        Shiny.setInputValue('perfume_price', defaults.perfume_price, {priority: 'event'});
        Shiny.setInputValue('dep_wt',        defaults.dep_wt,        {priority: 'event'});
        Shiny.setInputValue('dep_price',     defaults.dep_price,     {priority: 'event'});
        Shiny.setInputValue('num_packets',   defaults.num_packets,   {priority: 'event'});
        Shiny.setInputValue('packet_wt_g',   defaults.packet_wt_g,  {priority: 'event'});
        Shiny.setInputValue('raw_price',     defaults.raw_price,     {priority: 'event'});
        Shiny.setInputValue('label_charge',  defaults.label_charge,  {priority: 'event'});

        document.getElementById('perfume_wt').value    = defaults.perfume_wt;
        document.getElementById('perfume_price').value = defaults.perfume_price;
        document.getElementById('dep_wt').value        = defaults.dep_wt;
        document.getElementById('dep_price').value     = defaults.dep_price;
        document.getElementById('num_packets').value   = defaults.num_packets;
        document.getElementById('packet_wt_g').value   = defaults.packet_wt_g;
        document.getElementById('raw_price').value     = defaults.raw_price;
        document.getElementById('label_charge').value  = defaults.label_charge;
      });
    "))
  ),

  div(class = "app-shell",

    # ── Header ──
    div(class = "app-header",
      tags$span(class = "logo-icon", "\U0001F9F2"),
      tags$h1("અગરબત્તી કોસ્ટ કેલ્ક્યુલેટર"),
      tags$p(class = "subtitle", "તમારા વ્યવસાય માટે ઉત્પાદન ખર્ચ કેલ્ક્યુલેટર")
    ),

    # ── A: અત્તર વિગત ──
    div(class = "section-card",
      div(class = "section-title",
        tags$span(class = "section-icon", "\U0001F4A7"),
        "અ. અત્તર વિગત"
      ),

      div(class = "form-group",
        tags$label(`for` = "perfume_wt", "અત્તર વજન (કિ.ગ્રા.)"),
        numericInput("perfume_wt", label = NULL,
                     value = NA_real_, min = 0, step = 0.1, width = "100%")
      ),

      div(class = "form-group",
        tags$label(`for` = "perfume_price", "અત્તર ભાવ (₹/કિ.ગ્રા.)"),
        div(class = "rupee-wrap",
          numericInput("perfume_price", label = NULL,
                       value = NA_real_, min = 0, step = 1, width = "100%")
        )
      )
    ),

    # ── બ: ડી.ઈ.પી. તેલ ──
    div(class = "section-card",
      div(class = "section-title",
        tags$span(class = "section-icon", "\U0001F6E2"),
        "બ. ડી.ઈ.પી. તેલ"
      ),

      div(class = "form-group",
        tags$label(`for` = "dep_wt", "ડી.ઈ.પી. વજન (કિ.ગ્રા.)"),
        numericInput("dep_wt", label = NULL,
                     value = NA_real_, min = 0, step = 0.1, width = "100%")
      ),

      div(class = "form-group",
        tags$label(`for` = "dep_price", "ડી.ઈ.પી. ભાવ (₹/કિ.ગ્રા.)"),
        div(class = "rupee-wrap",
          numericInput("dep_price", label = NULL,
                       value = NA_real_, min = 0, step = 1, width = "100%")
        )
      )
    ),

    # ── ક: ઉત્પાદન ──
    div(class = "section-card",
      div(class = "section-title",
        tags$span(class = "section-icon", "\U0001F4E6"),
        "ક. ઉત્પાદન"
      ),

      div(class = "form-group",
        tags$label(`for` = "num_packets", "પેકેટ સંખ્યા"),
        numericInput("num_packets", label = NULL,
                     value = NA_integer_, min = 1, step = 1, width = "100%")
      ),

      div(class = "form-group",
        tags$label(`for` = "packet_wt_g", "એક પેકેટ વજન (ગ્રામ)"),
        numericInput("packet_wt_g", label = NULL,
                     value = NA_real_, min = 0.1, step = 1, width = "100%")
      )
    ),

    # ── ડ: કાચી અગરબત્તી ──
    div(class = "section-card",
      div(class = "section-title",
        tags$span(class = "section-icon", "\U0001FA9D"),
        "ડ. કાચી અગરબત્તી"
      ),

      div(class = "form-group",
        tags$label(`for` = "raw_price", "કાચી અગરબત્તી નો ભાવ (₹/કિ.ગ્રા.)"),
        div(class = "rupee-wrap",
          numericInput("raw_price", label = NULL,
                       value = NA_real_, min = 0, step = 1, width = "100%")
        )
      )

      
    ),

    # ── ઈ: લેબલ અને પેકેજિંગ ──
    div(class = "section-card",
      div(class = "section-title",
        tags$span(class = "section-icon", "\U0001F3F7"),
        "ઈ. લેબલ અને પેકેજિંગ"
      ),

      div(class = "form-group",
        tags$label(`for` = "label_charge", "કુલ લેબલ + પેકેજિંગ ચાર્જ (₹)"),
        div(class = "rupee-wrap",
          numericInput("label_charge", label = NULL,
                       value = 1, min = 0, step = 1, width = "100%")
        ),
        tags$p(style = "font-size:0.78rem; color:#777; margin-top:6px;",
               "પેકેટ સંખ્યા દાખલ કરતાં આ આંક આપોઆપ આવી જશે. જરૂર પડ્યે બદલી શકો.")
      )
    ),

    # ── Buttons ──
    div(class = "btn-area",
      actionButton("calc_btn", "\u2714\uFE0F  ખર્ચ ગણો",
                   class = "btn-calc"),
      actionButton("reset_btn", "\u21BA  રીસેટ",
                   class = "btn-reset")
    ),

    uiOutput("validation_ui"),
    uiOutput("results_ui"),

    div(class = "app-footer",
      "અગરબત્તી કોસ્ટ કેલ્ક્યુલેટર \u2022 તમારા વ્યવસાય માટે"
    )
  )
)

# ── Server ────────────────────────────────────────────

server <- function(input, output, session) {



  DEFAULTS <- list(
    perfume_wt    = NA_real_,
    perfume_price = NA_real_,
    dep_wt        = NA_real_,
    dep_price     = NA_real_,
    num_packets   = NA_integer_,
    packet_wt_g   = NA_real_,
    raw_price     = NA_real_,
    label_charge  = 1
  )

  observeEvent(input$reset_btn, {
    session$sendCustomMessage("resetInputs", DEFAULTS)
    output$validation_ui <- renderUI(NULL)
    output$results_ui    <- renderUI(NULL)
  })

  calc_result <- eventReactive(input$calc_btn, {

    perfume_wt    <- input$perfume_wt
    perfume_price <- input$perfume_price
    dep_wt        <- input$dep_wt
    dep_price     <- input$dep_price
    num_packets   <- input$num_packets
    packet_wt_g   <- input$packet_wt_g
    raw_price     <- input$raw_price
    label_charge  <- input$label_charge

    errors <- character(0)

    if (is.na(perfume_wt)    || !is.finite(perfume_wt)    || perfume_wt    < 0)
      errors <- c(errors, "અત્તર વજન સાચો આંક દાખલ કરો.")
    if (is.na(perfume_price)  || !is.finite(perfume_price)  || perfume_price  < 0)
      errors <- c(errors, "અત્તર ભાવ સાચો આંક દાખલ કરો.")
    if (is.na(dep_wt)         || !is.finite(dep_wt)         || dep_wt         < 0)
      errors <- c(errors, "ડી.ઈ.પી. વજન સાચો આંક દાખલ કરો.")
    if (is.na(dep_price)      || !is.finite(dep_price)      || dep_price      < 0)
      errors <- c(errors, "ડી.ઈ.પી. ભાવ સાચો આંક દાખલ કરો.")
    if (is.na(num_packets)    || !is.finite(num_packets)    || num_packets    < 1 || num_packets != floor(num_packets))
      errors <- c(errors, "પેકેટ સંખ્યા પૂર્ણ ધન આંક હોવો જોઈએ (દા.ત. ૨૦૦).")
    if (is.na(packet_wt_g)   || !is.finite(packet_wt_g)   || packet_wt_g   <= 0)
      errors <- c(errors, "એક પેકેટ વજન શૂન્ય કરતાં વધારે હોવું જોઈએ.")
    if (is.na(raw_price)      || !is.finite(raw_price)      || raw_price      < 0)
      errors <- c(errors, "કાચો માલ ભાવ સાચો આંક દાખલ કરો.")
    if (is.na(label_charge)   || !is.finite(label_charge)   || label_charge   < 0)
      errors <- c(errors, "લેબલ અને પેકેજિંગ ચાર્જ ઋણ ન હોઈ શકે.")

    if (length(errors) > 0) return(list(ok = FALSE, errors = errors))

    # Step 1: કુલ મિશ્રણ વજન
    total_mix_kg <- num_packets * packet_wt_g / 1000

    # Step 2: કાચો માલ વજન
    raw_wt_kg <- total_mix_kg - perfume_wt - dep_wt

    if (raw_wt_kg < 0) {
      return(list(
        ok = FALSE,
        errors = c(paste0(
          "અત્તર (", perfume_wt, " કિ.ગ્રા.) + ડી.ઈ.પી. (", dep_wt,
          " કિ.ગ્રા.) = ", perfume_wt + dep_wt,
          " કિ.ગ્રા. — આ કુલ મિશ્રણ વજન ", round(total_mix_kg, 3),
          " કિ.ગ્રા. કરતાં વધારે છે. વજન તપાસો."
        ))
      ))
    }

    # Step 3: ખર્ચ
    cost_perfume <- perfume_wt  * perfume_price
    cost_dep     <- dep_wt      * dep_price
    cost_raw     <- raw_wt_kg   * raw_price
    cost_label   <- num_packets * label_charge

    # Step 4: કુલ બેચ ખર્ચ
    total_batch  <- cost_perfume + cost_dep + cost_raw + cost_label

    # Step 5: પ્રતિ પેકેટ ખર્ચ
    cost_per_pkt <- total_batch / num_packets

    list(
      ok           = TRUE,
      perfume_wt   = perfume_wt,
      dep_wt       = dep_wt,
      num_packets  = num_packets,
      total_mix_kg = total_mix_kg,
      raw_wt_kg    = raw_wt_kg,
      cost_perfume = cost_perfume,
      cost_dep     = cost_dep,
      cost_raw     = cost_raw,
      cost_label   = cost_label,
      total_batch  = total_batch,
      cost_per_pkt = cost_per_pkt
    )
  })

  # ── ભૂલ સંદેશ ──
  output$validation_ui <- renderUI({
    req(input$calc_btn > 0)
    res <- calc_result()
    if (res$ok) return(NULL)
    div(class = "validation-box",
      tags$span(class = "warn-icon", "\u26A0\uFE0F"),
      tags$div(
        tags$strong("નીચેની ભૂલો સુધારો:"),
        tags$ul(style = "margin: 6px 0 0 16px;",
          lapply(res$errors, function(e) tags$li(e))
        )
      )
    )
  })

  # ── પરિણામ ──
  output$results_ui <- renderUI({
    req(input$calc_btn > 0)
    res <- calc_result()
    if (!res$ok) return(NULL)

    div(class = "results-panel",
      tags$p(class = "results-title", "\U0001F4CA પરિણામ"),

      # Hero card
      div(class = "hero-cost-card",
        div(class = "hero-label", "પ્રતિ પેકેટ ઉત્પાદન ખર્ચ"),
        div(class = "hero-value", fmt_inr(res$cost_per_pkt)),
        div(class = "hero-sub",
            paste0(format(res$num_packets, big.mark = ","), " પેકેટની બેચ"))
      ),

      # વજન વિગત
      div(class = "breakdown-card",
        div(class = "breakdown-header", "\u2696\uFE0F  વજન વિગત"),
        div(class = "breakdown-row",
          span(class = "row-label", "કુલ મિશ્રણ વજન"),
          span(class = "row-value", fmt_kg(res$total_mix_kg))
        ),
        div(class = "breakdown-row",
          span(class = "row-label", "અત્તર વજન"),
          span(class = "row-value", fmt_kg(res$perfume_wt))
        ),
        div(class = "breakdown-row",
          span(class = "row-label", "ડી.ઈ.પી. વજન"),
          span(class = "row-value", fmt_kg(res$dep_wt))
        ),
        div(class = "breakdown-row",
          span(class = "row-label", "કાચી અગરબત્તી નો વજન (આપોઆપ)"),
          span(class = "row-value", fmt_kg(res$raw_wt_kg))
        )
      ),

      # ખર્ચ વિગત
      div(class = "breakdown-card",
        div(class = "breakdown-header", "\u20B9  ખર્ચ વિગત"),
        div(class = "breakdown-row",
          span(class = "row-label", paste0("અત્તર ખર્ચ (", fmt_kg(res$perfume_wt), ")")),
          span(class = "row-value", fmt_inr(res$cost_perfume))
        ),
        div(class = "breakdown-row",
          span(class = "row-label", paste0("ડી.ઈ.પી. ખર્ચ (", fmt_kg(res$dep_wt), ")")),
          span(class = "row-value", fmt_inr(res$cost_dep))
        ),
        div(class = "breakdown-row",
          span(class = "row-label", paste0("કાચી અગરબત્તી નો ખર્ચ (", fmt_kg(res$raw_wt_kg), ")")),
          span(class = "row-value", fmt_inr(res$cost_raw))
        ),
        div(class = "breakdown-row",
          span(class = "row-label",
               paste0("લેબલ+પેક (" , format(res$num_packets, big.mark = ","), " પેકેટ)")),
          span(class = "row-value", fmt_inr(res$cost_label))
        ),
        div(class = "breakdown-row total-row",
          span(class = "row-label", "કુલ બેચ ખર્ચ"),
          span(class = "row-value", fmt_inr(res$total_batch))
        )
      )
    )
  })
}

# ── Launch ────────────────────────────────────────────

shinyApp(ui = ui, server = server)
