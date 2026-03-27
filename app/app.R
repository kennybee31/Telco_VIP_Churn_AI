# 1. 載入套件
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(bsicons)
library(scales)
library(munsell)

# --- 加入這行，讓網頁讀取資料檔 ---
final_decision_table <- readRDS("final_decision_table.rds")

# 2. 戰略字典 (強化動態敏感度)
lang_dict <- list(
  "zh" = list(
    app_title = "電信 VIP 價值保衛戰：AI 戰略執行系統",
    tab1 = "戰略指揮中心", tab2 = "戰略執行清單",
    kpi1_t = "預計每月營收風險", kpi2_t = "極高風險 VIP", kpi3_t = "預計挽回收益 (ROI)",
    sl_con = "合約轉化目標 (%)", sl_sat = "滿意度優化 (1-5分)", sl_sup = "支援效率提升 (%)",
    ai_h = "AI 實時戰略建議",
    p1_h = "風險遷移震撼圖：Baseline vs 模擬後", p2_h = "戰略成本與收益分析",
    col_id = "客戶ID", col_fee = "月費", col_prob = "流失風險", col_act = "挽救戰略", col_chan = "執行管道",
    act_1 = "贈送 5G 數據包 (30GB)", act_2 = "一年約 85 折優惠券", act_3 = "專屬 VIP 續約方案",
    chan_1 = "系統自動短訊", chan_2 = "客服中心電聯", chan_3 = "資深客戶經理親訪",
    # AI 洞察邏輯文字
    ai_1 = "【緊急】滿意度過低！這正嚴重抵消您的合約成效，請優先優化服務品質。",
    ai_2 = "【戰略】支援效率已達標，目前每提升 0.5 分滿意度將回補更多 VIP 營收。",
    ai_3 = "【機會】合約轉化率具備高權重，建議立即發動大規模續約優惠專案。",
    ai_4 = "【優化】目前處於戰略平衡點，建議針對救援名單進行『經理親訪』。",
    legal = "【法律聲明】本建議基於 XGBoost 模擬。符合 ISO 42001 AI 治理規範。",
    source = "數據源：250K Customer Churn | 更新：2026-03-27"
  ),
  "en" = list(
    app_title = "Telecom Churn AI: Strategic Execution",
    tab1 = "Strategy Center", tab2 = "Execution List",
    kpi1_t = "Revenue at Risk", kpi2_t = "High Risk VIPs", kpi3_t = "Expected ROI",
    sl_con = "Contract Target (%)", sl_sat = "Sat. Improvement (1-5)", sl_sup = "Support Eff. (%)",
    ai_h = "AI Strategic Insight",
    p1_h = "Risk Migration: Baseline vs Simulated", p2_h = "Cost-Benefit Analysis",
    col_id = "CustomerID", col_fee = "Monthly", col_prob = "Risk (%)", col_act = "Strategy", col_chan = "Channel",
    act_1 = "5G Data Pack (30GB)", act_2 = "15% Off 1Y Contract", act_3 = "Bespoke VIP Renewal",
    chan_1 = "Automated SMS", chan_2 = "Call Center", chan_3 = "Relationship Manager",
    ai_1 = "CRISIS: Low satisfaction is neutralizing your efforts. Fix service quality now!",
    ai_2 = "STRATEGY: Support efficiency is optimal. Boosting satisfaction now yields peak ROI.",
    ai_3 = "OPPORTUNITY: Contract conversion is highly effective. Scale up renewal campaigns.",
    ai_4 = "OPTIMIZED: Current strategy is balanced. Focus on high-touch VIP outreach.",
    legal = "[Legal] Predictions based on XGBoost. ISO 42001 Compliant.",
    source = "Source: 250K Churn Data | Updated: 2026-03-27"
  )
)

# 3. UI
ui <- page_navbar(
  title = textOutput("app_title"),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#16A085"),
  nav_panel(textOutput("tab1_name"),
            layout_sidebar(
              sidebar = sidebar(
                width = 320,
                radioButtons("lang", "Language / 語言", choices = c("中文" = "zh", "English" = "en"), inline = TRUE),
                hr(),
                # 移除了 renderUI，改用靜態 Slider 配合 observe 更新，解決反應遲鈍問題
                sliderInput("s_con", "Contract Target", 0, 100, 20, post = "%"),
                sliderInput("s_sat", "Satisfaction Improvement", 1, 5, 2, step = 1, ticks = TRUE),
                sliderInput("s_sup", "Support Efficiency", 0, 100, 30, post = "%"),
                hr(),
                card(card_header(textOutput("ai_h_ui")), textOutput("ai_txt"), style = "background-color: #f8fcfb; border-left: 5px solid #16A085; min-height: 120px;")
              ),
              layout_column_wrap(
                width = 1/3,
                value_box(title = textOutput("kpi1_t"), value = textOutput("kpi1_v"), showcase = bs_icon("graph-down"), theme = "danger"),
                value_box(title = textOutput("kpi2_t"), value = textOutput("kpi2_v"), showcase = bs_icon("people"), theme = "warning"),
                value_box(title = textOutput("kpi3_t"), value = textOutput("kpi3_v"), showcase = bs_icon("calculator"), theme = "success")
              ),
              layout_column_wrap(
                width = 1/2,
                card(card_header(textOutput("p1_h_ui")), plotlyOutput("dist_plot")),
                card(card_header(textOutput("p2_h_ui")), plotlyOutput("roi_plot"))
              )
            )
  ),
  nav_panel(textOutput("tab2_name"), card(DTOutput("vip_dt"))),
  nav_spacer(),
  nav_item(span(textOutput("footer"), style = "font-size: 0.8rem; color: #95a5a6; padding: 15px;"))
)

# 4. Server
server <- function(input, output, session) {
  # 語系同步核心 (修正 Issue 1)
  L <- reactive({ lang_dict[[input$lang]] })
  
  observe({
    updateSliderInput(session, "s_con", label = L()$sl_con)
    updateSliderInput(session, "s_sat", label = L()$sl_sat)
    updateSliderInput(session, "s_sup", label = L()$sl_sup)
  })
  
  output$app_title <- renderText({ L()$app_title }); output$tab1_name <- renderText({ L()$tab1 }); output$tab2_name <- renderText({ L()$tab2 })
  output$ai_h_ui <- renderText({ L()$ai_h }); output$footer <- renderText({ paste(L()$legal, "|", L()$source) })
  output$kpi1_t <- renderText({ L()$kpi1_t }); output$kpi2_t <- renderText({ L()$kpi2_t }); output$kpi3_t <- renderText({ L()$kpi3_t })
  output$p1_h_ui <- renderText({ L()$p1_h }); output$p2_h_ui <- renderText({ L()$p2_h })
  
  # 模擬運算引擎
  sim_data <- reactive({
    req(input$s_sat, input$s_con, input$s_sup)
    # 權重參數：滿意度影響最大
    sat_eff <- (input$s_sat - 1) * 0.12
    sup_eff <- (input$s_sup / 100) * 0.05
    
    final_decision_table %>%
      mutate(
        sim_prob = pmax(0, pmin(1, .pred_1 - sat_eff - sup_eff)),
        strategy = case_when(is_hvc == "VIP" & sim_prob > 0.8 ~ L()$act_3, is_hvc == "VIP" ~ L()$act_2, TRUE ~ L()$act_1),
        channel = case_when(is_hvc == "VIP" & sim_prob > 0.8 ~ L()$chan_3, is_hvc == "VIP" ~ L()$chan_2, TRUE ~ L()$chan_1)
      )
  })
  
  # --- 修正：AI 戰略洞察敏感度引擎 ---
  output$ai_txt <- renderText({
    # 強制監聽所有拉桿變動
    con <- input$s_con; sat <- input$s_sat; sup <- input$s_sup
    
    # 使用層級過濾邏輯，確保每個狀態都有對應文本
    if (sat < 2) {
      return(L()$ai_1)
    } else if (sup > 60 && sat < 4) {
      return(L()$ai_2)
    } else if (con > 40) {
      return(L()$ai_3)
    } else {
      return(L()$ai_4)
    }
  })
  
  # 圖表渲染
  output$dist_plot <- renderPlotly({
    res <- sim_data()
    plot_df <- bind_rows(
      final_decision_table %>% select(prob = .pred_1) %>% mutate(Group = "Baseline"),
      res %>% select(prob = sim_prob) %>% mutate(Group = "Strategized")
    ) %>% filter(prob > 0.1)
    
    p <- ggplot(plot_df, aes(x = prob, fill = Group)) +
      geom_density(alpha = 0.6, color = NA) +
      scale_fill_manual(values = c("#BDC3C7", "#16A085")) +
      theme_minimal() +
      labs(x = "Risk Prob", y = "Density")
    
    ggplotly(p) %>% layout(margin = list(t = 50), legend = list(orientation = "h", y = 1.1))
  })
  
  output$roi_plot <- renderPlotly({
    res <- sim_data()
    orig_risk <- sum(final_decision_table$monthly_charges[final_decision_table$.pred_1 > 0.5])
    sim_risk <- sum(res$monthly_charges[res$sim_prob > 0.5])
    plot_ly(x = c("Before", "After"), y = c(orig_risk, sim_risk), type = "bar", marker = list(color = c("#E74C3C", "#27AE60"))) %>%
      layout(yaxis = list(title = "USD $"), margin = list(t=30))
  })
  
  # KPI 與表格
  output$kpi1_v <- renderText({
    res <- sim_data(); val <- sum(res$monthly_charges[res$sim_prob > 0.5])
    paste0("$ ", format(round(val, 0), big.mark = ","))
  })
  output$kpi2_v <- renderText({
    res <- sim_data(); format(nrow(res %>% filter(is_hvc == "VIP", sim_prob > 0.8)), big.mark = ",")
  })
  output$kpi3_v <- renderText({
    res <- sim_data()
    orig_risk <- sum(final_decision_table$monthly_charges[final_decision_table$.pred_1 > 0.5])
    sim_risk <- sum(res$monthly_charges[res$sim_prob > 0.5])
    val <- (orig_risk - sim_risk) * 0.7
    paste0("$ ", format(round(val, 0), big.mark = ","))
  })
  
  output$vip_dt <- renderDT({
    res <- sim_data()
    table_dt <- res %>%
      filter(sim_prob > 0.5) %>%
      select(customer_id, monthly_charges, sim_prob, strategy, channel) %>%
      mutate(sim_prob = paste0(round(sim_prob * 100, 1), "%")) %>%
      arrange(desc(monthly_charges))
    colnames(table_dt) <- c(L()$col_id, L()$col_fee, L()$col_prob, L()$col_act, L()$col_chan)
    datatable(table_dt, options = list(pageLength = 10, dom = 'Bfrtip')) %>% formatCurrency(2, "$")
  })
}

shinyApp(ui, server)