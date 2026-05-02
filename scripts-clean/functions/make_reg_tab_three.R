make_reg_tab <- function(inputdf,pred) {
  tab <- inputdf %>%
    mutate("β estimate [95% CI]" = sprintf("%.3f [%.2f, %.2f]", estimate, conf.low, conf.high)) %>%
    mutate(
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".", 
        TRUE           ~ "N.S." 
      )
    ) %>%
    select(Model, "β estimate [95% CI]", std.error, statistic, p.value, stars) %>%
    arrange(desc(Model)) %>%
    flextable() %>%
    colformat_double(
      j = c("std.error", "statistic"),
      digits = 3,          
      big.mark = ""       
    ) %>%
    set_header_labels(
      std.error = "Std Error",
      statistic = "t statistic",
      p.value = "p",
      stars = "sig"
    ) %>%
    compose(
      j = "p.value", 
      i = ~ p.value < 0.001, # Only apply to small values
      value = as_paragraph(
        as_chunk(formatC(p.value, format = "e", digits = 1) %>% 
                   str_replace("e.*", " \u00D7 10")),
        as_chunk(str_extract(formatC(p.value, format = "e"), "[-|+]\\d+"), 
                 props = fp_text(vertical.align = "superscript"))
      )
    ) %>%
    colformat_double(
      j = "p.value",
      i =  ~ p.value >= 0.001,
      digits = 3
    ) %>%
    align(align = "center", part = "header") %>%  
    set_caption(
      caption = as_paragraph(
        as_chunk(
          paste("Table SX: Summary of Regression Results for ", pred, sep=""), 
          props = fp_text_default(
            font.family = "Arial", 
            font.size = 11, 
            underlined = TRUE))), 
      fp_p = fp_par(text.align = "left"),
      align_with_table = FALSE) %>%   
    fix_border_issues() %>%            # Fixes lines broken by merging
    font(fontname = "Arial", part = "all") %>%
    autofit()  
  
  return(tab)
  
}