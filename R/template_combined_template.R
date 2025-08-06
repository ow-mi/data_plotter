# Combined Template (for backward compatibility)
# This builds the combined template from the separate parts

# ggplot_template <- paste(
#   "# Combined plotting code - built from separate templates",
#   "# Text code:",
#   "text_code <- '", gsub("'", "\\\\'", ggplot_text_template), "'",
#   "",
#   "# Static code:", 
#   "static_code <- '", gsub("'", "\\\\'", ggplot_static_template), "'",
#   "",
#   "# Interactive code:",
#   "interactive_code <- '", gsub("'", "\\\\'", ggplot_interactive_template), "'",
#   "",
#   "# Table code:",
#   "table_code <- '", gsub("'", "\\\\'", ggplot_table_template), "'",
#   "",
#   "# Final conditional code:",
#   ggplot_final_template,
#   sep = "\n"
# )