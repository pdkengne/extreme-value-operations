quarto::quarto_render(
  input = "authoring.qmd", 
  output_format = c("pdf", "html", "docx"),
  output_file = NULL
)


quarto::quarto_publish_doc(
  "authoring.qmd", 
  server = "rpubs.com"
)



