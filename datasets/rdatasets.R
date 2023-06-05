
load("~/git_projects/kausalanalyse_book/datasets/mlda.rda")

write(
  rjson::toJSON(mlda), 
  file = "~/git_projects/kausalanalyse_book/datasets/mlda.json"
)
