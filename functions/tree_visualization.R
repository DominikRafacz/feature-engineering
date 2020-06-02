visualize_tree <- function(model, filename){
  png(file_out(filename), width = 10500, height = 1500)
  rpart.plot::rpart.plot(model$learner.model)
  dev.off()
}