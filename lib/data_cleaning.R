data_cleaning <- function(filename){
  
  filepath       <- paste0("../data/nameset/", filename, ".txt")
  QuestAuthor    <- data.frame(scan(file = filepath, 
                                    what = list(Coauthor = "", Paper = "", Journal = ""),
                                    sep = ">",
                                    quiet = TRUE),
                               stringsAsFactors = TRUE)
  library(stringr)
  # extract canonical author id befor "_"
  QuestAuthor$AuthorID  <- str_replace(QuestAuthor$Coauthor, "_.*", "")
  # extract paper number under same author between "_" and first whitespace
  QuestAuthor$PaperNO   <- str_replace(QuestAuthor$Coauthor, "\\d+_", "")
  QuestAuthor$PaperNO   <- str_replace(QuestAuthor$Coauthor, "(\\s+.*)*", "")
  # extract coauthor name
  QuestAuthor$Coauthor  <- str_replace(QuestAuthor$Coauthor,"^.*?\\s","")
  QuestAuthor$Coauthor  <- str_replace_all(QuestAuthor$Coauthor,"<","")
  # extract papaer name
  QuestAuthor$Paper     <- str_replace_all(QuestAuthor$Paper,"<","")
  # assign paperID
  QuestAuthor$PaperID   <- rownames(QuestAuthor)
  
  write.csv(QuestAuthor, file = paste0("../output/cleaned_data/",filename, ".csv"))
}
