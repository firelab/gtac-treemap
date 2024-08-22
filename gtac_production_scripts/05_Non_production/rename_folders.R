# Rename folders


# shell(paste('rename', 
#             sprintf("content/%s-content", pu_name),
#             sprintf("content/%s", other_name)))


folders <- list.files("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2022_Production/03_Evaluation/", full.names = TRUE)

folders <- folders[3:length(folders)]

for(i in seq_along(folders)) {
  
  # test
  #i = 1
  
  folder_in <- folders[i]
  
  # target layer comparison
  file.rename(from = glue::glue("{folder_in}/02_Target_Layer_Comparison/"),
            to = glue::glue("{folder_in}/01_Target_Layer_Comparison/")
  )

  # OOB 
  file.rename(from = glue::glue("{folder_in}/01_OOB_Evaluation/"),
              to = glue::glue("{folder_in}/02_OOB_Evaluation/")
  )
  
  # CV
  file.rename(from = glue::glue("{folder_in}/01_Cross_Validation/"),
              to = glue::glue("{folder_in}/03_Cross_Validation/")
  )
  
  # FIA 
  unlink(glue::glue("{folder_in}/03_FIA_Comparison/"), recursive = TRUE)
  
}

