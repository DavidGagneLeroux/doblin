test_that("Reference file has the correct format", {

  eM <- "# The reference file format has not been respected. Make sure it contains 3 columns named: ID, Time and Reads."
  Column_1 <- c("testing")
  Column_2 <- c(1.675)
  mock_df <- data.frame(Column_1, Column_2)
  expect_error(plotInitFreq(mock_df), eM, fixed = TRUE)

  eM <- "# The values in 'Reads' column of reference file must be numeric."
  ID <- c(1,2,3,4,5)
  Time <- c("d1","d1","d1","d1", "d1")
  Reads <- c("20","20","20","20","20")
  mock_df <- data.frame(ID, Time, Reads)
  expect_error(plotInitFreq(mock_df), eM, fixed = TRUE)

  eM <- "# The values in 'Time' column of reference file must be numeric."
  ID <- c(1,2,3,4,5)
  Time <- c("d1","d1","d1","d1", "d1")
  Reads <- c(20,20,0,20,20)
  mock_df <- data.frame(ID, Time, Reads)
  expect_error(plotInitFreq(mock_df), eM, fixed = TRUE)

})
