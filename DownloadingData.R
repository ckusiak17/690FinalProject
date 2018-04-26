#This code takes in the original .data file and changes the column names
#to the original variable names. Next, the data file is saved in both .Rda and
#.csv formats. To load the data set into your global environment, simply 
#double click on the spam.Rda file in this directory. The .csv is about
#700 KB and thus can't be pushed through GitHub and instead must be 
#shared via email or DropBox.

spambase <- read.csv("spambase.data")

names <- c("word_make", "word_address", "word_all", "word_3d", "word_our", "word_over", "word_remove",
           "word_internet", "word_order", "word_mail", "word_receive", "word_will", "word_people",
           "word_report", "word_addresses", "word_free", "word_business", "word_email", "word_you",
           "word_credit", "word_your", "word_font", "word_000", "word_money", "word_hp", "word_hpl",
           "word_george", "word_650", "word_lab", "word_labs", "word_telnet", "word_857", "word_data",
           "word_415", "word_85", "word_technology", "word_1999", "word_parts", "word_pm",
           "word_direct", "word_cs", "word_meeting", "word_original", "word_project", "word_re",
           "word_edu", "word_table", "word_conference", "semicolon", "parenthesis", "bracket",
           "exclamation", "question", "hashtag", "capital_average", "capital_longest", "capital_total",
           "spam"
           )

colnames(spambase) <- names

save(spambase, file = "spam.Rda")
write.csv(spambase, file = "spam.csv")

