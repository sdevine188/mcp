library(tidyverse)
library(KeyboardSimulator)

# microsoft word shortcutes
# https://support.microsoft.com/en-us/office/use-the-keyboard-to-work-with-the-ribbon-in-word-467089f1-b4f9-4407-a80a-6a1f351fd44e

# list of keyboardSimulator commands
# ?keyboard_value

# create finalize_country_profile_as_pdf()
finalize_country_profile_as_pdf <- function(current_country, current_fy) {
        
        # delete old blank_doc if it exists
        if("output/charts/country_profile_blank_doc.docx" %in% dir_ls("output/charts")) {
                
                file_delete(path = "output/charts/country_profile_blank_doc.docx")
        }
        
        # create blank_doc
        read_docx() %>% print(target = "output/charts/country_profile_blank_doc.docx")
        
        # sleep 
        Sys.sleep(time = 3)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # if necessary, delete old pdf output 
        old_pdf_path <- str_c("output/charts/Malign Influence Resilience Index - ", current_country, " FY ", as.character(current_fy), " Country Profile.pdf")
        if(old_pdf_path %in% dir_ls("output/charts")) {
                
                file_delete(path = old_pdf_path)
        }
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # open doc 
        
        # get filename 
        # filename <- str_c("C:\\Users\\sdevine\\Desktop\\usaid\\mcp\\malign_influence\\",
        #                 "output\\charts\\country_profile_", current_country, ".docx")
        filename <- str_c("C:\\Users\\Stephen\\Desktop\\usaid\\mcp\\malign_influence\\",
                          "output\\charts\\country_profile_", current_country, ".docx")
        
        # open word doc 
        shell(shQuote(string = filename), wait = FALSE)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # center image horizontally and vertically
        
        # sleep 
        Sys.sleep(time = 7)
        
        # click on image in doc
        mouse.move(x = 960, y = 540)
        mouse.click(button = "left")
        
        # bring image in front of text so it can be moved
        keybd.press(button = "Shift+F10")
        keybd.press(button = "w")
        keybd.press(button = "n")
        
        # move image randomly/slightly so that align center/vertical will work
        mouse.move(x = 960, y = 540)
        keybd.press(button = "left")
        keybd.press(button = "up")
        
        
        # center image vertically
        keybd.press(button = "Alt")
        keybd.press(button = "p")
        keybd.press(button = "a")
        keybd.press(button = "a")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        
        # center image horizontally
        keybd.press(button = "Alt")
        keybd.press(button = "p")
        keybd.press(button = "a")
        keybd.press(button = "a")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        keybd.press(button = "right")
        keybd.press(button = "right")
        keybd.press(button = "right")
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # add, size, and position text box with hyperlink
        
        # sleep 
        # Sys.sleep(time = 2)
        Sys.sleep(time = 5)
        
        # open insert menu
        keybd.press(button = "Alt+n")
        
        # open text box menu
        keybd.press(button = "x")
        
        # select draw text box
        keybd.press(button = "d")
        
        # # draw text box on old gov laptop
        # mouse.move(x = 960, y = 540)
        # mouse.click(button = "left", hold = FALSE)
        
        # draw text box on new gov laptop
        mouse.move(x = 500, y = 400)
        mouse.click(button = "left", hold = FALSE)
        
        # get text
        pre_linked_text_1 <- "See the Malign Influence Resilience Index "
        linked_text_1_first_word <- "shared "
        linked_text_1_remainder <- "drive"
        # evntually can link to dashboard e.g. "; a dashboard is also available."
        post_linked_text_1 <- " for more information, including the methodology guide."
        # linked_text_2_first_word <- "methodology "
        # linked_text_2_remainder <- "guide"
        # post_linked_text_2 <- "."
        
        # get url_links
        url_link_1 <- "https://drive.google.com/drive/folders/1XKM9HqG8VDVXwN_nZ22DXVLONC9kwuPQ"
        # url_link_2 <- "https://drive.google.com/file/d/1JOK2vbQZGa54iDt3vFkbzFk5i-GilEB_/view?usp=sharing"
        
        # type linked_text_1, and undo auto-capitalization after first word
        keybd.type_string(string = linked_text_1_first_word)
        keybd.press(button = "Ctrl+z")
        keybd.type_string(string = linked_text_1_remainder)
        
        # sleep 
        Sys.sleep(time = 2)
        
        # add hyperlink to linked_text_1
        keybd.press(button = "Ctrl+a")
        keybd.press(button = "Shift+F10")
        keybd.press(button = "i")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        keybd.type_string(string = url_link_1)
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # type pre_linked_text_1
        keybd.press(button = "Ctrl+up")
        keybd.type_string(string = pre_linked_text_1)
        
        # sleep 
        Sys.sleep(time = 2)
        
        # type post_linked_text_1
        keybd.press(button = "Ctrl+down")
        keybd.type_string(string = post_linked_text_1)
        
        # # cut current text temporarily and add linked_text_2, and undo auto-capitalization after first word
        # keybd.press(button = "Ctrl+a")
        # keybd.press(button = "Ctrl+x")
        # keybd.type_string(string = linked_text_2_first_word)
        # keybd.press(button = "Ctrl+z")
        # keybd.type_string(string = linked_text_2_remainder)
        # 
        # # add hyperlink to linked_text_2
        # keybd.press(button = "Ctrl+a")
        # keybd.press(button = "Shift+F10")
        # keybd.press(button = "i")
        # keybd.press(button = "down")
        # keybd.press(button = "Enter")
        # keybd.type_string(string = url_link_2)
        # keybd.press(button = "Enter")
        # 
        # # paste back in prior text that was cut, and backspace to eliminate auto-newline
        # keybd.press(button = "Ctrl+up")
        # keybd.press(button = "Ctrl+v")
        # keybd.press(button = "backspace")
        # 
        # # type post_linked_text_2
        # keybd.press(button = "Ctrl+down")
        # keybd.type_string(string = post_linked_text_2)
        
        # sleep 
        Sys.sleep(time = 2)
        
        # set text font 
        keybd.press(button = "Ctrl+a")
        keybd.press(button = "Alt+h")
        keybd.press(button = "f")
        keybd.press(button = "f")
        keybd.type_string(string = "Calibri")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # set text size
        keybd.press(button = "Alt+h")
        keybd.press(button = "f")
        keybd.press(button = "s")
        keybd.press(button = "9")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # set text fontface to italics
        keybd.press(button = "Ctrl+a")
        keybd.press(button = "Ctrl+i")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # remove text box border
        # mouse.move(x = 960, y = 540) # old gov laptop
        mouse.move(x = 500, y = 400)
        keybd.press(button = "Alt")
        keybd.press(button = "j")
        keybd.press(button = "d")
        keybd.press(button = "s")
        keybd.press(button = "o")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)

        # resize text box
        keybd.press(button = "Alt")
        keybd.press(button = "j")
        keybd.press(button = "d")
        keybd.press(button = "h")
        keybd.type_string(string = '.3"')
        keybd.press(button = "Enter")
        keybd.press(button = "Alt")
        keybd.press(button = "j")
        keybd.press(button = "d")
        keybd.press(button = "h")
        keybd.press(button = "w")
        keybd.type_string(string = '8.02"')
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # reposition text box
        # mouse.move(x = 960, y = 540) # old gov laptop
        mouse.move(x = 500, y = 400) # personal laptop
        mouse.click(button = "right", hold = FALSE)
        keybd.press(button = "l")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.type_string(string = '-.96"')
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.type_string(string = '10.21"')
        keybd.press(button = "Enter")
        mouse.click(button = "left", hold = FALSE)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # save as pdf and close pdf
        
        # sleep 
        Sys.sleep(time = 5)
        
        # get output_name
        output_name <- str_c("Malign Influence Resilience Index - ", current_country, " FY ", as.character(current_fy), " Country Profile.pdf")
        
        # # name output (on usaid laptop)
        # keybd.press(button = "Alt+f")
        # keybd.press(button = "a")
        # Sys.sleep(time = 2)
        # keybd.press(button = "Tab")
        # keybd.press(button = "Tab")        
        # keybd.press(button = "Tab")
        # keybd.press(button = "Tab")
        # keybd.type_string(string = output_name)
        
        # name output (on personal laptop)
        keybd.press(button = "Alt+f")
        keybd.press(button = "a")
        Sys.sleep(time = 2)
        keybd.press(button = "b")
        Sys.sleep(time = 2)
        keybd.type_string(string = output_name)
        
        # sleep 
        Sys.sleep(time = 2)
        
        # select pdf as output format
        keybd.press(button = "Tab")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "down")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 2)
        
        # # save (on usaid laptop)
        # keybd.press(button = "Tab")
        # keybd.press(button = "Enter")
        
        # save (on personal laptop)
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Tab")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 5)
        
        # close pdf
        keybd.press(button = "Ctrl+w")
        keybd.press(button = "Enter")

        # sleep 
        Sys.sleep(time = 3)
        
        
        #//////////////////////////////////////////////////////////////////////////////////////
        
        
        # close current_country word doc
        
        # get blank_doc_filename
        # blank_doc_filename <- str_c("C:\\Users\\sdevine\\Desktop\\usaid\\mcp\\malign_influence\\",
        #                   "output\\charts\\country_profile_blank_doc.docx")
        blank_doc_filename <- str_c("C:\\Users\\Stephen\\Desktop\\usaid\\mcp\\malign_influence\\",
                                    "output\\charts\\country_profile_blank_doc.docx")
        
        # open blank_doc
        shell(shQuote(string = blank_doc_filename), wait = FALSE)
        
        # sleep 
        Sys.sleep(time = 5)
        
        # switch view to current country_profile word doc and close it
        keybd.press(button = "Ctrl+F6")
        keybd.press(button = "Alt+f")
        keybd.press(button = "c")
        keybd.press(button = "right")
        keybd.press(button = "Enter")
        
        # sleep 
        Sys.sleep(time = 3)
        
        # close blank_doc
        keybd.press(button = "Alt+f")
        keybd.press(button = "c")
        
        # sleep
        Sys.sleep(time = 3)
        
        # delete blank_doc
        file_delete(path = "output/charts/country_profile_blank_doc.docx")
        
        # sleep
        Sys.sleep(time = 3)
}


#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////


# test

# create country_profile word doc

# source("code/get_country_profile.R")
# walk(.x = fmir %>%
#             # filter(mcp_grouping %in% c("E&E Balkans", "E&E Eurasia", "E&E graduates", "CARs")) %>%
#             filter(country == "Albania") %>%
#             # filter(country == "N. Macedonia") %>%
#             distinct(country) %>% pull(country),
#     .f = ~ get_country_profile(current_country = .x, current_fy = 2022))


#//////////////////

# source("code/finalize_country_profile_as_pdf.R")

# finalize_country_profile_as_pdf for single country_profile doc
# finalize_country_profile_as_pdf(current_country = "Albania", current_fy = 2022)
# finalize_country_profile_as_pdf(current_country = "N. Macedonia", current_fy = 2022)

# finalize_country_profile_as_pdf for batch of country_profile docs
# fmir %>% filter(country %in% c("Albania", "N. Macedonia")) %>%
#         distinct(country) %>% pull(country) %>%
#         walk(.x = ., .f = ~ finalize_country_profile_as_pdf(current_country = .x, current_fy = 2022))


