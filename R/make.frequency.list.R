
# #################################################
# Function for generating a frequency list of words or other (linguistic)
# features. It basically counts the elements of a vector and returns a vector
# of these elements in descending order of frequency.
# Refer to help(make.frequency.list) for farther details.
# #################################################

make.frequency.list <- function(data,
                                value = FALSE,
                                head = NULL,
                                relative = TRUE) {
        
        #####################################
        # first, sanitize the input dataset
        
        # test if the dataset belongs to 'stylo.corpus' class
        if(class(data) == "stylo.corpus" | is.list(data) == TRUE) {
                # unlist, or make one long text out of the corpus samples
                data <- enframe(unlist(data, FALSE, FALSE))    
                # otherwise, test if the dataset is a vector
        } else if(is.vector(data) == FALSE) {        
                # whet it is not, produce an error message and stop
                stop("unable to make a list of frequencies")
        }
        
        # test if the dataset has at least two elements
        if(nrow(data) < 3) {
                stop("you try to measure frequencies of an empty vector!")
        }
        #####################################
        
        # count each feature and sort them by descending rate of occurrence
        frequent.features <- data %>% 
                group_by(value) %>% 
                summarise(result = n()) %>% 
                arrange(desc(result))
        
        # calculate relative frequencies, if requested
        if(relative == TRUE) {
                frequent.features <- frequent.features %>% 
                        mutate(result = (result / nrow(data)) * 100)
        }
        
        # return the top n features, if specified
        if(!is.null(head)) {
                frequent.features <- top_n(frequent.features, head)
        }
        
        # only return rates of occurrence if requested
        if(value == FALSE) {
                result_list <- frequent.features$value
        } else {
                result_list <- frequent.features$result
                names(result_list) <- frequent.features$value
                result_list <- as.table(result_list)
        }
        
        return(result_list)
}

