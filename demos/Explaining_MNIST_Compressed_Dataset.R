######## CONTEXTUAL INFORMATION ONLY, NOT RELEVANT TO UNDERSTAND THE EXAMPLE

##
## How was the MNIST simplistic dataset of RLCS package created? See below!
##


# extract_number <- function(mnist_labs, mnist_imgs, t_num) {
#   mnist_subset <- which(mnist_labs %in% c(t_num))
#   mnist_sub_state <- list()
#   mnist_sub_49b <- list()
#   for(set_num in mnist_subset) {
#     t_sharp_image <- matrix(ifelse(mnist_imgs[set_num,] > 100, 1, 0), ncol=28, byrow=TRUE)
#     t_7_square <- matrix(rep(0, 49), ncol=7, byrow=T)
#     for(i in 1:7) {
#       for(j in 1:7) {
#         t_7_square[i, j] <- any(t_sharp_image[(((j-1)*4+1):(j*4)), (((i-1)*4+1):(i*4))] == 1)
#       }
#     }
#     mnist_sub_state[[length(mnist_sub_state)+1]] <- t_sharp_image
#     mnist_sub_49b[[length(mnist_sub_49b)+1]] <- t_7_square
#   }
#   list(sharp_images = sapply(mnist_sub_state, \(x) {paste(x, collapse="")}),
#        state = sapply(mnist_sub_49b, \(x) {paste(x, collapse="")}))
# }
#
# make_mnist_env <- function() {
#   ## Careful this one can take a moment!
#   dslabs::read_mnist(download = T, destdir = "./data/mnist_download")
#   mnist <- dslabs::read_mnist("./data/mnist_download")
#   mnist_train <- mnist$train
#   res_list0 <- extract_number(mnist_train$labels, mnist_train$images, 0)
#   res_list1 <- extract_number(mnist_train$labels, mnist_train$images, 1)
#
#   rbind(data.frame(sharp_image = c(res_list0$sharp_images), state = c(res_list0$state), class = 0),
#         data.frame(sharp_image = c(res_list1$sharp_images), state = c(res_list1$state), class = 1))
# }

## The simplified MNIST Data set included in the RLCS package
## was created with the above functions.

# mnist_bin01_49b <- make_mnist_env()
## The above was simply executed already and saved into the RLCS Package Data
## Only to save you time and space.
