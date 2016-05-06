install.packages("sparsesvd")
require(sparsesvd)
source("http://bioconductor.org/biocLite.R")
install.packages("BiocInstaller",
                 repos="http://bioconductor.org/packages/3.2/bioc")
require(BiocInstaller)
biocLite("EBImage")

require(EBImage)
Image <- readImage('E:/panda.JPG')
display(Image)

img.data=imageData(Image)
img.data.frame=as.data.frame(img.data)
img.matrix=as.matrix(img.data.frame, nrow=nrow(img.data.frame),ncol=ncol(img.data.frame))
img.matrix.sparse=Matrix::Matrix(img.matrix, sparse=TRUE)
res=sparsesvd(img.matrix.sparse, rank=300L)

img.matrix.svd=res$u %*% diag(res$d) %*% t(res$v)
img.matrix.svd.red=res$u %*% diag(res$d)
Matrix::rankMatrix(img.matrix)

display(img.matrix.svd)
display(img.matrix.svd.red)
Matrix::rankMatrix(img.matrix.svd)

plot(cumsum(res$d^2/sum(res$d^2))) 
