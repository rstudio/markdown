library(markdown)

## Download
url = "http://www.r-project.org/Rlogo.png"
path = tempdir()
pathname = file.path(tempdir(), basename(url))
if (!file_test("-f", pathname)) download.file(url, pathname)

## Encode
data = markdown:::.b64EncodeFile(pathname)
str(data)

unlink(pathname)
