# T-SQL Data Definition Documentation

---

This R package, `tsql.documentation`, utilizes the MS SQL metadata to compile, via knitter, a comprehensive and easily navigable documentation deliverable for database deployments.

## Motivation

Building clean and concise data definition documentation over and over again is mind-numbing, especially since everything that is needed is [usually] stored in the inherent metadata.  I needed a way to pull all this together and build a PDF (with navigation), WITHOUT the need of an over priced documentation tool.

## Prerequisites

The two required packages, `rmarkdown` and `RODBC` - both of which are installing when this library is loaded.  However, since the result renders a PDF file via `pandoc`, you need to manually install LaTeX, i.e. [MikTeX](https://miktex.org/).  Additionally, for ease of installation, `devtools` is recommended.

## Installation

Using the `devtools` function, install with the below:

```r
install_github('mjfii/TSQL-Documentation')
library('tsql.documentation')
```

## Examples

To open a connection (note: only integrated security is available):

```r
cnn <- get.connection('<server_name>','<database_name>')
```

To load a data frame with markdown:
```r
cnn <- get.connection('<server_name>','<database_name>')  
md <- get.markdown(cnn)  
```

To write full documentation to disk:
```r
cnn <- get.connection('<server_name>','<database_name>')  
doc <- get.documentation(cnn, 'c:/' , 'documentation.pdf')  
```

## Contributors

Michael Flanigan  
 email: [mick.flanigan@gmail.com](mick.flanigan@gmail.com)  
 twitter: [@mjfii](https://twitter.com/mjfii)  

# Versioning

0.0.0.9000 - Initial deployment (2017-01-26)
