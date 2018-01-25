<!-- README.md is generated from README.Rmd. Please edit that file -->
eml2
====

The goal of `eml2` is to provide both a drop-in replacement for the higher-level functions of the existing EML package while also providing additional functionality.

`eml2` uses only simple and familiar list structures (S3 classes) instead of the more cumbersome use of S4 found in the original EML. While the higher-level functions are identical, this makes it easier to for most users and developers to work with `eml` objects and also to write their own functions for creating and manipulating EML objects. Under the hood, `eml2` relies on the [emld](https://github.com/cboettig/emld) package, which uses a Linked Data representation for EML. It is this approach which lets us combine the simplicity of lists with the specificy required by the XML schema.

Creating EML
------------

Here we show a the creation of a relatively complete EML document using `eml2`. This closely parallels the function calls shown in the original EML [R-package vignette](https://ropensci.github.io/EML/articles/creating-EML.html).

``` r
library(eml2)
library(emld)
```

### Coverage metadata

``` r
geographicDescription <- "Harvard Forest Greenhouse, Tom Swamp Tract (Harvard Forest)"
coverage <- 
  set_coverage(begin = '2012-06-01', end = '2013-12-31',
               sci_names = "Sarracenia purpurea",
               geographicDescription = geographicDescription,
               west = -122.44, east = -117.15, 
               north = 37.38, south = 30.00,
               altitudeMin = 160, altitudeMaximum = 330,
               altitudeUnits = "meter")
```

Creating methods
----------------

Read in methods written in a word doc:

``` r
methods_file <- system.file("examples/hf205-methods.docx", package = "EML")
methods <- set_methods(methods_file)
```

Creating parties
----------------

``` r
R_person <- person("Aaron", "Ellison", ,"fakeaddress@email.com", "cre", 
                  c(ORCID = "0000-0003-4151-6081"))
aaron <- as_emld(R_person)
```

``` r
others <- c(as.person("Benjamin Baiser"), as.person("Jennifer Sirota"))
associatedParty <- as_emld(others)
associatedParty[[1]]$role <- "Researcher"
associatedParty[[2]]$role <- "Researcher"
```

``` r
HF_address <- list(
                  deliveryPoint = "324 North Main Street",
                  city = "Petersham",
                  administrativeArea = "MA",
                  postalCode = "01366",
                  country = "USA")
```

``` r
publisher <- list(
                 organizationName = "Harvard Forest",
                 address = HF_address)
```

``` r
contact <- 
  list(
    individualName = aaron$individualName,
    electronicMailAddress = aaron$electronicMailAddress,
    address = HF_address,
    organizationName = "Harvard Forest",
    phone = "000-000-0000")
```

### Attribute Metadata

Here we use attribute metadata and factor definitions as given from separate csv files.

``` r
attributes <- read.table(system.file("extdata/hf205_attributes.csv", package = "eml2"))
factors <- read.table(system.file("extdata/hf205_factors.csv", package = "eml2"))
attributeList <- 
  set_attributes(attributes, 
                 factors, 
                 col_classes = c("character", 
                                 "Date",
                                 "Date",
                                 "Date",
                                 "factor",
                                 "factor",
                                 "factor",
                                 "numeric"))
```

Data file format
----------------

Default `.csv` format.

``` r
physical <- set_physical("hf205-01-TPexp1.csv")
```

Assembling the `dataTable`
--------------------------

``` r
dataTable <- list(
                 entityName = "hf205-01-TPexp1.csv",
                 entityDescription = "tipping point experiment 1",
                 physical = physical,
                 attributeList = attributeList)
```

Creating a `keywordSet`
-----------------------

``` r
keywordSet <- list(
    list(
        keywordThesaurus = "LTER controlled vocabulary",
        keyword = list("bacteria",
                    "carnivorous plants",
                    "genetics",
                    "thresholds")
        ),
    list(
        keywordThesaurus = "LTER core area",
        keyword =  list("populations", "inorganic nutrients", "disturbance")
        ),
    list(
        keywordThesaurus = "HFR default",
        keyword = list("Harvard Forest", "HFR", "LTER", "USA")
        ))
```

Lastly, some of the elements needed for `eml` object can simply be given as text strings.

``` r
pubDate <- "2012" 

title <- "Thresholds and Tipping Points in a Sarracenia 
Microecosystem at Harvard Forest since 2012"

intellectualRights <- "http://www.lternet.edu/data/netpolicy.html."
```

Read in an abstract written in Markdown:

``` r
abstract_file <-  system.file("examples/hf205-abstract.md", package = "EML")
abstract <- set_TextType(abstract_file)
```

``` r
dataset <- list(
               title = title,
               creator = aaron,
               pubDate = pubDate,
               intellectualRights = intellectualRights,
               abstract = abstract,
               associatedParty = associatedParty,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               dataTable = dataTable)
```

``` r
eml <- list(
           "#packageId" = uuid::UUIDgenerate(),  
           "#system" = "uuid",
           dataset = dataset)
```

``` r
write_eml(eml, "eml.xml")
eml_validate("eml.xml")
#> [1] FALSE
#> attr(,"errors")
#> [1] "Element 'para': This element is not expected."                                     
#> [2] "Element 'section': Missing child element(s). Expected is one of ( para, section )."
#> [3] "Element 'section': Missing child element(s). Expected is one of ( para, section )."
```
