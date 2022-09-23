.__C__linreg <-
new("refClassRepresentation", fieldClasses = list(FunctionFormula = "formula", 
    RegressionCoeficientMatrix = "matrix", FittedValues = "matrix", 
    Residuals = "matrix", DegreesOfFreedom = "numeric", ResidualVariance = "matrix", 
    VarianceOfTheRegressionCoefficients = "vector", TValues = "vector", 
    DataName = "character", PValues = "vector"), fieldPrototypes = <environment>, 
    refMethods = <environment>, refSuperClasses = "envRefClass", 
    slots = list(.xData = structure("environment", package = "methods")), 
    contains = list(envRefClass = new("SClassExtension", subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("envRefClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            class(from) <- "envRefClass"
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            for (what in ".xData") slot(from, what) <- slot(value, 
                what)
            from
        }, simple = TRUE, by = character(0), dataPart = FALSE, 
        distance = 1), .environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure(".environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            {
                class(from) <- ".environment"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, ".environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), refClass = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refClass") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "envRefClass", strict = strict)
            {
                from <- as(from, ".environment", strict = strict)
                from@.xData
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = FALSE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3), refObject = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refObject", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refObject") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3)), virtual = FALSE, prototype = <S4 object of class NULL>, 
    validity = NULL, access = list(), className = structure("linreg", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), versionKey = <pointer: 0x7fe686a6a6d0>, 
    sealed = FALSE)
.__global__ <-
c("FunctionFormula", "RegressionCoeficientMatrix", "FittedValues", 
"Residuals", "DegreesOfFreedom", "ResidualVariance", "VarianceOfTheRegressionCoefficients", 
"TValues", "DataName", "PValues", "summary", "coef", "pred", 
"resid", "plot", "print", "setPValues", "setTValues", "setVarianceOfTheRegressionCoefficients", 
"setResidualVariance", "setDegreesOfFreedom", "setResidual", 
"setFittedValues", "setRegCofficient", "initialize", "field", 
"trace", "getRefClass", "initFields", "copy", "callSuper", ".objectPackage", 
"export", "untrace", "getClass", "show", "usingMethods", ".objectParent", 
"import", ".self")
.requireCachedGenerics <-
structure(list("$", "$<-"), package = c("base", "base"))
