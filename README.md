# Rnets
The `Rnets` package for mapping relationships in antimicrobial resistances (AMR) in bacterial populations. The sets of estimated relationships are treated as networks; The name of the package, core function, and analysis result, 'Rnet', is derived from the phrase "Resistance relationship network." AMR surveillance programs have produced huge amounts of data, but new methods are needed to interpret and study this volume of data. The `Rnets` package applies the graphical least absolute selection and shrinkage operator, also referred to as the 'graphical LASSO', to determine which resistances are correlated, and which are conditionally independent. Our goal in developing this package was to make the Rnets method easily accessible to all research. Therefore the core function in the package, `Rnet` accepts input in a commonly used format, with isolates data stored in rows and respective minimum inhibitory concentration (MIC) data stored in columns, and directly and quickly produces useful analyses with the mudane data handling taken care of behind the scenes. A suite of additional functions is included to interact with and visualize the analysis results.

A more in-depth description of the methods employed by the package are available in the included vignette.

# Use
The process of estimating a network from raw MIC data can be broadly divided into 3 primary phases:

1. Selection of $\lambda$
2. Induction of sparsity using the graphical LASSO
3. Visualization and interpretation

Briefly, $\lambda$ is a penalty applied to the correlation matrix; Larger $\lambda$ values tend to reduce more parrtial correlations to 0, resulting in sparser networks with fewer edges. To produce informative networks, $\lambda$ should be between smallest and largest absolute values of the correlation matrix's elements. Several methods have been described to select L~1~. The `L1_selection` function employs the Stability Approach to Regularization Selection (StARS) proposed by [CITATION NEEDED]. We have found $\lambda$ = 0.25 is typically a reasonable penalty for estimating R-nets.

```
L1
```

# Installation
The latest stable version of ```Rnets``` is available on the author's GitHub and can be installed using the following code:
```
library(devtools)
install-github('WJL-NCSU/Rnets')
```

The latest development branch of the project, which is not guarunteed to be stable, can also be accessed from the author's GitHub using:
```
library(devtools)
install-github('WJL-NCSU/Rnets', branch = 'dev')
```

The `Rnets` package has the following dependancies:

* The graphical LASSO method is performed using the `glasso` function in the eponymous package maintained by Rob Tibshiriani
* Networks are handled and plotted using a variety of functions in the `igraph` package maintained by Gábor Csárdi.
* Data aggregation over multiple strata is handled using the efficient `rbindlist` function in the `data.table` package maintained by Matt Dowle. 

All three dependencies are available on CRAN as of 1.Dec.2017. 