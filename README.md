# PaleoClimR

**PaleoClimR** is an R package designed for extracting, processing, and visualizing paleoclimate data from cGENIE model outputs. This package facilitates the analysis of climate variables across various depths and geographical locations, making it an essential tool for researchers in paleoclimatology.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Functions](#functions)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)

## Installation

You can install the latest version of **PaleoClimR** from GitHub:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install PaleoClimR from GitHub
devtools::install_github("richardstockey/PaleoClimR")
```

## Usage
```R

library(PaleoClimR)

# Example of extracting data
data <- cGENIE.data.3D(var = "ocn_O2", experiment = "my_experiment", year = "default")
```

## Authors

- **Richard G. Stockey** - *Conceptualisation, Maintainer, Primary contributor* - [richardstockey](https://github.com/richardstockey)
- **Alison T. Cribb** - *Contributor* - [atcribb](https://github.com/atcribb)
- **Przemyslaw Gruszka** - *Contributor* - [PrzemGruszka](https://github.com/PrzemGruszka)
- **Aspen Sartin** - *Contributor* - [aspensartin](https://github.com/aspensartin)
- **Pedro Monarrez** - *Contributor* - [pmmonarrez](https://github.com/pmmonarrez)


 Note that contributions involve elements of conceptualisation, beta testing and discussion that may not be reflected within standard github definitions of the term "Contributor". 