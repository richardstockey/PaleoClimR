# PaleoClimR

[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/PaleoClimR)](https://cran.r-project.org/package=PaleoClimR)


**PaleoClimR** is an R package designed for extracting, processing, and visualizing paleoclimate data from cGENIE model outputs. This package facilitates the analysis of climate variables across various depths and geographical locations, making it an essential tool for researchers in paleoclimatology.

## Table of Contents

- [Authors](#authors)
- [Installation](#installation)
- [Usage](#usage)
- [Contributing](#contributing)
- [Code of Conduct](#code-of-conduct)

- **Richard G. Stockey** - *Conceptualisation, Maintainer, Primary Contributor for Initial Release* - [richardstockey](https://github.com/richardstockey)
- **Alison T. Cribb** - *Contributor* - [atcribb](https://github.com/atcribb)
- **Przemyslaw Gruszka** - *Contributor* - [PrzemGruszka](https://github.com/PrzemGruszka)
- **Aspen Sartin** - *Contributor* - [aspensartin](https://github.com/aspensartin)
- **Pedro Monarrez** - *Contributor* - [pmmonarrez](https://github.com/pmmonarrez)

Note that contributions as defined here involve elements of conceptualisation, beta testing and discussion, as well as primary code contribution, that may not be reflected within standard GitHub definitions of the term ***Contributor***. 

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

## Contributing

Contributions are welcome! Here are some ways you can contribute to the PaleoClimR project:

1. **Report Bugs**:
    - If you find a bug, please report it by opening an issue on the [GitHub Issues](https://github.com/richardstockey/PaleoClimR/issues) page.

2. **Suggest Features**:
    - If you have an idea for a new feature or an improvement, please suggest it by opening an issue on the [GitHub Issues](https://github.com/richardstockey/PaleoClimR/issues) page.

3. **Submit Pull Requests**:
    - If you want to contribute code, follow these steps:
      1. Fork the repository on GitHub.
      2. Clone your forked repository to your local machine.
      3. Create a new branch for your feature or bug fix.
      4. Make your changes and commit them with clear and descriptive commit messages.
      5. Push your changes to your forked repository.
      6. Open a pull request on the original repository, describing your changes and the problem they solve.

4. **Improve Documentation**:
    - If you find any issues with the documentation or think it can be improved, feel free to make changes and submit a pull request.

5. **Code Reviews**:
    - Review open pull requests and provide feedback to help improve the code quality.

### Getting Started

1. **Fork the Repository**:
    - Go to the [PaleoClimR repository](https://github.com/richardstockey/PaleoClimR) and click the "Fork" button in the top right corner.

2. **Clone the Repository**:
    - Clone your forked repository to your local machine using the following command:
      ```sh
      git clone https://github.com/your-username/PaleoClimR.git
      ```

3. **Create a Branch**:
    - Create a new branch for your feature or bug fix:
      ```sh
      git checkout -b feature-or-bugfix-name
      ```

4. **Make Changes**:
    - Make your changes to the codebase.

5. **Commit Changes**:
    - Commit your changes with a clear and descriptive commit message:
      ```sh
      git commit -m "Description of the changes"
      ```

6. **Push Changes**:
    - Push your changes to your forked repository:
      ```sh
      git push origin feature-or-bugfix-name
      ```

7. **Open a Pull Request**:
    - Go to the original repository and open a pull request, describing your changes and the problem they solve.

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project, you agree to abide by its terms. This code of conduct is adapted from the [PaleoVerse Code of Conduct](https://github.com/PaleoVerse/paleoverse/blob/main/CODE_OF_CONDUCT.md) and we are indebted to the work of its authors.

Thank you for your interest in contributing to PaleoClimR! Your contributions are greatly appreciated.
