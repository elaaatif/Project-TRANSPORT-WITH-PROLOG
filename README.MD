# Public Transport Journey Calculator

A system for calculating public transport journeys, including finding the earliest and latest schedules, determining routes with minimum stations, and more.

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgments](#acknowledgments)

## Introduction

This project provides a tool for calculating public transport journeys in a metro network. It includes features such as finding the earliest and latest schedules, determining routes with the minimum number of stations, and more.

## Features

- **Earliest Schedule Calculation:** Find the earliest schedule for a public transport journey.
- **Latest Schedule Calculation:** Find the latest schedule for a public transport journey.
- **Minimum Stations Route:** Determine the route with the minimum number of stations.
- **Metro Network Exploration:** Explore the metro network to find connected stations and routes.

## Getting Started

Follow these instructions to get a copy of the project up and running on your local machine.

### Prerequisites

Make sure you have the following installed:

- Prolog interpreter (e.g., SWI-Prolog)
- Or you can use VSC as follows :
            Install VSC and the "Prolog" extension by arthurmilchior.
            Install a Prolog interpreter (e.g., SWI-Prolog or GNU Prolog) and set it as the default in VSC.
            Open .pl file and run using the "Prolog: Consult" command.
            For debugging, set breakpoints, create a Prolog launch configuration, and use the debugging features in VSC.

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/Elaatif/
Change into the project directory:

bash

cd public-transport-journey-calculator
Run the Prolog script using your Prolog interpreter:


bash

swipl your_script.pl
Usage
Describe how to use the project, including examples and code snippets.

Example:
prolog
% Example usage of finding the earliest schedule
?- ligtot1(station_a, station_b, line_1, [10, 0], earliest_schedule).
