# Haskell Average House Prices Project

This Haskell project provides functionalities to manage and analyze average house price data. It consists of modules for interacting with a SQLite database, fetching data from a URL, parsing data, and a main module for the application's entry point. This project was completed as part of a group effort during my Masters University course.

## Modules

### Database

This module (`Database.hs`) provides functions for interacting with a SQLite database, including creating tables, dropping tables, saving data, and querying data.

### Fetch

The `Fetch.hs` module handles fetching data from a specified URL using HTTP requests.

### Parse

The `Parse.hs` module is responsible for parsing data, primarily JSON data, into Haskell data types.

### Main

The `Main.hs` module serves as the entry point for the application, providing a menu-based interface for users to interact with various functionalities offered by the project.

## Data Types (Types.hs)

This module defines the data types used throughout the project, including `Region`, `Price`, `Record`, and `Records`, along with instances for serialization and deserialization.

## Usage

1. **Database Interaction**: Use functions provided in the `Database` module to interact with the SQLite database, such as creating tables, dropping tables, and querying data.

2. **Data Fetching**: Utilize the `get_data_from_url` function in the `Fetch` module to fetch data from a specified URL.

3. **Data Parsing**: Employ the `parseRecords` function in the `Parse` module to parse fetched data into Haskell data types.

4. **Menu Interface**: Run the application's entry point (`Main.hs`), which presents a menu-based interface for users to perform various operations, such as creating tables, fetching data, querying data, and more.

## Main Script (Main.hs)

The `Main.hs` script serves as the entry point for the application, providing a menu-based interface for users to interact with various functionalities offered by the project. Here's a brief list of what the script does:

- Creates a menu interface displaying options for different operations related to the project functionalities.
- Allows users to select options such as creating tables, dropping tables, downloading data from a URL, querying data by region or area code, retrieving total row counts, calculating average house prices within a date range, writing data to a JSON file, and quitting the application.
- Executes corresponding functions based on the selected menu option.
- Provides feedback to the user indicating the progress of operations and whether they are completed successfully.
- Offers an intuitive way for users to interact with the project functionalities without requiring direct knowledge of Haskell programming.

## Contributing

This project has now concluded and will be archived. No contributions are required however please feel free to utilise the code if learning Haskell.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
