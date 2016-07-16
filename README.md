# rester

Rester is a simple tool to run REST API integration tests that are defined in a spreadsheet like Excel or CSV file.

## Why?

There are plenty of excelent DSLs and tools for REST API testing like [rest-assured](https://github.com/rest-assured/rest-assured), but overtime it becomes repetitive to define test cases in code. The tests follow generally the same pattern of specifying request data and checking for response data, such as HTTP status code or the responde payload. It seems more natural to define them as just data, and a spreadsheet is a good way to edit, store and represent data.

The other benefit of defining test cases in spreadsheets is readability. Non developers, like business analysts or users may be able to read, understand and even define the test cases.


## Installation

Download from http://example.com/FIXME.

## Usage

Create your tests suites in a CSV File [like this](btc-markets.csv)
Run the script with:

    $ lein run -m rester.core <base-url-rest-server> <path to your csv file>
or

    $ java -jar rester-0.1.0-standalone.jar <base-url-rest-server> <path to your csv file>

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
