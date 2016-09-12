# rester
Rester is a simple tool to run REST API integration tests defined in a Excel spreadsheet or CSV file.

![build status](https://travis-ci.org/rinconjc/rester.svg?branch=master)

## Why?

There are plenty of excelent DSLs and tools for REST API testing like [rest-assured](https://github.com/rest-assured/rest-assured), but overtime it becomes repetitive to define test cases in code. The tests follow generally the same pattern of specifying request data and checking for response data, such as HTTP status code or the responde payload. It seems more natural to define them just as data, and a spreadsheet is perhaps a good way to edit, store and represent data.

The other benefit of defining test cases in spreadsheets is readability. Non developers, like business analysts or users may be able to read, understand and even define the test cases.

## How?

Test cases have to be defined in a Excel spreadsheet or CSV file using the below format.

| Test Suite | Test Case | Target URL | Method | Headers | Payload | Params | Expected Status | Expected Body | Expected Headers | Request Options | Extractions |
| ---------- | --------- | ---------- | ------ | ------- | ------- | ------ | -------- | ----- | ----- | ------- | ------ |
| Users API | Create a user | $apiServer$/users | POST | Authorization: $apiAuth$, Content-Type:application/json | {name:"John Smith", email:"john.smith@someco.com"} |  | 200 | {id:"#\\d+", name:"John Smith"} | Content-Type:application/json |  | createdUserId=$.id |
|  | Update user | $apiServer$/user/$createdUserId$ | PUT | Authorization: $apiAuth$, Content-Type:application/json | {name:"John Smith Jr.", email:"john.smith.jr@someco.com"} |  | 200 | {id:"#\\d+", name:"John Smith Jr."} | Content-Type:application/json |  | updatedUserId=$.id |
|  | Delete user | $apiServer$/user/$createdUserId$ | DELETE | Authorization: $apiAuth$ |  |  | 200 | {id:"#\\d+"} | Content-Type:application/json |  | deletedUserId=$.id |
|  | Deleted user should not exist | $apiServer$/user/$createdUserId$ | GET | Authorization: $apiAuth$  |  |  | 404 |  | Content-Type:application/json |  |  |
| Another Suite  | Othe test name | http://myserver/myresource | POST | Authorization: $apiAuth$  |  |  | 200 |  | Content-Type:application/json |  |  |

Here's a explanation of the columns:
* **Test Suite**, and **Test Case** are self-explanatory, they are just some description of the test suite/case.
* **Target URL**, is the URL of the request, it supports placeholders (more details below)
* **Method**, is the request method
* **Headers**, is a comma separated list of request headers to be sent in the request. It also supports placeholders, as in *Authorization: $apiAuth$*
* **Payload**, is the body of the request and can be any string. For JSON content double quoting fields is optional. It will be parsed and normalised at execution time (to pass content as-is specify *DONT_PARSE_PAYLOAD* in the Req. options)
* **Expected status**, is the expected HTTP response status (Mandatory).
* **Expected body**, is part or complete content that the response body is expected to contain. It's optional and only verifies the specified body.
* **Expected headers**, a comma separated list of header names and values. This is also optional and checks only if the expected headers are present in the response headers.
* **Request Options**, special options for the request. Only *DONT_PARSE_PAYLOAD* is supported at the moment.
* **Extractions**, is a comma separated list of variable extractions from the response body. Currently, it supports JSON Path expressions and JSON responses. The extracted variable names can be used as placeholders in other tests cases. 

### Features
* **Placeholders**, variables that are substituted at execution time. They should be surrounded with $ characters. e.g. $apiServer$ above. The values of the placeholders are resolved at run-time from the command line arguments, or the extracted values from dependent requests, or special build-in functions (see *builtin expressions* below).
* **Variable extraction**. There used to define dependent tests, e.g. one test creates one entry and another gets or updates such entry.
* **Dependent tests**. The test cases are run in parallel (configurable # of threads, 4 threads by default), and in no specific order, unless for dependent tests. Dependencies are determined implicitly from the *placeholder* sets of every test and the exported *variable extractions* of other tests. E.g In the above example *Update user* depends on *create a user* because the former uses the placeholder *createdUserId*, which is defined as extracted variable in the *create a user* test case.  
* **Date expressions**. Expressions like $today$ or $today+1year-2weeks$ can be specified as placeholders. Currently, *now*,*today* and *tomorrow* are supported as dynamic dates. The expressions can use any of min, mins, hour, hours, day, days, week, weeks, month, months, year, years.     
* **Regex support**. The expected headers and expected body, can include regex expressions using a *#* prefix. E.g. {id:"#\\d+"} 

## Usage

Create your tests suites in a CSV File [like this](sample-tests.csv)
Run the script with:

    $ lein run -m rester.core <path to your csv file> :placeholder-name placeholder-value ...
or

    $ java -jar rester-0.1.0-beta2.jar <path to your csv file> :placeholder-name placeholder-value ...

## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
