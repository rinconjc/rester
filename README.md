# rester ![build status](https://travis-ci.org/rinconjc/rester.svg?branch=master)
*Rester* is a REST API integration test tool that executes tests defined in simple data formats such a spreadsheet, CSV, YAML, or EDN.

## Why?

There are plenty of excellent DSLs and tools for REST API testing like [rest-assured](https://github.com/rest-assured/rest-assured), but overtime it becomes repetitive to define test cases in code. The tests follow generally the same pattern of specifying request data and checking for response data, such as HTTP status code or the response payload. It seems more natural to define them just as data, and a spreadsheet is perhaps a good way to edit, store and represent data.

The other benefit of defining test cases in spreadsheets is readability. Non-developers, like business analysts or users may be able to read, understand and even define the test cases.

## How?

Test cases can be written in any of these formats: CSV, EXCEL, YAML, or EDN

For CSV and EXCEL it should have the following format:

| Test Suite | Test Case | Target URL | Method | Headers | Payload | Params | Expected Status | Expected Body | Expected Headers | Options | Extractions | Priority |
| ---------- | --------- | ---------- | ------ | ------- | ------- | ------ | -------- | ----- | ----- | ------- | ------ | ------ |
| Users API | Create a user | $apiServer$/users | POST | Authorization: $apiAuth$, Content-Type:application/json | {name:"John Smith", email:"john.smith@someco.com"} |  | 200 | {id:"#\\d+", name:"John Smith"} | Content-Type:application/json |  | createdUserId=$.id | |
|  | Update user | $apiServer$/user/$createdUserId$ | PUT | Authorization: $apiAuth$, Content-Type:application/json | {name:"John Smith Jr.", email:"john.smith.jr@someco.com"} |  | 200 | {id:"#\\d+", name:"John Smith Jr."} | Content-Type:application/json |  | updatedUserId=$.id | |
|  | Delete user | $apiServer$/user/$createdUserId$ | DELETE | Authorization: $apiAuth$ |  |  | 200 | {id:"#\\d+"} | Content-Type:application/json |  | deletedUserId=$.id | |
|  | Deleted user should not exist | $apiServer$/user/$createdUserId$ | GET | Authorization: $apiAuth$  |  |  | 404 |  | Content-Type:application/json |  |  | |
| Another Suite  | Othe test name | http://myserver/myresource | POST | Authorization: $apiAuth$  |  |  | 200 |  | Content-Type:application/json |  |  | |

Here's a explanation of each column:
* **Test Suite**, and **Test Case** are self-explanatory, they are just some description of the test suite/case.
* **Target URL**, is the URL of the request, it supports placeholders (more details below)
* **Method**, is the request method (GET, POST, PUT, DELETE, PATCH, OPTIONS)
* **Headers**, is a comma separated list of request headers to be sent in the request. It also supports placeholders, as in *Authorization: $apiAuth$*
* **Payload**, is the body of the request and can be any string. For JSON content double quoting fields is optional. It will be parsed and normalised at execution time (to pass content as-is specify *DONT_PARSE_PAYLOAD* in the Req. options)
* **Expected status**, is the expected HTTP response status (Mandatory).
* **Expected body**, is the partial or complete content that the response body is expected to contain. It's optional and only verifies the specified body.
* **Expected headers**, a comma separated list of header names and values. This is also optional and checks only if the expected headers are present in the response headers.
* **Options**, special options for the request. The following options are supported:

    * **dont_parse_payload** allows sending the body content as-is
    * **before** and **after**. These options allow specifying tests that should be executed before, and/or after the current test. They should have the following format: before=test-name-1,test-name2
    * **skip** This is another option that allows skipping the execution of the current test, if the skip value matches the skip option specified in the command line.

* **Extractions**, is a comma separated list of variable extractions from the response body. Currently, it supports JSON Path expressions and JSON responses. The extracted variable names can be used as placeholders in other tests cases.
* **Priority**. Assigning priorities to test cases allows sequencing their execution. Tests with lower priority will be executed before tests with higher priority. This is another way of defining dependencies without using variable-extraction and placeholders.

For YAML, it also uses the same fields as in the above CSV format, but follows the below structure.

``` yaml
Auth Tests:
  Access token request: &auth_request !!ignore
    POST: https://api.example.com/auth?grant_type=client_credentials
    headers:
      Content-Type: application/json
    expect:
      Content-Type: application/json
  Auth request without credentials:
    <<: *auth_request
    expect:
      status: 401
      headers:
        Content-Type: application/json
      body:|
        {error_code: "401",
        detail: "Missing credentials"}
  Auth request with valid credentials:
    <<: *auth_request
    headers:
      Authorization: Basic ${API_UATH}
    expect:
      status: 200
      headers:
        Content-Type: application/json
      body:
        {access_token: "#.+",
        expires_in: "#\\d+"}
    options:
      priority: 1
      extractors:
        access_token: $.access_token
Transactions Tests :
  Get transactions:
    GET: https://api.example.com/transactions
    headers:
      Authorization: Bearer ${access_token}
    expect:
      status: 200
      headers:
        Content-Type: application/json
      body:
        [{id: "#\\d+", amount: "#\\d+\\.?\\d*"}]
```

For EDN, it uses an equivalent structure to YAML.

``` edn
{"Market data"
 {"Invalid path"
  {:get "$baseUrl$/marketx/BTC/AUD/tick"
   :expect {:status 404}}
  "Valid path"
  {:get "$baseUrl$/market/BTC/AUD/tick"
   :expect {:status 200 :body "{currency:\"AUD\", instrument:\"BTC\"}"}}}
 "Fake API"
 {"Create Post"
  {:post "$fakeServer$/posts" :headers {"content-Type" "application/json"}
   :body "{userId:10, title:\"title1\", body:\"test post1\"}"
   :expect {:status 201 :body "{id: \"#\\d+\"}"
            :headers {"Content-Type" "application/json; charset=utf-8"}}}
  "Retrieve Post"
  {:get "$fakeServer$/posts/$postId$" :headers {"Content-Type" "application/json"}
   :expect {:status 200 :body "{title:\"title1\"}"
            :headers {"Content-Type" "application/json; charset=utf-8"}}}}}

```

### Features
* **Placeholders**, variables that are substituted at execution time. They should be surrounded with $ characters. e.g. $apiServer$ above. The values of the placeholders are resolved at run-time from the command line arguments, or the extracted values from dependant requests, or special built-in functions (see *built-in expressions* below).
* **Variable extraction**. They are used to extract values from response bodies, which could be used to feed into other tests, e.g. one test creates one entry, and another gets or updates such entry. The extraction works with JSON responses and supports the JSON Path syntax.
* **Dependent tests**. The test cases are run in parallel (configurable # of threads, 4 threads by default), and in no specific order, unless for dependent tests. Dependencies are determined implicitly from the *placeholder* sets of every test and the exported *variable extractions* of other tests. E.g In the above example *Update user* depends on *create a user* because the former uses the placeholder *createdUserId*, which is defined as extracted variable in the *create a user* test case.
* **Date expressions**. Expressions like $today$ or $today+1year-2weeks$ can be specified as placeholders. Currently, *now*,*today* and *tomorrow* are supported as dynamic dates. The expressions can use any of min, mins, hour, hours, day, days, week, weeks, month, months, year, years.
* **Regex support**. The expected headers and expected body, can include regex expressions using a *#* prefix. E.g. {id:"#\\d+"}
* **Priority**. Assigning priorities to test cases allows sequencing their execution. Tests with lower priority will be executed before tests with higher priority. This is another way of defining dependencies without using variable-extraction and placeholders.
* **before** and **after**. These options allow specifying tests that should be executed before, and/or after the current test. They should have the following format: before=test-name-1,test-name2
* **skip** This is another option that allows skipping the execution of the current test, if the skip value matches the skip option specified in the command line.

## Usage

Create your tests suites in your preferred format [like this](example/sample-tests.csv)

Download and save the [**Rester**](bin/rester) shell script to somewhere visible to your system path, e.g $HOME/bin/

Execute the script as follows:

    $ rester <options> <path to your test file>

For example to execute the sample tests use the following options:

    $ rester -b baseUrl=https://api.btcmarkets.net -b fakeServer=https://jsonplaceholder.typicode.com -b postId=10 example/sample-tests.csv


## License

Copyright © 2016 Rester

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
