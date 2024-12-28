# JSONPath

![build status](https://github.com/sophiecollard/jsonpath/actions/workflows/build.yml/badge.svg)

A [partial](#status) implementation of the [JSONPath specification](https://www.rfc-editor.org/rfc/rfc9535) in Elm.

## Live demo

Before adding this package to your project, you can try out the latest version [here](https://jsonpath-demo.lon1.cdn.digitaloceanspaces.com/index.html).

## Installation

```sh
elm install sophiecollard/jsonpath
```

## Example

```elm
import Json.Decode
import JsonPath
import JsonPath.Extractor

sampleJson : Json.Decode.Value
sampleJson =
    ... -- Your JSON here. See the sampleJson value in docs/Sample.elm for instance.

extractedJson : Result JsonPath.Error Json.Decode.Value
extractedJson =
    JsonPath.Extractor.run
        "$.store.book[*].author"
        sampleJson
```

The `JsonPath.Extractor` module exposes `run` and `runStrict` functions, both of which take 2 arguments:
  1. A `String` representing a JSONPath expression
  2. A value of type `Json.Decode.Value`

Remember that you can parse a raw JSON `String` into a `Json.Decode.Value` like this:

```elm
parseJson : String -> Result Json.Decode.Error Json.Decode.Value
parseJson string =
    Json.Decode.decodeString Json.Decode.value string
```

### `run` vs `runStrict`

The distinction between `run` and `runStrict` is best understood with an example, using the [raw](docs/sample.json) or [parsed](docs/Sample.elm) JSON sample in the [docs](docs/) folder.

With `runStrict`, attempts to extract the elements at `$.store.book[*].isbn` will fail because the first two books do not have an `isbn` key:

```elm
JsonPath.Extractor.runStrict "$.store.book[*].isbn" sampleJson ==
    Err (KeyNotFound [ DownIndex 0, DownKey "book", DownKey "store" ] "isbn")
```

With `run` however, entries missing the `isbn` key are ignored and the ISBNs of the last two books returned:

```elm
JsonPath.Extractor.run "$.store.book[*].isbn" sampleJson ==
    Ok (Json.Encode.list Json.Encode.string [ "0-553-21311-3", "0-395-19395-8" ])
```

Note that this only works with JSONPath expressions which return a list of results. With expressions which return exactly one result, both functions will yield an error if an index or key is missing.

```elm
JsonPath.Extractor.run "$.store.book[5]" sampleJson ==
    (Err (IndexNotFound [ DownKey "book", DownKey "store" ] 5))
```

## Status

This package is a work in progress and does not yet support the full [JSONPath specification](https://www.rfc-editor.org/rfc/rfc9535). Below is a summary of the supported syntax:

### Identifiers

| Identifier   | Syntax | Supported |
| ------------ | ------ | --------- |
| Root node    | `$`    | ✅        |
| Current node | `@`    | ❌        |

### Segments

| Segment     | Syntax | Example                         | Supported |
| ----------- | ------ | ------------------------------- | --------- |
| Children    | `.`    | `$.store.book.0.author`         | ✅        |
| Children    | `[]`   | `$.store.book[0][author,title]` | ✅        |
| Descendants | `..`   | `$.store..price`                | ✅        |
| Descendants | `..[]` | `$.store..[author,title]`       | ✅        |

### Selectors

| Selector          | Syntax            | Example                       | Supported |
| ----------------- | ----------------- | ----------------------------- | --------- |
| Wildcard          | `*`               | `$.store.book[*]`             | ✅        |
| Array slice       | `start:end:step`  | `$.store.book[0:4:-2]`        | ✅        |
| Index             | `1`               | `$.store.book[1,2,3] `        | ✅        |
| Name / key        | `name`            | `$.store.book[author,title]`  | ✅        |
| Filter expression | `?<logical-expr>` | `$.store.book[?@.price < 10]` | ❌        |

## Licence

Copyright 2024 [Sophie Collard](https://github.com/sophiecollard).

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the "License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
