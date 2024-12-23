# JSONPath

![build status](https://github.com/sophiecollard/jsonpath/actions/workflows/build.yml/badge.svg)

A partial implementation of the [JSONPath specification](https://www.rfc-editor.org/rfc/rfc9535) in Elm.

## Quick start

```elm
import Json.Decode
import JsonPath
import JsonPath.Extractor

jsonSample : Json.Decode.Value
jsonSample =
    ... -- Your JSON here

extractedJson : Result JsonPath.Error Json.Decode.Value
extractedJson =
    JsonPath.Extractor.run
        "$.store.book[*].author"
        sampleJson
```

## Status

This package is a work in progress and does not yet support the full [JSONPath specification](https://www.rfc-editor.org/rfc/rfc9535). Below is a summary of the supported syntax:

### Identifiers

| Identifier   | Syntax | Supported |
| ------------ | ------ | --------- |
| Root node    | `$`    | ✅        |
| Current node | `@`    | ❌        |

### Segments

| Segment       | Syntax  | Example                      | Supported |
| ------------- | ------- | ---------------------------- | --------- |
| Child           | `.`   | `$.store.book.author`        | ✅        |
| Children        | `[]`  | `$.store.book[author,title]` | ✅        |
| All descendants | `..`  | `$.store..price`             | ❌        |

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
