# Ocaml protoc plugin
[![Main workflow](https://github.com/andersfugmann/ocaml-protoc-plugin/actions/workflows/workflow.yml/badge.svg)](https://github.com/andersfugmann/ocaml-protoc-plugin/actions/workflows/workflow.yml)

The goal of Ocaml protoc plugin is to create plugin for
the google protobuf compiler (`protoc`) to generate Ocaml types and
serialization and de-serialization functions from a `.proto`
file. Ocaml-protoc-plugin aims to be a fully compliant implementation
of the google protobuffer standard and guidelines.

The main features include:
* Messages are mapped to idiomatic OCaml types, using modules
* Support service descriptions
* proto3 compliant
* proto2 compliant
* Supports includes
* Supports proto2 extensions
* Builtin support for google well known types
* Configurable annotations for all generated types
* Json serialization and deserialization based on protobuf
  specification

## Comparison with other OCaml protobuf handlers.

| Feature           | ocaml-protoc-plugin | ocaml-protoc  | ocaml-pb            |
| -------           | ------------------- | ------------  | ---------------     |
| Ocaml types       | Supported           | Supported     | Defined runtime[^1] |
| Service endpoints | Supported           | Supported     | N/A                 |
| proto3            | Supported           | Supported[^3] | Supported           |
| proto2            | Supported           | Supported[^3] | Supported           |
| proto2 extends    | Supported           | Ignored       | Supported           |
| proto2 groups     | Not supported[^2]   | Ignored       | ?                   |
| json mappings     | Supported           | Supported     | ?                   |

[^1] Ocaml-bp has a sister project `Ocaml-bp-plugin` which emit
Ocaml-pb definitions from a `.proto`. The plugin parses files are proto2
Ocaml type definitions (all fields are option types), and repeated
fields are not packed by default.

[^2] Groups has been deprecated by google and should not be used.

[^3] `ocaml_protoc` will always transmit all fields that are not
marked optional, and does not *strictly* comply to the protobuf
specification.


## Types
Basic types are mapped trivially to Ocaml types:

Primitive types:

| Protobuf Type                                | Ocaml type       |
| -------------                                | ----------       |
| int32, int64, uint32, uint64, sint32, sint64 | int[^5]          |
| fixed64, sfixed64, fixed32, sfixed32         | int32, int64[^5] |
| bool                                         | bool             |
| float, double                                | float            |
| string                                       | string           |
| bytes                                        | bytes            |

[^5] The plugin supports changing the type for scalar types to
int/int64/int32. See options section below.

A message <name> declaration is compiled to a module <Name> with a record type
`t`. Single field messages are mapped not wrapped in a record and messages without any fields are mapped to unit.

Packages are trivially mapped to modules.
Included proto files (`<name>.proto`) are assumed to have been compiled
to `<name>.ml`, and types in included proto files are referenced by
their fill name.

Compound types are mapped like:

| Protobuf Type | Ocaml type                                                              |
| ------------- | ----------                                                              |
| oneof         | Polymorphic variants:  `[ Field1 of fieldtype1, Field1 of fieldtype2 ]` |
| repeated 'a   | 'a list                                                                 |
| message       | message option                                                          |
| enum          | Abstract data types: `` Enum1, Enum2, Enum3 ``                          |
| map<'a, 'b>   | Associative list: ('a * 'b) list                                        |


## Proto2 type support
The specification for proto2 states that when deserializing a message,
fields which are not transmitted should be set the the default value
(either 0, or the value of the default option).

However, It seems to be the norm for proto2, that it should
be possible to determine if a field was transmitted or not. Therefore
all non-repeated fields in proto2 are option types - unless the field has a
default value, or is a required field.

The proto2 specification states that no default values should be
transmitted. However, as it is normal to be able to identify if a
field has been transmitted or not, only fields with an explicit
default value will be omitted when the value for the field matches the
default value.

## Invocation
If the plugin is available in the path as `protoc-gen-ocaml`, then you
can generate the Ocaml code by running

```
  protoc --ocaml_out=. --ocaml_opt=<options> file.proto
```

## Options

*Options* control the code/types generated.

| Option               | Description                                                     | Example                      | Default |
| -----------          | ------------------------------                                  | -----------------------      | ------- |
| annot                | Type annotations.                                               | `annot=[@@deriving show]`    | ""      |
| debug                | Enable debugging                                                | `debug`                      | Not set |
| open                 | Add open at top of generated files. May be given multiple times | `open=Base.Sexp`             | []      |
| int64\_as\_int       | Map \*int64 types to int instead of `int64`                     | `int64_as_int=false`         | true    |
| int32\_as\_int       | Map \*int32 types to int instead of `int32`                     | `int32_as_int=false`         | true    |
| fixed\_as\_int       | Map \*fixed\* types to `int`                                    | `fixed_as_int=true`          | false   |
| singleton\_record    | Messages with only one field will be wrapped in a record        | `singleton_record=true`      | false   |
| prefix\_output\_with\_package | Emit files prefixed with their package name. This allows multiple protofiles of the same name with different package names to be used | `prefix_output_with_package=true` | false |
| singleton\_oneof\_as\_option | Oneof declarations only containing one field are mapped to a single optional field | singleton\_oneof\_as\_option=false | true |

Parameters are separated by `;`

If `protoc-gen-ocaml` is not located in the path, it is possible to
specify the exact path to the plugin:

```
protoc --plugin=protoc-gen-ocaml=../plugin/ocaml-protocol-plugin.exe --ocaml_out=. <file>.proto
```

### Json serialization and deserialization
Ocaml-proto-plugin can serialize and deserialize to/from
json. Using the function `to_json`, `from_json` and
`from_json_exn` similar to regular protobuffer serialization and
deserialization.

Json serialization can be controlled using optional arguments:

```ocaml
val to_json: Json_options.t -> t -> Json.t
```

The `Json.t` type is identical to
`Yojson.Basic.t`. ocaml-protoc-plugin does does not depend on `Yojson`
to limit number of dependencies.


The `options` argument allows finer grained control over json
serialization. The followin options are supported:

| argument  | comment  | default  |
|---|---|---|
| `enum_names`  | By default enum names are used when serializing to json. Setting to false will use enum values instead | true  |
| `json_names`  | By default Json will use camelCase names for fields. Setting to false will use the names as defined in the proto definition file | true  |
| `omit_default_values` | By default json will not serialize default values to reduce size. Setting to false will for all values to be emitted to the json file. | true |

Json deserialization will accept json constructed with any combination
of options.

The json serialization and deserialization has support for
special handling of the following types to follow the
[protobuf
specification](https://protobuf.dev/programming-guides/proto3/#json)
as close as possible:

| type | json example | notes |
|  --  |   --    | -- |
| bytes | "Ynl0ZXM=" | Base64 encoded |
| google.protobuf.Empty | {} | |
| google.protobuf.Duration | "123.456s" | |
| google.protobuf.Timestamp | "2024-03-08T20:54:43.000000000Z" |
| google.protobuf.Value | { "x": 4 } |  |
| google.protobuf.Struct | { "x": 4 } | |
| google.protobuf.ListValue | [ 4,5,6 ] | |
| google.protobuf.FieldMask | "camelCase,camelCased" | Not guaranteed to be reversible due to camel case conversion |
| google.protobuf.Any | | Not handled, as the ocaml-protobuf-plugin does not support dynamic protobuf parsing |


Json serialization and deserializaiton is not well optimized and
should not be used in performance critical applications.

### Older versions of protoc
Some older versons of google protobuf compiler `protoc` does not
support the `--ocaml_opt` flag. As an alternative, options can also be
passed with the `--ocaml_out` flag:

```
protoc --ocaml_out=annot=debug;[@@deriving show { with_path = false }, eq]:. <file>.proto
```

## Mangle generated names
Idiomatic protobuf names are somewhat alien to
Ocaml. `Ocaml_protoc_plugin` has an option to mangle protobuf names
into somewhat more Ocaml idiomatic names. When this option is set (see
below), names are mangled to snake case as described in the table
below:

| Protobyf type | Protobuf name            | Ocaml name               |
|:--------------|:-------------------------|:-------------------------|
| package       | `CapitalizedSnakeCase`   | `Capitalized_snake_case` |
| message       | `CapitalizedSnakeCase`   | `Capitalized_snake_case` |
| field         | `lowercased_snake_case`  | `lowercased_snake_case`  |
| oneof name    | `lowercased_snake_case`  | `lowercased_snake_case`  |
| oneof field   | `capitalized_snake_case` | `Capitalized_snake_case` |
| enum          | `CAPITALIZED_SNAKE_CASE` | `Capitalized_snake_case` |
| service name  | `CapitalizedSnakeCase`   | `Capitalized_snake_case` |
| rpc name      | `LowercasedSnakeCase`    | `lowercased_snake_case`  |

`protoc` cannot guarantee that names do not clash when mangling is
enabled. If a name clash is detected (eg. `SomeMessage` and
`some_message` exists in same file) an apostrophe is appended to the
name to make sure names are unique.

The algorithm for converting CamelCased names to snake_case is done by
injecting an underscore between any lowercase and uppercase character
and then lowercasing the result.

### Setting mangle option
Name mangling option can only be controlled from within the protobuf
specification file. This is needed as protobuf files may reference each
other and it its important to make sure that included names are
referenced correctly across compilation units (and invocations of
protoc).

To set the option use:
```protobuf
// This can be placed in a common file and then included
import "google/protobuf/descriptor.proto";
message options { bool mangle_names = 1; }
extend google.protobuf.FileOptions {
    options ocaml_options = 1074;
}

// This option controls name generation. If true names are converted
// into more ocaml ideomatic names
option (ocaml_options) = { mangle_names:true };

// This message will be mapped to module name My_proto_message
message MyProtoMessage { }
```

### Deprecation annotations in proto files
Protobug specification (.proto file) allow for deprecating *files*,
*enums*, *enum values*, *messages*, *message fields*, *services* and
*methods*.

These deprecations are kept in the ocaml mapping to generate
[alerts](https://ocaml.org/manual/alerts.html) for alert category
'protobuf'.

Alerts are automatically enabled as warnings. To disable deprecation
alerts, either add the compiler flag `-alert -protobuf`, or though ocaml
annotations e.g. by adding `[@@@ocaml.alert "-protobuf"]` to the top
of a file.


## Using dune
Below is a dune rule for generating code for `test.proto`. The
`google_include` target is used to determine the base include path for
google protobuf well known types.
```
(rule
 (targets google_include)
 (action (with-stdout-to %{targets}
          (run pkg-config protobuf --variable=includedir))))

(rule
 (targets test.ml)
 (deps
  (:proto test.proto))
 (action
  (run protoc -I %{read-lines:google_include} -I .  "--ocaml_opt=annot=[@@deriving show { with_path = false }, eq]" --ocaml_out=. %{proto})))
```

## Service interface
Service interfaces create a module with values that just references
the request and reply pair. These binding can then be used with
function in `Protobuf.Service`.

The call function will take a `string -> string` function, which
implement message sending -> receiving.

The service function is a `string -> string` function which takes a
handler working over the actual message types.

## Proto2 extensions
Proto2 extensions allows for messages to be extended. For each
extending field, the plugin create a module with a get and set
function for reading/writing extension fields.

Below is an example on how to set and get extension fields


```protobuf
// file: ext.proto
syntax = "proto2";
message Foo {
  required uint32 i = 1;
  extensions 100 to 200;

}
extend Foo {
  optional uint32 bar = 128;
  optional string baz = 129;
}
```

```ocaml
(* file: test.ml *)

open Extensions

(* Set extensions *)
let _ =
  let foo = Foo.{ i = 31; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo_with_bar = Bar.set foo (Some 42) in
  let foo_with_baz = Baz.set foo (Some "Test String") in
  let foo_with_bar_baz = Baz.set foo_with_bar (Some "Test String") in

  (* Get extensions *)
  let open Ocaml_protoc_plugin.Result in
  Bar.get foo_with_bar >>= fun bar ->
  Baz.get foo_with_baz >>= fun baz ->
  assert (bar = Some 42);
  assert (baz = Some "Test String");
  Bar.get foo_with_bar_baz >>= fun bar' ->
  Baz.get foo_with_bar_baz >>= fun baz' ->
  assert (bar' = Some 42);
  assert (baz' = Some "Test String");
  return ()

```
Extensions are replaced by proto3 `Any` type, and use is discouraged.

## Proto3 Any type
No special handling of any type is supported, as Ocaml does not allow
for runtime types, so any type must be handled manually by
serializing and deserializing the embedded message.

## Proto3 Optional fields
Proto3 optional fields are handled in the same way as proto2 optional
fields; The type is an option type, and always transmissted when set.

## Imported protofiles
The generated code assumes that imported modules (generated from proto
files) are available in the compilation scope. If the modules
generated from imported protofiles resides in different a different
scope (e.g. is compiled with `wrapped true`, they need to be made
available by adding parameter `open=<module name>` to make the modules
available for the compilation.

### Google Well know types
Protobuf distributes a set of [*Well-Known
types*](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf).
`ocaml-protoc-plugin` installs compiled versions of these. These can
be used by linking with the package `ocaml-protoc-plugin.google_types`, and adding
option `open=Google_types` to the list of parameters

The distributed google types are compiled using default parameters,
i.e. without any ppx annotations.

If you want to change this, or add type annotations, you can copy the
[dune](https://github.com/andersfugmann/ocaml-protoc-plugin/tree/main/src/google_types/dune)
from the distribution to your own project, and make alterations
there. See the [echo\_deriving](https://github.com/andersfugmann/ocaml-protoc-plugin/tree/main/examples/echo_deriving)
example on how to do this.

# Example

`test.proto`
```protobuf
syntax = "proto3";
message Address {
  enum Planet {
    Earth = 0; Mars  = 1; Pluto = 2;
  }
  string street = 1;
  uint64 number = 2;
  Planet planet = 3;
}

message Person {
  uint64 id       = 1;
  string name     = 2;
  Address address = 3;
}
```

`$ protoc --ocaml_out=. test.proto`

Generates a file `test.ml` with the following signature:

```ocaml
module Address : sig
  module rec Planet : sig
    type t = Earth | Mars | Pluto
    val to_int: t -> int
    val from_int: int -> t Protobuf.Deserialize.result
  end
  val name': unit -> string
  type t = {
    street: string;
    number: int;
    planet: Planet.t;
  }
  val make: ?street:string ?number:int ?planet:Planet.t -> unit -> t
  val to_proto: t -> Protobuf.Writer.t
  val from_proto: Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) result
end
module Person : sig
  val name': unit -> string
  type t = {
    id: int;
    name: string;
    address: Address.t option;
  }
  val make: ?id:int ?name:string ?planet:Address.t -> unit -> t
  val to_proto: t -> Protobuf.Writer.t
  val from_proto: Protobuf.Reader.t -> (t, Protobuf.Deserialize.error) result
end = struct
```

Note that if `test.proto` had a package declaration such as `package testing`,
the modules `Address` and `Person` listed above would be defined as sub-modules
of a top-level module `Testing`.

The function `make` allows the user to create message without
specifying all (or any) fields. Using this function will allow users
to add fields to message later without needing to modify any code, as
new fields will be set to default values.

`Protobuf.Reader` and `Protobuf.Writer` are used then reading or
writing protobuf binary format. Below is an example on how to decode a message
and how to read a message.

```ocaml
let string_of_planet = function
  | Address.Earth -> "earth"
  | Mars -> "mars"
  | Pluto -> "pluto"
in

let read_person binary_message =
  let reader = Protobuf.Reader.create binary_message in
  match Person.from_proto reader in
  | Ok Person.{ id; name; address = Some Address { street; number; planet } } ->
    Printf.printf "P: %d %s - %s %s %d\n" id name (string_of_planet planet) street number
  | Ok Person.{ id; name; address = None } ->
    Printf.printf "P: %d %s - Address unknown\n" id name
  | Error _ -> failwith "Could not decode"
```

More examples can be found under
[examples](https://github.com/andersfugmann/ocaml-protoc-plugin/tree/main/examples)

# Benchmarks

ocaml-protoc-plugin has been optimized for speec, and is comparable [ocaml-protoc](https://github.com/mransan/ocaml-protoc)

Numbers below shows benchmark comparing encoding and decoding speed to
`ocaml-protoc`. The benchmark is run with flambda settings: `-O3 -unbox-closures -unboxed-types -remove-unused-arguments -rounds 4 -inline 100.00 -inline-max-depth 5 -inline-max-unroll 5 -unsafe`. Benchmarks are made on a Intel i5, i5-5257U CPU
@ 2.70GHz, using ocaml switch `ocaml-option-flambda.1,ocaml-variants.5.2.0~beta1+options`

|          Test               |   plugin ns/run   |   protoc ns/run  | ratio |
|           --                |     --     |     --     |  --   |
|                bench/decode |  151046.64 |  146006.25 |  1.03 |
|                bench/encode |  113419.40 |  162761.03 |  0.70 |
|                int64/decode |      24.27 |      22.04 |  1.10 |
|                int64/encode |      42.98 |      42.50 |  1.01 |
|                float/decode |      24.09 |      23.59 |  1.02 |
|                float/encode |      44.96 |      33.30 |  1.35 |
|               string/decode |      42.76 |      38.35 |  1.11 |
|               string/encode |      94.54 |      52.77 |  1.79 |
|                 enum/decode |      26.44 |      25.47 |  1.04 |
|                 enum/encode |      75.54 |      32.69 |  2.31 |
|                empty/decode |       7.42 |       5.32 |  1.39 |
|                empty/encode |       5.63 |      43.40 |  0.13 |
|           int64 list/decode |   15060.06 |   14977.46 |  1.01 |
|           int64 list/encode |   10132.74 |   11871.89 |  0.85 |
|           float list/decode |   14221.61 |   13123.94 |  1.08 |
|           float list/encode |    8596.24 |   12644.59 |  0.68 |
|          string list/decode |   45024.73 |   57261.46 |  0.79 |
|          string list/encode |   39081.08 |   51495.56 |  0.76 |
|     map<int64,int64>/decode |   81572.19 |   91668.48 |  0.89 |
|     map<int64,int64>/encode |   92840.01 |   49106.69 |  1.89 |
| varint_boxed(0xffffffff)/decode |      19.25 |      24.17 |  0.80 |
| varint_boxed(0xffffffff)/encode |       4.22 |      14.84 |  0.28 |
| varint_unboxed(0xffffffff)/decode |      19.48 |      22.94 |  0.85 |
| varint_unboxed(0xffffffff)/encode |       3.22 |      15.16 |  0.21 |
| varint_boxed(0xffffffffffffffff)/decode |      36.33 |      41.21 |  0.88 |
| varint_boxed(0xffffffffffffffff)/encode |      15.30 |      19.18 |  0.80 |
| varint_unboxed(0xffffffffffffffff)/decode |      35.67 |      39.70 |  0.90 |
| varint_unboxed(0xffffffffffffffff)/encode |       7.17 |      21.06 |  0.34 |

# Acknowledgements
This work is based on
[issuu/ocaml-protoc-plugin](https://github.com/issuu/ocaml-protoc-plugin).
Thanks to [Issuu](https://issuu.com) who initially developed this library/plugin.

Thanks to all contributers:
* Anders Fugmann <anders@fugmann.net>
* Andreas Dahl <andreas@hrdahl.dk>
* Dario Teixeira <dte@issuu.com>
* Kate <kit.ty.kate@disroot.org>
* Martin Slota <msl@issuu.com>
* Nymphium <s1311350@gmail.com>
* Rauan Mayemir <rauan@mayemir.io>
* Tim McGilchrist <timmcgil@gmail.com>
* Virgile Prevosto <virgile.prevosto@m4x.org>
* Wojtek Czekalski <me@wczekalski.com>
