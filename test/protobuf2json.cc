/** Stub for reference implementation of
    json -> protobuf
    protobuf -> json
*/

#include <google/protobuf/util/type_resolver_util.h>
#include <google/protobuf/util/json_util.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/compiler/importer.h>
#include <google/protobuf/descriptor_database.h>
#include <google/protobuf/descriptor.h>
#include <string>
#include <ostream>
#include <sstream>

// Uncomment, to lookup the protobuf file in the filesystem when no protobuf file is specified
//#define USE_FILESYSTEM

#ifdef USE_FILESYSTEM
#include <filesystem>
#endif

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"

// Compatibility with ocaml <= 4.10
#ifndef Val_none
#define Val_none Val_int(0)
#endif
#ifndef Is_some
#define Is_some(v) Is_block(v)
#endif
#ifndef Is_none
#define Is_none(v) ((v) == Val_none)
#endif
#ifndef Some_val
#define Some_val(v) Field(v, 0)
#endif

using namespace google::protobuf;

util::TypeResolver* make_resolver(const std::string include, const std::string proto_file) {
    auto source_tree = new compiler::DiskSourceTree();
    source_tree->MapPath("", ".");
    source_tree->MapPath("", include);
    source_tree->MapPath("/", include);
    auto importer = new compiler::Importer(source_tree, NULL);

    if (proto_file.size() == 0) {
#ifdef USE_FILESYSTEM
        for(const auto& p : std::filesystem::directory_iterator(".")) {
            if(p.path().extension() == ".proto") {
                auto * fd = importer->Import(p.path().filename());
            }
        }
#else
        caml_failwith("No protofile specified");
#endif
    } else {
        auto * fd = importer->Import(proto_file);
    }
    return util::NewTypeResolverForDescriptorPool("type.googleapis.com", importer->pool());
}

std::string make_url(const char * type) {
    return std::string("type.googleapis.com/") + std::string(type);
}

extern "C" CAMLprim value protobuf2json(value google_include_path, value proto_file, value type, value data) {
    CAMLparam4(google_include_path, proto_file, type, data);

    std::string protobuf_file = Is_some(proto_file) ? String_val(Some_val(proto_file)) : "";
    std::string url = make_url(String_val(type));
    auto resolver = make_resolver(String_val(google_include_path), protobuf_file);

    io::ArrayInputStream input(String_val(data), caml_string_length(data));
    std::string output_str;
    io::StringOutputStream output(&output_str);

    util::JsonPrintOptions options;
    options.add_whitespace = true;
    //options.always_print_primitive_fields = true;
    auto status = BinaryToJsonStream(
        resolver, url, &input, &output, options);

    if (!status.ok()) {
        std::string msg = status.ToString();
        caml_invalid_argument(msg.c_str());
    }
    CAMLreturn(caml_alloc_initialized_string(output_str.size(), output_str.c_str()));
}

extern "C" CAMLprim value json2protobuf(value google_include_path, value proto_file, value type, value data) {
    CAMLparam4(google_include_path, proto_file, type, data);

    std::string protobuf_file = Is_some(proto_file) ? String_val(Some_val(proto_file)) : "";
    std::string url = make_url(String_val(type));
    auto resolver = make_resolver(String_val(google_include_path), protobuf_file);

    io::ArrayInputStream input(String_val(data), caml_string_length(data));
    std::string output_str;
    io::StringOutputStream output(&output_str);

    util::JsonParseOptions options;
    options.ignore_unknown_fields = true;

    auto status = JsonToBinaryStream(
        resolver, url, &input, &output, options);

    if (!status.ok()) {
        std::string msg = status.ToString();
        caml_invalid_argument(msg.c_str());
    }
    CAMLreturn(caml_alloc_initialized_string(output_str.size(), output_str.c_str()));
}
