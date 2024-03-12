// pkgconf --libs protobuf

// g++ -o bpjson bpjson.cc `pkgconf --libs protobuf
// ./bpjson <proto_file> <message type> < <binary data>

#include <google/protobuf/util/type_resolver_util.h>
#include <google/protobuf/util/json_util.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/compiler/importer.h>
#include <google/protobuf/descriptor_database.h>
#include <google/protobuf/descriptor.h>
#include <string>
#include <ostream>
#include <sstream>
#include <filesystem>

#include <string.h>
using namespace google::protobuf;


util::TypeResolver * make_resolver(const char* include, const char* proto_file) {
    auto source_tree = new compiler::DiskSourceTree();
    source_tree->MapPath("", ".");
    source_tree->MapPath("", include);
    source_tree->MapPath("/", include);
    auto importer = new compiler::Importer(source_tree, NULL);

    if (proto_file[0] == 0) {
        for(const auto& p : std::filesystem::directory_iterator(".")) {
            if(p.path().extension() == ".proto") {
                auto * fd = importer->Import(p.path().filename());
            }
        }
    } else {
        auto * fd = importer->Import(proto_file);
    }
    return util::NewTypeResolverForDescriptorPool("type.googleapis.com", importer->pool());
}

std::string make_url(const char * type) {
    return std::string("type.googleapis.com/") + std::string(type);
}

char* status_to_string(const util::Status& status, const std::string& output_str) {
    if (status.ok()) {
        return strdup(output_str.c_str());
    } else {
        std::string s = status.ToString();
        return strdup(s.c_str());
    }
}

extern "C" char* protobuf2json(const char *google_include_path, const char* proto_file, const char* type, const void* in_data, int data_length) {
    std::string url = make_url(type);
    auto resolver = make_resolver(google_include_path, proto_file);

    io::ArrayInputStream input(in_data, data_length);
    std::string output_str;
    io::StringOutputStream output(&output_str);

    util::JsonPrintOptions options;
    options.add_whitespace = true;
    //options.always_print_primitive_fields = true;
    auto status = BinaryToJsonStream(
        resolver, url, &input, &output, options);
    return status_to_string(status, output_str);
}

extern "C" char* json2protobuf(const char *google_include_path, const char* proto_file, const char* type, const void* in_data, int data_length) {
    std::string url = make_url(type);
    auto resolver = make_resolver(google_include_path, proto_file);

    io::ArrayInputStream input(in_data, data_length);
    std::string output_str;
    io::StringOutputStream output(&output_str);

    util::JsonParseOptions options;
    options.ignore_unknown_fields = true;
    //options.always_print_primitive_fields = true;

    auto status = JsonToBinaryStream(
        resolver, url, &input, &output, options);
    return status_to_string(status, output_str);
    // This function should return the length of the buffer also.
}
