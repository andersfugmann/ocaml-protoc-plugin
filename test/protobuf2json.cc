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

#include <string.h>
using namespace google::protobuf;

extern "C" char* protobuf2json(const char *proto, const char* type, const void* in_data, int data_length) {
    std::string url = std::string("type.googleapis.com/") + std::string(type);
    compiler::DiskSourceTree source_tree;
    source_tree.MapPath("", ".");
    source_tree.MapPath("/", "/");

    compiler::SourceTreeDescriptorDatabase database(&source_tree);
    FileDescriptorProto fdp;
    database.FindFileByName(proto, &fdp);
    SimpleDescriptorDatabase db;
    db.Add(fdp);
    DescriptorPool pool(&db);
    auto resolver = util::NewTypeResolverForDescriptorPool("type.googleapis.com", &pool);
    io::ArrayInputStream input(in_data, data_length);
    std::string output_str;
    io::StringOutputStream output(&output_str);

    util::JsonPrintOptions options;
    options.add_whitespace = true;
    //options.always_print_primitive_fields = true;
    auto status = BinaryToJsonStream(
        resolver, url, &input, &output, options);

    if (status.ok()) {
        return strdup(output_str.c_str());
    } else {
        std::string s = status.ToString();
        return strdup(s.c_str());
    }
}
