#include <cstdlib>
#include <iostream>
#include <fstream>
#include <memory>
#include "tools/cpp/runfiles/runfiles.h"
using bazel::tools::cpp::runfiles::Runfiles;

int main(int argc, char** argv) {
    std::string workspace(std::getenv("TEST_WORKSPACE"));
    std::string expected(argv[1]);
    std::string filename(argv[2]);

    std::string error;
    std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv[0], &error));
    if (runfiles == nullptr) {
        std::cerr << "Failed to create runfiles: " << error << "\n";
        return 1;
    }

    std::string path = runfiles->Rlocation(workspace + "/" + filename);

    std::ifstream file(path);
    std::string content;
    std::getline(file, content);

    if (content != std::string(argv[1])) {
        std::cerr << "Expected '" << expected << "' but found '" << content << "'\n";
        return 1;
    }
}
