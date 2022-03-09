#include "protoclient.h"

#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/message.h>
#include <google/protobuf/util/delimited_message_util.h>
#include "tools/haskell_module_worker/worker_protocol.pb.h"

struct ProtoClient {
    google::protobuf::io::ZeroCopyInputStream* input;
    google::protobuf::io::FileOutputStream* output;
};

extern "C" struct ProtoClient* createProtoClient(int fdIn, int fdOut) {
    struct ProtoClient* pc = new ProtoClient();
    pc->input = new google::protobuf::io::FileInputStream(fdIn);
    pc->output = new google::protobuf::io::FileOutputStream(fdOut);
    return pc;
}

extern "C" int readWorkRequest(struct ProtoClient* pc, char*** args, int* nargs, int* verbosity) {
    blaze::worker::WorkRequest message;
    bool rc = google::protobuf::util::ParseDelimitedFromZeroCopyStream(&message, pc->input, NULL);

    if (rc) {
        *verbosity = message.verbosity();
        *nargs = message.arguments_size();
        *args = (char**)malloc((*nargs)*sizeof(char*));
        for(int i=0;i<*nargs;i++) {
            (*args)[i] = (char*) strdup(message.arguments(i).c_str());
        }
    }

    return !rc;
}

extern "C" int writeWorkResponse(struct ProtoClient* pc, int exit_code, const char* output) {
    blaze::worker::WorkResponse message;
    message.set_exit_code(exit_code);
    message.set_output(output);
    message.clear_request_id();
    message.clear_was_cancelled();
    bool rc = google::protobuf::util::SerializeDelimitedToZeroCopyStream(message, pc->output);
    if (rc)
        pc->output->Flush();
    return !rc;
}
