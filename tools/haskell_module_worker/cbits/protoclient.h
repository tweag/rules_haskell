#ifndef RULES_HASKELL_WORKER_PROTOCLIENT_H
#define RULES_HASKELL_WORKER_PROTOCLIENT_H

#ifdef __cplusplus
extern "C" {
#endif

struct ProtoClient;

// Takes files descriptor for reading and writing messages from/to.
struct ProtoClient* createProtoClient(int, int);

// Reads a WorkRequest
//
// Input files are ignored for now.
int readWorkRequest(struct ProtoClient*, int* request_id, char*** args, int* nargs, int* verbosity, char** sandbox_dir);

// Writes a WorkResponse
int writeWorkResponse(struct ProtoClient*, int request_id, int exit_code, const char* output);

#ifdef __cplusplus
}
#endif

#endif // RULES_HASKELL_WORKER_PROTOCLIENT_H
