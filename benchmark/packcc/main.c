/* PackCC JSON benchmark (validation only): the parser is generated from
 * json.peg; a context is created and destroyed per parse, as PackCC parsers
 * are used. */
#include <string.h>

#include "harness.h"
#include "json.h"

static int validate(void *p) {
    struct json_input *in = (struct json_input *)p;
    in->pos = 0;
    in->error = 0;
    json_context_t *ctx = json_create(in);
    json_parse(ctx, NULL);
    json_destroy(ctx);
    return !in->error;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: packcc_bench <json_file>\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    struct json_input in = {data, len, 0, 0};
    bench_run("PackCC", len, validate, &in);
    return 0;
}
