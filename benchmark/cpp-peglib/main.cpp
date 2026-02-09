#include <chrono>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <string>

#include <peglib.h>

// JSON grammar from cpp-peglib's own grammar/json.peg (RFC 4627)
static const char* JSON_GRAMMAR = R"(
json        <- object / array

object      <- '{' (member (',' member)*)? '}' { no_ast_opt }
member      <- string ':' value

array       <- '[' (value (',' value)*)? ']'

value       <- boolean / null / number / string / object / array

boolean     <- 'false' / 'true'
null        <- 'null'

number      <- < minus int frac exp >
minus       <- '-'?
int         <- '0' / [1-9][0-9]*
frac        <- ('.' [0-9]+)?
exp         <- ([eE] [-+]? [0-9]+)?

string      <- '"' < char* > '"'
char        <- unescaped / escaped
escaped     <- '\\' (["\\/bfnrt] / 'u' [a-fA-F0-9]{4})
unescaped   <- [\u0020-\u0021\u0023-\u005b\u005d-\u10ffff]

%whitespace <- [ \t\r\n]*
)";

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: cpppeg_bench <json_file>\n");
        return 1;
    }

    // Read input file
    std::ifstream file(argv[1], std::ios::binary | std::ios::ate);
    if (!file) {
        fprintf(stderr, "Cannot open %s\n", argv[1]);
        return 1;
    }
    auto size = file.tellg();
    file.seekg(0);
    std::string input(size, '\0');
    file.read(input.data(), size);

    // Compile grammar
    fprintf(stderr, "Compiling JSON grammar...\n");
    peg::parser parser;

    parser.set_logger([](size_t line, size_t col, const std::string& msg) {
        fprintf(stderr, "  Grammar error at %zu:%zu: %s\n", line, col, msg.c_str());
    });

    if (!parser.load_grammar(JSON_GRAMMAR)) {
        fprintf(stderr, "Grammar compilation failed\n");
        return 1;
    }

    // Enable packrat parsing for best performance
    parser.enable_packrat_parsing();

    // Verify parse works
    if (!parser.parse(input)) {
        fprintf(stderr, "Parse failed on input\n");
        return 1;
    }
    fprintf(stderr, "Parse OK. Benchmarking...\n");

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    uint64_t iters = 1000;
    for (;;) {
        auto start = std::chrono::high_resolution_clock::now();
        for (uint64_t i = 0; i < iters; ++i) {
            parser.parse(input);
        }
        auto end = std::chrono::high_resolution_clock::now();
        double elapsed_s = std::chrono::duration<double>(end - start).count();

        if (elapsed_s >= 2.0) {
            double per_parse_us = (elapsed_s * 1e6) / iters;
            double ops_per_sec = iters / elapsed_s;

            fprintf(stderr, "\n-- cpp-peglib (%zu bytes) --\n", input.size());
            fprintf(stderr, "  Iterations:  %lu\n", iters);
            fprintf(stderr, "  Total:       %.4fs\n", elapsed_s);
            fprintf(stderr, "  Per-parse:   %.2fus\n", per_parse_us);
            fprintf(stderr, "  Ops/sec:     %.0f\n", ops_per_sec);
            break;
        }

        // Scale up
        if (elapsed_s < 0.1) {
            iters *= 20;
        } else {
            iters = static_cast<uint64_t>(iters * 2.5 / elapsed_s);
        }
    }

    return 0;
}
