#include <chrono>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <string>

#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/json.hpp>

namespace pegtl = tao::pegtl;

// Full JSON grammar + EOF
struct json_grammar : pegtl::must<pegtl::json::text, pegtl::eof> {};

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: pegtl_bench <json_file>\n");
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

    // Verify parse works
    {
        pegtl::memory_input in(input, argv[1]);
        if (!pegtl::parse<json_grammar>(in)) {
            fprintf(stderr, "Parse failed\n");
            return 1;
        }
    }
    fprintf(stderr, "Parse OK. Benchmarking...\n");

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    uint64_t iters = 1000;
    for (;;) {
        auto start = std::chrono::high_resolution_clock::now();
        for (uint64_t i = 0; i < iters; ++i) {
            pegtl::memory_input in(input, argv[1]);
            pegtl::parse<json_grammar>(in);
        }
        auto end = std::chrono::high_resolution_clock::now();
        double elapsed_s = std::chrono::duration<double>(end - start).count();

        if (elapsed_s >= 2.0) {
            double per_parse_us = (elapsed_s * 1e6) / iters;
            double ops_per_sec = iters / elapsed_s;

            fprintf(stderr, "\n-- PEGTL (%zu bytes) --\n", input.size());
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
