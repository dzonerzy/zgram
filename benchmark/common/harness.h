/* Shared timing harness for the C/C++ benchmarks (same method and output
 * format as the other benchmarks): auto-calibrate until one batch takes
 * >= 2 seconds, then report time per parse. */
#ifndef BENCH_HARNESS_H
#define BENCH_HARNESS_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static inline double bench_now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec * 1e-9;
}

/* Read a whole file; the caller frees *out. */
static inline size_t bench_read_file(const char *path, char **out) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "Cannot open %s\n", path);
        exit(1);
    }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)malloc((size_t)size + 1);
    if (fread(buf, 1, (size_t)size, f) != (size_t)size) {
        fprintf(stderr, "Cannot read %s\n", path);
        exit(1);
    }
    buf[size] = 0;
    fclose(f);
    *out = buf;
    return (size_t)size;
}

/* Keep a result alive so the compiler can't drop the parse. */
#define BENCH_KEEP(x) __asm__ volatile("" : : "r"(x) : "memory")

/* parse(ctx) must return nonzero on success. */
static inline void bench_run(const char *name, size_t input_len, int (*parse)(void *), void *ctx) {
    if (!parse(ctx)) {
        fprintf(stderr, "Parse failed\n");
        exit(1);
    }
    fprintf(stderr, "Parse OK. Benchmarking...\n");
    uint64_t iters = 1000;
    for (;;) {
        double start = bench_now();
        for (uint64_t i = 0; i < iters; ++i) {
            int ok = parse(ctx);
            BENCH_KEEP(ok);
        }
        double elapsed = bench_now() - start;
        if (elapsed >= 2.0) {
            fprintf(stderr, "\n-- %s (%zu bytes) --\n", name, input_len);
            fprintf(stderr, "  Iterations:  %lu\n", (unsigned long)iters);
            fprintf(stderr, "  Total:       %.4fs\n", elapsed);
            fprintf(stderr, "  Per-parse:   %.2fus\n", elapsed * 1e6 / (double)iters);
            fprintf(stderr, "  Ops/sec:     %.0f\n", (double)iters / elapsed);
            return;
        }
        iters = elapsed < 0.1 ? iters * 20 : (uint64_t)((double)iters * 2.5 / elapsed);
    }
}

#endif
