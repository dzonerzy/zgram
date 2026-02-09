#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== zgram vs PEGTL vs cpp-peglib vs pest vs rust-peg — JSON Parse Benchmark ==="
echo ""

# Generate test data
echo "Generating test data..."
python3 "$SCRIPT_DIR/generate_data.py"
echo ""

# Build zgram benchmark
echo "Building zgram benchmark..."
cd "$ROOT_DIR"
zig build -Doptimize=ReleaseFast bench
ZGRAM_BIN="$ROOT_DIR/zig-out/bin/zgram_bench"
echo ""

# Build PEGTL benchmark
echo "Building PEGTL benchmark..."
mkdir -p "$SCRIPT_DIR/pegtl/build"
cd "$SCRIPT_DIR/pegtl/build"
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-O3" > /dev/null 2>&1
make -j"$(nproc)" > /dev/null 2>&1
PEGTL_BIN="$SCRIPT_DIR/pegtl/build/pegtl_bench"
echo ""

# Build cpp-peglib benchmark
echo "Building cpp-peglib benchmark..."
mkdir -p "$SCRIPT_DIR/cpp-peglib/build"
cd "$SCRIPT_DIR/cpp-peglib/build"
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-O3" > /dev/null 2>&1
make -j"$(nproc)" > /dev/null 2>&1
CPPPEG_BIN="$SCRIPT_DIR/cpp-peglib/build/cpppeg_bench"
echo ""

# Build pest benchmark
echo "Building pest benchmark..."
cd "$SCRIPT_DIR/pest"
cargo build --release > /dev/null 2>&1
PEST_BIN="$SCRIPT_DIR/pest/target/release/pest_bench"
echo ""

# Build rust-peg benchmark
echo "Building rust-peg benchmark..."
cd "$SCRIPT_DIR/rust-peg"
cargo build --release > /dev/null 2>&1
RUSTPEG_BIN="$SCRIPT_DIR/rust-peg/target/release/rustpeg_bench"
echo ""

# Run benchmarks
for size in small medium large; do
    FILE="$SCRIPT_DIR/data/$size.json"
    BYTES=$(wc -c < "$FILE")
    echo "============================================"
    echo "  $size.json ($BYTES bytes)"
    echo "============================================"
    echo ""
    "$ZGRAM_BIN" "$FILE" 2>&1
    echo ""
    "$PEGTL_BIN" "$FILE" 2>&1
    echo ""
    "$CPPPEG_BIN" "$FILE" 2>&1
    echo ""
    "$PEST_BIN" "$FILE" 2>&1
    echo ""
    "$RUSTPEG_BIN" "$FILE" 2>&1
    echo ""
done
