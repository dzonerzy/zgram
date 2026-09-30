#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== zgram vs rust-peg, Spirit X3, lexy, PEGTL, pest, PackCC, cpp-peglib — JSON Parse Benchmark ==="
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

# Build lexy benchmarks (generic tree + typed AST)
echo "Building lexy benchmarks..."
mkdir -p "$SCRIPT_DIR/lexy/build"
cd "$SCRIPT_DIR/lexy/build"
cmake .. -DCMAKE_BUILD_TYPE=Release > /dev/null 2>&1
make -j"$(nproc)" > /dev/null 2>&1
LEXY_BIN="$SCRIPT_DIR/lexy/build/lexy_bench"
LEXY_AST_BIN="$SCRIPT_DIR/lexy/build/lexy_ast_bench"
echo ""

# Build Boost.Spirit X3 benchmarks (flat tree + typed AST)
echo "Building Spirit X3 benchmarks..."
mkdir -p "$SCRIPT_DIR/spirit-x3/build"
cd "$SCRIPT_DIR/spirit-x3/build"
cmake .. -DCMAKE_BUILD_TYPE=Release > /dev/null 2>&1
make -j"$(nproc)" > /dev/null 2>&1
X3_BIN="$SCRIPT_DIR/spirit-x3/build/spirit_x3_bench"
X3_AST_BIN="$SCRIPT_DIR/spirit-x3/build/spirit_x3_ast_bench"
echo ""

# Build PackCC benchmark
echo "Building PackCC benchmark..."
mkdir -p "$SCRIPT_DIR/packcc/build"
cd "$SCRIPT_DIR/packcc/build"
cmake .. -DCMAKE_BUILD_TYPE=Release > /dev/null 2>&1
make -j"$(nproc)" > /dev/null 2>&1
PACKCC_BIN="$SCRIPT_DIR/packcc/build/packcc_bench"
echo ""

# Build pest benchmark
echo "Building pest benchmark..."
cd "$SCRIPT_DIR/pest"
cargo build --release > /dev/null 2>&1
PEST_BIN="$SCRIPT_DIR/pest/target/release/pest_bench"
echo ""

# Build rust-peg tree-building benchmark
echo "Building rust-peg (tree) benchmark..."
cd "$SCRIPT_DIR/rust-peg-tree"
cargo build --release > /dev/null 2>&1
RUSTPEG_TREE_BIN="$SCRIPT_DIR/rust-peg-tree/target/release/rustpeg_tree_bench"
echo ""

# Build rust-peg benchmark
echo "Building rust-peg benchmark..."
cd "$SCRIPT_DIR/rust-peg"
cargo build --release > /dev/null 2>&1
RUSTPEG_BIN="$SCRIPT_DIR/rust-peg/target/release/rustpeg_bench"
echo ""

# Run benchmarks
for size in small medium large strings; do
    FILE="$SCRIPT_DIR/data/$size.json"
    BYTES=$(wc -c < "$FILE")
    echo "============================================"
    echo "  $size.json ($BYTES bytes)"
    echo "============================================"
    echo ""
    "$ZGRAM_BIN" "$FILE" 2>&1
    echo ""
    "$ZGRAM_BIN" "$FILE" --validate 2>&1
    echo ""
    "$RUSTPEG_TREE_BIN" "$FILE" 2>&1
    echo ""
    "$X3_BIN" "$FILE" --tree 2>&1
    echo ""
    "$X3_BIN" "$FILE" 2>&1
    echo ""
    "$X3_AST_BIN" "$FILE" 2>&1
    echo ""
    "$LEXY_BIN" "$FILE" --tree 2>&1
    echo ""
    "$LEXY_BIN" "$FILE" 2>&1
    echo ""
    "$LEXY_AST_BIN" "$FILE" 2>&1
    echo ""
    "$PEGTL_BIN" "$FILE" --tree 2>&1
    echo ""
    "$PACKCC_BIN" "$FILE" 2>&1
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

# ── Expression grammar ──
for size in small medium large deep; do
    FILE="$SCRIPT_DIR/data/expr_$size.txt"
    BYTES=$(wc -c < "$FILE")
    echo "============================================"
    echo "  expr_$size.txt ($BYTES bytes)"
    echo "============================================"
    echo ""
    "$ZGRAM_BIN" "$FILE" --expr 2>&1
    echo ""
    "$ZGRAM_BIN" "$FILE" --expr --validate 2>&1
    echo ""
    "$SCRIPT_DIR/rust-peg-tree/target/release/expr" "$FILE" 2>&1
    echo ""
    "$SCRIPT_DIR/rust-peg/target/release/expr" "$FILE" 2>&1
    echo ""
    "$SCRIPT_DIR/spirit-x3/build/spirit_x3_expr_bench" "$FILE" --tree 2>&1
    echo ""
    "$SCRIPT_DIR/spirit-x3/build/spirit_x3_expr_bench" "$FILE" 2>&1
    echo ""
    "$SCRIPT_DIR/lexy/build/lexy_expr_bench" "$FILE" --tree 2>&1
    echo ""
    "$SCRIPT_DIR/lexy/build/lexy_expr_bench" "$FILE" 2>&1
    echo ""
    "$SCRIPT_DIR/pegtl/build/pegtl_expr_bench" "$FILE" --tree 2>&1
    echo ""
    "$SCRIPT_DIR/pegtl/build/pegtl_expr_bench" "$FILE" 2>&1
    echo ""
    "$SCRIPT_DIR/pest/target/release/expr" "$FILE" 2>&1
    echo ""
done
