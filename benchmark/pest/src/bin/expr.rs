// pest expression benchmark: parse into pest's token pairs (its tree).
use pest::Parser;
use pest_derive::Parser;
use std::env;
use std::fs;
use std::time::Instant;

#[derive(Parser)]
#[grammar = "src/expr.pest"]
struct ExprParser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: expr <expr_file>");
        std::process::exit(1);
    }
    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    let pairs = ExprParser::parse(Rule::text, &input).expect("Parse failed");
    eprintln!("Parse OK ({} nodes). Benchmarking...", pairs.flatten().count());

    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            let r = ExprParser::parse(Rule::text, &input);
            std::hint::black_box(&r);
        }
        let elapsed_s = start.elapsed().as_secs_f64();
        if elapsed_s >= 2.0 {
            eprintln!("\n-- pest ({} bytes) --", input.len());
            eprintln!("  Iterations:  {}", iters);
            eprintln!("  Total:       {:.4}s", elapsed_s);
            eprintln!("  Per-parse:   {:.2}us", (elapsed_s * 1e6) / iters as f64);
            eprintln!("  Ops/sec:     {:.0}", iters as f64 / elapsed_s);
            break;
        }
        if elapsed_s < 0.1 {
            iters *= 20;
        } else {
            iters = (iters as f64 * 2.5 / elapsed_s) as u64;
        }
    }
}
