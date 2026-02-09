use pest::Parser;
use pest_derive::Parser;
use std::env;
use std::fs;
use std::time::Instant;

#[derive(Parser)]
#[grammar = "src/json.pest"]
struct JsonParser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: pest_bench <json_file>");
        std::process::exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    // Verify parse works
    JsonParser::parse(Rule::json, &input).expect("Parse failed");
    eprintln!("Parse OK. Benchmarking...");

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            let _ = JsonParser::parse(Rule::json, &input);
        }
        let elapsed = start.elapsed();
        let elapsed_s = elapsed.as_secs_f64();

        if elapsed_s >= 2.0 {
            let per_parse_us = (elapsed_s * 1e6) / iters as f64;
            let ops_per_sec = iters as f64 / elapsed_s;

            eprintln!("\n-- pest ({} bytes) --", input.len());
            eprintln!("  Iterations:  {}", iters);
            eprintln!("  Total:       {:.4}s", elapsed_s);
            eprintln!("  Per-parse:   {:.2}us", per_parse_us);
            eprintln!("  Ops/sec:     {:.0}", ops_per_sec);
            break;
        }

        if elapsed_s < 0.1 {
            iters *= 20;
        } else {
            iters = (iters as f64 * 2.5 / elapsed_s) as u64;
        }
    }
}
