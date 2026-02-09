use std::env;
use std::fs;
use std::time::Instant;

peg::parser! {
    grammar json_parser() for str {
        pub rule json() = _ value() _ ![_]

        rule value()
            = object()
            / array()
            / string()
            / number()
            / "true"
            / "false"
            / "null"

        rule object()
            = "{" _ "}"
            / "{" _ pair() ("," _ pair())* _ "}"

        rule pair()
            = _ string() _ ":" _ value() _

        rule array()
            = "[" _ "]"
            / "[" _ value() ("," _ value())* _ "]"

        rule string()
            = "\"" char()* "\""

        rule char()
            = escape()
            / plain()

        rule escape()
            = "\\" ['"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't']
            / "\\u" hex() hex() hex() hex()

        rule hex()
            = ['0'..='9' | 'a'..='f' | 'A'..='F']

        rule plain()
            = [^ '"' | '\\']

        rule number()
            = "-"? int() frac()? exp()?

        rule int()
            = "0"
            / ['1'..='9'] ['0'..='9']*

        rule frac()
            = "." ['0'..='9']+

        rule exp()
            = ['e' | 'E'] ['+' | '-']? ['0'..='9']+

        rule _()
            = [' ' | '\t' | '\n' | '\r']*
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: rustpeg_bench <json_file>");
        std::process::exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    // Verify parse works
    json_parser::json(&input).expect("Parse failed");
    eprintln!("Parse OK. Benchmarking...");

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            let _ = json_parser::json(&input);
        }
        let elapsed = start.elapsed();
        let elapsed_s = elapsed.as_secs_f64();

        if elapsed_s >= 2.0 {
            let per_parse_us = (elapsed_s * 1e6) / iters as f64;
            let ops_per_sec = iters as f64 / elapsed_s;

            eprintln!("\n-- rust-peg ({} bytes) --", input.len());
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
