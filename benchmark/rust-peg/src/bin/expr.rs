// rust-peg expression benchmark (validation only). Same grammar as zgram's
// expression benchmark: call is tried before ident (backtracking).
use std::env;
use std::fs;
use std::time::Instant;

peg::parser! {
    grammar expr_parser() for str {
        pub rule expr() = _ sum() _ ![_]
        rule sum() = product() (_ ['+' | '-'] _ product())*
        rule product() = power() (_ ['*' | '/' | '%'] _ power())*
        rule power() = unary() (_ "^" _ power())?
        rule unary() = "-" _ unary() / primary()
        rule primary() = number() / call() / ident() / "(" _ sum() _ ")"
        rule call() = ident() _ "(" _ args()? _ ")"
        rule args() = sum() (_ "," _ sum())*
        rule number() = ['0'..='9']+ ("." ['0'..='9']+)? (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?
        rule ident() = ['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*
        rule _() = [' ' | '\t' | '\n' | '\r']*
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: rustpeg_expr <expr_file>");
        std::process::exit(1);
    }
    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    expr_parser::expr(&input).expect("Parse failed");
    eprintln!("Parse OK. Benchmarking...");

    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            let r = expr_parser::expr(&input);
            std::hint::black_box(&r);
        }
        let elapsed_s = start.elapsed().as_secs_f64();
        if elapsed_s >= 2.0 {
            eprintln!("\n-- rust-peg ({} bytes) --", input.len());
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
