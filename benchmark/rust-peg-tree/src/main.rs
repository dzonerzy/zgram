use std::env;
use std::fs;
use std::time::Instant;

/// A parse tree node: the same information zgram records per node
/// (rule, byte span, children). Built with idiomatic rust-peg actions.
pub struct Node {
    pub rule: &'static str,
    pub start: usize,
    pub end: usize,
    pub children: Vec<Node>,
}

impl Node {
    fn count(&self) -> usize {
        1 + self.children.iter().map(Node::count).sum::<usize>()
    }
}

fn node(rule: &'static str, start: usize, end: usize, children: Vec<Node>) -> Node {
    Node { rule, start, end, children }
}

// Same grammar as benchmark/rust-peg, with actions that build a tree. Nodes
// are made for the rules zgram's JSON grammar keeps (value, object, pair,
// array, string, number); the others only match, like zgram's @silent rules.
peg::parser! {
    grammar json_parser() for str {
        pub rule json() -> Node = _ v:value() _ ![_] { v }

        rule value() -> Node
            = s:position!() kids:value_kids() e:position!() { node("value", s, e, kids) }

        rule value_kids() -> Vec<Node>
            = o:object() { vec![o] }
            / a:array() { vec![a] }
            / st:string() { vec![st] }
            / n:number() { vec![n] }
            / "true" { vec![] }
            / "false" { vec![] }
            / "null" { vec![] }

        rule object() -> Node
            = s:position!() "{" _ "}" e:position!() { node("object", s, e, vec![]) }
            / s:position!() "{" _ ps:(pair() ++ ("," _)) _ "}" e:position!() { node("object", s, e, ps) }

        rule pair() -> Node
            = s:position!() _ k:string() _ ":" _ v:value() _ e:position!() { node("pair", s, e, vec![k, v]) }

        rule array() -> Node
            = s:position!() "[" _ "]" e:position!() { node("array", s, e, vec![]) }
            / s:position!() "[" _ vs:(value() ++ ("," _)) _ "]" e:position!() { node("array", s, e, vs) }

        rule string() -> Node
            = s:position!() "\"" char()* "\"" e:position!() { node("string", s, e, vec![]) }

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

        rule number() -> Node
            = s:position!() "-"? int() frac()? exp()? e:position!() { node("number", s, e, vec![]) }

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
        eprintln!("Usage: rustpeg_tree_bench <json_file>");
        std::process::exit(1);
    }

    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    // Verify parse works
    let tree = json_parser::json(&input).expect("Parse failed");
    eprintln!("Parse OK ({} nodes). Benchmarking...", tree.count());

    // Auto-calibrate: find iteration count that takes >= 2 seconds
    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            // The tree is built and dropped every iteration, as in real use
            let tree = json_parser::json(&input);
            std::hint::black_box(&tree);
        }
        let elapsed = start.elapsed();
        let elapsed_s = elapsed.as_secs_f64();

        if elapsed_s >= 2.0 {
            let per_parse_us = (elapsed_s * 1e6) / iters as f64;
            let ops_per_sec = iters as f64 / elapsed_s;

            eprintln!("\n-- rust-peg tree ({} bytes) --", input.len());
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
