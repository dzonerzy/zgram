// rust-peg expression benchmark building the same tree as zgram (nodes for
// expr, sum, product, power, unary, call, number, ident) through actions.
use std::env;
use std::fs;
use std::time::Instant;

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

fn cons(first: Node, mut rest: Vec<Node>) -> Vec<Node> {
    rest.insert(0, first);
    rest
}

peg::parser! {
    grammar expr_parser() for str {
        pub rule expr() -> Node
            = s:position!() _ x:sum() _ e:position!() ![_] { node("expr", s, e, vec![x]) }
        rule sum() -> Node
            = s:position!() f:product() r:(_ ['+' | '-'] _ p:product() { p })* e:position!() { node("sum", s, e, cons(f, r)) }
        rule product() -> Node
            = s:position!() f:power() r:(_ ['*' | '/' | '%'] _ p:power() { p })* e:position!() { node("product", s, e, cons(f, r)) }
        rule power() -> Node
            = s:position!() u:unary() r:(_ "^" _ p:power() { p })? e:position!() { node("power", s, e, cons(u, r.into_iter().collect())) }
        rule unary() -> Node
            = s:position!() "-" _ u:unary() e:position!() { node("unary", s, e, vec![u]) }
            / s:position!() p:primary() e:position!() { node("unary", s, e, vec![p]) }
        rule primary() -> Node
            = number() / call() / ident() / "(" _ x:sum() _ ")" { x }
        rule call() -> Node
            = s:position!() i:ident() _ "(" _ a:args()? _ ")" e:position!() { node("call", s, e, cons(i, a.unwrap_or_default())) }
        rule args() -> Vec<Node>
            = f:sum() r:(_ "," _ x:sum() { x })* { cons(f, r) }
        rule number() -> Node
            = s:position!() ['0'..='9']+ ("." ['0'..='9']+)? (['e' | 'E'] ['+' | '-']? ['0'..='9']+)? e:position!() { node("number", s, e, vec![]) }
        rule ident() -> Node
            = s:position!() ['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']* e:position!() { node("ident", s, e, vec![]) }
        rule _() = [' ' | '\t' | '\n' | '\r']*
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: rustpeg_tree_expr <expr_file>");
        std::process::exit(1);
    }
    let input = fs::read_to_string(&args[1]).expect("Cannot read file");

    let tree = expr_parser::expr(&input).expect("Parse failed");
    eprintln!("Parse OK ({} nodes). Benchmarking...", tree.count());

    let mut iters: u64 = 1000;
    loop {
        let start = Instant::now();
        for _ in 0..iters {
            let r = expr_parser::expr(&input);
            std::hint::black_box(&r);
        }
        let elapsed_s = start.elapsed().as_secs_f64();
        if elapsed_s >= 2.0 {
            eprintln!("\n-- rust-peg tree ({} bytes) --", input.len());
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
