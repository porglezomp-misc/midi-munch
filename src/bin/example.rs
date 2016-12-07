extern crate midi;

use midi::parse_midi;
use std::io::{self, Read};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).unwrap();
    println!("{:?}", parse_midi(&input));
}
