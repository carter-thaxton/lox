
use crate::runtime::{Environment, Value};
use std::time::Instant;

fn clock(start_time: &Instant) -> f64 {
    Instant::now().duration_since(*start_time).as_secs_f64()
}

pub fn global_names() -> Vec<&'static str> {
  vec!["clock"]
}

pub fn define_globals(env: &mut Environment) {
    // built-in globals
    let start_time = Instant::now();
    let clock_fn = Value::builtin_fn(
        "clock",
        0,
        Box::new(move |_| Ok(Value::Number(clock(&start_time)))),
    );

    env.define("clock", clock_fn);
}

