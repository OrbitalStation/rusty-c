fn calculate_square(n: i32) -> i32 {
	n * n
}

fn fibonacci(n: u64) -> u64 {
	if n < 2 {
		return 1;
	} else {
		return fibonacci(n - 1) + fibonacci(n - 2);
	}
}
