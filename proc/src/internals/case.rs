pub fn screaming_snake_case(camel_case: &str) -> String {
    let mut snake = String::new();
    for (i, ch) in camel_case.char_indices() {
        if i > 0 && ch.is_uppercase() {
            snake.push('_');
        }
        snake.push(ch.to_ascii_uppercase());
    }
    snake
}
