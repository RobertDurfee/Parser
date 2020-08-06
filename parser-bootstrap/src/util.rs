#[macro_export]
macro_rules! map {
    ($($x:expr => $y:expr),*) => {{
        let mut temp_map = std::collections::BTreeMap::new();
        $(temp_map.insert($x, $y);)*
        temp_map
    }}
}

