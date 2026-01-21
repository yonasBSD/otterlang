use otterc_ffi::{DependencyConfig, RustStubGenerator, TypeSpec, extract_crate_spec};

#[test]
fn test_type_spec_extensions() {
    assert!(matches!(TypeSpec::List(Box::new(TypeSpec::I32)), TypeSpec::List(_)));
    assert!(matches!(TypeSpec::Map(Box::new(TypeSpec::Str), Box::new(TypeSpec::I32)), TypeSpec::Map(_, _)));
    assert!(matches!(TypeSpec::Option(Box::new(TypeSpec::Str)), TypeSpec::Option(_)));
    
    assert_eq!(TypeSpec::List(Box::new(TypeSpec::I32)).to_rust(), "*const ::std::os::raw::c_char");
    assert_eq!(TypeSpec::Map(Box::new(TypeSpec::Str), Box::new(TypeSpec::I32)).to_rust(), "*const ::std::os::raw::c_char");
    assert_eq!(TypeSpec::Option(Box::new(TypeSpec::Str)).to_rust(), "*const ::std::os::raw::c_char");
}

#[test]
fn test_async_naming() {
    let test_crate_dir = std::env::temp_dir().join("test_async_naming");
    std::fs::create_dir_all(&test_crate_dir).unwrap();

    let manifest = "[package]
name = \"test_async_naming\"
version = \"0.1.0\"
edition = \"2021\"
";

    let lib_rs = "
pub async fn async_function(x: i32) -> i32 {
    x * 2
}

pub mod module {
    pub async fn nested_async(y: f64) -> f64 {
        y * 3.0
    }
}
";

    std::fs::write(test_crate_dir.join("Cargo.toml"), manifest).unwrap();
    std::fs::create_dir_all(test_crate_dir.join("src")).unwrap();
    std::fs::write(test_crate_dir.join("src/lib.rs"), lib_rs).unwrap();

    let dep = DependencyConfig {
        name: "test_async_naming".to_string(),
        version: None,
        path: Some(test_crate_dir.clone()),
        features: vec![],
        default_features: true,
    };

    let spec = extract_crate_spec(&dep);
    if let Ok(spec) = spec {
        let generator = RustStubGenerator::new("test_async_naming".to_string(), dep);
        let functions = generator.functions_from_crate_spec(&spec);
        
        let async_func = functions.iter().find(|f| f.name.contains("async_function"));
        if let Some(func) = async_func {
            assert!(!func.name.contains("async_function_async_function"), "Should not duplicate function name");
            assert!(func.name.contains("_spawn") || func.name.contains("_await"), "Should have spawn or await suffix");
        }
    }

    let _ = std::fs::remove_dir_all(test_crate_dir);
}

#[test]
fn test_method_generation() {
    let test_crate_dir = std::env::temp_dir().join("test_method_gen");
    std::fs::create_dir_all(&test_crate_dir).unwrap();

    let manifest = "[package]
name = \"test_method_gen\"
version = \"0.1.0\"
edition = \"2021\"
";

    let lib_rs = "
pub struct Counter {
    value: i32,
}

impl Counter {
    pub fn new() -> Self {
        Counter { value: 0 }
    }
    
    pub fn increment(&mut self) -> i32 {
        self.value += 1;
        self.value
    }
    
    pub fn get(&self) -> i32 {
        self.value
    }
}
";

    std::fs::write(test_crate_dir.join("Cargo.toml"), manifest).unwrap();
    std::fs::create_dir_all(test_crate_dir.join("src")).unwrap();
    std::fs::write(test_crate_dir.join("src/lib.rs"), lib_rs).unwrap();

    let dep = DependencyConfig {
        name: "test_method_gen".to_string(),
        version: None,
        path: Some(test_crate_dir.clone()),
        features: vec![],
        default_features: true,
    };

    let spec = extract_crate_spec(&dep);
    if let Ok(spec) = spec {
        let generator = RustStubGenerator::new("test_method_gen".to_string(), dep);
        let functions = generator.functions_from_crate_spec(&spec);
        
        let has_new = functions.iter().any(|f| f.name.contains("new"));
        let has_increment = functions.iter().any(|f| f.name.contains("increment"));
        let has_get = functions.iter().any(|f| f.name.contains("get"));
        
        assert!(has_new || has_increment || has_get, "Should generate at least one method");
    }

    let _ = std::fs::remove_dir_all(test_crate_dir);
}

#[test]
fn test_field_accessor_generation() {
    let test_crate_dir = std::env::temp_dir().join("test_field_accessors");
    std::fs::create_dir_all(&test_crate_dir).unwrap();

    let manifest = "[package]
name = \"test_field_accessors\"
version = \"0.1.0\"
edition = \"2021\"
";

    let lib_rs = "
pub struct Point {
    pub x: f64,
    pub y: f64,
    z: f64,
}

pub struct Person {
    pub name: String,
    pub age: u32,
}
";

    std::fs::write(test_crate_dir.join("Cargo.toml"), manifest).unwrap();
    std::fs::create_dir_all(test_crate_dir.join("src")).unwrap();
    std::fs::write(test_crate_dir.join("src/lib.rs"), lib_rs).unwrap();

    let dep = DependencyConfig {
        name: "test_field_accessors".to_string(),
        version: None,
        path: Some(test_crate_dir.clone()),
        features: vec![],
        default_features: true,
    };

    let spec = extract_crate_spec(&dep);
    if let Ok(spec) = spec {
        let generator = RustStubGenerator::new("test_field_accessors".to_string(), dep);
        let functions = generator.functions_from_crate_spec(&spec);
        
        let has_x_field = functions.iter().any(|f| f.name.contains("x"));
        let has_y_field = functions.iter().any(|f| f.name.contains("y"));
        let has_name_field = functions.iter().any(|f| f.name.contains("name"));
        
        assert!(has_x_field || has_y_field || has_name_field, "Should generate field accessors");
    }

    let _ = std::fs::remove_dir_all(test_crate_dir);
}

#[test]
fn test_try_variant_generation() {
    let test_crate_dir = std::env::temp_dir().join("test_try_variants");
    std::fs::create_dir_all(&test_crate_dir).unwrap();

    let manifest = "[package]
name = \"test_try_variants\"
version = \"0.1.0\"
edition = \"2021\"
";

    let lib_rs = "
pub fn simple_function(x: i32) -> i32 {
    x * 2
}

pub fn result_function(x: i32) -> Result<i32, String> {
    if x > 0 {
        Ok(x * 2)
    } else {
        Err(\"negative\".to_string())
    }
}

pub fn option_function(x: i32) -> Option<i32> {
    if x > 0 {
        Some(x * 2)
    } else {
        None
    }
}
";

    std::fs::write(test_crate_dir.join("Cargo.toml"), manifest).unwrap();
    std::fs::create_dir_all(test_crate_dir.join("src")).unwrap();
    std::fs::write(test_crate_dir.join("src/lib.rs"), lib_rs).unwrap();

    let dep = DependencyConfig {
        name: "test_try_variants".to_string(),
        version: None,
        path: Some(test_crate_dir.clone()),
        features: vec![],
        default_features: true,
    };

    let spec = extract_crate_spec(&dep);
    if let Ok(spec) = spec {
        let generator = RustStubGenerator::new("test_try_variants".to_string(), dep);
        let functions = generator.functions_from_crate_spec(&spec);
        
        let function_names: Vec<&str> = functions.iter().map(|f| f.name.as_str()).collect();
        
        let has_simple_try = functions.iter().any(|f| f.name.contains("simple_function") && f.name.contains("_try"));
        let has_result_try = functions.iter().any(|f| f.name.contains("result_function") && f.name.contains("_try"));
        let has_option_try = functions.iter().any(|f| f.name.contains("option_function") && f.name.contains("_try"));
        
        assert!(has_simple_try, "Should generate _try variant for simple function. Found: {:?}", function_names);
        assert!(has_result_try, "Should generate _try variant for Result function. Found: {:?}", function_names);
        assert!(has_option_try, "Should generate _try variant for Option function. Found: {:?}", function_names);
    }

    let _ = std::fs::remove_dir_all(test_crate_dir);
}
