# OtterLang Tutorial

Welcome to OtterLang! This tutorial will guide you through learning the language step by step, from your first program to building real applications.

## Table of Contents

### Getting Started
1. [Installation](#installation)
2. [Your First Program](#your-first-program)
3. [Understanding the Basics](#understanding-the-basics)

### Core Language Features
4. [Variables and Types](#variables-and-types)
5. [Functions](#functions)
6. [Control Flow](#control-flow)
7. [Collections](#collections)
8. [Structs and Custom Types](#structs-and-custom-types)
9. [Enums and Pattern Matching](#enums-and-pattern-matching)

### Practical Programming
10. [Error Handling](#error-handling)
11. [Modules and Code Organization](#modules-and-code-organization)
12. [Concurrency](#concurrency)

### Advanced Topics
13. [Generic Types](#generic-types)
14. [Best Practices](#best-practices)

---

## Installation

Before you begin, make sure OtterLang is installed on your system. See the [Installation Guide](INSTALLATION.md) for detailed setup instructions.

**Quick check:** Run `otter --version` in your terminal to verify installation.

---

## Your First Program

Let's start with the classic "Hello, World!" program. This will help you understand the basic structure of an OtterLang program.

Create a file called `hello.ot`:

```otter
fn main():
    println("Hello, World!")
```

Save the file and run it:

```bash
otter run hello.ot
```

**Expected output:**
```
Hello, World!
```

### What's happening here?

- `fn main():` - Every OtterLang program starts with a `main` function. This is the entry point.
- `println(...)` - This function prints text to the console and adds a newline.
- Indentation - Notice the 4 spaces before `println`. OtterLang uses indentation (like Python) to define code blocks.

> [!TIP]
> Use `print()` instead of `println()` if you don't want a newline after your output.

**Try it yourself:** Modify the program to print your name instead of "Hello, World!".

---

## Understanding the Basics

### Syntax Overview

OtterLang uses:
- **Indentation** for code blocks (4 spaces standard)
- **Colons (`:`)** to start new blocks
- **Type inference** - you often don't need to specify types
- **Python-like** syntax with static typing

### Comments

Add comments to explain your code:

```otter
# This is a single-line comment
# Multi-line comments use multiple hash lines
# Like this
```

### Running Code

You can run OtterLang code in several ways:

```bash
# Run a file directly
otter run program.ot

# Check for errors without running
otter check program.ot

# Build an executable
otter build program.ot -o myprogram
```

---

## Variables and Types

OtterLang is statically typed, but the compiler can often infer types automatically.

### Basic Types

OtterLang provides several built-in types:

```otter
fn main():
    # Integers (64-bit signed by default)
    age = 42
    count: int = 100
    
    # Floating-point numbers (64-bit)
    pi = 3.14159
    temperature: float = 98.6
    
    # Strings (UTF-8)
    name = "Otter"
    greeting: string = "Hello"
    
    # Booleans
    is_active = true
    is_complete: bool = false
    
    # The unit type (no value)
    nothing = None
```

### Type Annotations

While type inference is powerful, explicit type annotations are recommended for:
- Function parameters and return types
- Public APIs
- When you want to be explicit about your intent

```otter
fn calculate_total(items: list<int>) -> int:
    total = 0
    for item in items:
        total = total + item
    return total
```

### Variable Declaration

Use `let` to declare new variables:

```otter
fn main():
    let x = 10
    let name: string = "Alice"
    
    # Reassignment (no 'let' needed)
    x = 20
    name = "Bob"
```

> [!NOTE]
> OtterLang doesn't have a `mut` keyword. All variables are mutable by default.

### Type System Summary

| Type | Description | Example |
|------|-------------|---------|
| `int` / `i32` | 32-bit integer | `42` |
| `i64` | 64-bit integer | `1000000` |
| `float` / `f64` | 64-bit float | `3.14` |
| `bool` | Boolean | `true`, `false` |
| `string` / `str` | UTF-8 string | `"hello"` |
| `list<T>` | Dynamic array | `[1, 2, 3]` |
| `dict<K, V>` | Dictionary | `{"key": "value"}` |
| `unit` / `None` | Unit type | `None` |

---

## Functions

Functions are the building blocks of OtterLang programs. They allow you to organize code into reusable units.

### Basic Function Syntax

```otter
fn greet(name: string) -> string:
    return f"Hello, {name}!"

fn main():
    message = greet("World")
    println(message)
```

**Key points:**
- `fn` keyword starts a function definition
- Parameters are in parentheses with type annotations
- `-> type` specifies the return type (optional but recommended)
- `:` starts the function body
- `return` exits the function with a value

### Function Parameters

Functions can have multiple parameters:

```otter
fn add(x: int, y: int) -> int:
    return x + y

fn greet(name: string, prefix: string = "Hello") -> string:
    return f"{prefix}, {name}!"

fn main():
    result = add(10, 20)  # 30
    
    # Using default parameter
    msg1 = greet("Alice")  # "Hello, Alice!"
    
    # Overriding default
    msg2 = greet("Bob", "Hi")  # "Hi, Bob!"
```

> [!IMPORTANT]
> Once a parameter has a default value, all subsequent parameters must also have defaults.

### F-Strings (String Interpolation)

OtterLang supports f-strings for easy string formatting:

```otter
fn main():
    name = "Otter"
    age = 42
    message = f"My name is {name} and I'm {age} years old"
    println(message)  # "My name is Otter and I'm 42 years old"
    
    # You can use expressions
    result = f"2 + 2 = {2 + 2}"  # "2 + 2 = 4"
```

### Function Scope

Functions can call other functions:

```otter
fn double(x: int) -> int:
    return x * 2

fn quadruple(x: int) -> int:
    return double(double(x))

fn main():
    result = quadruple(5)  # 20
    println(f"Result: {result}")
```

---

## Control Flow

Control flow allows your program to make decisions and repeat actions.

### Conditional Statements

Use `if`, `elif`, and `else` for conditional execution:

```otter
fn check_age(age: int):
    if age >= 18:
        println("Adult")
    elif age >= 13:
        println("Teenager")
    else:
        println("Child")

fn main():
    check_age(20)  # "Adult"
    check_age(15)  # "Teenager"
    check_age(8)   # "Child"
```

### Pattern Matching with `match`

The `match` expression is powerful for handling multiple cases:

```otter
fn day_name(day: int) -> string:
    return match day:
        case 1:
            "Monday"
        case 2:
            "Tuesday"
        case 3:
            "Wednesday"
        case 4:
            "Thursday"
        case 5:
            "Friday"
        case 6:
            "Saturday"
        case 7:
            "Sunday"
        case _:
            "Invalid day"

fn main():
    println(day_name(1))  # "Monday"
    println(day_name(8))  # "Invalid day"
```

The `_` pattern matches anything (wildcard).

### Loops

#### While Loops

```otter
fn main():
    let count = 0
    while count < 5:
        println(f"Count: {count}")
        count = count + 1
```

#### For Loops

Iterate over collections:

```otter
fn main():
    # Iterate over a list
    numbers = [1, 2, 3, 4, 5]
    for num in numbers:
        println(f"Number: {num}")
    
    # Iterate over a range
    for i in 0..5:
        println(f"Index: {i}")
    
    # Iterate over a string
    text = "hello"
    for char in text:
        println(char)
```

> [!NOTE]
> The `start..end` syntax creates a range that's exclusive of `end` (like `[start, end)`).

### Loop Control

```otter
fn main():
    # break - exit the loop immediately
    for i in 0..10:
        if i == 5:
            break
        println(i)  # Prints 0, 1, 2, 3, 4
    
    # continue - skip to next iteration
    for i in 0..10:
        if i % 2 == 0:
            continue
        println(i)  # Prints odd numbers: 1, 3, 5, 7, 9
```

---

## Collections

OtterLang provides two main collection types: lists (arrays) and dictionaries (maps).

### Lists (Arrays)

Lists are ordered, dynamic collections:

```otter
fn main():
    # Create a list
    numbers = [1, 2, 3, 4, 5]
    
    # Access elements (0-based indexing)
    first = numbers[0]    # 1
    last = numbers[4]     # 5
    
    # Get length
    count = len(numbers)  # 5
    
    # Modify lists
    numbers += [6, 7, 8]  # Append elements
    numbers[0] = 10       # Modify element
    
    # Iterate
    for num in numbers:
        println(num)
    
    # List comprehensions (create new lists)
    squares = [x * x for x in 1..6]  # [1, 4, 9, 16, 25]
    evens = [x for x in 1..10 if x % 2 == 0]  # [2, 4, 6, 8, 10]
```

### Dictionaries (Maps)

Dictionaries store key-value pairs:

```otter
fn main():
    # Create a dictionary
    person = {
        "name": "Alice",
        "age": 30,
        "city": "New York"
    }
    
    # Access values
    name = person["name"]      # "Alice"
    age = person["age"]        # 30
    
    # Add or modify entries
    person["email"] = "alice@example.com"
    person["age"] = 31
    
    # Check if key exists
    if "name" in person:
        println("Name found!")
    
    # Iterate over keys
    for key in person:
        println(f"{key}: {person[key]}")
    
    # Dictionary comprehensions
    squares = {x: x * x for x in 1..6}
    # {1: 1, 2: 4, 3: 9, 4: 16, 5: 25}
```

> [!TIP]
> Use dictionaries when you need to look up values by a key, and lists when order matters.

---

## Structs and Custom Types

Structs allow you to create custom data types with named fields and methods.

### Defining Structs

```otter
struct Point:
    x: float
    y: float

struct Person:
    name: string
    age: int
    email: string

fn main():
    # Create struct instances
    origin = Point(x=0.0, y=0.0)
    point = Point(x=3.0, y=4.0)
    
    # Access fields
    println(f"Point: ({point.x}, {point.y})")
    
    # Create a person
    person = Person(name="Alice", age=30, email="alice@example.com")
    println(f"Hello, {person.name}!")
```

> [!IMPORTANT]
> Struct instantiation uses parentheses and equals signs: `Point(x=0.0, y=0.0)`, not curly braces.

### Struct Methods

Methods are functions defined inside structs:

```otter
use math

struct Point:
    x: float
    y: float
    
    fn distance_from_origin(self) -> float:
        return math.sqrt(self.x * self.x + self.y * self.y)
    
    fn distance_to(self, other: Point) -> float:
        dx = self.x - other.x
        dy = self.y - other.y
        return math.sqrt(dx * dx + dy * dy)

fn main():
    p1 = Point(x=3.0, y=4.0)
    p2 = Point(x=0.0, y=0.0)
    
    dist1 = p1.distance_from_origin()  # 5.0
    dist2 = p1.distance_to(p2)         # 5.0
    
    println(f"Distance: {dist1}")
```

> [!NOTE]
> The `self` parameter is automatically inserted if you omit it. The method receives the struct instance as its first parameter.

### Type Aliases

Create alternative names for types:

```otter
type UserId = int
type Email = string
type Coordinates = Point

struct Person:
    name: string
    age: int
    email: string

fn create_user(id: UserId, email: Email) -> Person:
    return Person(name="Unknown", age=0, email=email)

fn calculate_distance(a: Coordinates, b: Coordinates) -> float:
    dx = a.x - b.x
    dy = a.y - b.y
    return math.sqrt(dx * dx + dy * dy)
```

Type aliases make your code more readable and self-documenting.

---

## Enums and Pattern Matching

Enums allow you to define a type that can be one of several variants. They're perfect for representing choices or states.

### Defining Enums

```otter
enum Result<T, E>:
    Ok: (T)
    Err: (E)

enum Option<T>:
    Some: (T)
    None

enum Status:
    Pending
    Processing
    Complete
    Failed: (string)  # Variant with data
```

### Using Enums

```otter
fn divide(x: float, y: float) -> Result<float, string>:
    if y == 0.0:
        return Result.Err("Division by zero")
    return Result.Ok(x / y)

fn find_user(id: int) -> Option<Person>:
    if id == 1:
        return Option.Some(Person(name="Alice", age=30, email="alice@example.com"))
    return Option.None
```

### Pattern Matching with Enums

Pattern matching is the idiomatic way to work with enums:

```otter
use std/core

fn main():
    # Match on Result
    match divide(10.0, 2.0):
        case Result.Ok(value):
            println(f"Result: {value}")
        case Result.Err(error):
            println(f"Error: {error}")
    
    # Match on Option
    match find_user(1):
        case Option.Some(user):
            println(f"Found: {user.name}")
        case Option.None:
            println("User not found")
    
    # Match on simple enum
    status = Status.Processing
    match status:
        case Status.Pending:
            println("Waiting...")
        case Status.Processing:
            println("In progress...")
        case Status.Complete:
            println("Done!")
        case Status.Failed(reason):
            println(f"Failed: {reason}")
```

> [!TIP]
> Pattern matching is exhaustive - you must handle all possible cases. Use `_` as a catch-all if needed.

---

## Error Handling

OtterLang uses `Result<T, E>` for error handling instead of exceptions. This makes error handling explicit and type-safe.

### Using Result

```otter
use std/core

fn safe_divide(a: float, b: float) -> Result<float, string>:
    if b == 0.0:
        return Result.Err("Division by zero")
    return Result.Ok(a / b)

fn read_file(path: string) -> Result<string, string>:
    # Simulate file reading
    if path == "":
        return Result.Err("Empty path")
    return Result.Ok("file contents")

fn main():
    # Handle errors with pattern matching
    match safe_divide(10.0, 2.0):
        case Result.Ok(result):
            println(f"Result: {result}")
        case Result.Err(error):
            println(f"Error: {error}")
    
    # Chain operations
    match read_file("data.txt"):
        case Result.Ok(content):
            println(f"Read {len(content)} bytes")
        case Result.Err(error):
            println(f"Failed to read file: {error}")
```

### Option for Optional Values

Use `Option<T>` when a value might be absent (but it's not an error):

```otter
use std/core

fn find_user(id: int) -> Option<Person>:
    # Simulate database lookup
    if id == 1:
        return Option.Some(Person(name="Alice", age=30, email="alice@example.com"))
    return Option.None

fn main():
    match find_user(1):
        case Option.Some(user):
            println(f"Found user: {user.name}")
        case Option.None:
            println("User not found")
```

### When to Use Result vs Option

- **`Result<T, E>`**: Use when an operation can fail and you want to communicate why
- **`Option<T>`**: Use when a value might simply not exist (not an error condition)

---

## Modules and Code Organization

As your programs grow, organizing code into modules becomes essential.

### Creating Modules

Each `.ot` file is a module. Create a module by defining functions, structs, etc.:

```otter
# math_utils.ot
pub fn add(a: float, b: float) -> float:
    return a + b

pub fn multiply(a: float, b: float) -> float:
    return a * b

pub fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

The `pub` keyword makes items visible to other modules.

### Importing Modules

```otter
# main.ot
use math_utils

fn main():
    sum = math_utils.add(5.0, 3.0)
    product = math_utils.multiply(4.0, 2.0)
    fact = math_utils.factorial(5)
    
    println(f"Sum: {sum}, Product: {product}, Factorial: {fact}")
```

### Module Aliases

Use aliases to shorten long module names:

```otter
use math_utils as math

fn main():
    result = math.add(1.0, 2.0)
    println(f"Result: {result}")
```

### Standard Library Modules

OtterLang comes with many standard library modules:

```otter
use time
use json
use http
use io

fn main():
    # Use time functions
    now = time.now()
    
    # Parse JSON
    data = json.parse('{"name": "Otter"}')
    
    # Make HTTP requests
    response = http.get("https://example.com")
    
    # File I/O
    content = io.read_file("data.txt")
```

> [!NOTE]
> Only built-in primitives are available without imports. Most functionality requires explicit `use` statements.

---

## Concurrency

OtterLang provides built-in support for concurrent programming with tasks.

### Spawning Tasks

Use `spawn` to run functions concurrently:

```otter
use time

fn fibonacci(n: int) -> int:
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

fn compute_task(id: int) -> string:
    time.sleep(100)  # Simulate work
    result = fibonacci(20 + id)
    return f"Task {id} result: {result}"

fn main():
    # Spawn concurrent tasks
    task1 = spawn compute_task(1)
    task2 = spawn compute_task(2)
    task3 = spawn compute_task(3)
    
    # Wait for all tasks to complete
    result1 = await task1
    result2 = await task2
    result3 = await task3
    
    println("All tasks completed:")
    println(result1)
    println(result2)
    println(result3)
```

### Understanding spawn and await

- `spawn function_call(...)` - Starts a task and returns a handle
- `await handle` - Waits for the task to complete and returns its result
- Tasks run concurrently, potentially on different threads

> [!TIP]
> Use tasks for I/O-bound operations or CPU-intensive work that can run in parallel.

---

## Generic Types

While OtterLang doesn't support generic function parameters yet, you can use generic structs and enums.

### Generic Structs

```otter
struct Container<T>:
    items: list<T>
    
    fn add(self, item: T) -> unit:
        self.items += [item]
    
    fn get(self, index: int) -> Option<T>:
        if index >= 0 and index < len(self.items):
            return Option.Some(self.items[index])
        return Option.None

fn main():
    # Create containers with different types
    string_container = Container(items=[])
    string_container.add("hello")
    string_container.add("world")
    
    int_container = Container(items=[])
    int_container.add(1)
    int_container.add(2)
    
    match string_container.get(0):
        case Option.Some(value):
            println(f"First string: {value}")
        case Option.None:
            println("No value")
```

### Generic Enums

The standard library provides generic enums:

```otter
use std/core

enum Option<T>:
    Some: (T)
    None

enum Result<T, E>:
    Ok: (T)
    Err: (E)
```

These are already available in the standard library, so you typically don't need to define them yourself.

> [!NOTE]
> Generic function parameters (like `fn first<T>(...)`) are not yet supported. Use type inference or generic structs instead.

---

## Best Practices

### Code Style

1. **Use meaningful names**: `calculate_total` is better than `calc` or `ct`
2. **Add type annotations**: Especially for public APIs and function signatures
3. **Keep functions focused**: Each function should do one thing well
4. **Use comments wisely**: Explain *why*, not *what*

### Error Handling

1. **Use Result for operations that can fail**: Make errors explicit
2. **Use Option for optional values**: When absence isn't an error
3. **Handle all cases**: Pattern matching ensures you don't miss cases

### Organization

1. **Group related code**: Put related functions and types together
2. **Use modules**: Split large programs into multiple files
3. **Export public APIs**: Use `pub` for items other modules should use

### Performance Tips

1. **Use appropriate collections**: Lists for ordered data, dicts for lookups
2. **Consider concurrency**: Use `spawn` for parallelizable work
3. **Profile before optimizing**: Measure first, optimize second

---

## Next Steps

Now that you've learned the basics, here are some ways to continue:

1. **Explore Examples**: Check out the `examples/` directory for real code
2. **Read the Language Spec**: See [LANGUAGE_SPEC.md](LANGUAGE_SPEC.md) for complete details
3. **Browse the API**: Check [API_REFERENCE.md](API_REFERENCE.md) for standard library functions
4. **Build Something**: Try creating your own project!

### Recommended Learning Path

1. ✅ **Basics**: Variables, functions, control flow
2. ✅ **Data Structures**: Lists, dictionaries, structs
3. ✅ **Error Handling**: Result and Option types
4. ✅ **Organization**: Modules and code structure
5. ✅ **Concurrency**: Tasks and async programming
6. 🚀 **Advanced**: Generic types, FFI, advanced patterns

---

## Common Pitfalls

### Struct Instantiation

❌ **Wrong:**
```otter
point = Point{x: 0.0, y: 0.0}  # This is for pattern matching!
```

✅ **Correct:**
```otter
point = Point(x=0.0, y=0.0)  # Use parentheses and equals
```

### Pattern Matching vs Instantiation

- **Instantiation**: `Point(x=0.0, y=0.0)` - creates a new struct
- **Pattern Matching**: `Point{x, y}` - destructures an existing struct

### Missing Imports

❌ **Wrong:**
```otter
fn main():
    data = json.parse('{}')  # Error: json not imported
```

✅ **Correct:**
```otter
use json

fn main():
    data = json.parse('{}')
```

### Type Annotations

While type inference is helpful, explicit types make code clearer:

```otter
# Good - explicit types
fn process(items: list<int>) -> int:
    return len(items)

# Also works, but less clear
fn process(items):
    return len(items)
```

---

## Getting Help

- **Documentation**: See [LANGUAGE_SPEC.md](LANGUAGE_SPEC.md) for complete language reference
- **Examples**: Check `examples/` directory for working code
- **API Reference**: See [API_REFERENCE.md](API_REFERENCE.md) for standard library

Happy coding with OtterLang! 🦦
