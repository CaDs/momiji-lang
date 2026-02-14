# Momiji Programming Language Specification

**Version 0.1 Draft — January 2026**

> *Momiji (紅葉) — The Japanese maple leaf, symbol of change and natural beauty.*

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Design Principles](#2-design-principles)
3. [Lexical Structure](#3-lexical-structure)
4. [Types](#4-types)
5. [Variables and Constants](#5-variables-and-constants)
6. [Operators](#6-operators)
7. [Control Flow](#7-control-flow)
8. [Functions and Methods](#8-functions-and-methods)
9. [Structs and Classes](#9-structs-and-classes)
10. [Interfaces](#10-interfaces)
11. [Generics](#11-generics)
12. [Enums and Pattern Matching](#12-enums-and-pattern-matching)
13. [Error Handling](#13-error-handling)
14. [Modules and Visibility](#14-modules-and-visibility)
15. [Concurrency](#15-concurrency)
16. [Memory Management](#16-memory-management)
17. [Standard Library Overview](#17-standard-library-overview)
18. [Appendix: Grammar Summary](#18-appendix-grammar-summary)

---

## 1. Introduction

Momiji is a statically-typed, compiled programming language designed for building reliable, efficient software with an emphasis on developer experience. It combines:

- **Ruby-inspired syntax** — expressive, readable, joyful to write
- **Go-like compilation speed** — fast incremental builds enabling rapid iteration
- **Memory safety without garbage collection pauses** — automatic reference counting with optional ownership control
- **First-class concurrency** — lightweight fibers and channels built into the language

Momiji targets application development, web services, CLI tools, and game development where both developer productivity and runtime performance matter.

### 1.0 Implementation Status (v0.1.x MVP Subset)

The full language described in this document is the long-term direction.  
Current compiler releases in the `0.1.x` line intentionally implement a strict MVP subset focused on correctness and diagnostics.

**Implemented in v0.1.x:**

- Top-level `def ... end` function declarations
- Parameters with explicit types
- Primitive literals: `Int`, `Float`, `Bool`, `String`, `nil`
- Local variables via assignment (`x = ...`) and explicit `let` (`let mut` accepted as compatibility alias)
- Expressions:
  - arithmetic, comparison, logical, bitwise operators
  - function calls with parentheses: `puts("hello")`
  - array literals and indexing
- Control flow: `if/else`, `while`, `for`
- Builtins: `puts`, `print`
- Commands:
  - `momiji check` (`--changed`, `--timings`) on file or directory targets
  - `momiji run` (`--timings`)
  - `momiji build` (`--timings`)

`momiji check --changed` uses a local cache keyed by file metadata and API fingerprint.
Body-only edits re-check only changed files; API changes trigger downstream re-checks.

**Not yet implemented in v0.1.x (planned):**

- `module`, `import`, visibility (`pub`)
- `struct`, `class`, `property`
- `match` / `when`
- interfaces, generics, enums, advanced error handling model
- concurrency (`spawn`, `select`, channels)
- ARC/ownership model and full memory-management semantics

**MVP syntax policy for now:**

- Statement boundaries must be explicit (newline / `;` / block terminator).
- Function calls in expression statements require parentheses.
- Ruby-style no-paren calls are intentionally deferred until parser redesign.

### 1.1 Hello World

```momiji
def main
  puts "Hello, Momiji!"
end
```

### 1.2 A Taste of Momiji

```momiji
module Game::Combat

import Std::Math
import Game::Entities.(Player, Enemy)

pub struct AttackResult
  property damage : Int
  property critical : Bool
  property killed : Bool
end

pub def attack(attacker : mut Player, defender : mut Enemy) : AttackResult
  damage = calculate_damage(attacker.stats, defender.stats)
  critical = Random.chance(attacker.crit_rate)
  
  damage *= 2 if critical
  defender.health -= damage
  
  AttackResult.new(
    damage: damage,
    critical: critical,
    killed: !defender.alive?
  )
end

def calculate_damage(atk_stats, def_stats)
  base = atk_stats.power - def_stats.armor
  Math.max(1, base)
end
```

---

## 2. Design Principles

Momiji is guided by ten core principles:

| Principle | Japanese | Core Idea |
|-----------|----------|-----------|
| **Naturalness** | 自然 (Shizen) | Code reads like thought |
| **Harmony** | 調和 (Chōwa) | Consistent patterns everywhere |
| **Clarity** | 明確 (Meikaku) | Explicit at boundaries, inferred within |
| **Safety** | 安全 (Anzen) | Safe by default, unsafe by ceremony |
| **Concurrency** | 並行 (Heikō) | Parallelism feels natural |
| **Growth** | 成長 (Seichō) | Scales from script to system |
| **Tooling** | 道具 (Dōgu) | First-class IDE and compiler support |
| **Coexistence** | 共存 (Kyōzon) | Seamless interop with C, system APIs |
| **Lightweight** | 軽量 (Keiyō) | Minimal runtime, deploy anywhere |
| **Beauty** | 美 (Bi) | Aesthetics matter |

### 2.1 Compilation Model

Momiji is designed for fast incremental compilation:

- **Module-level isolation** — Each module is an independent compilation unit
- **Explicit public interfaces** — Public functions require type signatures
- **Local type inference** — Types are inferred within functions, not globally
- **Parallel compilation** — Modules compile in parallel after dependency resolution

Target compilation times:
- Cold build (50k LOC): < 30 seconds
- Incremental build (one file changed): < 2 seconds
- Type checking only (for IDE): < 500ms

---

## 3. Lexical Structure

### 3.1 Character Set

Momiji source files are UTF-8 encoded. Identifiers may contain Unicode letters and digits.

### 3.2 Comments

```momiji
# Single-line comment

#[
  Multi-line comment.
  Can span multiple lines.
]#

## Documentation comment (attached to following declaration)
## Supports Markdown formatting.
pub def important_function
  # ...
end
```

### 3.3 Identifiers

```momiji
# Regular identifiers: start with letter or underscore
name
_private
player1
日本語  # Unicode allowed

# Constants: SCREAMING_SNAKE_CASE
MAX_PLAYERS
DEFAULT_TIMEOUT

# Types: PascalCase
Player
GameEngine
HttpClient
```

### 3.4 Keywords

```momiji
# Declarations
def        class      struct     enum       interface
module     import     pub        property   alias

# Control flow
if         elsif      else       unless     case
match      when       while      until      for
in         loop       break      next       return

# Values
true       false      nil        self

# Modifiers
mut        own        ref        static

# Other
do         end        and        or         not
spawn      select     channel    yield      defer
```

### 3.5 Literals

```momiji
# Integers
42                    # Int (Int64)
42_i32                # Int32
42_u8                 # UInt8
0xFF                  # Hexadecimal
0b1010                # Binary
0o755                 # Octal
1_000_000             # Underscores for readability

# Floats
3.14                  # Float (Float64)
3.14_f32              # Float32
1.0e10                # Scientific notation
1e-5                  # Also valid

# Strings
"Hello, world!"       # Regular string
"Tab:\tNewline:\n"    # Escape sequences
"Value: #{expr}"      # String interpolation
"Quote: \""           # Escaped quote

# Raw strings (no escape processing)
%q(raw string with "quotes")
%q[can use different delimiters]

# Heredocs
<<-TEXT
  Multi-line string.
  Leading whitespace is preserved.
TEXT

<<~TRIMMED
  Multi-line string.
  Leading whitespace is trimmed to minimum indent.
TRIMMED

# Characters
'a'                   # Char
'\n'                  # Escape sequence
'あ'                  # Unicode character

# Symbols
:active
:north
:http_ok

# Arrays
[1, 2, 3]             # Array(Int)
[] of String          # Empty Array(String)
Array(Int).new        # Also empty Array(Int)

# Hashes/Maps
{"a" => 1, "b" => 2}  # Map(String, Int)
{} of String => Int   # Empty Map
{name: "Alice", age: 30}  # Shorthand when keys are symbols

# Tuples
{1, "hello", true}    # Tuple(Int, String, Bool)

# Ranges
1..10                 # Inclusive (1 to 10)
1...10                # Exclusive (1 to 9)
..5                   # Begin-less (up to 5)
3..                   # End-less (3 onwards)

# Regex
/pattern/             # Regex
/pattern/i            # Case-insensitive
```

---

## 4. Types

### 4.1 Primitive Types

| Type | Size | Description |
|------|------|-------------|
| `Int8` | 8-bit | Signed integer |
| `Int16` | 16-bit | Signed integer |
| `Int32` | 32-bit | Signed integer |
| `Int64` | 64-bit | Signed integer |
| `Int` | 64-bit | Alias for `Int64` (default) |
| `UInt8` | 8-bit | Unsigned integer |
| `UInt16` | 16-bit | Unsigned integer |
| `UInt32` | 32-bit | Unsigned integer |
| `UInt64` | 64-bit | Unsigned integer |
| `Float32` | 32-bit | IEEE 754 single precision |
| `Float64` | 64-bit | IEEE 754 double precision |
| `Float` | 64-bit | Alias for `Float64` (default) |
| `Bool` | 1 byte | `true` or `false` |
| `Char` | 4 bytes | Unicode scalar value |
| `String` | varies | Immutable UTF-8 string |
| `Nil` | 0 bytes | Absence of value |

### 4.2 Compound Types

```momiji
# Arrays — homogeneous, dynamic size
Array(Int)
Array(String)

# Maps — key-value pairs
Map(String, Int)
Map(Symbol, Player)

# Sets — unique values
Set(Int)
Set(String)

# Tuples — fixed-size, heterogeneous
Tuple(Int, String, Bool)
{Int, String, Bool}    # Shorthand

# Nullable types — union with Nil
String?                # String | Nil
Int?                   # Int | Nil
Player?                # Player | Nil
```

### 4.3 Type Aliases

```momiji
alias UserId = Int64
alias Handler = (Event) -> Nil
alias StringMap(V) = Map(String, V)

# Usage
user_id : UserId = 12345
handlers : Array(Handler) = []
config : StringMap(String) = {}
```

### 4.4 Type Inference Rules

Momiji infers types locally within functions. The rules are:

**Inferred (no annotation needed):**
```momiji
# From literals
name = "Momiji"           # String
count = 42                # Int
ratio = 3.14              # Float
active = true             # Bool

# From constructors
player = Player.new("Hero")  # Player
point = Point.new(1, 2)      # Point

# From function returns (if function has declared return type)
user = find_user(id)         # Whatever find_user returns

# From collections with contents
numbers = [1, 2, 3]          # Array(Int)
scores = {"a" => 1}          # Map(String, Int)

# From expressions
total = price * quantity     # Type of multiplication result
```

**Annotation required:**
```momiji
# Empty collections
items : Array(String) = []
cache : Map(Int, Player) = {}

# Nil as initial value
user : User? = nil

# When you want a supertype
animal : Animal = Dog.new

# Non-default numeric types
big : Int64 = 0
precise : Float32 = 0.0

# Public function signatures (always required)
pub def process(data : Input) : Output
```

---

## 5. Variables and Constants

### 5.1 Variable Declaration

```momiji
# Inferred type
name = "Alice"
count = 0

# Explicit type
name : String = "Alice"
count : Int = 0

# Multiple assignment
x, y, z = 1, 2, 3
a, b = b, a  # Swap

# Destructuring
{x, y} = get_point()
[first, second, *rest] = items
{name:, age:} = person  # Shorthand for {name: name, age: age}
```

### 5.2 Mutability

All variables are mutable by default within their scope:

```momiji
count = 0
count = count + 1  # OK
count += 1         # Also OK
```

### 5.3 Constants

```momiji
# Module-level constants (computed at compile time)
MAX_PLAYERS = 100
DEFAULT_NAME = "Player"
PI = 3.14159265359

# Type annotation when needed
TIMEOUT : Duration = 30.seconds
BUFFER_SIZE : UInt32 = 1024

# Constants are immutable
MAX_PLAYERS = 200  # ERROR: cannot reassign constant
```

### 5.4 Instance Variables

```momiji
class Player
  # Instance variables declared with property
  property name : String
  property health : Int
  property position : Vec3
  
  def initialize(@name, @health = 100)
    @position = Vec3.zero
  end
  
  def wounded?
    @health < @max_health  # Access with @
  end
end
```

---

## 6. Operators

### 6.1 Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `//` | Integer division | `a // b` |
| `%` | Modulo | `a % b` |
| `**` | Exponentiation | `a ** b` |
| `-` | Unary negation | `-a` |

### 6.2 Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `a == b` |
| `!=` | Not equal | `a != b` |
| `<` | Less than | `a < b` |
| `>` | Greater than | `a > b` |
| `<=` | Less or equal | `a <= b` |
| `>=` | Greater or equal | `a >= b` |
| `<=>` | Spaceship (comparison) | `a <=> b` |

### 6.3 Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `and` / `&&` | Logical AND | `a and b` |
| `or` / `\|\|` | Logical OR | `a or b` |
| `not` / `!` | Logical NOT | `not a` |

### 6.4 Bitwise Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `a & b` |
| `\|` | Bitwise OR | `a \| b` |
| `^` | Bitwise XOR | `a ^ b` |
| `~` | Bitwise NOT | `~a` |
| `<<` | Left shift | `a << n` |
| `>>` | Right shift | `a >> n` |

### 6.5 Assignment Operators

```momiji
a = b      # Assignment
a += b     # a = a + b
a -= b     # a = a - b
a *= b     # a = a * b
a /= b     # a = a / b
a //= b    # a = a // b
a %= b     # a = a % b
a **= b    # a = a ** b
a &= b     # a = a & b
a |= b     # a = a | b
a ^= b     # a = a ^ b
a <<= b    # a = a << b
a >>= b    # a = a >> b
a &&= b    # a = a && b (short-circuit)
a ||= b    # a = a || b (short-circuit)
```

### 6.6 Other Operators

```momiji
# Range
1..10      # Inclusive range
1...10     # Exclusive range

# Nil coalescing
value = maybe_nil || default
value = maybe_nil.or(default)

# Safe navigation
user?.profile?.name  # Returns nil if any part is nil

# Not-nil assertion
user!.name  # Panic if user is nil

# Type check
value is String
value is not Int

# Type cast
value as String    # Panic if not String
value as? String   # Returns nil if not String
```

### 6.7 Operator Precedence

From highest to lowest:

1. `!` `~` `+` `-` (unary)
2. `**`
3. `*` `/` `//` `%`
4. `+` `-`
5. `<<` `>>`
6. `&`
7. `|` `^`
8. `<` `<=` `>` `>=`
9. `==` `!=` `<=>` `is`
10. `&&` `and`
11. `||` `or`
12. `..` `...`
13. `?:` (ternary)
14. `=` `+=` etc.
15. `not`

---

## 7. Control Flow

### 7.1 Conditionals

```momiji
# If expression
if condition
  do_something
elsif other_condition
  do_other
else
  do_default
end

# If as expression (returns value)
result = if score > 90
  :excellent
elsif score > 70
  :good
else
  :needs_improvement
end

# Single-line if
do_thing if condition

# Unless (negated if)
unless condition
  handle_missing
end

do_thing unless done?

# Ternary
result = condition ? value_if_true : value_if_false

# Case expression (equality matching)
case value
when 1
  "one"
when 2, 3
  "two or three"
when 4..10
  "four to ten"
else
  "other"
end

# Case with no argument (like if-elsif chain)
case
when score > 90
  :excellent
when score > 70
  :good
else
  :needs_improvement
end
```

### 7.2 Pattern Matching

```momiji
# Match expression (exhaustive pattern matching)
match value
  0 => "zero"
  1 => "one"
  n if n < 0 => "negative"
  n if n > 100 => "large"
  _ => "other"
end

# Destructuring patterns
match point
  {0, 0} => "origin"
  {x, 0} => "on x-axis at #{x}"
  {0, y} => "on y-axis at #{y}"
  {x, y} => "at (#{x}, #{y})"
end

# Enum patterns
match result
  Ok(value) => process(value)
  Err(:not_found) => show_404
  Err(:unauthorized) => redirect_login
  Err(e) => log_error(e)
end

# With guards
match player
  {health: 0} => handle_death
  {health: h, armor: a} if h < 20 and a == 0 => critical_warning
  {health: h} if h < 50 => low_health_warning
  _ => continue
end
```

### 7.3 Loops

```momiji
# While loop
while condition
  do_something
end

# Until loop (negated while)
until done?
  process_next
end

# Infinite loop
loop do
  break if should_stop?
  tick
end

# For loop (iteration)
for item in collection
  process(item)
end

for i in 0...10
  puts i
end

for key, value in map
  puts "#{key}: #{value}"
end

# Times
5.times do |i|
  puts "Iteration #{i}"
end

# Each (method form)
collection.each do |item|
  process(item)
end

# Loop control
for item in items
  next if item.skip?       # Skip to next iteration
  break if item.done?      # Exit loop
  next item.alternate if item.special?  # Skip with value (for iterators)
end
```

### 7.4 Guard Clauses

```momiji
# Early return with if
def process(user : User?)
  return if user.nil?
  return unless user.active?
  
  # Main logic here
  do_processing(user)
end

# Guard with pattern matching
def process(user : User?)
  guard Some(u) = user else
    return Err(:no_user)
  end
  
  guard u.active? else
    return Err(:inactive)
  end
  
  # u is now available and known to be active
  Ok(do_processing(u))
end

# If-let style (Crystal's if with narrowing)
if user = find_user(id)
  # user is non-nil here
  process(user)
end

if Some(value) = optional_result
  # value is unwrapped here
  use(value)
end
```

---

## 8. Functions and Methods

### 8.1 Function Definition

```momiji
# Basic function
def greet(name : String) : String
  "Hello, #{name}!"
end

# With default parameters
def greet(name : String, greeting : String = "Hello") : String
  "#{greeting}, #{name}!"
end

# Named parameters at call site
greet("Alice", greeting: "Hi")

# Variadic parameters
def sum(*numbers : Int) : Int
  numbers.reduce(0) { |acc, n| acc + n }
end

sum(1, 2, 3, 4, 5)

# Keyword arguments (required named parameters)
def create_user(*, name : String, email : String, age : Int) : User
  User.new(name, email, age)
end

create_user(name: "Alice", email: "alice@example.com", age: 30)

# Mixed positional and keyword
def connect(host : String, *, port : Int = 80, timeout : Duration = 30.seconds)
  # ...
end

connect("localhost", port: 8080)
```

### 8.2 Return Values

```momiji
# Explicit return
def max(a : Int, b : Int) : Int
  return a if a > b
  return b
end

# Implicit return (last expression)
def max(a : Int, b : Int) : Int
  if a > b
    a
  else
    b
  end
end

# Multiple return values (via tuple)
def divide(a : Int, b : Int) : {Int, Int}
  {a // b, a % b}
end

quotient, remainder = divide(10, 3)

# No return value
def log(message : String) : Nil
  puts "[LOG] #{message}"
end

# Nil can be omitted
def log(message : String)
  puts "[LOG] #{message}"
end
```

### 8.3 Blocks and Closures

```momiji
# Block syntax (do...end for multi-line, braces for single-line)
items.each do |item|
  process(item)
  log(item)
end

items.each { |item| process(item) }

# Block shorthand
items.map { |x| x.name }
items.map { .name }          # Equivalent shorthand
items.map(&.name)            # Crystal-style shorthand

items.select { |x| x.active? }
items.select { .active? }
items.select(&.active?)

# Methods accepting blocks
def with_timing(&block)
  start = Time.now
  result = block.call
  elapsed = Time.now - start
  puts "Took #{elapsed}"
  result
end

with_timing do
  expensive_operation
end

# Typed blocks
def transform(items : Array(T), &block : T -> U) : Array(U) forall T, U
  result = [] of U
  items.each { |item| result << block.call(item) }
  result
end

# Closures (capture outer variables)
multiplier = 2
doubled = numbers.map { |n| n * multiplier }

# Explicit capture
counter = 0
increment = -> { counter += 1 }
increment.call  # counter is now 1

# Lambda syntax
add = ->(a : Int, b : Int) { a + b }
add.call(1, 2)  # => 3

# Proc type
handler : Proc(Int, Int, Int) = ->(a, b) { a + b }
```

### 8.4 Method Visibility

```momiji
class Calculator
  # Public by default within class
  def add(a : Int, b : Int) : Int
    a + b
  end
  
  # Explicitly private
  private def validate(n : Int)
    raise "Invalid" if n < 0
  end
  
  # Protected (accessible in subclasses)
  protected def internal_state
    @state
  end
end

module Utils
  # Private to module
  def helper
    # ...
  end
  
  # Public (accessible from outside module)
  pub def public_api(input : String) : String
    helper
    process(input)
  end
end
```

### 8.5 Operator Overloading

```momiji
struct Vec3
  property x, y, z : Float
  
  def +(other : Vec3) : Vec3
    Vec3.new(x + other.x, y + other.y, z + other.z)
  end
  
  def -(other : Vec3) : Vec3
    Vec3.new(x - other.x, y - other.y, z - other.z)
  end
  
  def *(scalar : Float) : Vec3
    Vec3.new(x * scalar, y * scalar, z * scalar)
  end
  
  def ==(other : Vec3) : Bool
    x == other.x and y == other.y and z == other.z
  end
  
  def [](index : Int) : Float
    case index
    when 0 then x
    when 1 then y
    when 2 then z
    else raise IndexError.new
    end
  end
  
  def []=(index : Int, value : Float)
    case index
    when 0 then @x = value
    when 1 then @y = value
    when 2 then @z = value
    else raise IndexError.new
    end
  end
end
```

---

## 9. Structs and Classes

### 9.1 Structs (Value Types)

Structs are value types, typically stack-allocated and copied by value.

```momiji
struct Point
  property x : Float
  property y : Float
  
  def initialize(@x, @y)
  end
  
  def distance_to(other : Point) : Float
    dx = x - other.x
    dy = y - other.y
    Math.sqrt(dx * dx + dy * dy)
  end
  
  def self.origin : Point
    Point.new(0.0, 0.0)
  end
end

# Usage
p1 = Point.new(0.0, 0.0)
p2 = Point.new(3.0, 4.0)
p1.distance_to(p2)  # => 5.0

# Structs are copied
p3 = p1
p3.x = 10.0  # p1.x is still 0.0
```

### 9.2 Classes (Reference Types)

Classes are reference types, heap-allocated and reference-counted.

```momiji
class Player
  property id : PlayerId
  property name : String
  property health : Int
  property max_health : Int
  property position : Vec3
  property inventory : Inventory
  
  def initialize(@name, @max_health = 100)
    @id = PlayerId.generate
    @health = @max_health
    @position = Vec3.zero
    @inventory = Inventory.new
  end
  
  def alive? : Bool
    @health > 0
  end
  
  def heal(amount : Int) : Int
    old = @health
    @health = Math.min(@health + amount, @max_health)
    @health - old  # Return actual healing done
  end
  
  def take_damage(amount : Int)
    @health = Math.max(0, @health - amount)
  end
end

# Usage
player = Player.new("Hero")
other = player  # Both reference same object
other.take_damage(10)
player.health  # => 90 (same object)
```

### 9.3 Property Declarations

```momiji
class Example
  # Read-write property
  property name : String
  
  # Read-only property (getter only)
  getter id : Int
  
  # Write-only property (setter only)  
  setter secret : String
  
  # Property with default value
  property count : Int = 0
  
  # Nilable property
  property parent : Node?
  
  # Lazy property (computed on first access)
  lazy property expensive : Data do
    compute_expensive_data
  end
end
```

### 9.4 Initialization

```momiji
class Connection
  property host : String
  property port : Int
  property timeout : Duration
  property connected : Bool
  
  # Primary initializer
  def initialize(@host, @port = 80, @timeout = 30.seconds)
    @connected = false
  end
  
  # Named initializer
  def self.local(port : Int) : Connection
    Connection.new("localhost", port)
  end
  
  # From configuration
  def self.from_config(config : Config) : Connection
    Connection.new(
      config.host,
      port: config.port,
      timeout: config.timeout
    )
  end
end

# Usage
conn1 = Connection.new("example.com")
conn2 = Connection.new("example.com", port: 443)
conn3 = Connection.local(8080)
conn4 = Connection.from_config(my_config)
```

### 9.5 Inheritance

```momiji
class Animal
  property name : String
  
  def initialize(@name)
  end
  
  def speak : String
    "..."
  end
end

class Dog < Animal
  property breed : String
  
  def initialize(name : String, @breed)
    super(name)
  end
  
  def speak : String
    "Woof!"
  end
end

class Cat < Animal
  def speak : String
    "Meow!"
  end
end

# Usage
animals = [Dog.new("Rex", "German Shepherd"), Cat.new("Whiskers")]
animals.each { |a| puts a.speak }
```

### 9.6 Abstract Classes

```momiji
abstract class Shape
  abstract def area : Float
  abstract def perimeter : Float
  
  def describe : String
    "A shape with area #{area} and perimeter #{perimeter}"
  end
end

class Circle < Shape
  property radius : Float
  
  def initialize(@radius)
  end
  
  def area : Float
    Math::PI * radius * radius
  end
  
  def perimeter : Float
    2 * Math::PI * radius
  end
end

class Rectangle < Shape
  property width : Float
  property height : Float
  
  def initialize(@width, @height)
  end
  
  def area : Float
    width * height
  end
  
  def perimeter : Float
    2 * (width + height)
  end
end
```

---

## 10. Interfaces

Interfaces define contracts that types can implement.

### 10.1 Interface Definition

```momiji
interface Drawable
  def draw(canvas : Canvas) : Nil
end

interface Comparable(T)
  def compare(other : T) : Ordering
end

interface Hashable
  def hash_code : UInt64
end

interface Stringable
  def to_string : String
end

# Interfaces can have default implementations
interface Printable
  def to_string : String
  
  def print : Nil
    puts to_string
  end
end
```

### 10.2 Implementing Interfaces

```momiji
struct Point
  property x, y : Float
  
  impl Drawable
    def draw(canvas : Canvas) : Nil
      canvas.draw_point(x, y)
    end
  end
  
  impl Comparable(Point)
    def compare(other : Point) : Ordering
      self_dist = Math.sqrt(x*x + y*y)
      other_dist = Math.sqrt(other.x*other.x + other.y*other.y)
      self_dist <=> other_dist
    end
  end
  
  impl Stringable
    def to_string : String
      "(#{x}, #{y})"
    end
  end
end
```

### 10.3 Interface Composition

```momiji
# Combine multiple interfaces
interface Entity : Drawable & Updatable & Serializable
end

# Or use & in constraints
def process(T : Drawable & Serializable)(item : T)
  item.draw(canvas)
  save(item.serialize)
end
```

### 10.4 Built-in Interfaces

```momiji
interface Equatable
  def ==(other : Self) : Bool
end

interface Orderable : Equatable
  def <=>(other : Self) : Ordering
  
  # Default implementations
  def <(other : Self) : Bool
    (self <=> other) == Ordering::Less
  end
  
  def >(other : Self) : Bool
    (self <=> other) == Ordering::Greater
  end
  
  def <=(other : Self) : Bool
    (self <=> other) != Ordering::Greater
  end
  
  def >=(other : Self) : Bool
    (self <=> other) != Ordering::Less
  end
end

interface Iterable(T)
  def each(&block : T -> Nil) : Nil
end

interface Iterator(T)
  def next : T?
end

interface Numeric
  def +(other : Self) : Self
  def -(other : Self) : Self
  def *(other : Self) : Self
  def /(other : Self) : Self
  def zero : Self
end
```

---

## 11. Generics

### 11.1 Generic Types

```momiji
# Generic struct
struct Pair(A, B)
  property first : A
  property second : B
  
  def initialize(@first, @second)
  end
  
  def swap : Pair(B, A)
    Pair.new(second, first)
  end
end

# Usage
point = Pair(Int, Int).new(10, 20)
entry = Pair(String, Float).new("score", 95.5)

# Generic class
class Stack(T)
  property items : Array(T)
  
  def initialize
    @items = [] of T
  end
  
  def push(item : T)
    @items.push(item)
  end
  
  def pop : T?
    @items.pop?
  end
  
  def peek : T?
    @items.last?
  end
  
  def empty? : Bool
    @items.empty?
  end
  
  def size : Int
    @items.size
  end
end

# Usage
stack = Stack(Int).new
stack.push(1)
stack.push(2)
stack.pop  # => 2
```

### 11.2 Generic Functions

```momiji
# Type parameter after function name
def identity(T)(value : T) : T
  value
end

# Type inferred from arguments
identity(42)        # T = Int
identity("hello")   # T = String

# Multiple type parameters
def swap(A, B)(pair : Pair(A, B)) : Pair(B, A)
  Pair.new(pair.second, pair.first)
end

# With explicit type arguments
result = identity(Int)(42)
```

### 11.3 Constrained Generics

```momiji
# Single constraint
def max(T : Orderable)(a : T, b : T) : T
  if a > b then a else b end
end

# Multiple constraints
def process(T : Stringable & Hashable)(value : T) : String
  "#{value.to_string} (hash: #{value.hash_code})"
end

# Constraint on generic type
def sum(T : Numeric)(items : Array(T)) : T
  items.reduce(T.zero) { |acc, x| acc + x }
end

# Usage
max(10, 20)           # Works: Int is Orderable
max("a", "b")         # Works: String is Orderable
max(player1, player2) # Error unless Player implements Orderable
```

### 11.4 Where Clauses

```momiji
# Complex constraints with where
def merge(K, V)(a : Map(K, V), b : Map(K, V)) : Map(K, V) where K : Hashable
  result = Map(K, V).new
  a.each { |k, v| result[k] = v }
  b.each { |k, v| result[k] = v }
  result
end

# Multiple where clauses
def compare_and_print(T)(a : T, b : T) : Ordering where T : Orderable, T : Stringable
  puts "Comparing #{a.to_string} and #{b.to_string}"
  a <=> b
end
```

---

## 12. Enums and Pattern Matching

### 12.1 Simple Enums

```momiji
enum Direction
  North
  South
  East
  West
end

direction = Direction::North

# Enums are comparable
direction == Direction::North  # true

# Convert to/from integers
Direction::South.value  # => 1
Direction.from_value(2)  # => Direction::East

# Iteration
Direction.each { |d| puts d }
```

### 12.2 Enums with Values

```momiji
enum HttpStatus
  Ok = 200
  Created = 201
  BadRequest = 400
  NotFound = 404
  ServerError = 500
end

status = HttpStatus::NotFound
status.value  # => 404

# Methods on enums
enum HttpStatus
  # ...
  
  def success? : Bool
    value >= 200 and value < 300
  end
  
  def error? : Bool
    value >= 400
  end
end
```

### 12.3 Algebraic Data Types

```momiji
enum Option(T)
  Some(T)
  None
  
  def some? : Bool
    match self
      Some(_) => true
      None => false
    end
  end
  
  def none? : Bool
    not some?
  end
  
  def unwrap : T
    match self
      Some(v) => v
      None => panic("unwrap on None")
    end
  end
  
  def unwrap_or(default : T) : T
    match self
      Some(v) => v
      None => default
    end
  end
  
  def map(U)(&block : T -> U) : Option(U)
    match self
      Some(v) => Some(block.call(v))
      None => None
    end
  end
end

enum Result(T, E)
  Ok(T)
  Err(E)
  
  def ok? : Bool
    match self
      Ok(_) => true
      Err(_) => false
    end
  end
  
  def unwrap : T
    match self
      Ok(v) => v
      Err(e) => panic("unwrap on Err: #{e}")
    end
  end
  
  def map(U)(&block : T -> U) : Result(U, E)
    match self
      Ok(v) => Ok(block.call(v))
      Err(e) => Err(e)
    end
  end
  
  def and_then(U)(&block : T -> Result(U, E)) : Result(U, E)
    match self
      Ok(v) => block.call(v)
      Err(e) => Err(e)
    end
  end
end
```

### 12.4 Pattern Matching Details

```momiji
# Literal patterns
match value
  0 => "zero"
  1 => "one"
  _ => "other"
end

# Variable binding
match value
  n => "got #{n}"
end

# Tuple patterns
match pair
  {0, 0} => "origin"
  {x, 0} => "on x-axis"
  {0, y} => "on y-axis"
  {x, y} => "at (#{x}, #{y})"
end

# Enum patterns
match option
  Some(x) => "has #{x}"
  None => "empty"
end

# Nested patterns
match result
  Ok(Some(value)) => "success with #{value}"
  Ok(None) => "success but empty"
  Err(e) => "error: #{e}"
end

# Or patterns
match value
  1 | 2 | 3 => "small"
  4 | 5 | 6 => "medium"
  _ => "large"
end

# Range patterns
match age
  0...13 => "child"
  13...20 => "teenager"
  20...65 => "adult"
  _ => "senior"
end

# Guard clauses
match point
  {x, y} if x == y => "on diagonal"
  {x, y} if x > y => "below diagonal"
  {x, y} => "above diagonal"
end

# Struct/class patterns
match player
  {health: 0} => "dead"
  {health: h, shield: s} if h + s < 20 => "critical"
  {health: h} if h < 50 => "wounded"
  _ => "healthy"
end

# Array patterns
match items
  [] => "empty"
  [x] => "single: #{x}"
  [x, y] => "pair: #{x}, #{y}"
  [first, *rest] => "first: #{first}, rest: #{rest}"
end

# Type patterns
match value
  x : Int => "integer #{x}"
  x : String => "string #{x}"
  x : Array => "array of #{x.size}"
  _ => "unknown"
end
```

---

## 13. Error Handling

### 13.1 Result Type

```momiji
# Prefer Result for expected, recoverable errors
def parse_int(s : String) : Result(Int, ParseError)
  if s.empty?
    return Err(ParseError::Empty)
  end
  
  # ... parsing logic ...
  
  Ok(parsed_value)
end

# Using Result
match parse_int("42")
  Ok(n) => puts "Parsed: #{n}"
  Err(e) => puts "Error: #{e}"
end
```

### 13.2 Propagation Operator

```momiji
# The ? operator propagates errors
def process_config(path : String) : Result(Config, Error)
  content = read_file(path)?        # Returns early if Err
  data = parse_json(content)?       # Returns early if Err
  config = validate(data)?          # Returns early if Err
  Ok(config)
end

# Equivalent to:
def process_config(path : String) : Result(Config, Error)
  match read_file(path)
    Err(e) => return Err(e)
    Ok(content) =>
      match parse_json(content)
        Err(e) => return Err(e)
        Ok(data) =>
          match validate(data)
            Err(e) => return Err(e)
            Ok(config) => Ok(config)
          end
      end
  end
end
```

### 13.3 Option and Nil Handling

```momiji
# Option type for absence of value
def find(items : Array(T), target : T) : Option(T) forall T
  items.each do |item|
    return Some(item) if item == target
  end
  None
end

# Nil-safe access
user?.profile?.settings?.theme  # Returns nil if any is nil

# Nil coalescing
value = maybe_nil || default
value = maybe_nil.or(default)

# Nil assertion (panics if nil)
value = maybe_nil!
```

### 13.4 Panic for Unrecoverable Errors

```momiji
# Panic for bugs, not expected errors
def get(index : Int) : T
  panic("index out of bounds: #{index}") if index < 0 or index >= size
  @data[index]
end

# With formatted message
panic("unexpected state: expected #{expected}, got #{actual}")

# Unreachable code marker
match direction
  North => go_north
  South => go_south
  East => go_east
  West => go_west
  _ => unreachable("all directions covered")
end
```

### 13.5 Assertions

```momiji
# Debug assertions (removed in release builds)
debug_assert(index >= 0)
debug_assert(buffer.size <= MAX_SIZE, "buffer overflow")

# Always-on assertions
assert(config.valid?, "invalid configuration")
```

### 13.6 Defer

```momiji
# Defer executes when leaving scope
def process_file(path : String) : Result(Data, Error)
  file = open(path)?
  defer file.close  # Always called, even on early return
  
  content = file.read_all?
  parse(content)
end

# Multiple defers execute in reverse order
def complex_operation
  resource1 = acquire1()
  defer release1(resource1)
  
  resource2 = acquire2()
  defer release2(resource2)
  
  # On exit: release2 called first, then release1
end
```

---

## 14. Modules and Visibility

### 14.1 Module Declaration

```momiji
# File: src/game/combat.mj
module Game::Combat

# Module contents...

# End of file implicitly ends module
```

### 14.2 Imports

```momiji
# Import entire module
import Std::Math

# Import specific items
import Std::Collections.(Array, Map, Set)

# Import with alias
import Std::FileSystem as FS

# Import all public items
import Game::Events.*

# Relative imports (within same module hierarchy)
import .utils           # Game::Combat::Utils if in Game::Combat
import ..entities       # Game::Entities if in Game::Combat
```

### 14.3 Visibility Rules

```momiji
module MyLib

# Private by default (visible only within this module)
def helper
  # ...
end

struct InternalData
  # ...
end

# Public (visible to importers)
pub def public_api(input : String) : Output
  # ...
end

pub struct PublicType
  # Public fields
  pub property name : String
  pub property value : Int
  
  # Private field
  property internal : Data
end

# Public constant
pub MAX_SIZE = 1024

# Public type alias
pub alias Handler = (Event) -> Nil
```

### 14.4 Module Organization

```
src/
├── main.mj              # Entry point
├── game/
│   ├── mod.mj           # module Game (re-exports submodules)
│   ├── player.mj        # module Game::Player
│   ├── combat.mj        # module Game::Combat
│   └── entities/
│       ├── mod.mj       # module Game::Entities
│       ├── enemy.mj     # module Game::Entities::Enemy
│       └── npc.mj       # module Game::Entities::NPC
└── utils/
    ├── mod.mj           # module Utils
    └── logging.mj       # module Utils::Logging
```

```momiji
# src/game/mod.mj
module Game

# Re-export submodules
pub import .player.*
pub import .combat.*
pub import .entities.*
```

---

## 15. Concurrency

### 15.1 Fibers

Fibers are lightweight, cooperatively scheduled threads.

```momiji
# Spawn a fiber
spawn do
  expensive_computation()
end

# Spawn with name (for debugging)
spawn name: "background-worker" do
  loop do
    process_next_job()
    Fiber.yield  # Cooperatively yield
  end
end

# Wait for fiber completion
fiber = spawn do
  compute_result()
end

result = fiber.await  # Blocks until fiber completes
```

### 15.2 Channels

Channels provide safe communication between fibers.

```momiji
# Create a channel
channel = Channel(String).new

# Buffered channel
buffered = Channel(Int).new(capacity: 10)

# Send and receive
spawn do
  channel.send("Hello")
  channel.send("World")
  channel.close
end

# Receiving
message = channel.receive  # Blocks until message available

# Non-blocking receive
if message = channel.receive?
  process(message)
end

# Iterate over channel
for message in channel
  process(message)
end
# Loop ends when channel is closed

# Close channel
channel.close
channel.closed?  # => true
```

### 15.3 Select

```momiji
# Wait on multiple channels
select
when message = messages.receive
  handle_message(message)
when tick = timer.receive
  update(tick)
when error = errors.receive
  log_error(error)
when timeout 5.seconds
  handle_timeout()
end

# Non-blocking select
select
when message = messages.receive
  handle_message(message)
else
  # No message available
  do_other_work()
end
```

### 15.4 Parallel Iteration

```momiji
# Parallel map
results = items.parallel.map { |item| expensive(item) }

# Parallel each
items.parallel.each { |item| process(item) }

# With concurrency limit
items.parallel(max: 4).map { |item| expensive(item) }
```

### 15.5 Mutex and Synchronization

```momiji
# Mutex for protecting shared state
mutex = Mutex(GameState).new(initial_state)

mutex.synchronize do |state|
  state.update()
end

# Or with lock/unlock
mutex.lock
state = mutex.value
state.update()
mutex.unlock

# WaitGroup for coordination
wg = WaitGroup.new

5.times do |i|
  wg.add
  spawn do
    process(i)
    wg.done
  end
end

wg.wait  # Blocks until all done
```

### 15.6 Atomic Operations

```momiji
# Atomic values for lock-free operations
counter = Atomic(Int).new(0)

counter.add(1)        # Atomic increment
counter.sub(1)        # Atomic decrement
counter.get           # Read current value
counter.set(100)      # Set value
counter.swap(200)     # Set and return old value

# Compare and swap
old = 100
new = 200
if counter.compare_and_swap(old, new)
  # Successfully swapped
end
```

---

## 16. Memory Management

### 16.1 Overview

Momiji uses automatic reference counting (ARC) by default:

- **Classes** — Reference types, heap-allocated, reference-counted
- **Structs** — Value types, stack-allocated when possible, copied by value
- No garbage collection pauses
- Deterministic destruction

### 16.2 Value Types (Structs)

```momiji
struct Point
  property x, y : Float
end

p1 = Point.new(1.0, 2.0)
p2 = p1  # Copied
p2.x = 10.0
p1.x  # Still 1.0

# Structs are passed by value (copied)
def modify(point : Point)
  point.x = 100.0  # Modifies local copy
end

modify(p1)
p1.x  # Still 1.0
```

### 16.3 Reference Types (Classes)

```momiji
class Player
  property name : String
end

player1 = Player.new("Alice")
player2 = player1  # Reference copy
player2.name = "Bob"
player1.name  # "Bob" (same object)

# Classes are passed by reference
def modify(player : Player)
  player.name = "Changed"
end

modify(player1)
player1.name  # "Changed"
```

### 16.4 Ownership Annotations

For performance-critical code, explicit ownership control is available:

```momiji
# ref — immutable borrow (default for classes)
def inspect(player : ref Player)
  puts player.name
  # Cannot modify player
end

# mut — mutable borrow
def heal(player : mut Player, amount : Int)
  player.health += amount
end

# own — ownership transfer
def consume(player : own Player)
  # We own player, caller can't use it anymore
  process(player)
end  # player is destroyed here

# Usage
player = Player.new("Hero")
inspect(player)   # Borrowed, player still valid
heal(player, 20)  # Mutably borrowed, player still valid
consume(player)   # Moved, player no longer accessible
# player.name     # ERROR: player was moved
```

### 16.5 Weak References

```momiji
# Weak references don't prevent deallocation
class Node
  property value : Int
  property children : Array(Node)
  property parent : Weak(Node)?  # Weak to avoid cycles
  
  def initialize(@value)
    @children = []
    @parent = nil
  end
end

# Using weak references
weak_ref = Weak(Player).new(player)

if strong = weak_ref.upgrade
  # Object still exists
  puts strong.name
else
  # Object was deallocated
  puts "Player gone"
end
```

---

## 17. Standard Library Overview

### 17.1 Core Types

```momiji
import Std::Core.(
  Int, Float, Bool, Char, String,
  Array, Map, Set,
  Option, Result,
  Tuple, Range
)
```

### 17.2 Collections

```momiji
import Std::Collections

# Array
arr = [1, 2, 3]
arr.push(4)
arr.pop
arr.first?
arr.last?
arr.map { |x| x * 2 }
arr.filter { |x| x > 2 }
arr.reduce(0) { |acc, x| acc + x }

# Map
map = {"a" => 1, "b" => 2}
map["c"] = 3
map["a"]?  # Option
map.keys
map.values
map.each { |k, v| puts "#{k}: #{v}" }

# Set
set = Set.new([1, 2, 3])
set.add(4)
set.remove(1)
set.contains?(2)
set.union(other_set)
set.intersection(other_set)
```

### 17.3 String Operations

```momiji
import Std::String

s = "Hello, World!"
s.size           # 13
s.empty?         # false
s.upcase         # "HELLO, WORLD!"
s.downcase       # "hello, world!"
s.split(", ")    # ["Hello", "World!"]
s.replace("World", "Momiji")
s.starts_with?("Hello")
s.ends_with?("!")
s.contains?("World")
s.trim
s.chars          # Iterator of Char
s[0..5]          # "Hello,"
```

### 17.4 File System

```momiji
import Std::FS

# Reading files
content = FS.read("file.txt")?
lines = FS.read_lines("file.txt")?

# Writing files
FS.write("file.txt", content)?
FS.append("file.txt", more_content)?

# File operations
FS.exists?("file.txt")
FS.delete("file.txt")?
FS.rename("old.txt", "new.txt")?
FS.copy("src.txt", "dst.txt")?

# Directories
FS.mkdir("new_dir")?
FS.rmdir("old_dir")?
FS.list_dir(".")?  # Array(DirEntry)

# Path manipulation
path = Path.new("src/game/player.mj")
path.parent       # "src/game"
path.filename     # "player.mj"
path.stem         # "player"
path.extension    # "mj"
path.join("other")
```

### 17.5 Networking

```momiji
import Std::Net

# TCP client
client = TCP.connect("example.com", 80)?
client.write("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")?
response = client.read_all?
client.close

# TCP server
server = TCP.listen("0.0.0.0", 8080)?
loop do
  client = server.accept?
  spawn do
    handle_client(client)
  end
end

# HTTP client
import Std::Net::HTTP

response = HTTP.get("https://api.example.com/data")?
response.status   # 200
response.body     # String
response.headers  # Map

HTTP.post("https://api.example.com/data", 
  body: json_data,
  headers: {"Content-Type" => "application/json"}
)?
```

### 17.6 JSON

```momiji
import Std::JSON

# Parsing
data = JSON.parse(json_string)?
data["name"]?.as_string?
data["age"]?.as_int?
data["items"]?.as_array?

# Serialization
obj = {"name" => "Alice", "age" => 30}
json_string = JSON.stringify(obj)

# Typed serialization with annotations
@[JSON::Serializable]
struct User
  property name : String
  property email : String
  property age : Int
end

user = User.from_json(json_string)?
json = user.to_json
```

### 17.7 Time and Duration

```momiji
import Std::Time

# Current time
now = Time.now
today = Date.today

# Duration
duration = 5.seconds
duration = 100.milliseconds
duration = 2.hours + 30.minutes

# Time arithmetic
future = now + 1.day
past = now - 1.week

# Formatting
now.format("%Y-%m-%d %H:%M:%S")
now.iso8601

# Sleeping
sleep(1.second)
sleep(100.milliseconds)
```

### 17.8 Math

```momiji
import Std::Math

Math.abs(-5)
Math.max(a, b)
Math.min(a, b)
Math.clamp(value, min, max)
Math.sqrt(16.0)
Math.pow(2.0, 10.0)
Math.sin(angle)
Math.cos(angle)
Math.floor(3.7)
Math.ceil(3.2)
Math.round(3.5)

Math::PI
Math::E
```

---

## 18. Appendix: Grammar Summary

### 18.1 Program Structure

```ebnf
program         = module_decl? import* declaration*
module_decl     = "module" module_path
import          = "import" import_path
declaration     = function | struct | class | enum | interface | constant
```

### 18.2 Types

```ebnf
type            = simple_type | generic_type | nullable_type | function_type
simple_type     = IDENTIFIER
generic_type    = IDENTIFIER "(" type ("," type)* ")"
nullable_type   = type "?"
function_type   = "(" (type ("," type)*)? ")" "->" type
```

### 18.3 Expressions

```ebnf
expression      = assignment | or_expr
assignment      = or_expr ("=" | "+=" | "-=" | ...) expression
or_expr         = and_expr (("or" | "||") and_expr)*
and_expr        = equality_expr (("and" | "&&") equality_expr)*
equality_expr   = comparison_expr (("==" | "!=") comparison_expr)*
comparison_expr = range_expr (("<" | ">" | "<=" | ">=") range_expr)*
range_expr      = additive_expr ((".." | "...") additive_expr)?
additive_expr   = mult_expr (("+" | "-") mult_expr)*
mult_expr       = unary_expr (("*" | "/" | "%" | "//") unary_expr)*
unary_expr      = ("!" | "-" | "not") unary_expr | postfix_expr
postfix_expr    = primary_expr (call | index | member | safe_nav)*
call            = "(" arguments? ")" block?
index           = "[" expression "]"
member          = "." IDENTIFIER
safe_nav        = "?." IDENTIFIER
```

### 18.4 Statements

```ebnf
statement       = expression_stmt | if_stmt | match_stmt | while_stmt
                | for_stmt | return_stmt | break_stmt | next_stmt
if_stmt         = "if" expression block ("elsif" expression block)* ("else" block)? "end"
match_stmt      = "match" expression pattern_clause+ "end"
pattern_clause  = pattern ("if" expression)? "=>" (expression | block)
while_stmt      = "while" expression block "end"
for_stmt        = "for" IDENTIFIER "in" expression block "end"
return_stmt     = "return" expression?
```

### 18.5 Declarations

```ebnf
function        = visibility? "def" IDENTIFIER type_params? params? (":" type)? block "end"
visibility      = "pub" | "private" | "protected"
type_params     = "(" IDENTIFIER ("," IDENTIFIER)* ")"
params          = "(" param ("," param)* ")"
param           = IDENTIFIER ":" type ("=" expression)?

struct          = visibility? "struct" IDENTIFIER type_params? struct_body "end"
struct_body     = (property | function | impl_block)*
property        = "property" IDENTIFIER ":" type ("=" expression)?

class           = visibility? "class" IDENTIFIER type_params? ("<" type)? class_body "end"
class_body      = (property | function | impl_block)*

enum            = visibility? "enum" IDENTIFIER type_params? enum_body "end"
enum_body       = enum_variant ("," enum_variant)* function*
enum_variant    = IDENTIFIER ("(" type ")")? ("=" INTEGER)?

interface       = visibility? "interface" IDENTIFIER type_params? (":" constraints)? interface_body "end"
interface_body  = function_signature*
```

---

## Acknowledgments

Momiji draws inspiration from:

- **Ruby** — Expressive syntax, developer happiness
- **Crystal** — Ruby-like syntax with static typing
- **Go** — Fast compilation, simple concurrency
- **Rust** — Memory safety concepts
- **Swift** — Reference counting, optionals
- **Kotlin** — Null safety, pragmatic design

---

*Momiji: Write naturally. Compile fast. Run safe.*
