# elm-stylus

A Stylus CSS preprocessor parser for Elm that converts a strict subset of Stylus syntax to CSS.

## Installation

```bash
elm install ad-si/elm-stylus
```

## Usage

```elm
import Stylus.Parser exposing (stylusToCss)

stylus : String
stylus = """
div
  width 400px
  height 300px
  background-color blue

h1, h2, h3
  color red
  font-size 18px

// This is a comment
.alert
  color rgb(255, 0, 0)
"""

css : Result (List (DeadEnd Context Problem)) String
css = stylusToCss stylus
```

This will produce:

```css
div{width:400px;height:300px;background-color:blue}
h1, h2, h3{color:red;font-size:18px}
/*.alert{color:rgb(255, 0, 0)}*/
.alert{color:rgb(255, 0, 0)}
```

## Supported Features

- **Selectors**: Element, class, ID, and attribute selectors
- **Properties**: Standard CSS properties with values
- **Comments**: Single-line comments with `//`
- **Multiple selectors**: Comma-separated selectors
- **Indentation-based nesting**: Two-space indentation for declarations

## Syntax

The parser supports a strict subset of Stylus syntax:

### Basic Rules
```stylus
selector
  property value
  another-property another-value
```

### Multiple Selectors
```stylus
h1, h2, .important
  font-weight bold
  color blue
```

### Comments
```stylus
// This is a comment
div
  margin 0
```

## API

### `stylusToCss : String -> Result (List (DeadEnd Context Problem)) String`

The main function that converts Stylus syntax to CSS. Returns a `Result` where:
- `Ok String` contains the generated CSS
- `Err (List (DeadEnd Context Problem))` contains parsing errors

### Types

- `Expression` - AST node types (Rule, Comment, Newlines)
- `Problem` - Parsing error types
- Additional parser functions are exposed for advanced use cases

## Development

```bash
# Run tests
make test

# Install dependencies
npm install
```

## License

AGPL-3.0