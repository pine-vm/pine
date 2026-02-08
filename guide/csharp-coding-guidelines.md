# C# Coding Guidelines

## Design

The design section covers rules that require inter-file or semantic analysis, or that cannot be resolved automatically with a tool.

The C# coding guidelines in Pine are stricter than Microsoft's general recommendations to achieve greater transparency and readability.

### Avoid the Intransparency of Infix Operators

Usages of the operators `==` and `!=` with primitive types (e.g., `int`, `string`) on one side and a literal or `null` on the other side must be replaced with pattern-matching syntax using `is` and `is not`.

Background:

+ C# allows overriding these operators, so that surprising behavior could be hidden in there.

## Formatting

The formatting section covers rules that do not require analysis beyond a single file and can be applied automatically.

The basis for formatting is `dotnet format`, which in turn applies rules we have specified in the `.editorconfig` files in the repository.

Since `dotnet format` is quite lenient, we add additional rules to achieve a more consistent style.

### Consistent Layout of List Items

Items in lists such as argument lists, parameter lists, collection expressions, tuples, and array/object/collection initializers must be distributed in one of two ways: Either all items on the same line or each item starting a new line.
If the last item is not on the same line as the first, the items must be placed so that each starts on a new line.

For multiline collection expressions, the closing bracket must be placed on a new line after the last item. For argument lists and parameter lists, the closing parentheses must be placed on the same line as the last item.

For argument lists in the multiline form, the first argument must be separated from the opening parens with a line break.

For example, the following code:

```csharp
int[] alfa =
    [1, 2,
    3];

var beta =
    func(1, 2, 3);

var gamma =
    func(1, 2,
    3);

decl =
    alfa(beta(
        gamma(delta(a, b))));
```

Must be formatted to:

```csharp
int[] alfa =
    [
    1,
    2,
    3
    ];

var beta =
    func(1, 2, 3);

var gamma =
    func(
        1,
        2,
        3);

decl =
    alfa(
        beta(
            gamma(delta(a, b))));
```

### Initializer on New Line

In declaration statements, the initializer may be placed on the same line.
The initializer must never start on the same line as the equals sign if the initializer expression itself spans multiple lines.

For example, the following code:

```csharp
var firstItemRange = getItemRange(separatedList.First);
var commentsAfterOpen = commentQueries.GetOnRowBetweenColumns(
    containerRange.Start.Row,
    containerRange.Start.Column,
    firstItemRange.Start.Column);
```

Must be formatted to:

```csharp
var firstItemRange = getItemRange(separatedList.First);

var commentsAfterOpen =
    commentQueries.GetOnRowBetweenColumns(
        containerRange.Start.Row,
        containerRange.Start.Column,
        firstItemRange.Start.Column);
```

### Line Breaks Depending on Line Length

When a line exceeds a length of 120 columns, line breaks must be inserted at the following locations:

+ Before a declaration initializer.
+ Before an expression body.
+ Before the dot in a member access expression.
+ Before the expression part of a switch expression arm.
+ In a conditional expression (splitting it to multiple lines as described in the conditional expression rule below).

Also, list forms such as argument lists, parameter lists, or object initializer lists must be switched to a multiline layout if the containing line exceeds the length threshold.

In automated formatting, these line breaks must be inserted starting from the outermost syntax node, only as far as necessary to meet the line length limit, so that the layout of inner expressions is preserved to the degree possible.

### Spacing between Statements

If a statement spans multiple lines, it must be separated from previous and following statements by an empty line.
If a statement spans multiple lines, it must be separated from following comments by an empty line.

### Spacing between Declarations

Type declarations, method declarations and member declarations must be separated by two empty lines.

### Conditional Expression

If a conditional expression spans multiple lines, each subexpression and token must be placed on their own line.

For example, the following code:

```csharp
count < 13
    ? 17
    : 21
```

Must be formatted to:

```csharp
count < 13
?
17
:
21
```

### Switch Expression

+ If a switch expression arm spans multiple lines, it must be separated from other arms by an empty line.
+ If a switch expression arm spans multiple lines, the expression must start on a new line after the pattern and arrow.
+ If the pattern syntax of a switch expression arm is `DiscardPattern`, it must always be separated from other arms by an empty line.
+ If a switch expression arm contains a `throw` expression, the expression must be placed on a new line after the pattern and arrow.

### Switch Statement

Every section of a switch statement must be separated from other sections by an empty line.

### Return Statement

If a return statement spans multiple lines, the expression must start on a new line after the `return` keyword;

### Indentation

Indentation must only depend on the content. That means, existing indent before formatting must never influence the resulting indentation after formatting.

### File

A file must end with one empty line.

