# Alg

alg.math does concise formulas for custom types.  Similar to operator overloading in other languages.

I'm making this for a project which needs to do various types of algebraic math.  Unless the project takes on a life of its own, things will mostly be implemented whent I need them.

Example:

```zig
var a = alg.mat.Matrix(f32, 2, 1).lit(.{
    1,
    2,
});

var b = alg.mat.Matrix(f32, 1, 2).lit(.{
    3, 4,
});

var c: f32 = 5;

var result = alg.math("a * b * c", .{
    .a = a,
    .b = b,
    .c = c,
});

try expectEqual(alg.mat.Matrix(f32, 2, 2).lit(.{
    15, 20,
    30, 40,
}), result);
```

Current limitations:

- Only a limited number of operations implemented so far.
- There is no order of operations.  All chained operations must be the same.  Use parethesis to determine order.  Eg, "(a * b) + c".
- Chained operations are always carried out left to right.  This may be inefficient for some equations, and not standard for others (eg, raising to a power).