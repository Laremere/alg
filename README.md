# Alg

alg.math does concise formulas for custom types.  Similar to operator overloading in other languages.

I'm making this for a private project which needs to do various types of algebraic math.  Unless Alg takes on a life of its own, things will mostly be implemented whent I need them.  Also I'm new to Zig and things are likely to not be idiomatic.  Issues, comments, and requests are welcome, but will only be acted upon as I see fit.

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

All more complex types have a single underlying type, and all operations require the same underlying type between operands.  Eg you can't add a matrix backed by floats with one backed by integers.

Implemented:
- Matrices:
  - Define matrix in terms of rows, and columns.
  - Addition between matrices of the same size.
  - Multiplication with compatible shaped matricies resulting in a third, possibly differently shapped, matrix.
  - Multiplication with scaler values, which multiplies each value in the matrix by the scaler.

Feature Wishlist:
- Types:
  - Floats
  - Integers
  - Comptime float and integers
  - Vectory / Array
  - Matrix
  - Affine Matrix
  - Geometic Algebra
  - Maybe custom functions?
  - Imaginary Numbers
  - Quaternion
- Operations
  - Add
  - multiply
  - dot
  - etc.
- Built in values?
  - e
  - pi
  - Identity matrix?  Is this useful?
- Make parse errors actually useful.
- Pairwise conversion of underlying type.