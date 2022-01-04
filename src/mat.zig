const expectEqual = @import("std").testing.expectEqual;
const Vector = @import("std").meta.Vector;

const math = @import("math.zig").math;

pub fn Matrix(comptime T: type, rows: comptime_int, columns: comptime_int) type {
    switch (@typeInfo(T)) {
        .Int, .Float => {},
        else => @compileError("Matrix only supports integers and floats"),
    }

    return struct {
        // Stored in column major order.
        values: [columns][rows]T,

        const T = T;
        const rows = rows;
        const columns = columns;
        const Self = @This();
        const isMatrixType = true;

        pub fn lit(v: anytype) @This() {
            const VType = @TypeOf(v);
            const vTypeInfo = @typeInfo(VType);
            if (vTypeInfo != .Struct) {
                @compileError("Expected tuple or struct argument, found " ++ @typeName(VType));
            }
            const fieldsInfo = vTypeInfo.Struct.fields;

            if (fieldsInfo.len != rows * columns) {
                @compileError("Wrong size literal for matrix");
            }

            var r: @This() = undefined;
            comptime var column: usize = 0;
            inline while (column < columns) : (column += 1) {
                comptime var row: usize = 0;
                inline while (row < rows) : (row += 1) {
                    r.values[column][row] = @field(v, fieldsInfo[row * columns + column].name);
                }
            }
            return r;
        }

        pub fn mul(self: Self, other: anytype) Self.mulReturnType(@TypeOf(other)) {
            const Other = @TypeOf(other);
            if (T == Other) {
                return self.mulSwap(other);
            }
            var r: Self.mulReturnType(@TypeOf(other)) = undefined;

            var selfRows: [rows]Vector(columns, T) = undefined;
            // TODO: Maybe putting into a big array and shuffling out values
            // would be faster?

            {
                var i: usize = 0;
                while (i < rows) : (i += 1) {
                    var row: [columns]T = undefined;
                    var j: usize = 0;
                    while (j < columns) : (j += 1) {
                        row[j] = self.values[j][i];
                    }
                    selfRows[i] = row;
                }
            }

            var i: usize = 0;
            while (i < Other.columns) : (i += 1) {
                // columns == other.rows
                var column: Vector(Other.rows, T) = other.values[i];
                var j: usize = 0;
                while (j < rows) : (j += 1) {
                    r.values[i][j] = @reduce(.Add, selfRows[j] * column);
                }
            }

            return r;
        }

        pub fn mulReturnType(comptime Other: type) type {
            if (T == Other) {
                return Self;
            }
            if (!@hasDecl(Other, "isMatrixType")) {
                return void;
            }
            if (T != Other.T) {
                return void;
                // @compileError("Matrix multiplcation value types must match");
            }
            if (columns != Other.rows) {
                return void;
                // @compileError("Matrix multiplcation sizes incompatible.");
            }
            return Matrix(T, rows, Other.columns);
        }

        pub fn mulSwap(self: Self, scaler: T) Self {
            var r: Self = undefined;
            var i: usize = 0;
            var scalerVec = @splat(rows, scaler);
            while (i < columns) : (i += 1) {
                r.values[i] = @as(Vector(rows, T), self.values[i]) * scalerVec;
            }
            return r;
        }

        pub fn add(self: Self, other: Self) Self {
            // TODO: Is there a way to do vector addition on the whole contents
            // while still letting on individual columns work??
            var r: Self = undefined;
            var i: usize = 0;
            while (i < columns) : (i += 1) {
                r.values[i] = @as(Vector(rows, T), self.values[i]) + @as(Vector(rows, T), other.values[i]);
            }
            return r;
        }

        pub fn sub(self: Self, other: Self) Self {
            var r: Self = undefined;
            var i: usize = 0;
            while (i < columns) : (i += 1) {
                r.values[i] = @as(Vector(rows, T), self.values[i]) - @as(Vector(rows, T), other.values[i]);
            }
            return r;
        }
    };
}

test "matrix multiplcation type" {
    const A = Matrix(f32, 5, 3);
    const B = Matrix(f32, 3, 4);
    const C = comptime A.mulReturnType(B);
    try expectEqual(5, C.rows);
    try expectEqual(4, C.columns);
}

test "matrix literal" {
    var a = Matrix(f32, 2, 2).lit(.{
        2, 3,
        4, 5,
    });
    try expectEqual(@as(f32, 2), a.values[0][0]);
    try expectEqual(@as(f32, 4), a.values[0][1]);
    try expectEqual(@as(f32, 3), a.values[1][0]);
    try expectEqual(@as(f32, 5), a.values[1][1]);
}

test "matrix multiplcation" {
    var a = Matrix(f32, 2, 3).lit(.{
        2, 3, 4,
        5, 6, 7,
    });

    var b = Matrix(f32, 3, 2).lit(.{
        8,  9,
        10, 11,
        12, 13,
    });

    var c = math("a * b", .{
        .a = a,
        .b = b,
    });

    try expectEqual(Matrix(f32, 2, 2).lit(.{
        94,  103,
        184, 202,
    }), c);
}

test "matrix addition" {
    var a = Matrix(f32, 2, 3).lit(.{
        2, 3, 4,
        5, 6, 7,
    });

    var b = Matrix(f32, 2, 3).lit(.{
        8,  9,  10,
        11, 12, 13,
    });

    var c = math("a + b", .{
        .a = a,
        .b = b,
    });

    try expectEqual(Matrix(f32, 2, 3).lit(.{
        10, 12, 14,
        16, 18, 20,
    }), c);

    c = math("a - b", .{
        .a = a,
        .b = b,
    });

    try expectEqual(Matrix(f32, 2, 3).lit(.{
        -6, -6, -6,
        -6, -6, -6,
    }), c);
}

test "matrix scale" {
    var a = Matrix(f32, 2, 2).lit(.{
        1, 2,
        3, 4,
    });
    var b: f32 = 2;

    var c = math("a * b", .{
        .a = a,
        .b = b,
    });

    try expectEqual(Matrix(f32, 2, 2).lit(.{
        2, 4,
        6, 8,
    }), c);

    var d = math("b * a", .{
        .a = a,
        .b = b,
    });

    try expectEqual(Matrix(f32, 2, 2).lit(.{
        2, 4,
        6, 8,
    }), d);
}
