const expectEqual = @import("std").testing.expectEqual;
const Vector = @import("std").meta.Vector;

const math = @import("math.zig").math;

pub fn Matrix(comptime T: type, rows: comptime_int, columns: comptime_int) type {
    switch (@typeInfo(T)) {
        .Int, .Float => {},
        else => @compileError("Matrix only supports integers and floats"),
    }

    return struct {
        columnMajorValues: [columns * rows]T,

        const T = T;
        const rows = rows;
        const columns = columns;
        const Self = @This();
        const SelfV = Vector(columns * rows, T);
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
                    r.columnMajorValues[column * rows + row] = @field(v, fieldsInfo[row * columns + column].name);
                }
            }
            return r;
        }

        pub fn ident() Self {
            if (rows != columns) {
                @panic("ident is only valid for square matrices.");
            }
            const arr = comptime init: {
                var arrInit = [_]T{0} ** (columns * rows);
                var i: usize = 0;
                while (i < arrInit.len) : (i += columns + 1) {
                    arrInit[i] = 1;
                }
                break :init arrInit;
            };
            return Self{
                .columnMajorValues = arr,
            };
        }

        pub fn mul(self: Self, other: anytype) Self.mulReturnType(@TypeOf(other)) {
            const Other = @TypeOf(other);
            if (T == Other) {
                return self.mulSwap(other);
            }
            var r: Self.mulReturnType(@TypeOf(other)) = undefined;

            var selfRows: [rows]Vector(columns, T) = undefined;
            // TODO: Maybe putting into a big vector and shuffling out values
            // would be faster?

            {
                var row: usize = 0;
                while (row < rows) : (row += 1) {
                    var rowArr: [columns]T = undefined;
                    var column: usize = 0;
                    while (column < columns) : (column += 1) {
                        rowArr[column] = self.columnMajorValues[column * rows + row];
                    }
                    selfRows[row] = rowArr;
                }
            }

            var column: usize = 0;
            while (column < Other.columns) : (column += 1) {
                var columnVec: Vector(Other.rows, T) = other.columnMajorValues[column * Other.rows ..][0..Other.rows].*;
                var row: usize = 0;
                while (row < rows) : (row += 1) {
                    r.columnMajorValues[column * rows + row] = @reduce(.Add, selfRows[row] * columnVec);
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
            var scalerVec = @splat(columns * rows, scaler);
            return Self{
                .columnMajorValues = @as(SelfV, self.columnMajorValues) * scalerVec,
            };
        }

        pub fn add(self: Self, other: Self) Self {
            return Self{
                .columnMajorValues = @as(SelfV, self.columnMajorValues) + @as(SelfV, other.columnMajorValues),
            };
        }

        pub fn sub(self: Self, other: Self) Self {
            return Self{
                .columnMajorValues = @as(SelfV, self.columnMajorValues) - @as(SelfV, other.columnMajorValues),
            };
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
    try expectEqual(@as(f32, 2), a.columnMajorValues[0]);
    try expectEqual(@as(f32, 4), a.columnMajorValues[1]);
    try expectEqual(@as(f32, 3), a.columnMajorValues[2]);
    try expectEqual(@as(f32, 5), a.columnMajorValues[3]);
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

test "identity matrix" {
    const T = Matrix(f32, 5, 5);

    var a = T.lit(.{
        1, 0, 0, 0, 0,
        0, 1, 0, 0, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 1, 0,
        0, 0, 0, 0, 1,
    });

    var b = T.ident();

    try expectEqual(a, b);
}
