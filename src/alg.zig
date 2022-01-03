const std = @import("std");
const expectEqual = @import("std").testing.expectEqual;
const Vector = std.meta.Vector;

// TODO:
//
// Types:
// - Floats
// - Integers
// - Comptime float and integers
// - Vectory / Array
// - Matrix
// - Affine Matrix
// - Geometic Algebra
// - Maybe custom functions?
// - Imaginary Numbers
// - Quaternion
//
// Operations
// Add, multiply, dot, etc.
//
// Built in values?
// e?
// pi?
// Identity matrix?  Is this useful?
//
// Make parse errors actually useful.

test "math" {
    std.debug.print("\n{}\n", .{math("a", .{ .a = @as(i8, 7) })});
    try expectEqual(@as(i8, 5), math("a", .{ .a = @as(i8, 5) }));
    try expectEqual(@as(i8, 15), math("a + a + a", .{ .a = @as(i8, 5) }));
    std.debug.print("\n{}\n", .{math("a + b", .{
        .a = @as(i8, 7),
        .b = @as(i8, 2),
    })});
    try expectEqual(15, math("a - b", .{ .a = 20, .b = 5 }));
    std.debug.print("\n{}\n", .{math("a - b", .{ .a = 1000000, .b = 5 })});
    // std.debug.print("\n{}\n", .{math("(-a) + a + (a+a+a", .{ .a = 10 })});
}

// Using an allocator (even just a fixed buffer) doesn't work during comptime yet.
// Instead just allocate something on the stack at the entry point, and work
// from there.  If we could use a real allocator, it'd be better for ast to
// contain pointer to concrete types, instead of concrete types pointing to asts.
// This would more efficiently pack memory.  However this way is much simpler given
// the constraints.
const StupidAlloc = struct {
    asts: [100]Ast,
    index: isize,

    fn next(comptime self: *StupidAlloc) *Ast {
        var r = &self.asts[self.index];
        self.index += 1;
        return r;
    }
};

pub fn math(comptime eq: [:0]const u8, args: anytype) ReturnType(eq, @TypeOf(args)) {
    @setEvalBranchQuota(10000);
    comptime var parser = comptime Parser.init(eq, @TypeOf(args));
    comptime var root = parser.parse();
    return root.eval(args);
}

fn ReturnType(comptime eq: [:0]const u8, argsType: type) type {
    comptime var parser = comptime Parser.init(eq, argsType);
    comptime var root = parser.parse();
    return root.ReturnType();
}

const BlockTerm = enum {
    eof,
    rParen,

    // fn name(self: BlockTerm) [:0]const u8 {
    //     switch (self) {
    //         .eof => "end of equation",
    //         .closeParen => "close parentheses",
    //     }
    // }
};

const Parser = struct {
    alloc: StupidAlloc,
    argsType: type,
    tokenizer: Tokenizer,

    // Careful: StupidAlloc is large, and pointers to it's allocations will change
    // when it's moved.
    fn init(eq: [:0]const u8, comptime argsType: type) Parser {
        return Parser{
            .alloc = StupidAlloc{
                .asts = undefined,
                .index = 0,
            },
            .argsType = argsType,
            .tokenizer = Tokenizer.init(eq),
        };
    }

    fn parse(comptime self: *Parser) *Ast {
        // parse should only be called once, but it's easy to fix that not working,
        // so why not?
        self.tokenizer.index = 0;
        return self.parseStatement(BlockTerm.eof);
    }

    fn parseStatement(comptime self: *Parser, expectedBlockTerm: BlockTerm) *Ast {
        var lhs = switch (self.parseElement()) {
            .operand => |op| {
                var rhs = switch (self.parseElement()) {
                    .operand => @compileError("Two operands in a row."),
                    .value => |rhs| rhs,
                    .blockTerm => {
                        @compileError("Unexpected end of block");
                    },
                };
                switch (self.parseElement()) {
                    .operand => @compileError("Operand after unary operation"),
                    .value => @compileError("Value after unary operation"),
                    .blockTerm => |blockTerm| {
                        if (expectedBlockTerm == blockTerm) {
                            return UnaryOp.init(&self.alloc, op, rhs);
                        }
                        @compileError("Incorrect termination of a statement.");
                    },
                }
            },
            .value => |lhs| lhs,
            .blockTerm => {
                @compileError("Empty block");
            },
        };
        var firstOp: ?Token.Tag = null;
        while (true) {
            var op = switch (self.parseElement()) {
                .operand => |op| op,
                .value => {
                    @compileError("Multiple values in a row");
                },
                .blockTerm => |blockTerm| {
                    if (expectedBlockTerm == blockTerm) {
                        return lhs;
                    }
                    @compileError("Incorrect termination of a statement.");
                },
            };
            if (firstOp) |correctOp| {
                if (op != correctOp) {
                    @compileError("Mismatching operations");
                }
            } else {
                firstOp = op;
            }
            var rhs = switch (self.parseElement()) {
                .operand => @compileError("Unexpected double operand"),
                .value => |rhs| rhs,
                .blockTerm => @compileError("Unexpected block termination"),
            };
            lhs = BinaryOp.init(&self.alloc, lhs, op, rhs);
        }
    }

    const Element = union(enum) {
        operand: Token.Tag,
        value: *Ast,
        blockTerm: BlockTerm,
    };

    fn parseElement(comptime self: *Parser) Element {
        var token = self.tokenizer.next();
        switch (token.tag) {
            .invalid => @compileError("Invalid equation"),

            .eof => {
                return Element{ .blockTerm = BlockTerm.eof };
            },
            .lParen => {
                return Element{ .value = self.parseStatement(BlockTerm.rParen) };
            },
            .rParen => {
                return Element{ .blockTerm = BlockTerm.rParen };
            },

            .plus, .minus => {
                return Element{ .operand = token.tag };
            },

            .identifier => {
                return Element{ .value = Identifier.init(&self.alloc, self.tokenizer.source(token), self.argsType) };
            },
        }
    }
};

const Ast = union(enum) {
    identifier: Identifier,
    binaryOp: BinaryOp,
    unaryOp: UnaryOp,

    fn eval(comptime ast: *Ast, args: anytype) ast.ReturnType() {
        return switch (ast.*) {
            .identifier => |v| v.eval(args),
            .binaryOp => |v| v.eval(args),
            .unaryOp => |v| v.eval(args),
        };
    }

    fn ReturnType(comptime ast: *Ast) type {
        return switch (ast.*) {
            .identifier => |v| v.return_type,
            .binaryOp => |v| v.return_type,
            .unaryOp => |v| v.return_type,
        };
    }
};

const Identifier = struct {
    name: []const u8,
    return_type: type,

    fn init(comptime alloc: *StupidAlloc, name: []const u8, comptime argsType: type) *Ast {
        comptime var S = switch (@typeInfo(argsType)) {
            .Struct => |v| v,
            else => @compileError("math args must be a struct or touple."),
        };
        for (S.fields) |field| {
            if (std.mem.eql(u8, name, field.name)) {
                var r = alloc.next();
                r.* = Ast{
                    .identifier = Identifier{
                        .name = name,
                        .return_type = field.field_type,
                    },
                };
                return r;
            }
        } else {
            @compileError("Identifier in equation not found in passed info: " ++ name);
        }
    }

    fn eval(comptime ident: *const Identifier, args: anytype) ident.return_type {
        return @field(args, ident.name);
    }
};

const BinaryOp = struct {
    lhs: *Ast,
    op: Op,
    rhs: *Ast,
    return_type: type,

    const Op = enum {
        addErr,
        subErr,
        // mulErr,
    };

    fn init(comptime alloc: *StupidAlloc, lhs: *Ast, opToken: Token.Tag, rhs: *Ast) *Ast {
        var op = switch (opToken) {
            .plus => Op.addErr,
            .minus => Op.subErr,
            else => @compileError("Invalid binary operator"),
        };

        var lhsRet = lhs.ReturnType();
        var rhsRet = rhs.ReturnType();
        if (lhsRet != rhsRet) {
            @compileError("Values on either side of binary op do not match type.");
        }
        var r = alloc.next();
        r.* = Ast{
            .binaryOp = BinaryOp{
                .lhs = lhs,
                .op = op,
                .rhs = rhs,
                .return_type = lhsRet,
            },
        };
        return r;
    }

    fn eval(comptime self: *const BinaryOp, args: anytype) self.return_type {
        return switch (self.op) {
            .addErr => if (comptime isBuiltinScalar(self.return_type)) {
                return self.lhs.eval(args) + self.rhs.eval(args);
            } else {
                @compileError("No custom adding yet");
            },
            .subErr => if (comptime isBuiltinScalar(self.return_type)) {
                return self.lhs.eval(args) - self.rhs.eval(args);
            } else {
                @compileError("No custom subtraction yet");
            },
        };
    }
};

const UnaryOp = struct {
    op: Op,
    rhs: *Ast,
    return_type: type,

    const Op = enum {
        negate,
    };

    fn init(comptime alloc: *StupidAlloc, opToken: Token.Tag, rhs: *Ast) *Ast {
        var op = switch (opToken) {
            .minus => Op.negate,
            else => @compileError("Invalid unary operator"),
        };
        var r = alloc.next();
        r.* = Ast{ .unaryOp = UnaryOp{
            .op = op,
            .rhs = rhs,
            .return_type = rhs.ReturnType(),
        } };
        return r;
    }

    fn eval(comptime self: *const UnaryOp, args: anytype) self.return_type {
        return switch (self.op) {
            .negate => {
                return -self.rhs.eval(args);
            },
        };
    }
};

// fn parseInt(comptime str: []const u8) comptime_int {
//     var r: comptime_int = 0;
//     // todo: non-base 10 integers?
//     for (str) |chr| {
//         switch (chr) {
//             '0' => r = r * 10,
//             '1' => r = r * 10 + 1,
//             '2' => r = r * 10 + 2,
//             '3' => r = r * 10 + 3,
//             '4' => r = r * 10 + 4,
//             '5' => r = r * 10 + 5,
//             '6' => r = r * 10 + 6,
//             '7' => r = r * 10 + 7,
//             '8' => r = r * 10 + 8,
//             '9' => r = r * 10 + 9,
//             else => @compileError("invalid integer"),
//         }
//     }
//     return r;
// }

// test "parse integer" {
//     try expectEqual(0, parseInt("0"));
//     try expectEqual(10, parseInt("010"));
//     try expectEqual(9876543210, parseInt("9876543210"));
// }

fn isBuiltinScalar(comptime v: type) bool {
    return switch (@typeInfo(v)) {
        .Int => true,
        .Float => true,
        .ComptimeFloat => true,
        .ComptimeInt => true,
        else => false,
    };
}

// fn InvalidCombo(comptime a: type, comptime b: type, comptime op: []const u8) noreturn {
//     @compileError("Invalid combination of " ++ @typeName(a) ++ " and " ++ @typeName(b) ++ " for operation " ++ op ++ ".");
// }

// fn logStruct(s: anytype) void {
//     comptime var T = @TypeOf(s);
//     @compileLog(T);
//     @compileLog(s);
//     comptime var S = switch (@typeInfo(T)) {
//         .Struct => |v| v,
//         else => @compileError("log struct only takes structs."),
//     };
//     inline for (S.fields) |field| {
//         @compileLog(field.name);
//     }
// }

const Token = struct {
    tag: Tag,
    start: usize,
    end: usize,

    const Tag = enum {
        invalid,
        eof,
        identifier,
        plus,
        minus,
        lParen,
        rParen,
    };
};

const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    fn init(buffer: [:0]const u8) Tokenizer {
        return Tokenizer{
            .buffer = buffer,
            .index = 0,
        };
    }

    const State = enum {
        start,
        identifier,
        // plus,
    };

    fn next(self: *Tokenizer) Token {
        var state = State.start;
        var r = Token{
            .tag = .eof,
            .start = self.index,
            .end = undefined,
        };

        outer: while (true) : (self.index += 1) {
            const c = if (self.index < self.buffer.len) self.buffer[self.index] else 0;
            // @compileLog(@tagName(state), self.index, c);
            switch (state) {
                .start => switch (c) {
                    0 => {
                        break :outer;
                    },
                    ' ' => {
                        r.start = self.index + 1;
                    },
                    'a'...'z', 'A'...'A', '_' => {
                        state = .identifier;
                    },
                    '+' => {
                        r.tag = .plus;
                        self.index += 1;
                        break :outer;
                    },
                    '-' => {
                        r.tag = .minus;
                        self.index += 1;
                        break :outer;
                    },
                    '(' => {
                        r.tag = .lParen;
                        self.index += 1;
                        break :outer;
                    },
                    ')' => {
                        r.tag = .rParen;
                        self.index += 1;
                        break :outer;
                    },
                    else => {
                        r.tag = .invalid;
                        break :outer;
                    },
                },
                .identifier => switch (c) {
                    'a'...'z', 'A'...'A', '_' => {},
                    else => {
                        r.tag = .identifier;
                        break :outer;
                    },
                },
            }
        }
        // @compileLog("Tag out", @tagName(r.tag));
        r.end = self.index;
        return r;
    }

    fn source(self: *Tokenizer, token: Token) []const u8 {
        return self.buffer[token.start..token.end];
    }
};

pub fn Img(comptime T: type) type {
    return struct {
        v: Vector(2, T),

        fn init(scalar: T, imaginary: T) Img(T) {
            return Img(T){
                .v = [_]T{ scalar, imaginary },
            };
        }

        fn mul(self: Img(T), other: Img(T)) Img(T) {
            // I have no clue if this Vector version is faster than
            // just writing out the multiplcations.  Good practice
            // with Vectors, though.
            const scalarOnly: Vector(2, i32) = [_]i32{ 0, 0 };
            const imaginaryOnly: Vector(2, i32) = [_]i32{ 1, 1 };
            const flipped: Vector(2, i32) = [_]i32{ 1, 0 };
            const invertISquared: Vector(2, T) = [_]T{ -1, 1 };

            var otherS = @shuffle(T, other.v, undefined, scalarOnly);
            var otherI = @shuffle(T, other.v, undefined, imaginaryOnly);
            var selfFlipped = @shuffle(T, self.v, undefined, flipped);

            return Img(T){
                .v = (self.v * otherS) + (selfFlipped * otherI * invertISquared),
            };
        }

        fn add(self: Img(T), other: Img(T)) Img(T) {
            return Img(T){ .v = self.v + other.v };
        }
    };
}

test "imaginary" {
    const T = Img(f32);
    try expectEqual(T.init(1, 1), T.init(0, 1).add(T.init(1, 0)));
    try expectEqual(T.init(-5, 10), T.init(1, 2).mul(T.init(3, 4)));
}

pub fn Matrix(comptime T: type, rows: comptime_int, columns: comptime_int) type {
    if (!isBuiltinScalar(T)) {
        @compileError("Matrix only supports built in values");
    }

    return struct {
        // Stored in column major order.
        values: [columns][rows]T,

        const T = T;
        const rows = rows;
        const columns = columns;
        const Self = @This();

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

        pub fn mul(self: Self, other: anytype) MatrixMultiplyReturnType(Self, @TypeOf(other)) {
            const Other = @TypeOf(other);
            var r: MatrixMultiplyReturnType(Self, Other) = undefined;

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

        pub fn scale(self: Self, scaler: T) Self {
            var r: Self = undefined;
            var i: usize = 0;
            var scalerVec = @splat(rows, scaler);
            while (i < columns) : (i += 1) {
                r.values[i] = @as(Vector(rows, T), self.values[i]) * scalerVec;
            }
            return r;
        }
    };
}

fn MatrixMultiplyReturnType(a: anytype, b: anytype) type {
    if (a.T != b.T) {
        @compileError("Matrix multiplcation value types must match");
    }
    if (a.columns != b.rows) {
        @compileError("Matrix multiplcation sizes incompatible.");
    }
    return Matrix(a.T, a.rows, b.columns);
}

test "matrix multiplcation type" {
    const A = Matrix(f32, 5, 3);
    const B = Matrix(f32, 3, 4);
    const C = comptime MatrixMultiplyReturnType(A, B);
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

    var c = a.mul(b);

    try expectEqual(Matrix(f32, 2, 2).lit(.{
        94,  103,
        184, 202,
    }), c);

    // std.debug.print("\n{}\n", .{c});
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

    var c = a.add(b);

    try expectEqual(Matrix(f32, 2, 3).lit(.{
        10, 12, 14,
        16, 18, 20,
    }), c);
}

test "matrix scale" {
    var a = Matrix(f32, 2, 2).lit(.{
        1, 2,
        3, 4,
    });

    var b = a.scale(2);

    try expectEqual(Matrix(f32, 2, 2).lit(.{
        2, 4,
        6, 8,
    }), b);
}
