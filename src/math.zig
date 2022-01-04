const std = @import("std");
const expectEqual = @import("std").testing.expectEqual;

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
    // std.debug.print("\n{}\n", .{math("a", .{ .a = @as(i8, 7) })});
    try expectEqual(@as(i8, 5), math("a", .{ .a = @as(i8, 5) }));
    try expectEqual(@as(i8, 15), math("a + a + a", .{ .a = @as(i8, 5) }));
    // std.debug.print("\n{}\n", .{math("a + b", .{
    //     .a = @as(i8, 7),
    //     .b = @as(i8, 2),
    // })});
    try expectEqual(15, math("a - b", .{ .a = 20, .b = 5 }));
    // std.debug.print("\n{}\n", .{math("a - b", .{ .a = 1000000, .b = 5 })});
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
            lhs = makeBinaryOp(&self.alloc, lhs, op, rhs);
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

            .plus, .minus, .asterisk => {
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
    scalerBinaryOp: ScalerBinaryOp,
    fnBinaryOp: FnBinaryOp,
    unaryOp: UnaryOp,

    fn eval(comptime ast: *Ast, args: anytype) ast.ReturnType() {
        return switch (ast.*) {
            .identifier => |v| v.eval(args),
            .scalerBinaryOp => |v| v.eval(args),
            .fnBinaryOp => |v| v.eval(args),
            .unaryOp => |v| v.eval(args),
        };
    }

    fn ReturnType(comptime ast: *Ast) type {
        return switch (ast.*) {
            .identifier => |v| v.ReturnType,
            .scalerBinaryOp => |v| v.ReturnType,
            .fnBinaryOp => |v| v.ReturnType,
            .unaryOp => |v| v.ReturnType,
        };
    }
};

const Identifier = struct {
    name: []const u8,
    ReturnType: type,

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
                        .ReturnType = field.field_type,
                    },
                };
                return r;
            }
        } else {
            @compileError("Identifier in equation not found in passed info: " ++ name);
        }
    }

    fn eval(comptime ident: *const Identifier, args: anytype) ident.ReturnType {
        return @field(args, ident.name);
    }
};

fn makeBinaryOp(comptime alloc: *StupidAlloc, lhs: *Ast, opToken: Token.Tag, rhs: *Ast) *Ast {
    const r = alloc.next();
    const Lhs = lhs.ReturnType();
    const Rhs = rhs.ReturnType();

    if (isBuiltinScalar(Lhs) and Lhs == Rhs) {
        const op = switch (opToken) {
            .plus => ScalerBinaryOp.Op.addErr,
            .minus => ScalerBinaryOp.Op.subErr,
            .asterisk => ScalerBinaryOp.Op.mulErr,
            else => @compileError("Invalid binary operator for scaler value"),
        };

        r.* = Ast{
            .scalerBinaryOp = ScalerBinaryOp{
                .lhs = lhs,
                .op = op,
                .rhs = rhs,
                .ReturnType = Lhs,
            },
        };
        return r;
    }

    const opName = switch (opToken) {
        .plus => "add",
        .minus => "sub",
        .asterisk => "mul",
        else => @compileError("Invalid binary operator for method call"),
    };

    if (CheckTypeForBinaryMethod(opName, Lhs, Rhs)) |T| {
        r.* = Ast{ .fnBinaryOp = FnBinaryOp{
            .lhs = lhs,
            .op = opName,
            .rhs = rhs,
            .ReturnType = T,
        } };
        return r;
    }

    const altOpName = opName ++ "Swap";
    if (CheckTypeForBinaryMethod(altOpName, Rhs, Lhs)) |T| {
        r.* = Ast{ .fnBinaryOp = FnBinaryOp{
            .lhs = rhs,
            .op = altOpName,
            .rhs = lhs,
            .ReturnType = T,
        } };
        return r;
    }
    @compileError(@typeName(Lhs) ++ " and " ++ @typeName(Rhs) ++ " are incompatible for " ++ opName ++ ".");
}

// If A has a method named opName, which takes B, return the return type.  Otherwise null.
fn CheckTypeForBinaryMethod(comptime opName: [:0]const u8, comptime A: type, comptime B: type) ?type {
    if (isBuiltinScalar(A)) {
        return null;
    }
    if (!@hasDecl(A, opName)) {
        return null;
    }

    const declInfo = std.meta.declarationInfo(A, opName);
    const FnType = switch (declInfo.data) {
        .Type => return null,
        .Var => return null,
        .Fn => |f| f.fn_type,
    };

    const fnTypeInfo = switch (@typeInfo(FnType)) {
        .Fn => |f| f,
        else => unreachable,
    };

    if (fnTypeInfo.args.len == 2 and
        fnTypeInfo.args[0].arg_type == A and
        fnTypeInfo.args[1].arg_type == B)
    {
        return fnTypeInfo.return_type;
    }

    const fnName = opName ++ "ReturnType";
    if (@hasDecl(A, fnName)) {
        const R = @field(A, fnName)(B);
        if (R != void) {
            return R;
        }
    }
    return null;
}

const ScalerBinaryOp = struct {
    lhs: *Ast,
    op: Op,
    rhs: *Ast,
    ReturnType: type,

    const Op = enum {
        addErr,
        subErr,
        mulErr,
    };

    fn eval(comptime self: *const ScalerBinaryOp, args: anytype) self.ReturnType {
        const lhs = self.lhs.eval(args);
        const rhs = self.rhs.eval(args);

        return switch (self.op) {
            .addErr => lhs + rhs,
            .subErr => lhs - rhs,
            .mulErr => lhs * rhs,
        };
    }
};

const FnBinaryOp = struct {
    lhs: *Ast,
    op: [:0]const u8,
    rhs: *Ast,
    ReturnType: type,

    fn eval(comptime self: *const FnBinaryOp, args: anytype) self.ReturnType {
        const lhs = self.lhs.eval(args);
        const rhs = self.rhs.eval(args);

        return @field(lhs, self.op)(rhs);
    }
};

const UnaryOp = struct {
    op: Op,
    rhs: *Ast,
    ReturnType: type,

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
            .ReturnType = rhs.ReturnType(),
        } };
        return r;
    }

    fn eval(comptime self: *const UnaryOp, args: anytype) self.ReturnType {
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
        asterisk,
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
                    '*' => {
                        r.tag = .asterisk;
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
        r.end = self.index;
        return r;
    }

    fn source(self: *Tokenizer, token: Token) []const u8 {
        return self.buffer[token.start..token.end];
    }
};
