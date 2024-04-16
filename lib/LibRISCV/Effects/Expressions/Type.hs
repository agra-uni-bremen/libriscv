module LibRISCV.Effects.Expressions.Type where

data Expr a =
    FromImm a |
    -- ^ Create a new 'Expr' from a given immediate value.
    FromInt Int Integer |
    -- ^ Create an expression from a concrete 'Integer', treating it as a fixed-width two's
    -- complement value of the size (in bits) specified by the first argument to the constructor.
    ZExt Int (Expr a) |
    -- ^ Zero extend an expression by adding the given amount of bits to it, for example,
    -- an 8-bit value can be zero-extended to 32-bit by passing 24 as the first argument.
    SExt Int (Expr a) |
    -- ^ Same as 'ZExt' but performs sign-extension instead of zero-extension.
    Extract Int Int (Expr a) |
    -- ^ Extract a specified amount of bits from an expression. The first argument specifies
    -- the first bit which should be extracted, the second specifies the amount of bits to extract.
    Add  (Expr a) (Expr a) |
    Sub  (Expr a) (Expr a) |
    Eq   (Expr a) (Expr a) |
    Slt  (Expr a) (Expr a) |
    Sge  (Expr a) (Expr a) |
    Ult  (Expr a) (Expr a) |
    Uge  (Expr a) (Expr a) |
    And  (Expr a) (Expr a) |
    Or   (Expr a) (Expr a) |
    Xor  (Expr a) (Expr a) |
    LShl (Expr a) (Expr a) |
    LShr (Expr a) (Expr a) |
    AShr (Expr a) (Expr a) |
    Mul  (Expr a) (Expr a) |
    UDiv (Expr a) (Expr a) |
    SDiv (Expr a) (Expr a) |
    URem (Expr a) (Expr a) |
    SRem (Expr a) (Expr a)

-- XXX: For binary operations, consider creation of a smart constructor which
-- allows passing an immediate value of type `a` as a second parameter. See
-- LibRISCV.Effects.Expressions.Generator for more information.
