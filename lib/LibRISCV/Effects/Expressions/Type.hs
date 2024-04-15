module LibRISCV.Effects.Expressions.Type where

data Expr a =
    FromImm a |
    FromInt Int Integer |
    ZExt Int (Expr a) |
    SExt Int (Expr a) |
    Extract Int Int (Expr a) |
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
