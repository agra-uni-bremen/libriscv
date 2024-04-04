{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}

module LibRISCV.Semantics.Utils where

import Control.Monad.Freer
import LibRISCV.Effects.Decoding.Language
import LibRISCV.Effects.Operations.Language
import qualified LibRISCV.Effects.Operations.Language as Op
import LibRISCV.Effects.Expressions.Language
import LibRISCV.Effects.Expressions.Expr
import Control.Applicative

-- TODO add newTypes for type safety
-- decode and read register
decodeAndReadIType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadIType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) decodeRD decodeImmI

-- decode and read register
decodeAndReadBType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadBType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeImmB

-- decode and read register
decodeAndReadSType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadSType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeImmS

-- decode and read register
decodeAndReadRType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadRType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeRD

-- decode and read register
decodeJType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v)
decodeJType = liftA2 (,) decodeRD decodeImmJ

decodeUType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v)
decodeUType = liftA2 (,) decodeRD decodeImmU

writeRegister :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => v -> Expr v -> Eff r ()
writeRegister reg e = eval e >>= Op.writeRegister reg

readRegister :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Expr v -> Eff r v
readRegister e = eval e >>= Op.readRegister 

writePC :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Expr v -> Eff r ()
writePC e = eval e >>= Op.writePC

load :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Size -> Expr v -> Eff r v
load s e = eval e >>= Op.load s

store :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Size -> Expr v -> Expr v -> Eff r ()
store s r e = do
    reg <- eval r
    v   <- eval e
    Op.store s reg v

