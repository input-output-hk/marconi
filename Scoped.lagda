\begin{code}
module Scoped where
\end{code}

\begin{code}
open import Data.Nat
open import Data.Fin hiding (_-_)
open import Data.List hiding (map; _++_)

open import Builtin.Constant.Type
open import Builtin
open import Raw
\end{code}

\begin{code}
data ScopedKind : Set where
  *   : ScopedKind
  #   : ScopedKind
  _⇒_ : ScopedKind → ScopedKind → ScopedKind

{-# FOREIGN GHC import Scoped #-}
{-# COMPILE GHC ScopedKind = data ScKind (ScKiStar | ScKiSize | ScKiFun) #-}

data ScopedTy (n : ℕ) : Set where
  `    : Fin n → ScopedTy n
  _⇒_  : ScopedTy n → ScopedTy n → ScopedTy n
  Π    : ScopedKind → ScopedTy (suc n) → ScopedTy n
  ƛ    : ScopedKind → ScopedTy (suc n) → ScopedTy n
  _·_  : ScopedTy n → ScopedTy n → ScopedTy n
  con  : TyCon → ScopedTy n
  size : ℕ → ScopedTy n

--{-# COMPILE GHC ScopedTy = data ScTy (ScTyVar | ScTyFun | ScTyPi | ScTyLambda | ScTyApp | ScTyCon | ScTySize) #-}

data Weirdℕ : Set where
  Z : Weirdℕ
  S : Weirdℕ → Weirdℕ
  T : Weirdℕ → Weirdℕ

data WeirdFin : Weirdℕ → Set where
  Z : ∀{n} → WeirdFin (S n)
  S : ∀{n} → WeirdFin n → WeirdFin (S n)
  T : ∀{n} → WeirdFin n → WeirdFin (T n)
  
∥_∥ : Weirdℕ → ℕ
∥ Z   ∥ = zero
∥ S n ∥ = ∥ n ∥
∥ T n ∥ = suc ∥ n ∥


open import Data.Integer
open import Data.String

-- could index by size here, is there any point?
data SizedTermCon : Set where
  integer    : ∀ s
    → (i : ℤ)
    → BoundedI s i
    → SizedTermCon
  bytestring : ∀ s
    → (b : ByteString)
    → BoundedB s b
    → SizedTermCon
  size       : ℕ → SizedTermCon
  string     : String → SizedTermCon

data ScopedTm : Weirdℕ → Set where
  `    : ∀{n} → WeirdFin n → ScopedTm n 
  Λ    : ∀{n} → ScopedKind → ScopedTm (T n) → ScopedTm n
  _·⋆_ : ∀{n} → ScopedTm n → ScopedTy ∥ n ∥ → ScopedTm n
  ƛ    : ∀{n} → ScopedTy ∥ n ∥ → ScopedTm (S n) → ScopedTm n
  _·_  : ∀{n} → ScopedTm n → ScopedTm n → ScopedTm n
  con  : ∀{n} → SizedTermCon → ScopedTm n
  error : ∀{n} → ScopedTy ∥ n ∥ → ScopedTm n
  builtin : ∀{n} → Builtin → List (ScopedTy ∥ n ∥) → List (ScopedTm n)
          → ScopedTm n

-- SCOPE CHECKING / CONVERSION FROM RAW TO SCOPED

-- should just use ordinary kind for everything
deBruijnifyK : RawKind → ScopedKind
deBruijnifyK * = *
deBruijnifyK (K ⇒ J) = deBruijnifyK K ⇒ deBruijnifyK J
deBruijnifyK # = #

open import Data.Vec hiding (_>>=_; map; _++_; [_])
open import Data.Maybe
open import Data.String
open import Relation.Nullary
open import Category.Monad
import Level
open RawMonad {f = Level.zero} monad

velemIndex : String → ∀{n} → Vec String n → Maybe (Fin n)
velemIndex x [] = nothing
velemIndex x (x' ∷ xs) with x Data.String.≟ x'
velemIndex x (x' ∷ xs) | yes p = just zero
velemIndex x (x' ∷ xs) | no ¬p = map Fin.suc (velemIndex x xs)

deBruijnifyTy : ∀{n} → Vec String n → RawTy → Maybe (ScopedTy n)
deBruijnifyTy g (` α) = map ` (velemIndex α g)
deBruijnifyTy g (A ⇒ B) = do
  A ← deBruijnifyTy g A
  B ← deBruijnifyTy g B
  return (A ⇒ B)
deBruijnifyTy g (Π x K B) =
  map (Π (deBruijnifyK K)) (deBruijnifyTy (x ∷ g) B)
deBruijnifyTy g (ƛ x K B) =
  map (ƛ (deBruijnifyK K)) (deBruijnifyTy (x ∷ g) B)
deBruijnifyTy g (A · B) = do
  A ← deBruijnifyTy g A
  B ← deBruijnifyTy g B
  return (A · B)
deBruijnifyTy g (con b)     = just (con b)
deBruijnifyTy g (size n)    = just (size n)

data WeirdVec (X : Set) : Weirdℕ → Set where
  nil : WeirdVec X Z
  consS : ∀{n} → X → WeirdVec X n → WeirdVec X (S n)
  consT : ∀{n} → X → WeirdVec X n → WeirdVec X (T n)

∥_∥Vec : ∀{X n} → WeirdVec X n → Vec X ∥ n ∥
∥ nil        ∥Vec = []
∥ consS x xs ∥Vec = ∥ xs ∥Vec
∥ consT x xs ∥Vec = x ∷ ∥ xs ∥Vec

velemIndexWeird : String → ∀{n} → WeirdVec String n → Maybe (WeirdFin n)
velemIndexWeird x nil = nothing
velemIndexWeird x (consS x' xs) with x Data.String.≟ x'
velemIndexWeird x (consS x' xs) | yes p = just Z
velemIndexWeird x (consS _  xs) | no ¬p = map S (velemIndexWeird x xs)
velemIndexWeird x (consT _  xs) = map T (velemIndexWeird x xs)

-- this could return a proof that that something is out of bounds
checkSize : RawTermCon → Maybe (SizedTermCon)
checkSize (integer s i) with boundedI? s i
checkSize (integer s i) | yes p    = just (integer s i p)
checkSize (integer s i) | no ¬p    = nothing
checkSize (bytestring s b) with boundedB? s b
checkSize (bytestring s b) | yes p = just (bytestring s b p)
checkSize (bytestring s b) | no ¬p = nothing
checkSize (size s)                 = just (size s)
checkSize (string x)               = just (string x)

deBruijnifyTm : ∀{n} → WeirdVec String n → RawTm → Maybe (ScopedTm n)
deBruijnifyTm g (` x) = map ` (velemIndexWeird x g)
deBruijnifyTm g (ƛ x A L) = do
  A ← deBruijnifyTy ∥ g ∥Vec A
  L ← deBruijnifyTm (consS x g) L
  return (ƛ A L)
deBruijnifyTm g (L · M) = do
  L ← deBruijnifyTm g L
  M ← deBruijnifyTm g M
  return (L · M)
deBruijnifyTm g (Λ x K L) =
  map (Λ (deBruijnifyK K)) (deBruijnifyTm (consT x g) L)
deBruijnifyTm g (L ·⋆ A) = do
  L ← deBruijnifyTm g L
  A ← deBruijnifyTy ∥ g ∥Vec A
  return (L ·⋆ A)
deBruijnifyTm g (con t) = map con (checkSize t)
deBruijnifyTm g (error A) = map error (deBruijnifyTy ∥ g ∥Vec A)
deBruijnifyTm g (builtin b) = just (builtin b [] []) 
\end{code}

-- SATURATION OF BUILTINS


\begin{code}
open import Data.Product
open import Data.Sum

builtinMatcher : ∀{n} → ScopedTm n → (Builtin × List (ScopedTy ∥ n ∥) × List (ScopedTm n)) ⊎ ScopedTm n
builtinMatcher (builtin b As ts) = inj₁ (b , As , ts)
builtinMatcher t              = inj₂ t

arity : Builtin → ℕ
arity _ = 2

arity⋆ : Builtin → ℕ
arity⋆ _ = 1

open import Relation.Nullary

builtinEater : ∀{n} → Builtin
 → List (ScopedTy ∥ n ∥) → List (ScopedTm n) → ScopedTm n → ScopedTm n
builtinEater b As ts u
  with Data.List.length ts Data.Nat.+ 1 Data.Nat.≤? arity b
builtinEater b As ts u | yes p = builtin b As (ts Data.List.++ [ u ])
builtinEater b As ts u | no ¬p = builtin b As ts · u

builtinEater⋆ : ∀{n} → Builtin
  → List (ScopedTy ∥ n ∥) → List (ScopedTm n) → ScopedTy ∥ n ∥ → ScopedTm n
builtinEater⋆ b As ts A
  with Data.List.length ts Data.Nat.+ 1 Data.Nat.≤? arity⋆ b
builtinEater⋆ b As ts A | yes p = builtin b (As Data.List.++ [ A ]) ts
builtinEater⋆ b As ts A | no ¬p = builtin b As ts ·⋆ A

saturate : ∀{n} → ScopedTm n → ScopedTm n
saturate (` x)          = ` x
saturate (Λ K t)        = Λ K (saturate t)
saturate (t ·⋆ A)       with builtinMatcher (saturate t)
saturate (t ·⋆ A) | inj₁ (b , As , ts) = builtinEater⋆ b As ts A
saturate (t ·⋆ A) | inj₂ t'            = t' ·⋆ A
saturate (ƛ A t)        = ƛ A (saturate t) 
saturate (t · u)        with builtinMatcher (saturate t)
saturate (t · u) | inj₁ (b , As , ts) = builtinEater b As ts (saturate u)
saturate (t · u) | inj₂ t'            = t' · saturate u 
saturate (con c)        = con c
saturate (error A)      = error A
saturate (builtin b As ts) = builtin b As ts
  -- I don't think As or ts can be unsaturated, could be enforced by
  -- seperate representations for sat and unsat terms
\end{code}

\begin{code}
unDeBruijnifyK : ScopedKind → RawKind
unDeBruijnifyK * = *
unDeBruijnifyK (K ⇒ J) = unDeBruijnifyK K ⇒ unDeBruijnifyK J
unDeBruijnifyK # = #
\end{code}

\begin{code}
wtoℕ : ∀{n} → WeirdFin n → ℕ
wtoℕ Z = zero
wtoℕ (S i) = ℕ.suc (wtoℕ i)
wtoℕ (T i) = ℕ.suc (wtoℕ i)
\end{code}

\begin{code}
unDeBruijnifyC : SizedTermCon → RawTermCon
unDeBruijnifyC (integer s i x) = integer s i
unDeBruijnifyC (bytestring s b x) = bytestring s b
unDeBruijnifyC (size x) = size x
unDeBruijnifyC (string x) = string x
\end{code}

\begin{code}
unDeBruijnify⋆ : ∀{n} → Fin n → ScopedTy n → RawTy
unDeBruijnify⋆ i (` x) = ` ((Data.Integer.show (ℤ.pos (toℕ i) - ℤ.pos (toℕ x))))
unDeBruijnify⋆ i (A ⇒ B) = unDeBruijnify⋆ i A ⇒ unDeBruijnify⋆ i B
unDeBruijnify⋆ i (Π K A) = Π
  ((Data.Integer.show (ℤ.pos (toℕ i))))
  (unDeBruijnifyK K)
  (unDeBruijnify⋆ (Fin.suc i) A)
unDeBruijnify⋆ i (ƛ K A) = ƛ
  (Data.Integer.show (ℤ.pos (toℕ i)))
  (unDeBruijnifyK K)
  (unDeBruijnify⋆ (Fin.suc i) A)
unDeBruijnify⋆ i (A · B) = unDeBruijnify⋆ i A · unDeBruijnify⋆ i B
unDeBruijnify⋆ i (con c) = con c
unDeBruijnify⋆ i (size j) = size j
\end{code}

This should be run on unsaturated terms
\begin{code}
unDeBruijnify : ∀{n} → Fin ∥ n ∥ → WeirdFin n → ScopedTm n → RawTm
unDeBruijnify i⋆ i (` x) = ` (Data.Integer.show (ℤ.pos (wtoℕ i) - ℤ.pos (wtoℕ x)))
unDeBruijnify i⋆ i (Λ K t) = Λ
  (Data.Integer.show (ℤ.pos (wtoℕ i)))
  (unDeBruijnifyK K)
  (unDeBruijnify (Fin.suc i⋆) (T i) t)
unDeBruijnify i⋆ i (t ·⋆ A) = unDeBruijnify i⋆ i t ·⋆ unDeBruijnify⋆ i⋆ A
unDeBruijnify i⋆ i (ƛ A t) = ƛ
  ((Data.Integer.show (ℤ.pos (wtoℕ i))))
  (unDeBruijnify⋆ i⋆ A)
  (unDeBruijnify i⋆ (S i) t)
unDeBruijnify i⋆ i (t · u) = unDeBruijnify i⋆ i t · unDeBruijnify i⋆ i u
unDeBruijnify i⋆ i (con c) = con (unDeBruijnifyC c)
unDeBruijnify i⋆ i (error A) = error (unDeBruijnify⋆ i⋆ A)
unDeBruijnify i⋆ i (builtin b _ _) = builtin b
\end{code}

-- UGLY PRINTING

\begin{code}
open import Data.String

uglyWeirdFin : ∀{n} → WeirdFin n → String
uglyWeirdFin Z = "0"
uglyWeirdFin (T x) = "(T " ++ uglyWeirdFin x ++ ")"
uglyWeirdFin (S x) = "(S " ++ uglyWeirdFin x ++ ")"

{-
uglyTermCon : TermCon → String
uglyTermCon (integer x) = "(integer " ++ Data.Integer.show x ++ ")"
uglyTermCon (bytestring x) = "bytestring"
uglyTermCon size = "size"
-}
postulate showNat : ℕ → String

{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC showNat = T.pack . show #-}

uglyBuiltin : Builtin → String
uglyBuiltin addInteger = "addInteger"
uglyBuiltin _ = "other"
ugly : ∀{n} → ScopedTm n → String
ugly (` x) = "(` " ++ uglyWeirdFin x ++ ")"
ugly (ƛ _ t) = "(ƛ " ++ ugly t ++ ")"
ugly (t · u) = "( " ++ ugly t ++ " · " ++ ugly u ++ ")"
ugly (Λ _ t) = "(Λ " ++ ugly t ++ ")"
ugly (t ·⋆ A) = "( " ++ ugly t ++ " ·⋆ " ++ "TYPE" ++ ")"

ugly (con c) = "(con " -- ++ uglyTermCon c ++ ")"
ugly (builtin b As ts) =
  "(builtin " ++
  uglyBuiltin b ++
  " " ++
  Data.Integer.show (Data.Integer.ℤ.pos (Data.List.length As)) ++
  " " ++ 
  Data.Integer.show (Data.Integer.ℤ.pos (Data.List.length ts))
  ++ ")"
ugly (error _) = "error _"
\end{code}
