# Translating native vector boundaries

## Goal

Replace application-level `NativeVector Double` arguments with the Haskell
type already used by each caller:

- use `Data.Vector.Unboxed.Vector Double` for primitive PLAF storage;
- use `Numeric.LinearAlgebra.Vector Double` for frequencies, rates, fitnesses,
  and other numerical vectors; and
- do not convert callers or builtins between these two Haskell vector types.

Keep raw representations in the modules that implement the FFI itself.  Leave
result-only conversions and the generic `NativeVector a` cleanup to focused
follow-up work.

Every new function or lambda longer than three lines must have a one- or
two-line comment immediately before it explaining what it does.

## Review conclusions

1. Classify a boundary from its existing Haskell value, not from the shared
   C++ `DenseVector<double>` representation.
2. The two PLAF imports in `PopGen.Deploid` are unboxed-vector boundaries.
   The direct `NativeVector Double` arguments in numerical and substitution
   model modules are linear-algebra vector boundaries.
3. `trcall` translates the top-level argument spine.  It deliberately treats
   `EVector a`, `CMaybe a`, and `EPair a b` as opaque values and does not
   translate their elements.
4. Consequently, the direct vector argument in a signature such as
   `EVector (NativeVector Double) -> NativeVector Double -> ...` can be
   translated, but the nested occurrence cannot.
5. A blanket identity instance based on `RuntimeValue` would silently bypass
   translations for types that need them.  Identity translation must remain
   an explicit opt-in instance.
6. Implement `DefaultSignatures` first.  Default associated-type equations
   and constrained default methods then make an identity capability an empty
   instance without adding identity helper functions.
7. Translated linear-algebra outputs should be high-level containers, not raw
   identity outputs.  Concrete `Double` output instances are sufficient for
   this series; a generic `Element` output mechanism would be unused
   infrastructure.
8. Known result shapes should replace lazy native shape queries.  Do not force
   a native length or dimensions merely to discard them in the caller.
9. Do not add a C++ test executable.  Extend existing Haskell tests where the
   behavior is durable, and remove any temporary migration checks before the
   series is complete.

## Identity translation prerequisite

After `DefaultSignatures` exists, give the FFI classes identity defaults:

```haskell
class CInput a where
    type CInputType a result
    type CInputType a result = a -> result

    withCInput :: a -> CInputType a result -> result
    default withCInput
        :: CInputType a result ~ (a -> result)
        => a -> CInputType a result -> result
    withCInput value continuation = continuation value

class COutput a where
    type COutputType a
    type COutputType a = a

    fromCOutput :: COutputType a -> a
    default fromCOutput :: COutputType a ~ a => COutputType a -> a
    fromCOutput = id
```

This interface keeps identity translation explicit while reducing an identity
instance to, for example:

```haskell
instance CInput AlignmentMatrix
instance COutput (EVector a)
```

Simplify existing identity instances in the same prerequisite commit so that
the defaults are exercised immediately.  Instances with translated raw types,
including `String`, `Text`, Haskell tuples, unboxed vectors, linear-algebra
containers, and `IO`, retain explicit associated types and methods.

## Linear-algebra translation design

Add the input instance in `Numeric.LinearAlgebra.Data`:

```haskell
instance CInput (Vector a) where
    type CInputType (Vector a) result = NativeVector a -> result
    withCInput value continuation = continuation (nativeVector value)
```

The interface is generic because every linear-algebra vector occupies one raw
owner slot, independently of its element representation.

Add concrete translated outputs for `Double`:

```haskell
instance COutput (Vector Double)
instance COutput (Matrix Double)
```

`Vector Double` obtains a fallback length from the existing native size
operation.  Add native matrix row and column e-ops so `Matrix Double` can
obtain fallback dimensions from the returned object.  Make only the cached
linear-algebra size fields lazy; native payload fields retain their current
behavior.

Export these high-level metadata operations through `Numeric.LinearAlgebra`:

```haskell
overrideVectorSize :: Int -> Vector a -> Vector a
overrideMatrixDims :: Int -> Int -> Matrix a -> Matrix a
```

Their comments must state that the supplied shape is authoritative and should
only be used when guaranteed by the builtin contract.  These two operations
are preferable to exposing constructors, adding per-builtin wrappers, or
introducing a shape typeclass.  They are used immediately by every converted
builtin whose result shape is already known.

## Specified commits

Each numbered item is a separate `jj` commit.

1. **Add FFI identity defaults.**  Add the associated-type and constrained
   method defaults to `Compiler.FFI.Import`; simplify existing identity
   instances while retaining every non-identity instance explicitly.
2. **Translate linear-algebra vector inputs.**  Add `CInput (Vector a)` in
   `Numeric.LinearAlgebra.Data`.
3. **Translate Dirichlet vector arguments.**  Change
   `dirichletDensityNative` to `trcall` with two `Vector Double` arguments;
   keep the public list API and remove explicit `nativeVector` calls.
4. **Translate the Ewens mixture vectors.**  Add the empty identity
   `CInput VVI` instance, convert `ewensSamplingMixtureNative` to `trcall`, and
   retain its public list arguments and opaque `VVI` value.
5. **Translate opaque runtime-vector results.**  Add the empty identity
   `COutput (EVector a)` instance and document that its elements are not
   translated.
6. **Use unboxed vectors at PLAF boundaries.**  Convert both imports in
   `PopGen.Deploid` to `trcall` with `U.Vector Double`; remove explicit offset,
   count, owner, and `doubleVectorNativeView` plumbing.  Update the existing
   `Probability/PrimitiveVectorBuiltins` test in the same way while retaining
   its raw `U.Vector Int` selfing check.
7. **Translate SMC vector arguments.**  Add the empty identity
   `CInput AlignmentMatrix` instance, convert `smcDensityNative` and
   `smcTraceNative` to `trcall` with `Vector Double`, and preserve their public
   list arguments.
8. **Translate eigen-exponential vector arguments.**  Add empty identity
   instances for `COutput (CMaybe a)` and `CInput EigenSystem`, then convert
   `getEigensystemNative` and `lExpNative` to `trcall` with `Matrix Double` and
   `Vector Double`.  Keep `CMaybe (NativeMatrix Double)` opaque and retain the
   existing successful-result shape wrapper.
9. **Translate `Vector Double` results.**  Make the cached vector length lazy,
   add its concrete output instance and `overrideVectorSize`, and extend the
   existing linear-algebra Haskell test with translated `vectorKonstNative`
   coverage and a discarded-fallback-size check.
10. **Translate `Matrix Double` results.**  Add commented native row and column
    e-ops, make cached matrix dimensions lazy, add its concrete output instance
    and `overrideMatrixDims`, and test translated `matrixKonstNative` plus
    discarded fallback dimensions in the existing linear-algebra test.
11. **Translate floating vector operations.**  Convert both imports in
    `Numeric.Vector` to high-level `trcall`s.  Override unary output size from
    its input and binary output size with the existing singleton-broadcast
    rule.
12. **Translate core Markov vector boundaries.**  Convert `gtrSymNative`,
    `nonRevNative`, `plusGwfNative`, `equilibriumLimitNative`,
    `checkReversibleNative`, `checkStationaryNative`, and `flowNative`.  Use
    high-level matrix arguments required by `trcall` and preserve all known
    result shapes.  Leave matrix-only imports unchanged.
13. **Translate substitution-rate inputs.**  Convert
    `SModel.Markov.getEquilibriumRateNative` to `Matrix Double` and
    `Vector Double` arguments.
14. **Translate mutation-selection vectors.**  Convert both `SModel.MutSel`
    imports and replace their raw wrappers with shape overrides derived from
    the input matrix or frequency vector.
15. **Translate empirical triangular input.**  Convert only `symmetricNative`;
    WAG and LG frequencies are result-only and remain outside this series.
16. **Translate doublet frequency inputs.**  Convert `f2x4Native` and preserve
    its alphabet-derived output length.
17. **Translate codon vector inputs.**  Convert `f3x4Native` and
    `multiNucleotideNative`; retain alphabet-derived vector and matrix shapes.
18. **Translate RNA-editing frequency input.**  Convert `rnaEditingPiNative`
    and retain its alphabet-derived output length.
19. **Translate the direct modulated-model vector.**  Convert `levelProbs` and
    the result of `modulatedPiNative`.  Retain
    `EVector (NativeVector Double)` for component frequencies and add a brief
    `NOTE` explaining the shallow-translation limitation.
20. **Translate the direct weighted-frequency vector.**  Convert the
    distribution and result of `weightedFrequencyMatrixNative`.  Retain its
    nested `EVector (NativeVector Double)` argument with the same concise note.

## Postponed work

### Nested runtime vectors

`EVector (NativeVector Double)` cannot be translated by the current argument
spine mechanism.  Translating it requires rebuilding an opaque runtime vector
or introducing a collection-level translation interface.  A specialized
overlapping `CInput` instance would not naturally map one high-level element
type to the existing single raw `EVector` slot.  Reconsider this only as a
separate design change.

### Generic linear-algebra implementation

The `NativeVector a` operations in `Numeric.LinearAlgebra.Data` include both
FFI bootstrap operations and ordinary numerical operations.  Constructors,
indexing, and size queries must remain raw.  Converting the ordinary generic
operations requires polymorphic output sizing and a redesign of helpers such
as `unaryVector`, `binaryVector`, and lists of native vectors.  Do not add that
infrastructure during this concrete `Double` series.

### Result-only occurrences

Raw results in `Bio.Alignment`, decomposition results in
`Numeric.LinearAlgebra`, and WAG/LG frequency imports are not vector arguments.
Some are nested in opaque pairs or may need independent shape decisions.  They
belong to a separate output-focused cleanup.

## Verification

Use `build/gcc-16-debug-O` throughout.  Run the affected existing tests at the
following checkpoints:

1. After identity and primitive-vector changes, run `Foreign/ImportBoundary`
   and `Probability/PrimitiveVectorBuiltins`.
2. After SMC changes, run `SMCInterface`.
3. After linear-algebra output support and floating operations, run
   `Numeric/LinearAlgebra` and `Numeric/LinearAlgebra/Vector`.
4. After model conversions, run `SModel/Properties`, `Types/StrictFields/6`,
   and parse tests `19`, `x3/5`, and `25`.
5. Build all affected C++ and Haskell code with:

   ```bash
   ninja -C ../build/gcc-16-debug-O
   ```

6. Run the required integration test:

   ```bash
   meson test -C ../build/gcc-16-debug-O \
     'bali-phy:bali-phy 5d +A 50' --print-errorlogs
   ```

Finally, audit every remaining `NativeVector Double` foreign declaration.
Each must be FFI bootstrap code, result-only, a compiler ABI test, or one of
the two documented nested `EVector` cases.  If a later test exposes a problem
in one of these recent commits, fix it in an empty child of the introducing
commit and squash the correction back before completing the series.
