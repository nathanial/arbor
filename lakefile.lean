import Lake
open Lake DSL

package arbor where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

-- Core dependencies
require trellis from ".." / "trellis"
require tincture from ".." / "tincture"

-- Test dependencies
require crucible from ".." / "crucible"
require staple from ".." / "staple"

@[default_target]
lean_lib Arbor where
  roots := #[`Arbor]

lean_lib ArborTests where
  roots := #[`ArborTests]
  globs := #[.submodules `ArborTests]

@[test_driver]
lean_exe arbor_tests where
  root := `ArborTests.Main

lean_exe ascii_demo where
  root := `ArborTests.AsciiRendererTests
