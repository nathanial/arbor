import Lake
open Lake DSL

package arbor where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

-- Core dependencies
require trellis from git "https://github.com/nathanial/trellis" @ "v0.0.1"
require tincture from git "https://github.com/nathanial/tincture" @ "v0.0.1"

-- Test dependencies
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.1"

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
