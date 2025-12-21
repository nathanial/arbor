/-
  ASCII Renderer Tests
  Verifies that widgets can be rendered to ASCII art.
-/
import Arbor

open Arbor
open Arbor.Text

def testSimpleBox : IO Unit := do
  IO.println "=== Simple Box Test ==="
  let widget := build do
    box { backgroundColor := some (Tincture.Color.gray 0.5), minWidth := some 10, minHeight := some 5 }

  let ascii := renderWidget widget 20 10
  IO.println ascii
  IO.println ""

def testTextWidget : IO Unit := do
  IO.println "=== Text Widget Test ==="
  let widget := build do
    text' "Hello, World!" FontId.default Tincture.Color.white .left

  let ascii := renderWidget widget 30 5
  IO.println ascii
  IO.println ""

def testRowLayout : IO Unit := do
  IO.println "=== Row Layout Test ==="
  let widget := build do
    row (gap := 1) {} #[
      box { backgroundColor := some Tincture.Color.white, minWidth := some 5, minHeight := some 3 },
      box { backgroundColor := some (Tincture.Color.gray 0.5), minWidth := some 5, minHeight := some 3 },
      box { backgroundColor := some Tincture.Color.white, minWidth := some 5, minHeight := some 3 }
    ]

  let ascii := renderWidget widget 30 10
  IO.println ascii
  IO.println ""

def testColumnLayout : IO Unit := do
  IO.println "=== Column Layout Test ==="
  let widget := build do
    column (gap := 1) {} #[
      text' "Header" FontId.default Tincture.Color.white .left,
      text' "Content" FontId.default Tincture.Color.white .left,
      text' "Footer" FontId.default Tincture.Color.white .left
    ]

  let ascii := renderWidget widget 20 10
  IO.println ascii
  IO.println ""

def testNestedLayout : IO Unit := do
  IO.println "=== Nested Layout Test ==="
  let widget := build do
    column (gap := 1) (style := { backgroundColor := some (Tincture.Color.gray 0.3), padding := Trellis.EdgeInsets.uniform 1 }) #[
      text' "Title" FontId.default Tincture.Color.white .center,
      row (gap := 2) {} #[
        box { backgroundColor := some Tincture.Color.white, minWidth := some 8, minHeight := some 3 },
        box { backgroundColor := some Tincture.Color.white, minWidth := some 8, minHeight := some 3 }
      ],
      text' "Footer" FontId.default Tincture.Color.white .center
    ]

  let ascii := renderWidgetWithBorder widget 30 12 "Window"
  IO.println ascii
  IO.println ""

def testDebugBorders : IO Unit := do
  IO.println "=== Debug Borders Test ==="
  let widget := build do
    row (gap := 2) {} #[
      center (style := { minWidth := some 10, minHeight := some 5 }) do
        text' "A" FontId.default Tincture.Color.white .center,
      center (style := { minWidth := some 10, minHeight := some 5 }) do
        text' "B" FontId.default Tincture.Color.white .center
    ]

  let ascii := renderWidgetDebug widget 30 10
  IO.println ascii
  IO.println ""

/-! ## New Rendering Modes for AI Analysis -/

def testHierarchyMode : IO Unit := do
  IO.println "=== Hierarchy Mode Test (Best for AI) ==="
  -- Create a widget with semantic names
  let widget := build do
    namedColumn "root" (gap := 1) (style := { backgroundColor := some (Tincture.Color.gray 0.3), padding := Trellis.EdgeInsets.uniform 1 }) #[
      namedText "title" "My App Title" FontId.default Tincture.Color.white .center,
      namedRow "content" (gap := 2) {} #[
        namedBox "sidebar" { backgroundColor := some Tincture.Color.white, minWidth := some 8, minHeight := some 3 },
        namedBox "main" { backgroundColor := some Tincture.Color.white, minWidth := some 12, minHeight := some 3 }
      ],
      namedText "footer" "Status: OK" FontId.default Tincture.Color.white .center
    ]

  let ascii := renderHierarchyMode widget 40 12
  IO.println ascii
  IO.println ""

def testCombinedMode : IO Unit := do
  IO.println "=== Combined Mode Test (Visual + Legend) ==="
  let widget := build do
    namedColumn "app" (gap := 1) {} #[
      namedText "header" "Header Text" FontId.default Tincture.Color.white .left,
      namedRow "buttons" (gap := 1) {} #[
        namedBox "btn1" { backgroundColor := some (Tincture.Color.gray 0.5), minWidth := some 6, minHeight := some 2 },
        namedBox "btn2" { backgroundColor := some (Tincture.Color.gray 0.5), minWidth := some 6, minHeight := some 2 }
      ]
    ]

  let ascii := renderCombinedMode widget 25 8
  IO.println ascii
  IO.println ""

def testStructureMode : IO Unit := do
  IO.println "=== Structure Mode Test (Labeled Boundaries) ==="
  let widget := build do
    namedColumn "layout" (gap := 1) {} #[
      namedText "title" "Title" FontId.default Tincture.Color.white .center,
      namedBox "content" { minWidth := some 15, minHeight := some 4 }
    ]

  let ascii := renderStructureMode widget 25 10
  IO.println ascii
  IO.println ""

def main : IO Unit := do
  testSimpleBox
  testTextWidget
  testRowLayout
  testColumnLayout
  testNestedLayout
  testDebugBorders
  IO.println "─── New AI-Friendly Modes ───"
  IO.println ""
  testHierarchyMode
  testCombinedMode
  testStructureMode
  IO.println "All ASCII renderer tests completed!"
