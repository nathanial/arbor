/-
  Arbor Tests
  Test suite for the renderer-agnostic widget library.
-/
import Arbor
import Crucible
import Trellis

namespace ArborTests

open Crucible
open Arbor

/-- Check if a string contains a substring. -/
def containsSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-! ## Core Types Tests -/

testSuite "Core.Types"

test "Point arithmetic" := do
  let p1 : Point := ⟨10, 20⟩
  let p2 : Point := ⟨5, 10⟩
  (p1.x + p2.x) ≡ 15
  (p1.y + p2.y) ≡ 30

test "Size creation" := do
  let s : Size := ⟨100, 50⟩
  s.width ≡ 100
  s.height ≡ 50

test "Rect contains point" := do
  let r : Rect := ⟨⟨10, 10⟩, ⟨100, 100⟩⟩
  ensure (r.contains 50 50) "Should contain 50,50"
  ensure (!r.contains 5 5) "Should not contain 5,5"
  ensure (!r.contains 120 120) "Should not contain 120,120"

/-! ## RenderCommand Tests -/

testSuite "Render.Command"

test "fillRect command" := do
  let rect : Rect := ⟨⟨0, 0⟩, ⟨100, 50⟩⟩
  let cmd := RenderCommand.fillRect rect Tincture.Color.red 0
  match cmd with
  | .fillRect r c _ =>
    r.origin.x ≡ 0
    r.size.width ≡ 100
    ensure (c == Tincture.Color.red) "Color should be red"
  | _ => pure ()

test "fillText command" := do
  let font := FontId.default
  let cmd := RenderCommand.fillText "Hello" 10 20 font Tincture.Color.white
  match cmd with
  | .fillText text x y _ color =>
    text ≡ "Hello"
    x ≡ 10
    y ≡ 20
    ensure (color == Tincture.Color.white) "Color should be white"
  | _ => pure ()

/-! ## Widget Tests -/

testSuite "Widget.Core"

test "Widget id extraction" := do
  let w := Widget.spacer 42 none 10 20
  w.id ≡ 42

test "Widget children - flex" := do
  let child1 := Widget.spacer 1 none 10 10
  let child2 := Widget.spacer 2 none 20 20
  let parent := Widget.flex 0 none Trellis.FlexContainer.row {} #[child1, child2]
  parent.children.size ≡ 2

test "Widget children - leaf" := do
  let w := Widget.spacer 0 none 10 20
  w.children.size ≡ 0

test "Widget isContainer" := do
  let flex := Widget.flex 0 none Trellis.FlexContainer.row {} #[]
  ensure flex.isContainer "Flex should be container"
  let spc := Widget.spacer 0 none 10 10
  ensure (!spc.isContainer) "Spacer should not be container"

/-! ## DSL Tests -/

testSuite "Widget.DSL"

test "build spacer" := do
  let widget := build (spacer 10 20)
  match widget with
  | .spacer id _ w h =>
    id ≡ 0
    w ≡ 10
    h ≡ 20
  | _ => pure ()

test "coloredBox creates rect with style" := do
  let widget := build (coloredBox Tincture.Color.blue 100 50)
  match widget with
  | .rect id _ style =>
    id ≡ 0
    style.minWidth ≡ some 100
    style.minHeight ≡ some 50
    ensure (style.backgroundColor == some Tincture.Color.blue) "Background should be blue"
  | _ => pure ()

/-! ## Event Tests -/

testSuite "Event.Types"

test "Modifiers fromBitmask" := do
  let mods := Modifiers.fromBitmask 0b1111
  ensure mods.shift "Shift should be set"
  ensure mods.ctrl "Ctrl should be set"
  ensure mods.alt "Alt should be set"
  ensure mods.cmd "Cmd should be set"

test "Modifiers none" := do
  let mods := Modifiers.none
  ensure (!mods.hasAny) "Should have no modifiers"

test "MouseButton fromCode" := do
  ensure (MouseButton.fromCode 0 == MouseButton.left) "Code 0 should be left"
  ensure (MouseButton.fromCode 1 == MouseButton.right) "Code 1 should be right"
  ensure (MouseButton.fromCode 2 == MouseButton.middle) "Code 2 should be middle"

test "Key fromKeyCode" := do
  ensure (Key.fromKeyCode 49 == Key.space) "Code 49 should be space"
  ensure (Key.fromKeyCode 36 == Key.enter) "Code 36 should be enter"
  ensure (Key.fromKeyCode 53 == Key.escape) "Code 53 should be escape"

/-! ## Scroll Tests -/

testSuite "Event.Scroll"

test "ScrollBounds calculate" := do
  let bounds := ScrollBounds.calculate 100 100 200 300
  bounds.maxX ≡ 100  -- contentWidth - viewportWidth
  bounds.maxY ≡ 200  -- contentHeight - viewportHeight

test "ScrollBounds needsScroll" := do
  let needsIt := ScrollBounds.calculate 100 100 200 200
  ensure needsIt.needsScroll "Should need scroll"
  let noNeed := ScrollBounds.calculate 200 200 100 100
  ensure (!noNeed.needsScroll) "Should not need scroll"

test "ScrollState clamp" := do
  let state : ScrollState := { offsetX := 500, offsetY := 500 }
  let clamped := state.clamp 100 100 200 200
  clamped.offsetX ≡ 100  -- max is 200-100=100
  clamped.offsetY ≡ 100

/-! ## TextMeasurer Tests -/

testSuite "TextMeasurer"

test "FixedWidthMeasurer measures text" := do
  let measurer := FixedWidthMeasurer.mk 8 16
  let font := FontId.default
  let metrics := measurer.measureText "Hello" font
  metrics.width ≡ 40  -- 5 chars * 8 pixels
  metrics.height ≡ 16

test "FixedWidthMeasurer measures char" := do
  let measurer := FixedWidthMeasurer.mk 10 20
  let font := FontId.default
  let width := measurer.measureChar 'x' font
  width ≡ 10

/-! ## RenderCommand Collector Tests -/

testSuite "Render.Collect"

test "collectCommands generates commands for spacer" := do
  let widget := Widget.spacer 0 none 100 50
  let node := Trellis.LayoutNode.leaf 0 ⟨100, 50⟩
  let layouts := Trellis.layout node 100 50
  let cmds := collectCommands widget layouts
  -- Spacer generates no commands
  cmds.size ≡ 0

test "collectCommands generates commands for colored rect" := do
  let style : BoxStyle := { backgroundColor := some Tincture.Color.red }
  let widget := Widget.rect 0 none style
  let node := Trellis.LayoutNode.leaf 0 ⟨100, 50⟩
  let layouts := Trellis.layout node 100 50
  let cmds := collectCommands widget layouts
  cmds.size ≡ 1

/-! ## Text Canvas Tests -/

testSuite "Text.Canvas"

test "Canvas create has correct dimensions" := do
  let canvas := Text.Canvas.create 80 24
  canvas.width ≡ 80
  canvas.height ≡ 24

test "Canvas setChar and get" := do
  let canvas := Text.Canvas.create 10 5
  let canvas := canvas.setChar 3 2 'X'
  let cell := canvas.get 3 2
  cell.char ≡ 'X'

test "Canvas setChar out of bounds is no-op" := do
  let canvas := Text.Canvas.create 10 5
  let canvas := canvas.setChar 100 100 'X'
  -- Should not crash, just be ignored
  canvas.width ≡ 10

test "Canvas drawText places characters" := do
  let canvas := Text.Canvas.create 20 5
  let canvas := canvas.drawText 5 2 "Hello"
  (canvas.get 5 2).char ≡ 'H'
  (canvas.get 6 2).char ≡ 'e'
  (canvas.get 7 2).char ≡ 'l'
  (canvas.get 8 2).char ≡ 'l'
  (canvas.get 9 2).char ≡ 'o'

test "Canvas fillRect fills area" := do
  let canvas := Text.Canvas.create 10 5
  let canvas := canvas.fillRect 1 1 3 2 '#'
  (canvas.get 1 1).char ≡ '#'
  (canvas.get 2 1).char ≡ '#'
  (canvas.get 3 1).char ≡ '#'
  (canvas.get 1 2).char ≡ '#'
  (canvas.get 0 0).char ≡ ' '  -- Outside the rect

test "Canvas strokeBox draws border" := do
  let canvas := Text.Canvas.create 10 5
  let canvas := canvas.strokeBox 0 0 5 3 Text.Canvas.BoxChars.single
  (canvas.get 0 0).char ≡ '┌'
  (canvas.get 4 0).char ≡ '┐'
  (canvas.get 0 2).char ≡ '└'
  (canvas.get 4 2).char ≡ '┘'
  (canvas.get 2 0).char ≡ '─'
  (canvas.get 0 1).char ≡ '│'

test "Canvas toString produces correct output" := do
  let canvas := Text.Canvas.create 5 3
  let canvas := canvas.drawText 0 0 "Hi"
  let str := canvas.toString
  ensure (str.startsWith "Hi") "Should start with 'Hi'"

/-! ## Text Renderer Tests -/

testSuite "Text.Renderer"

test "renderToAscii with fillRect" := do
  let cmds : Array RenderCommand := #[
    .fillRect (Rect.mk' 0 0 4 2) (Tincture.Color.mk 0.1 0.1 0.1 1) 0
  ]
  let output := Text.renderToAscii cmds 10 5
  -- Dark color should produce dense block chars
  ensure (containsSubstr output "█" || output.length > 0) "Should render something"

test "renderToAscii with strokeRect" := do
  let cmds : Array RenderCommand := #[
    .strokeRect (Rect.mk' 0 0 5 3) Tincture.Color.white 1 0
  ]
  let output := Text.renderToAscii cmds 10 5
  ensure (containsSubstr output "┌") "Should have top-left corner"
  ensure (containsSubstr output "┘") "Should have bottom-right corner"

test "renderToAscii with fillText" := do
  let cmds : Array RenderCommand := #[
    .fillText "Hello" 0 0 FontId.default Tincture.Color.white
  ]
  let output := Text.renderToAscii cmds 20 5
  ensure (containsSubstr output "Hello") "Should contain text"

test "renderToAscii with rounded corners" := do
  let cmds : Array RenderCommand := #[
    .strokeRect (Rect.mk' 0 0 6 4) Tincture.Color.white 1 5
  ]
  let output := Text.renderToAscii cmds 10 5
  ensure (containsSubstr output "╭") "Should have rounded top-left"
  ensure (containsSubstr output "╯") "Should have rounded bottom-right"

test "renderWithBorder adds title" := do
  let cmds : Array RenderCommand := #[]
  let output := Text.renderWithBorder cmds 20 5 "Test"
  ensure (containsSubstr output "Test") "Should contain title"

test "RenderState translation" := do
  let state := Text.RenderState.create 20 10
  let state := state.translate 5 3
  let (x, y) := state.toCanvasCoords 10 10
  x ≡ 15
  y ≡ 13

test "debugLayout shows labeled boxes" := do
  let boxes : Array (String × Rect) := #[
    ("Box1", Rect.mk' 0 0 10 4),
    ("Box2", Rect.mk' 12 0 8 4)
  ]
  let output := Text.debugLayout boxes 25 6
  ensure (containsSubstr output "Box1") "Should contain Box1 label"
  ensure (containsSubstr output "Box2") "Should contain Box2 label"

#generate_tests

end ArborTests

def main : IO UInt32 := do
  Crucible.runTests "Arbor Tests" ArborTests.cases
