/-
  Arbor Text Renderer
  Interprets RenderCommands and draws them to a text Canvas.
  Useful for debugging, testing, and terminal output.
-/
import Arbor.Text.Canvas
import Arbor.Render.Command

namespace Arbor.Text

open Arbor

/-- Render state for tracking transforms and clips. -/
structure RenderState where
  canvas : Canvas
  /-- Current translation offset (x, y). -/
  translateX : Float := 0
  translateY : Float := 0
  /-- Transform stack for save/restore. -/
  transformStack : Array (Float × Float) := #[]
  /-- Clip stack (not fully implemented in text mode). -/
  clipStack : Array Rect := #[]
deriving Inhabited

namespace RenderState

def create (width height : Nat) : RenderState :=
  ⟨Canvas.create width height, 0, 0, #[], #[]⟩

/-- Convert float coordinates to canvas coordinates with current transform. -/
def toCanvasCoords (s : RenderState) (x y : Float) : (Nat × Nat) :=
  let tx := x + s.translateX
  let ty := y + s.translateY
  (tx.toUInt32.toNat, ty.toUInt32.toNat)

/-- Push current transform onto stack. -/
def pushTransform (s : RenderState) : RenderState :=
  { s with transformStack := s.transformStack.push (s.translateX, s.translateY) }

/-- Pop transform from stack. -/
def popTransform (s : RenderState) : RenderState :=
  match s.transformStack.back? with
  | some (tx, ty) =>
    { s with
      translateX := tx
      translateY := ty
      transformStack := s.transformStack.pop }
  | none => s

/-- Apply translation. -/
def translate (s : RenderState) (dx dy : Float) : RenderState :=
  { s with
    translateX := s.translateX + dx
    translateY := s.translateY + dy }

/-- Push a clip rectangle. -/
def pushClip (s : RenderState) (rect : Rect) : RenderState :=
  { s with clipStack := s.clipStack.push rect }

/-- Pop clip rectangle. -/
def popClip (s : RenderState) : RenderState :=
  { s with clipStack := s.clipStack.pop }

end RenderState

/-- Map color luminance to a fill character.
    Darker colors use denser characters. -/
def colorToFillChar (color : Color) : Char :=
  -- Use RGB average as simple luminance
  let lum := (color.r + color.g + color.b) / 3.0
  if lum < 0.2 then '█'       -- Very dark
  else if lum < 0.4 then '▓'  -- Dark
  else if lum < 0.6 then '▒'  -- Medium
  else if lum < 0.8 then '░'  -- Light
  else ' '                     -- Very light (background)

/-- Execute a single render command. -/
def executeCommand (state : RenderState) (cmd : RenderCommand) : RenderState :=
  match cmd with
  | .fillRect rect color cornerRadius =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let h := rect.height.toUInt32.toNat
    let ch := colorToFillChar color
    -- For rounded corners in text mode, we'll use box drawing
    if cornerRadius > 0 && w >= 2 && h >= 2 then
      -- Draw with rounded corners
      let canvas := state.canvas
        |> (·.fillRect (x + 1) y (w - 2) 1 ch)           -- Top edge
        |> (·.fillRect (x + 1) (y + h - 1) (w - 2) 1 ch) -- Bottom edge
        |> (·.fillRect x (y + 1) w (h - 2) ch)           -- Middle rows
        |> (·.setChar x y '╭')
        |> (·.setChar (x + w - 1) y '╮')
        |> (·.setChar x (y + h - 1) '╰')
        |> (·.setChar (x + w - 1) (y + h - 1) '╯')
      { state with canvas }
    else
      let canvas := state.canvas.fillRect x y w h ch
      { state with canvas }

  | .strokeRect rect _color _lineWidth cornerRadius =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let h := rect.height.toUInt32.toNat
    let chars := if cornerRadius > 0 then Canvas.BoxChars.rounded else Canvas.BoxChars.single
    let canvas := state.canvas.strokeBox x y w h chars
    { state with canvas }

  | .fillText text x y _font _color =>
    let (cx, cy) := state.toCanvasCoords x y
    let canvas := state.canvas.drawText cx cy text
    { state with canvas }

  | .fillTextBlock text rect _font _color align _valign =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let canvas := match align with
      | .left => state.canvas.drawText x y text
      | .center => state.canvas.drawTextCentered x y w text
      | .right => state.canvas.drawTextRight x y w text
    { state with canvas }

  | .pushClip rect =>
    state.pushClip rect

  | .popClip =>
    state.popClip

  | .pushTranslate dx dy =>
    state.pushTransform.translate dx dy

  | .popTransform =>
    state.popTransform

  | .save =>
    state.pushTransform

  | .restore =>
    state.popTransform

/-- Execute a sequence of render commands. -/
def executeCommands (state : RenderState) (cmds : Array RenderCommand) : RenderState :=
  cmds.foldl executeCommand state

/-- Render commands to an ASCII string.
    This is the main entry point for text-based rendering.

    Parameters:
    - cmds: The render commands to execute
    - width: Canvas width in characters
    - height: Canvas height in characters

    Returns: A string representation of the rendered output. -/
def renderToAscii (cmds : Array RenderCommand) (width height : Nat) : String :=
  let state := RenderState.create width height
  let finalState := executeCommands state cmds
  finalState.canvas.toString

/-- Render commands to a Canvas for further manipulation. -/
def renderToCanvas (cmds : Array RenderCommand) (width height : Nat) : Canvas :=
  let state := RenderState.create width height
  let finalState := executeCommands state cmds
  finalState.canvas

/-- Render with a border around the entire output. -/
def renderWithBorder (cmds : Array RenderCommand) (width height : Nat)
    (title : String := "") (chars : Canvas.BoxChars := .single) : String := Id.run do
  -- Render content in the inner area
  let innerWidth := width - 2
  let innerHeight := height - 2
  let innerState := RenderState.create innerWidth innerHeight
  let innerFinal := executeCommands innerState cmds
  -- Create outer canvas with border
  let mut outer := Canvas.create width height
  outer := outer.strokeBox 0 0 width height chars
  -- Copy inner content
  for row in [:innerHeight] do
    for col in [:innerWidth] do
      let cell := innerFinal.canvas.get col row
      outer := outer.set (col + 1) (row + 1) cell
  -- Add title if provided
  if !title.isEmpty then
    let titleStr := s!" {title} "
    let startX := 2
    outer := outer.drawText startX 0 titleStr
  outer.toString

/-- Pretty-print a widget layout for debugging.
    Shows boxes with their positions and sizes. -/
def debugLayout (boxes : Array (String × Rect)) (width height : Nat) : String := Id.run do
  let mut cmds : Array RenderCommand := #[]
  for (label, rect) in boxes do
    -- Stroke the box
    cmds := cmds.push (.strokeRect rect Tincture.Color.white 1 0)
    -- Add label
    cmds := cmds.push (.fillText label (rect.x + 1) (rect.y) FontId.default Tincture.Color.white)
  renderToAscii cmds width height

end Arbor.Text
