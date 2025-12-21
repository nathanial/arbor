/-
  Arbor Render Command Collector
  Convert widget trees + layouts into RenderCommand arrays.
  This is the key abstraction that makes rendering backend-independent.
-/
import Arbor.Widget.Core
import Arbor.Render.Command
import Trellis

namespace Arbor

/-- Render command collector state. -/
structure CollectState where
  commands : Array RenderCommand := #[]
deriving Inhabited

/-- Collector monad for accumulating render commands. -/
abbrev CollectM := StateM CollectState

namespace CollectM

/-- Emit a single render command. -/
def emit (cmd : RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands.push cmd }

/-- Emit multiple render commands. -/
def emitAll (cmds : Array RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands ++ cmds }

/-- Run the collector and return the commands. -/
def execute {α : Type} (m : CollectM α) : Array RenderCommand :=
  (StateT.run m {}).2.commands

end CollectM

/-- Collect box background and border render commands based on BoxStyle. -/
def collectBoxStyle (rect : Trellis.LayoutRect) (style : BoxStyle) : CollectM Unit := do
  let r : Rect := ⟨⟨rect.x, rect.y⟩, ⟨rect.width, rect.height⟩⟩

  -- Background
  if let some bg := style.backgroundColor then
    CollectM.emit (.fillRect r bg style.cornerRadius)

  -- Border
  if let some bc := style.borderColor then
    if style.borderWidth > 0 then
      CollectM.emit (.strokeRect r bc style.borderWidth style.cornerRadius)

/-- Collect render commands for wrapped text with alignment. -/
def collectWrappedText (contentRect : Trellis.LayoutRect) (font : FontId)
    (color : Color) (align : TextAlign) (textLayout : TextLayout)
    (lineHeight : Float) : CollectM Unit := do
  -- Start y at top of content rect
  let mut y := contentRect.y

  for line in textLayout.lines do
    -- Calculate x based on alignment
    let x := match align with
      | .left => contentRect.x
      | .center => contentRect.x + (contentRect.width - line.width) / 2
      | .right => contentRect.x + contentRect.width - line.width

    CollectM.emit (.fillText line.text x y font color)
    y := y + lineHeight

/-- Collect render commands for single-line text (no wrapping). -/
def collectSingleLineText (contentRect : Trellis.LayoutRect) (text : String)
    (font : FontId) (color : Color) (align : TextAlign) (textWidth : Float) : CollectM Unit := do
  -- Calculate x based on alignment
  let x := match align with
    | .left => contentRect.x
    | .center => contentRect.x + (contentRect.width - textWidth) / 2
    | .right => contentRect.x + contentRect.width - textWidth

  CollectM.emit (.fillText text x contentRect.y font color)

/-- Collect render commands for a widget tree using computed layout positions.
    The widget should have been measured (text layouts computed) before calling this.
    Returns an array of RenderCommands that can be executed by any backend. -/
partial def collectWidget (w : Widget) (layouts : Trellis.LayoutResult) : CollectM Unit := do
  let some computed := layouts.get w.id | return
  let borderRect := computed.borderRect
  let contentRect := computed.contentRect

  match w with
  | .rect _ style =>
    collectBoxStyle borderRect style

  | .text _ content font color align _ textLayoutOpt =>
    match textLayoutOpt with
    | some textLayout =>
      -- Use estimated line height (we don't have font metrics here,
      -- so we estimate from the textLayout height / number of lines)
      let lineHeight := if textLayout.lines.isEmpty then 16.0
                        else textLayout.totalHeight / textLayout.lines.size.toFloat
      collectWrappedText contentRect font color align textLayout lineHeight
    | none =>
      -- Fallback to single-line rendering with estimated width
      collectSingleLineText contentRect content font color align contentRect.width

  | .spacer _ _ _ =>
    -- Spacers don't render anything
    pure ()

  | .flex _ _ style children =>
    collectBoxStyle borderRect style
    for child in children do
      collectWidget child layouts

  | .grid _ _ style children =>
    collectBoxStyle borderRect style
    for child in children do
      collectWidget child layouts

  | .scroll _ style scrollState _ _ child =>
    -- Render background
    collectBoxStyle borderRect style

    -- Set up clipping to content area
    let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
    CollectM.emit (.pushClip clipRect)

    -- Save state and apply scroll offset
    CollectM.emit .save
    CollectM.emit (.pushTranslate (-scrollState.offsetX) (-scrollState.offsetY))

    -- Render child
    collectWidget child layouts

    -- Restore state
    CollectM.emit .popTransform
    CollectM.emit .restore
    CollectM.emit .popClip

/-- Collect render commands for a widget tree.
    This is the main entry point for converting a widget tree to render commands. -/
def collectCommands (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute (collectWidget w layouts)

/-- Collect render commands with an initial save/restore wrapper. -/
def collectCommandsWithSave (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute do
    CollectM.emit .save
    collectWidget w layouts
    CollectM.emit .restore

end Arbor
