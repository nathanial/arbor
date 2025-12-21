/-
  Arbor - Renderer-Agnostic Widget Library

  Arbor provides a declarative widget system that generates abstract
  render commands instead of directly rendering to a specific backend.

  Key abstractions:
  - RenderCommand: Abstract drawing operations (fillRect, fillText, etc.)
  - TextMeasurer: Typeclass for measuring text (pluggable backends)
  - Widget: Declarative widget tree (flex, grid, text, scroll, etc.)

  Usage:
  1. Build a widget tree using the DSL (row, column, text', box, etc.)
  2. Measure the tree with measureWidget (using a TextMeasurer backend)
  3. Compute layout with Trellis.layout
  4. Collect render commands with collectCommands
  5. Execute commands with your rendering backend
-/

-- Core types
import Arbor.Core.Types
import Arbor.Core.TextMeasurer

-- Render commands
import Arbor.Render.Command
import Arbor.Render.Collect

-- Widget system
import Arbor.Widget.Core
import Arbor.Widget.DSL
import Arbor.Widget.TextLayout
import Arbor.Widget.Measure

-- Event system
import Arbor.Event.Types
import Arbor.Event.HitTest
import Arbor.Event.Scroll

-- Text-based rendering (for debugging/testing)
import Arbor.Text.Canvas
import Arbor.Text.Mode
import Arbor.Text.Renderer
