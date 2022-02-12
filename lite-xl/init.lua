local core   = require "core"
local keymap = require "core.keymap"
local config = require "core.config"
local style  = require "core.style"

------------------------------ Themes ----------------------------------------

core.reload_module("colors.solarized_light")

--------------------------- Key bindings -------------------------------------

-- key binding:
-- keymap.add { ["ctrl+escape"] = "core:quit" }

------------------------------- Fonts ----------------------------------------

-- customize fonts:
-- style.font = renderer.font.load(DATADIR .. "/fonts/FiraSans-Regular.ttf", 14 * SCALE)
-- style.code_font = renderer.font.load(DATADIR .. "/fonts/JetBrainsMono-Regular.ttf", 14 * SCALE)
--
-- font names used by lite:
-- style.font          : user interface
-- style.big_font      : big text in welcome screen
-- style.icon_font     : icons
-- style.icon_big_font : toolbar icons
-- style.code_font     : code
--
-- the function to load the font accept a 3rd optional argument like:
--
-- {antialiasing="grayscale", hinting="full"}
--
-- possible values are:
-- antialiasing: grayscale, subpixel
-- hinting: none, slight, full

------------------------------ Plugins ----------------------------------------

-- Automatically inserts closing brackets and quotes. 
config.plugins.autoinsert = true

-- Underlays color values with their resultant color.
config.plugins.colorpreview = true

-- Copy file location to clipboard
config.plugins.copyfilelocation = true

-- Displays git branch and insert/delete count in status bar
config.plugins.gitstatus = true

-- Linter
-- local lintplus = require "plugins.lintplus"
-- lintplus.setup.lint_on_doc_load()
-- lintplus.setup.lint_on_doc_save()

-- config.plugins.lintplus = true

-- C++
config.plugins.language_cpp = true

-- Make
config.plugins.language_make = true

-- Go
config.plugins.language_go = true
config.plugins.gofmt = true

-- Perl 
config.plugins.language_perl = true

-- PHP
config.plugins.language_php = true
-- config.plugins.lintplus_php = true

-- Shell
config.plugins.language_sh = true

