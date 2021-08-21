local function disable_default_plugins()
  vim.g.loaded_gzip = 1
  vim.g.loaded_tar = 1
  vim.g.loaded_tarPlugin = 1
  vim.g.loaded_zip = 1
  vim.g.loaded_zipPlugin = 1
  vim.g.loaded_getscript = 1
  vim.g.loaded_getscriptPlugin = 1
  vim.g.loaded_vimball = 1
  vim.g.loaded_vimballPlugin = 1
  vim.g.loaded_matchit = 1
  vim.g.loaded_matchparen = 1
  vim.g.loaded_2html_plugin = 1
  vim.g.loaded_logiPat = 1
  vim.g.loaded_rrhelper = 1
  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1
  vim.g.loaded_netrwSettings = 1
  vim.g.loaded_netrwFileHandlers = 1
end


-- Welcome to your magic kit!
-- This is the first file Neovim will load.
-- We'll ensure we have a plugin manager and Aniseed.
-- This will allow us to load more Fennel based code and download more plugins.

-- Enable Aniseed's automatic compilation and loading of Fennel source code.
-- Aniseed looks for this when it's loaded then loads the rest of your
-- configuration if it's set.
vim.g["aniseed#env"] = {module = "magic.init"}

-- Now head to fnl/magic/init.fnl to continue your journey.
-- Try pressing gf on the file path to [g]o to the [f]ile.

disable_default_plugins()
require'core.utils'
require'core.options'
require'core.keymaps'
require'core.autocmds'

vim.cmd [[runtime plugin/direnv.vim]]
require'plugin'
