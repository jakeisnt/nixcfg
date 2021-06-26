local actions = require 'telescope.actions'
local sorters = require 'telescope.sorters'

require('telescope').setup {
  defaults = {
    layout_strategy = 'horizontal',
    prompt_prefix = '❯ ',
    selection_caret = '❯ ',
    prompt_position = 'bottom',
    sorting_strategy = 'descending',
    mappings = {
      i = {['<C-j>'] = actions.move_selection_next, ['<C-k>'] = actions.move_selection_previous},
      n = {['<C-c>'] = actions.close},
    },
  },
}

local M = {}

function M.find_dotfiles()
  require('telescope.builtin').find_files {
    prompt_title = ' dotfiles ',
    find_command = {'rg', '--files', '--hidden', '--sort=path'},
    cwd = '/etc/nixos',
  }
end

M.project_files = function()
  local ok = pcall(require'telescope.builtin'.git_files)
  if not ok then require'telescope.builtin'.find_files() end
end

return M
