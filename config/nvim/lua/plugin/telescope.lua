local actions = require 'telescope.actions'
local sorters = require 'telescope.sorters'

require('telescope').setup {
  defaults = {
    layout_strategy = 'horizontal',
    prompt_prefix = '❯ ',
    selection_caret = '❯ ',
    sorting_strategy = 'descending',
    theme = "ivy",
    mappings = {
      i = {['<C-j>'] = actions.move_selection_next, ['<C-k>'] = actions.move_selection_previous},
      n = {['<C-c>'] = actions.close},
    },
  },
  pickers = {
      -- Your special builtin config goes in here
      buffers = {
        sort_lastused = true,
        previewer = false,
        mappings = {
          i = {
            ["<c-d>"] = require("telescope.actions").delete_buffer,
            -- Right hand side can also be the name of the action as a string
            ["<c-d>"] = "delete_buffer",
          },
          n = {
            ["<c-d>"] = require("telescope.actions").delete_buffer,
          }
        }
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
