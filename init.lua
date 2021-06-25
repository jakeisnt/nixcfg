-------- helpers --------
local api, cmd, fn, g = vim.api, vim.cmd, vim.fn, vim.g

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}
local function opt(scope, key, value)
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  api.nvim_set_keymap(mode, lhs, rhs, options)
end

-------- plugin configuration --------

require'which-key'.setup {}
require'neogit'.setup {}

cmd 'colorscheme onedark'
require'lualine'.setup {
  options = {
    theme = 'auto',
      icons_enabled = false,
      section_separators = '',
      component_separators = '',
  },
}

-------- settings --------
local indent_width = 2
opt('b', 'expandtab', true)
opt('b', 'shiftwidth', indent_width)
opt('b', 'tabstop', indent_width)
opt('b', 'smartindent', true)
opt('o', 'completeopt', 'menuone,noinsert,noselect')
opt('o', 'hidden', true)
opt('o', 'ignorecase', true)
opt('o', 'smartcase', true)
opt('o', 'scrolloff', 4)
opt('o', 'sidescrolloff', 8)
opt('o', 'shiftround', true)
opt('o', 'splitbelow', true)
opt('o', 'splitright', true)
opt('o', 'termguicolors', true)
opt('w', 'list', true)
opt('w', 'number', true)
opt('w', 'relativenumber', true)
opt('w', 'wrap', false)
cmd 'set clipboard=unnamedplus'
cmd 'set mouse=a'
cmd 'set timeoutlen=500'

-------- mappings --------
g.mapleader = " "
g.maplocalleader = " "

map('n', '<A-j>', ':m .+1<cr>==') -- move cur line down
map('n', '<A-k>', ':m .-2<cr>==') -- move cur line up
map('n', '<leader>w', ':w<cr>')
map('n', '<leader>q', ':q<cr>')
map('n', '<leader>kb', ':bw<cr>')
map('n', '<leader>g', ':Neogit<cr>')

-- TODO easier way of getting to normal mode from terminal mode
-- map('t', '<c-<leader>>', '<c-\\><c-n><leader>')
-- change window with arrow keys, leaving hjkl for navigationin file
map('t', '<A-left>', '<c-\\><c-n><c-w>h')
map('t', '<A-down>', '<c-\\><c-n><c-w>j')
map('t', '<A-up>', '<c-\\><c-n><c-w>k')
map('t', '<A-right>', '<c-\\><c-n><c-w>l')

map('i', '<A-left>', '<Esc><c-w>h')
map('i', '<A-down>', '<Esc><c-w>j')
map('i', '<A-up>', '<Esc><c-w>k')
map('i', '<A-right>', '<Esc><c-w>l')

map('n', '<A-left>', '<c-w>h')
map('n', '<A-down>', '<c-w>j')
map('n', '<A-up>', '<c-w>k')
map('n', '<A-right>', '<c-w>l')

map('v', '<A-left>', '<Esc><c-w>h')
map('v', '<A-down>', '<Esc><c-w>j')
map('v', '<A-up>', '<Esc><c-w>k')
map('v', '<A-right>', '<Esc><c-w>l')

-------- telescope -------- TODO find more functions?
map('n', '<leader>fj', ':Telescope find_files<cr>')
map('n', '<leader>fk', ':Telescope file_browser hidden=true<cr>')
map('n', '<leader>fl', ':Telescope buffers<cr>')
map('n', '<leader>fp', ':Telescope live_grep<cr>')

-------- tree-sitter --------
-- TODO not sure this works given vim-polyglot being active
local ts = require'nvim-treesitter.configs'
ts.setup = {ensure_installed = 'maintained', highlight = {enable = true}}

------------ lsp --------
local nvim_lsp = require 'lspconfig'
local on_attach = function()
  require'completion'.on_attach()
  vim.cmd([[inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"]])
  vim.cmd([[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]])
end

local servers = { 'solargraph', 'rust_analyzer', 'ccls', 'pyright', 'sumneko_lua', 'hls', 'ocamllsp'}
for _, server in pairs(servers) do
  nvim_lsp[server].setup {
    on_attach = on_attach,
  }
end

map('n', '<leader>l,', '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>')
map('n', '<leader>l;', '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>')
map('n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<cr>')
map('n', '<leader>ld', '<cmd>lua vim.lsp.buf.definition()<cr>')
map('n', '<leader>lf', '<cmd>lua vim.lsp.buf.formatting()<cr>')
map('n', '<leader>lh', '<cmd>lua vim.lsp.buf.hover()<cr>')
map('n', '<leader>lm', '<cmd>lua vim.lsp.buf.rename()<cr>')
map('n', '<leader>lr', '<cmd>lua vim.lsp.buf.references()<cr>')
map('n', '<leader>ls', '<cmd>lua vim.lsp.buf.document_symbol()<cr>')
map('n', '<leader>li', '<cmd>LspInfo<cr>')
map('n', '<leader>lr', '<cmd>LspRestart<cr>')

-- lua lsp config
nvim_lsp.sumneko_lua.setup {
  cmd = {"lua-language-server"},
  on_attach = on_attach,
  settings = {
      Lua = {
          runtime = {
              version = 'LuaJIT',
              path = vim.split(package.path, ';'),
          },
          diagnostics = {
              globals = {'vim'},
          },
          workspace = {
              library = {
                  [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                  [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
              },
          },
          telemetry = {
            enable = false,
          },
      },
  },
}
