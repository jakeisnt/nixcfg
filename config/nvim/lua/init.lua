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
opt('o', 'hidden', true)
opt('o', 'ignorecase', true)
opt('o', 'smartcase', true)
opt('o', 'magic', true)
opt('o', 'wildmenu', true)
opt('o', 'incsearch', true)
opt('o', 'hlsearch', true)
opt('o', 'undofile', true)
opt('w', 'wrap', true)

opt('o', 'scrolloff', 4)
opt('o', 'completeopt', 'menuone,noinsert,noselect')
opt('o', 'sidescrolloff', 8)
opt('o', 'shiftround', true)
opt('o', 'splitbelow', true)
opt('o', 'splitright', true)
opt('o', 'termguicolors', true)
opt('w', 'list', true)
opt('w', 'number', true)
opt('w', 'relativenumber', true)
cmd 'set clipboard=unnamedplus'
cmd 'set mouse=a'
cmd 'set timeoutlen=500'
cmd 'set foldenable'
cmd 'set foldmethod=marker'
cmd 'set foldlevelstart=10'
cmd 'set textwidth=0'
cmd 'set wrapmargin=0'
cmd 'set formatoptions+=j' -- delete comment char when joining lines

cmd 'set lazyredraw'
cmd 'set nobackup'
cmd 'set nowritebackup'
cmd 'set noswapfile'
cmd 'set hidden'
cmd 'set history=1000'
cmd 'set scrolloff=3'
cmd 'set backspace=2'

cmd 'highlight Comment gui=italic'
cmd 'set foldmethod=indent'
cmd 'set cursorline'

-- show hidden characters and linewraps
-- https://github.com/dm3/cygwin-config/blob/master/.vimrc
cmd 'set listchars=tab:▸\\ ,extends:❯,precedes:❮'
cmd 'set showbreak=↪'

-------- mappings --------
g.mapleader = " "
g.maplocalleader = " "

map('n', '<A-j>', ':m .+1<cr>==') -- move cur line down
map('n', '<A-k>', ':m .-2<cr>==') -- move cur line up
map('n', '<leader>w', ':w<cr>')
map('n', '<leader>q', ':q<cr>')
map('n', '<leader>kb', ':bw<cr>')
map('n', '<leader>g', ':Neogit<cr>')

-- travel by visible lines
map('n', 'j', 'gj')
map('n', 'k', 'gk')
-- avoid pressing shift
map('n', ';', ':')

map('n', '<leader>.', ':NvimTreeToggle<CR>')
map('n', '<leader><leader>', ':e#<CR>')

-- f: find a file
map('n', '<leader>ff', ':Telescope find_files<cr>')
map('n', '<leader>fb', ':Telescope buffers<cr>')
map('n', '<leader>fl', ':Telescope live_grep<cr>')
map('n', '<leader>ft', ':NvimTreeFindFile<CR>')
map('n', '<leader>fk', ':Telescope file_browser hidden=true<cr>')



-- split the window
map('n', '<leader>wh', '<C-W>h')
map('n', '<leader>wj', '<C-W>j')
map('n', '<leader>wk', '<C-W>k')
map('n', '<leader>wl', '<C-W>l')
map('n', '<leader>ws', ':sp<CR>')
map('n', '<leader>wv', ':vsp<CR>')
map('n', '<leader>wc', '<C-W>c')

-- tab navigation
map('n', '<C-T>', ':tabnew<cr>')
map('n', '<C-W>', ':tabclose<cr>')
map('n', '<C-J>', ':tabprev<cr>')
map('n', '<C-K>', ':tabnext<cr>')
map('n', '<silent> <Tab>', ':bnext<cr>')
map('n', '<silent> <S-Tab>', ':bprevious<cr>')

-- cancel search with esc
map('n', '<silent> <Esc>', ':nohlsearch<Bar>:echo<CR>')

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

local servers = { 'solargraph', 'rust_analyzer', 'clangd', 'pyright', 'sumneko_lua', 'hls', 'ocamllsp', 'zls', 'tsserver' }
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
