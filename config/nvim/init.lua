io.popen("mkdir -p ~/.local/share/nvim/site/pack/packer/start/ && git clone https://github.com/wbthomason/packer.nvim ~/.local/share/nvim/site/pack/packer/start/packer.nvim")

local o = vim.o
local wo = vim.wo
local bo = vim.bo
local g = vim.g
local cmd = vim.cmd

u = require("utils")

-- editing
o.expandtab = true
o.tabstop = 4
o.shiftwidth = 4
o.smartindent = true
o.magic = true
o.clipboard = "unnamedplus"
o.textwidth = 0
o.wrapmargin = 0
o.undofile = true

-- search
o.incsearch = true
o.hlsearch = true
o.smartcase = true
o.wildmenu = true
o.wildignore = [[
.git,.hg,.svn
*.aux,*.out,*.toc
*.o,*.obj,*.exe,*.dll,*.manifest,*.rbc,*.class
*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
*.avi,*.divx,*.mp4,*.webm,*.mov,*.m2ts,*.mkv,*.vob,*.mpg,*.mpeg
*.mp3,*.oga,*.ogg,*.wav,*.flac
*.eot,*.otf,*.ttf,*.woff
*.doc,*.pdf,*.cbr,*.cbz
*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.kgb
*.swp,.lock,.DS_Store,._*
]]

-- performance
o.lazyredraw  = true 
g.nobackup    = true 
g.nowritebackup = true
g.noswapfile = true
o.hidden      = true 
o.history=1000 
-- g.complete-=5  -- limit autocomplete
-- show 3 lines before cursor at bottom
o.scrolloff = 3 
--  backspace is normal
o.backspace = "2"

-- show hidden chars and linewraps
o.list = true
-- o.listchars = 'tab:▸,extends:❯,precedes:❮'
o.showbreak = '↪'
o.completeopt = [[longest,menu,menuone]]
o.wildmode = 'longest:full,list:full'
o.splitbelow = true
o.splitright = true

-- visual tweaks
o.cmdheight = 1
o.signcolumn = "yes"
cmd('colorscheme nord')
cmd('syntax on')
o.background = "dark"
g.filetype = "plugin indent on"
o.cursorline = true
o.showmatch = true
o.termguicolors = true
o.foldmethod = "indent"

-- gitgutter
g.gitgutter_override_sign_column_highlight = 0
g.gitgutter_sign_added                     = '+'
g.gitgutter_sign_modified                  = '±'
g.gitgutter_sign_removed                   = '-'
g.gitgutter_sign_removed_first_line        = '^'
g.gitgutter_sign_modified_removed          = '#'

-- keymappings
g.mapleader = ' '
local map = vim.api.nvim_set_keymap
options = { noremap = true }

-- browser tab navigation
map('n', '<C-T>', ':tabnew<CR>', options)
map('n', '<C-W>', ':tabclose<CR>', options)
map('n', '<C-J>', ':tabprev<CR>', options)
map('n', '<C-K>', ':tabnext<CR>', options)
map('n', '<silent><Tab>', ':bnext<CR>', options)
map('n', '<silent><S-Tab>', ':bprevious<CR>', options)

-- fzf
map('n', '<leader>ff', ':Files<CR>', options)

-- git
map('n', '<leader>gg', ':Git<CR>', options)

-- movement shortcuts
-- move between panes
map('n', '<leader>wh', '<C-W>h', options)
map('n', '<leader>wj', '<C-W>j', options)
map('n', '<leader>wk', '<C-W>k', options)
map('n', '<leader>wl', '<C-W>l', options)

-- split and close windows
map('n', '<leader>ws', ':sp<CR>', options)
map('n', '<leader>wv', ':vsp<CR>', options)
map('n', '<leader>wc', '<C-W>c', options)

-- lsp

-- vim.lsp.set_log_level 'debug'
require'lspconfig'.clojure_lsp.setup{}
require'lspconfig'.rust_analyzer.setup{}
require'lspconfig'.rnix.setup{}
require'lspconfig'.zls.setup{}

-- package management!
return require('packer').startup(function()
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- movement
  use 'tpope/vim-surround'
  use 'junegunn/vim-easy-align'
  use 'matze/vim-move'
  use 'tomtom/tcomment_vim'
  use 'tpope/vim-repeat'
  use 'tpope/vim-unimpaired'
  use 'wellle/targets.vim'
  use 'junegunn/fzf'
  use 'junegunn/fzf.vim'
  use 'justinmk/vim-sneak'

  -- metrics
  use 'wakatime/vim-wakatime'

  -- ui
  use 'vim-airline/vim-airline'
  use 'arcticicestudio/nord-vim'
  use 'tpope/vim-vinegar'

  -- git
  use 'airblade/gitgutter'
  use 'tpope/vim-fugitive'

  -- specific language support
  use 'Olical/conjure'
  use 'neovim/nvim-lspconfig'

  -- etc
  use 'farmergreg/vim-lastplace'
  use 'wincent/terminus'
  use 'direnv/direnv.vim'
end)

