local nmap = U.keymap.nmap
local imap = U.keymap.imap
local tmap = U.keymap.tmap
local cmap = U.keymap.cmap
local xmap = U.keymap.xmap

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
nmap('<leader>', '<Nop>')
xmap('<leader>', '<Nop>')

-- Normal
nmap('Q', '<Nop>')
nmap('q:', '<Nop>')
nmap('<C-c>', '<Esc>')
nmap('Y', 'y$')
nmap('<A-j>', ':m .+1<cr>==') -- move cur line down
nmap('<A-k>', ':m .-2<cr>==') -- move cur line up
nmap('<leader>q', ':q<cr>')
nmap('<leader>kb', ':bw<cr>')

-- travel by visible lines
nmap('j', 'gj')
nmap('k', 'gk')

nmap('<Left>', 'h')
nmap('<Down>', 'j')
nmap('<Up>', 'k')
nmap('<Right>', 'l')

-- avoid pressing shift
nmap(';', ':')
nmap('<leader><leader>', ':e#<CR>')
-- select whole file
nmap('<leader>V', 'ggVG')
-- select last insertion
nmap('gV', '`[v`]')
-- cancel search with esc
nmap('<silent> <Esc>', ':nohlsearch<Bar>:echo<CR>')

-- http://karolis.koncevicius.lt/posts/porn_zen_and_vimrc/
-- make n always search forward and N backward
nmap ('<expr> n', '\'Nn\'[v:searchforward]')
nmap('<expr> N', '\'nN\'[v:searchforward]')
nmap('yil', '0y$')
nmap('<CR>', '{-> v:hlsearch ? ":nohl\\<CR>" : "\\<CR>"}()', {expr = true})

-- compress next multiline passage into a single line?
-- i don't entirely understand what this does
nmap('Q', 'gqap')
nmap('<C-s>', '<cmd>w<CR>')

-- Buffers
nmap('<Tab>', '<cmd>bn<CR>')
nmap('<S-Tab>', '<cmd>bp<CR>')
nmap('<space>bd', '<cmd>bd<CR>')

-- Window
nmap('<leader>wh', '<C-W>h')
nmap('<leader>wj', '<C-W>j')
nmap('<leader>wk', '<C-W>k')
nmap('<leader>wl', '<C-W>l')

nmap('<C-h>', '<cmd>wincmd h<CR>')
nmap('<C-j>', '<cmd>wincmd j<CR>')
nmap('<C-k>', '<cmd>wincmd k<CR>')
nmap('<C-l>', '<cmd>wincmd l<CR>')
nmap('<leader>ws', ':sp<CR>')

xmap('<leader>wh', '<C-W>h')
xmap('<leader>wj', '<C-W>j')
xmap('<leader>wk', '<C-W>k')
xmap('<leader>wl', '<C-W>l')
xmap('<leader>ws', ':sp<CR>')
xmap('<leader>wv', ':vsp<CR>')
xmap('<leader>wc', '<C-W>c')
xmap('<space>w=', '<cmd>wincmd =<CR>')

-- Tabs
nmap('<C-T>', ':tabnew<cr>')
nmap('<C-W>', ':tabclose<cr>')
nmap('<C-J>', ':tabprev<cr>')
nmap('<C-K>', ':tabnext<cr>')
nmap('<silent> <Tab>', ':bnext<cr>')
nmap('<silent> <S-Tab>', ':bprevious<cr>')

-- Insert
imap('<C-c>', '<Esc>')
imap('<S-CR>', '<Esc>o')
imap('<C-CR>', '<Esc>O')
imap('<Tab>', 'pumvisible() ? "\\<C-n>" : "\\<Tab>"', {expr = true})
imap('<S-Tab>', 'pumvisible() ? "\\<C-p>" : "\\<Tab>"', {expr = true})
-- imap('<CR>', 'v:lua.U.completion_confirm()', {expr = true})

-- Visual
xmap('<', '<gv')
xmap('>', '>gv')
xmap('K', ':move \'<-2<CR>gv-gv')
xmap('J', ':move \'>+1<CR>gv-gv')

-- backspace deletes selected text
xmap('<BS>', 'x')
xmap('Q', 'gq')
xmap('<silent> *', ':<C-u>call VisualSelection(\'\', \'\')<CR>/<C-R>=@/<CR><CR>')
xmap('<silent> #', ':<C-u>call VisualSelection(\'\', \'\')<CR>?<C-R>=@/<CR><CR>')

-- Command
cmap('<C-a>', '<Home>')
cmap('<C-e>', '<End>')
cmap('<C-h>', '<Left>')
cmap('<C-j>', '<Down>')
cmap('<C-k>', '<Up>')
cmap('<C-l>', '<Right>')
cmap('<C-d>', '<Del>')
cmap('<C-f>', '<C-R>=expand("%:p")<CR>')

-- Git
nmap('<leader>gg', ':Neogit<cr>')
nmap('<leader>gl', ':Neogit log<cr>')
nmap('<leader>gp', ':Neogit push<cr>')
nmap('<leader>gb', ':Neogit push<cr>')
nmap('<space>gc', ':Neogit commit<cr>')

nmap('<leader>gd', ':DiffviewOpen<cr>')
nmap('<leader>gD', ':DiffviewOpen main<cr>')

-- Telescope
nmap('<space>ff', '<cmd>lua require("plugin.telescope").project_files()<CR>')
nmap('<space>fb', '<cmd>Telescope buffers<CR>')
nmap('<space>,', '<cmd>Telescope buffers<CR>')
nmap('<space>fh', '<cmd>Telescope help_tags<CR>')
nmap('<space>fo', '<cmd>Telescope oldfiles<CR>')
nmap('<space>fl', '<cmd>Telescope live_grep<CR>')
nmap('<space>fd', '<cmd>lua require("plugin.telescope").find_dotfiles()<CR>')
nmap('<space>fk', ':Telescope file_browser hidden=true<cr>')

-- Nix
nmap('<space>hrn', '<cmd>!hey rebuild<CR>')


-- Vim surround ( noremap need to be false to work)
-- TODO what do these do??
nmap('ds', '<Plug>Dsurround', {noremap = false})
nmap('cs', '<Plug>Csurround', {noremap = false})
nmap('cS', '<Plug>CSurround', {noremap = false})
nmap('s', '<Plug>Ysurround', {noremap = false})
nmap('S', '<Plug>YSurround', {noremap = false})
nmap('ss', '<Plug>Yssurround', {noremap = false})
nmap('SS', '<Plug>YSsurround', {noremap = false})
xmap('s', '<Plug>VSurround', {noremap = false})
xmap('S', '<Plug>VgSurround', {noremap = false})

vim.cmd [[command! WipeReg for i in range(34,122) | silent! call setreg(nr2char(i), []) | endfor]]

