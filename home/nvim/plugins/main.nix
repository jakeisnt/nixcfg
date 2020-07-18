{ lib, ...}:
{
  programs.neovim.extraConfig = lib.mkBefore ''

" --- Plugins ---
" editing

" Navigation
" Plug 'justinmk/vim-sneak'                           " hunt with two chars
" Plug 'mhinz/vim-sayonara'                           " sensibly close buffer
" Plug 'majutsushi/tagbar'                            " browse tags of current file
" Plug 'vim-scripts/utl.vim'                          " text linking

" Appearance
" Plug 'junegunn/goyo.vim'             " minimalist vim
" Plug 'junegunn/limelight.vim'        " highlight current paragraph
" Plug 'camspiers/lens.vim'            " auto window resizing TODO do i really ever need this?
" Plug 'vim-scripts/folddigest.vim'    " displays folds as summary

" git
" Plug 'rbong/vim-flog'         " git branch viewer TODO may not be frequently used

" tools
" Plug 'farmergreg/vim-lastplace' " save place in file
" Plug 'mbbill/undotree'          " undo tree
" Plug 'wincent/terminus'         " better terminal integration

" file type support
" Plug 'sheerun/vim-polyglot'     " syntax highlighting
" Plug 'jceb/vim-orgmode'         " TODO keymappings for taking notes
" Plug 'inkarkat/vim-SyntaxRange' " syntax for just part of file - for org mode
" Plug 'mhinz/vim-startify'       " start menu

" --- Vim Settings ---
" folddigest
" TODO
" - improve look of digest
" - toggle digest
" let g:folddigest_options = 'vertical, flexnumwidth'
" let g:folddigest_size = 30

" ALE
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_enter = 0
let g:ale_fix_on_save = 1
let g:ale_lint_on_insert_leave = 1
let g:ale_completion_tsserver_autoimport = 1 " autoimport ts server
let g:ale_sign_column_always = 1
let g:airline#extensions#ale#enabled = 1

" custom fixers and linters for ALE
let b:ale_fixers = {'python': ['autopep8', 'yapf']}
let b:ale_linters = {'python': ['flake8', 'mypy']}

" TODO: compile org mode, latex, markdown to pdf and show pdf


" FZF
" match fzf colors with vim scheme
let g:fzf_colors = {
            \ 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'border':  ['fg', 'Ignore'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }

let g:fzf_preview_window = 'right:60%'
let g:fzf_buffers_jump = 1

" utilsnips TODO
" let g:UltiSnipsExpandTrigger='<tab>'
" let g:UltiSnipsJumpForwardTrigger='<c-b>'
" let g:UltiSnipsJumpBackwardTrigger='<c-z>'

" autoformat
" au BufWrite * :Autoformat
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

" editing
set expandtab             " expand tabs to spaces
set tabstop=4             " tabs are 4 spaces
set shiftwidth=4          " reindents are 4 spaces
set smartindent           " determines indentation with context
set magic                 " enable regex
set formatoptions+=j      " delete comment character when joining lines
set clipboard=unnamedplus " shared system clipboard
set textwidth=0           " line wrap
set wrapmargin=0
set undofile              " undo history persists across sessions

" search
set incsearch " highlight as characters are entered
set hlsearch  " persistent highlight of prev search
set smartcase " ignore case if search all lowercase
set wildmenu  " show menu of suggestions with tab-complete

" folding
set foldenable        " turn on folding
set foldmethod=marker " marker indicates folds TODO use markers?
set foldlevelstart=10
set modelines=1 " use modeline to configure marker
" vim:foldmethod=marker:foldlevel=0 (put at bottom of file for fold configuraion)

" performance
set lazyredraw   " only redraw components on change
set nobackup     " remove backups
set nowritebackup
set noswapfile
set hidden       " cache more
set history=1000 " default is 20
set complete-=5  " limit autocomplete
set scrolloff=3  " scroll 3l in advance of window
set backspace=2  " backspace is normal

" https://github.com/dm3/cygwin-config/blob/master/.vimrc
" show hidden characters and linewraps
" TODO make sure this works on other devices
set list
set listchars=tab:▸\ ,extends:❯,precedes:❮
set showbreak=↪
set completeopt=longest,menu,menuone
"               |       |    +-- Show popup even with one match
"               |       +------- Use popup menu with completions
"               +--------------- Insert longest completion match
set wildmode=longest:full,list:full
"            |            +-- List matches, complete first match
"            +--------------- Complete longest prefix, use wildmenu

set splitbelow  " sensible splits
set splitright


" visual
set cmdheight=1                " one line for commands
set shortmess+=c               " add to search
set signcolumn=yes             " always show sign column
syntax on                      " syntax highlighting
filetype plugin indent on      " file type detection
set cursorline                 " current line is visible
set showmatch                  " show matching braces
syntax enable                  " enable syntax highlighting
set background=dark
highlight Comment gui=italic | " make comments italic
set foldmethod=indent

let g:limelight_conceal_ctermfg = 'Gray'
let g:gitgutter_sign_column_always=1 " always display gutter

if (empty($TMUX))
    if (has('nvim'))
        " $NVIM_TUI_ENABLE_TRUE_COLOR=1
    endif
    if (has('termguicolors'))
        set termguicolors
    endif
endif


" airline
" TODO review config
let g:airline#extensions#ale#enabled          = 1
let g:airline#extensions#tabline#enabled      = 1
let g:airline#extensions#tabline#formatter    = 'unique_tail_improved'
let g:airline#extensions#bufferline#enabled   = 1
let g:airline#extensions#gutentags#enabled    = 1
let g:airline_powerline_fonts                 = 1
let g:airline_left_sep                        = '█'
let g:airline_right_sep                       = '█'
let g:airline_left_alt_sep                    = ' '
let g:airline#extensions#tabline#left_sep     = '█'
let g:airline#extensions#tabline#right_sep    = '█'
let g:airline#extensions#tabline#left_alt_sep = ' '


" --- Key Mappings ---
let mapleader=" "

" --- Text Navigation ---
" travel by visible lines
map j gj
map k gk

" avoid pressing shift
nnoremap ; :

" http://karolis.koncevicius.lt/posts/porn_zen_and_vimrc/
" make n always search forward and N backward
nnoremap <expr> n 'Nn'[v:searchforward]
nnoremap <expr> N 'nN'[v:searchforward]

" make ; always "find" forward and , backward
nnoremap <expr> ; getcharsearch().forward ? ';' : ','
nnoremap <expr> , getcharsearch().forward ? ',' : ';'

" Y behavior consistent with C and D
nnoremap Y y$
" select entire file
nnoremap <leader>V ggVG
" select last insertion
nnoremap gV `[v`]
" paragraph formatting
nnoremap Q gqap
vnoremap Q gq
" make backspace delete selected text
xmap <BS> x

" search for current selection in visual mode

" cancel search with esc
nnoremap <silent> <Esc> :nohlsearch<Bar>:echo<CR>

" text alignment
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" --- Vim Navigation ---

" commonly used shortcuts
nnoremap <leader>h <C-W>h
nnoremap <leader>j <C-W>j
nnoremap <leader>k <C-W>k
nnoremap <leader>l <C-W>l
nnoremap <leader>.    :NERDTreeToggle<CR>
" toggle folds
nnoremap <leader><tab> za
" toggle with previous file
nnoremap <Leader><Leader> :e#<CR>

" a ::
" b ::

" c :: Close
nmap <leader>cc    :Sayonara<CR>
nmap <leader>cn    :Sayonara!<CR>

" d ::

" e :: Edit
" edit vim config files (if they exist) TODO
function! EditConfig(config)
    if exists(a:config)
        execute 'tabedit '.a:config
    endif
endfunction

nnoremap <leader>ec    :edit ~/.vimrc<CR>

" f :: Find
" TODO: file path completion in edit mode
nmap <leader>ff     :Files<CR>
nmap <leader>fw     :Windows<CR>
nmap <leader>fh     :History<CR>
nmap <leader>ft     :Tags<CR>
nmap <leader>fc     :Commits<CR>
nmap <leader>fl     :Lines<CR>
nmap <leader>fb     :Buffers<CR>
nmap <leader>fg     :Gfiles<CR>
" nmap <leader>fm     :Marks<CR> TODO learn marks; swap with maps
" nmap <leader>fs     :Snippets<CR> TODO
nmap <leader>fc     :Commands<CR>
nmap <leader>fm     :Maps<CR>

" g :: Git
nmap <leader>g    :Gstatus<CR>

" i ::


" n :: New

" o :: Open
nmap <leader>og    :Gstatus<CR>
" open visual selection in new window
vmap o :NR<CR> 

" p :: Project
" q ::

" r :: Reload
map <leader>rr     :source ~/.vimrc<CR>
map <leader>hrr    :source ~/.vimrc<CR>:PlugInstall<CR>

" t :: Tab
nnoremap <leader>tn  :tabnew<CR>
nnoremap <leader>tc  :tabclose<CR>
nnoremap <leader>tj  :tabprev<CR>
nnoremap <leader>tk  :tabnext<CR>

" Browser-similar tab navigation
nnoremap <C-T> :tabnew<CR>
nnoremap <C-W> :tabclose<CR>
nnoremap <C-J> :tabprev<CR>
nnoremap <C-K> :tabnext<CR>
nnoremap <silent> <Tab> :bnext<CR>
nnoremap <silent> <S-Tab> :bprevious<CR>

" u :: Undo
map <leader>u      :UndotreeToggle<CR>


" w :: (split) Window
nnoremap <leader>wc  :sp<CR>
nnoremap <leader>wv  :vsp<CR>
nnoremap <leader>wr  <C-W>c

" x :: Execute?


" z ::

" --- Event Listeners ---

" command! -range FormatShellCmd <line1>!format_shell_cmd.py | " format shell command TODO
autocmd BufEnter * lcd %:p:h | " nerdtree opens in current dir
autocmd FocusLost * :wa | " save file when focus is lost

" remove trailing whitespace on save
" https://gitlab.com/kmidkiff/vim-configuration/-/blob/master/vimrc
function! RemoveWhitespace()
    :%s/\s\+$//e
endfunction

" smaller indentation for html, css
autocmd FileType css    setlocal shiftwidth=2 tabstop=2
autocmd FileType html   setlocal shiftwidth=2 tabstop=2
autocmd FileType markdown setlocal nofoldenable " fold may not work with markdown?

" open terminal drawer
" https://github.com/CKolkey/.dotfiles/blob/master/nvim_init.vim
let g:terminal_drawer = { 'win_id': v:null, 'buffer_id': v:null }

" Toggles terminal drawer
function! ToggleTerminalDrawer() abort
  if win_gotoid(g:terminal_drawer.win_id)
    hide
    set laststatus=2 showmode ruler
  else
    botright new
    if !g:terminal_drawer.buffer_id
      call termopen($SHELL, {"detach": 0})
      let g:terminal_drawer.buffer_id = bufnr("")
    else
      exec 'buffer' g:terminal_drawer.buffer_id
      call RemoveEmptyBuffers()
    endif

    exec 'resize' float2nr(&lines * 0.25)
    setlocal laststatus=0 noshowmode noruler
    setlocal nobuflisted
    startinsert!
    let g:terminal_drawer.win_id = win_getid()

    tnoremap <buffer><Esc> <C-\><C-n>
    nnoremap <buffer><silent><Esc> :q<cr>
    nnoremap <buffer><silent> q :q<CR>
  endif
endfunction

" REMOVE EMPTY BUFFERS {{{
function! RemoveEmptyBuffers()
  let buffers = filter(range(1, bufnr('$')), 'buflisted(v:val) && empty(bufname(v:val)) && bufwinnr(v:val)<0 && !getbufvar(v:val, "&mod")')
  if !empty(buffers)
      silent exe 'bw ' . join(buffers, ' ')
  endif
endfunction
" }}}
'';
}
