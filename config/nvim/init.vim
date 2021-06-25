Plug 'tpope/vim-surround', {'tag': 'v2.1'}         " close parens
Plug 'junegunn/vim-easy-align', {'tag': 'v2.10.0'} " align text
Plug 'matze/vim-move', {'tag': 'v1.4'}             " move lines without cut-paste
Plug 'tomtom/tcomment_vim', {'tag': 'v3.08.1'}     " autocomment support
Plug 'tpope/vim-repeat', {'tag': 'v1.2'}           " improves repeats
Plug 'tpope/vim-unimpaired', {'tag': 'v2.0'}       " bracket mapping
Plug 'chrisbra/NrrwRgn', {'tag': 'v0.33'}          " narrow region for editing file in new buffer
Plug 'wellle/targets.vim', {'tag': 'v0.5.0'}       " better targets

" Navigation
Plug 'justinmk/vim-sneak', {'tag': 'v1.9'}     " hunt with two chars
Plug 'vim-scripts/utl.vim'                     " text linking

" Appearance
Plug 'junegunn/goyo.vim', {'tag': 'v1.6.0'}      " minimalist vim
Plug 'junegunn/limelight.vim'                    " highlight current paragraph
Plug 'jlesquembre/coc-conjure'
Plug 'Olical/conjure'

" git
Plug 'tpope/vim-fugitive', {'tag': 'v3.2'}  " git convenience functions
Plug 'rbong/vim-flog', {'tag': 'v1.2.0'}    " git branch viewer

" tools
Plug 'farmergreg/vim-lastplace' " save place in file

Plug 'inkarkat/vim-SyntaxRange' " syntax for just part of file
Plug 'reedes/vim-pencil'        " writing

autocmd BufNewFile,BufRead *.tsx set filetype=typescriptreact

" --- Vim Settings ---
" folddigest
let g:folddigest_options = 'vertical, flexnumwidth'
let g:folddigest_size = 30


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


function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

let g:coc_snippet_next = '<tab>'


" autoformat
" au BufWrite * :Autoformat
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

" show hidden characters and linewraps
" https://github.com/dm3/cygwin-config/blob/master/.vimrc
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

" NERDTree settings
let g:NERDTreeIgnore = ["^.git$", "^node_modules$","^__pycache__$", "^venv$", "^.vscode$"]
let g:NERDTreeShowHidden=1
let NERDTreeShowHidden = 1                      " show hidden files
let NERDTreeMinimalUI = 1                       " minimal ui
let NERDTreeChDirMode = 2                       " change cwd with root dir
let NERDTreeHijackNetrw = 1                     " always use nerdtree
let NERDTreeDirArrowExpandable = "\u00a0"       " make arrows invisible
let NERDTreeDirArrowCollapsible = "\u00a0"      " make arrows invisible
let NERDTreeNodeDelimiter = "\u263a"            " smiley face

" airline
" TODO review config
let g:airline#extensions#coc#enabled          = 1
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


" --- Text Navigation ---
" travel by visible lines
map j gj
map k gk

" avoid pressing shift
nnoremap ; :

" Browser-similar tab navigation
nnoremap <C-T> :tabnew<CR>
nnoremap <C-W> :tabclose<CR>
nnoremap <C-J> :tabprev<CR>
nnoremap <C-K> :tabnext<CR>
nnoremap <silent> <Tab> :bnext<CR>
nnoremap <silent> <S-Tab> :bprevious<CR>

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
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

" cancel search with esc
nnoremap <silent> <Esc> :nohlsearch<Bar>:echo<CR>

" text alignment
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" --- Vim Navigation ---
" toggle folds
nnoremap <leader><tab> za

" f :: Find
nmap <leader>fw     :Windows<CR>
nmap <leader>fh     :History<CR>
nmap <leader>ft     :Tags<CR>
nmap <leader>fc     :Commits<CR>
nmap <leader>fg     :GFiles<CR>
nmap <leader>fm     :Marks<CR> 
nmap <leader>fs     :Snippets<CR> 
nmap <leader>fc     :Commands<CR>
nmap <leader>fm     :Maps<CR>

" m :: Mode
map <leader>mf      :Goyo<CR>
autocmd! User GoyoEnter Limelight  | set cursorline!
autocmd! User GoyoLeave Limelight! | set cursorline

" o :: Open
nmap <leader>ot    :Ttoggle<CR>
nmap <leader>ob    :TagbarToggle<CR>

" open visual selection in new window
vmap o :NR<CR>

" --- Event Listeners ---
autocmd BufEnter * lcd %:p:h | " nerdtree opens in current dir
autocmd FocusLost * :wa | " save file when focus is lost

" remove trailing whitespace on save
" https://gitlab.com/kmidkiff/vim-configuration/-/blob/master/vimrc
function! RemoveWhitespace()
  :%s/\s\+$//e
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

