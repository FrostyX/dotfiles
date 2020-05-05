" ------------------------------------------
" Vundle
" ------------------------------------------

" Install plugins:
"   :PluginInstall
"   vim +PluginInstall +qall
"
" Uninstall plugins:
"   :PluginClean

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-rhubarb'  " Enable :Gbrowse in fugitive
Plugin 'tomtom/tcomment_vim'
Plugin 'Raimondi/delimitMate'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'plasticboy/vim-markdown'
Plugin 'scrooloose/syntastic'
Plugin 'vimwiki/vimwiki'
Plugin 'junegunn/goyo.vim'
Plugin 'ElmCast/elm-vim'
Plugin 'chriskempson/base16-vim'

" Requires to `dnf install python3-jedi`
Plugin 'davidhalter/jedi-vim'

" Requires to `dnf install fzf`
Plugin 'junegunn/fzf.vim'

" My own plugins
" Plugin 'file:///home/jkadlcik/git/vim-fugitive-pagure'
Plugin 'FrostyX/vim-fugitive-pagure'  " Enable :Gbrowse for pagure.io

call vundle#end()



" ------------------------------------------
" Global
" ------------------------------------------

set nocompatible
set nowrap
set backspace=indent,eol,start

" Check if file was changed from the outside
set autoread

" Title
set title
set titlestring=VIM\ -\ %t

" Search
set hlsearch
set incsearch
set ignorecase

" Tab completion
set wildmenu
set wildmode=longest:full,full

" Dialog instead of failure
set confirm

" Language for spellcheck
set spelllang=cs
set encoding=utf-8

" Don't save backups in working directory
set backupdir=~/.vim/tmp
set directory=~/.vim/swp

" Filetype
filetype plugin on
filetype indent on



" ------------------------------------------
" Code
" ------------------------------------------
syntax on
set number
set nofoldenable

" Save when running external command
set autowrite

" Show matching bracket
set showmatch

" Indentation
set tabstop=4
set shiftwidth=4
set softtabstop=4
set autoindent
set smartindent

" Lines around cursor when scrolling
set scrolloff=5

" Delete whitespace on save
" reference: [whitespace-on-save]
autocmd BufWritePre * :%s/\s\+$//e



" ------------------------------------------
" Key bindings
" ------------------------------------------

let mapleader = ","

" Remap the FUCKIN' help
imap <F1> <Esc>
map <F1> <Esc>

" Copy & Paste
set pastetoggle=<F2>

" Splits
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Tabs
nmap <C-S-tab> :tabprevious<CR>
nmap <C-tab> :tabnext<CR>
map <C-S-tab> :tabprevious<CR>
map <C-tab> :tabnext<CR>
imap <C-S-tab> <ESC>:tabprevious<CR>i
imap <C-tab> <ESC>:tabnext<CR>i
imap <C-t> <ESC>:tabnew<CR>


" Because of czech keyboard layout
nmap <Leader>def g<C-]>



" ------------------------------------------
" Plugins
" ------------------------------------------

" Fugitive
cnoremap git Git
map <leader>ga :Gwrite<CR>
map <leader>gc :Gcommit<CR>
map <leader>gp :Git push<CR>

" NERDTree
map <C-n> :NERDTreeFind<CR>


" Jedi-vim
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = 0
let g:jedi#completions_command = "<C-n>"
autocmd FileType python setlocal completeopt-=preview

" tComment
map <leader>c <C-_><C-_>

" Syntastic
let g:syntastic_quiet_messages = { "level": "warnings" }

" Markdown
let g:vim_markdown_folding_disabled=1

" Vimwiki
let g:vimwiki_dir_link = 'index'
let g:vimwiki_hl_cb_checked = 2
let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]

au BufNewFile ~/vimwiki/diary/*.md :silent 0r !~/.vim/bin/fragment-vimwiki-diary '%'

" FZF
nmap ; :Buffers<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>t :Tags<CR>

" Don't suggest imports
let g:fzf_tags_command = 'ctags -R --python-kinds=-i'


" ------------------------------------------
" GUI specific settings
" ------------------------------------------
if has("gui_running")

	" Fix comments in solarized
	let g:solarized_italic=0

	colorscheme base16-eighties
	set background=dark
	set guioptions-=T  " No toolbar
	set guioptions-=r  " No right scrollbar
	set guioptions-=L  " No left scrollbar
	set guioptions-=m  " No menu

	" Menu on right click
	set mousemodel=popup

	" Invisible characters
	" reference: [invisible-characters]
	set list                            " Show invisible characters
	set listchars=tab:▸\ ,eol:¬,trail:· " Specify what display instead of invisible space

	" Highlight current line
	set cursorline
end


" ------------------------------------------
" Machine specific settings
" ------------------------------------------
let hostname = substitute(system('hostname'), '\n', '', '')
let terminal = substitute(system('echo $COLORTERM'), '\n', '', '')

if hostname == "thinkpad"
	colorscheme solarized
	set background=dark
	set guifont=Terminus\ 9
	highlight NonText guifg=#4a4a59     " EOL character color
	highlight SpecialKey guifg=#4a4a59  " Tab and space character color
	highlight SpecialKey guibg=#002B36

elseif hostname == "unused-4-222.brq.redhat.com"
	colorscheme base16-eighties
	set background=dark
	highlight NonText guifg=#4a4a59     " EOL character color
	highlight SpecialKey guifg=#4a4a59  " Tab and space character color
	highlight SpecialKey guibg=#2d2d2d

elseif hostname == "chromie" || hostname == "localhost.localdomain"
	if terminal == "rxvt-xpm"
		colorscheme base16-chalk
	end
end




" ------------------------------------------
" References
" ------------------------------------------

" [whitespace-on-save]
" http://vim.wikia.com/wiki/Remove_unwanted_spaces#Automatically_removing_all_trailing_whitespace [0]

" [copy-and-paste]
" http://www.linuxquestions.org/questions/linux-newbie-8/gvim-cut-copy-paste-374760/#post1916306

" [invisible-characters]
" http://vimcasts.org/episodes/show-invisibles/

" [ycm-jedi-vim]
" https://github.com/Valloric/YouCompleteMe/issues/234#issuecomment-37761434
