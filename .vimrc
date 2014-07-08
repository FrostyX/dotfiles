" ------------------------------------------
" Pathogen
" ------------------------------------------

" Adding new bundle
" cd ~/.vim/bundle/
" git submodule add git://github.com/foo/bar.git

let g:pathogen_disabled = [
	\ 'taglist',
	\ 'supertab',
	\ 'AutoComplPop',
	\ 'YouCompleteMe',
\]

" reference: [pathogen]
call pathogen#infect()
call pathogen#helptags()


if executable('ctags')
	call pathogen#interpose('bundle/taglist.vim')
endif

if filereadable($HOME.'/.vim/bundle/YouCompleteMe/third_party/ycmd/ycm_core.so')
	call pathogen#interpose('bundle/YouCompleteMe')
endif



" ------------------------------------------
" Global
" ------------------------------------------

"colorscheme solarized
"set background=dark
set nocompatible
set nowrap
set autochdir
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

" Terminal
map <leader>t :silent !urxvt -cd `pwd` &<CR>

" Saving
map <C-S> :w<CR>
imap <C-S> <ESC>:w<CR>i

" Select all
map <C-a> ggVG<CR>
imap <C-a> <ESC>ggVG<CR>i

" Copy & Paste
" reference: [copy-and-paste]
nmap <C-V> l"+gP
imap <C-V> <ESC><C-V>i
map <C-C> "+y
vmap <C-C> "+y

" Splits
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" Tabs
nmap <C-S-tab> :tabprevious<CR>
nmap <C-tab> :tabnext<CR>
nmap <C-t> :tabnew<CR>
map <C-t> :tabnew<CR>
map <C-S-tab> :tabprevious<CR>
map <C-tab> :tabnext<CR>
imap <C-S-tab> <ESC>:tabprevious<CR>i
imap <C-tab> <ESC>:tabnext<CR>i
imap <C-t> <ESC>:tabnew<CR>
map <C-w> :q<CR>
imap <C-w> <Esc>:q<CR>i



" ------------------------------------------
" Plugins
" ------------------------------------------

" Fugitive
cnoremap git Git
map <leader>ga :Gwrite<CR>
map <leader>gc :Gcommit<CR>
map <leader>gp :Git push<CR>
nnoremap <leader>fs :exec "file ". resolve(expand('%:p'))<CR>:e<CR>

" NERDTree
map <C-n> :NERDTreeToggle<CR>

" Taglist
map <C-m> :TlistToggle<CR>
let Tlist_Use_Right_Window = 1
let Tlist_GainFocus_On_ToggleOpen = 1

" Jedi-vim
" http://paste.debian.net/108459/
"let load_jedi = 0
"let g:jedi#auto_initialization = 0
"try
	"python import jedi
	"let load_jedi = 1
	"catch
"endtry

" Supertab
let g:SuperTabDefaultCompletionType = "<c-n>"

" AutoComplPop
let g:acp_behaviorKeywordLength = 3
let g:acp_behaviorKeywordCommand = "\<C-n>"

" tComment
map <leader>c <C-_><C-_>

" Syntastic
" :help syntastic_quiet_messages
let g:syntastic_quiet_messages = { "level": "warnings" }

" YouCompleteMe
" reference: [ycm-jedi-vim]
" jedi-vim {
	let g:jedi#auto_vim_configuration = 0
	let g:jedi#popup_on_dot = 0
	let g:jedi#popup_select_first = 0
	let g:jedi#completions_enabled = 0
	let g:jedi#completions_command = ""
	let g:jedi#show_call_signatures = "1"

	let g:jedi#goto_assignments_command = "<leader>pa"
	let g:jedi#goto_definitions_command = "<leader>pd"
	let g:jedi#documentation_command = "<leader>pk"
	let g:jedi#usages_command = "<leader>pu"
	let g:jedi#rename_command = "<leader>pr"
" }



" ------------------------------------------
" GUI specific settings
" ------------------------------------------
if has("gui_running")

	" Fix comments in solarized
	let g:solarized_italic=0

	colorscheme solarized
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
	highlight NonText guifg=#4a4a59     " EOL character color
	highlight SpecialKey guifg=#4a4a59  " Tab and space character color
	highlight SpecialKey guibg=#002B36

	" Highlight current line
	set cursorline

	" Terminus is not installed everywhere
	if isdirectory('/usr/share/fonts/terminus/')
		set guifont=Terminus\ 9
	end
end



" ------------------------------------------
" References
" ------------------------------------------

" [pathogen]
" http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/

" [whitespace-on-save]
" http://vim.wikia.com/wiki/Remove_unwanted_spaces#Automatically_removing_all_trailing_whitespace [0]

" [copy-and-paste]
" http://www.linuxquestions.org/questions/linux-newbie-8/gvim-cut-copy-paste-374760/#post1916306

" [invisible-characters]
" http://vimcasts.org/episodes/show-invisibles/

" [ycm-jedi-vim]
" https://github.com/Valloric/YouCompleteMe/issues/234#issuecomment-37761434
