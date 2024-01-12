vim.o.swapfile = false
vim.o.wrap = true
vim.o.linebreak = true
vim.o.list = false
vim.o.breakat = '   '
vim.opt.autochdir=true
vim.o.shell = '/usr/bin/sh'
vim.o.shell = '/bin/sh'
vim.wo.number = true
-- vim.wo.relativenumber = true
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = 'yes'
vim.o.updatetime = 1000
vim.o.timeout = false
vim.o.completeopt = 'menuone,noselect'
vim.o.termguicolors = true
vim.o.cmdheight = 1
vim.o.ruler = false
vim.o.termguicolors = true
vim.o.scrolloff = 5
vim.o.colorcolumn = 20
vim.o.tabstop = 4
vim.o.expandtab = true
vim.cmd('set shiftwidth=4 smarttab')
vim.o.shiftwidth = 4
vim.o.compatible = false
vim.o.hlsearch = false
vim.o.synmaxcol = 0
-- vim.o.cursorline = true
-- vim.o.guicursor=''


local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = '*',
})

vim.api.nvim_create_autocmd('VimLeave', {
	pattern = '*',
	callback = function () vim.opt.guicursor = { 'a:ver25' } end
})

vim.g.haskell_classic_highlighting = 1

vim.g.vimtex_delim_timeout = 60

vim.g.vimtex_complete_enabled = 1
vim.g.vimtex_quickfix_enabled = 0
vim.g.vimtex_view_method = 'zathura'
vim.g.latex_view_general_viewer = 'zathura'
vim.g.vimtex_compiler_progname = 'nvr'
vim.g.vimtex_view_general_options = '-reuse-instance -forward-search @tex @line @pdf'
-- vim.g.vimtex_view_automatic = 1
vim.g.vimtex_mappings_prefix = '\\'
-- vim.g.vimtex_compiler_method = 'latexmk'
vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 1, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])
-- vim.o.conceallevel = 1
-- vim.g.tex_conceal = 'abdmg'
-- vim.cmd('hi Conceal ctermbg=none')

vim.g.UltiSnipsExpandTrigger='<tab>'
vim.g.UltiSnipsJumpForwardTrigger='<C-Right>'
vim.g.UltiSnipsJumpBackwardTrigger='<C-Left>'
vim.g.UltiSnipsEditSplit='horizontal'

-- $markdown settings
--vim.o.vim_markdown_folding_disabled = 1
vim.o.vim_markdown_folding_level = 6
vim.o.vim_markdown_folding_style_pythonic = 1

vim.cmd.colorscheme 'onedark'
vim.cmd(
[[
    highlight Function guifg=burlywood
    highlight Number guifg=lightsteelblue
    highlight Include guifg=orchid
    highlight Type guifg=lightseagreen
    highlight Constant guifg=palevioletred gui=italic cterm=italic
    highlight Operator guifg=aquamarine
    highlight Keyword guifg=plum
]])
