vim.loader.enable()
local km = vim.keymap

km.set('n', 'w1', 'mL')
km.set('n', 'w2', 'mN')
km.set('n', 'w3', 'mM')
km.set('n', 'w4', 'mO')
km.set('n', 'e1', '\'L')
km.set('n', 'e2', '\'N')
km.set('n', 'e3', '\'M')
km.set('n', 'e4', '\'O')
km.set('n', '<S-End>', 'o<Esc>')
km.set('n', '<S-Home>', 'ddk')
km.set('n', '<C-S-Left>', '<<')
km.set('n', '<C-S-Right>', '>>')
km.set('v', '<C-S-Left>', '<gv')
km.set('v', '<C-S-Right>', '>gv')
km.set('v', '<S-Left>', 'ygv<Esc>')
km.set('n', '<S-Left>', 'yy')
km.set({'n', 'v'}, '<S-Right>', 'p')
km.set('i', '<S-Right>', '<esc>pa')
km.set('n', '<M-CR>', 'md*ggn')
km.set('n', '<M-]>', '\'d')
km.set('v', '<C-S-Down>', ':m \'>+1<CR>gv=gv')
km.set('v', '<C-S-Up>', ':m \'<-2<CR>gv=gv')
km.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })
km.set('v', 'z', '<esc>')
km.set('v', '<S-Up>', '<Up>')
km.set('v', '<S-Down>', '<Down>')
km.set('n', '<S-Down>', '<S-v>j')
km.set('n', '<S-Up>', '<S-v>k')
km.set('v', '<leader>a', ':s/\\d\\+/\\=(submatch(0)+1)/g')
km.set('n', '<leader>cw', ':%s/\\<<C-r><C-w>\\>/')
km.set('n', 'cw', 'ciw')
km.set('n', 'dw', 'diw')
km.set('n', 'vw', 'viw')
km.set({'v', 'x'}, '<leader>p', '\"_dP')
km.set('n', '<leader>f', 'zf%')
km.set('n', '<C-End>', 'k<S-v>jj<S-j>')
km.set('v', '<M-End>', 'J')
km.set('n', '<C-z>', 'u')
km.set('n', '<C-M-z>', ':UndotreeToggle<CR>')
km.set('n', '<M-z>', '<C-r>')
km.set('n', 'daa', 'F,dt)')
km.set('n', '<C-space>', 'yy<C-del>p')

-- $keymaps:insert
km.set('i', '<C-Space>', ' ')
km.set('i', '<C-Delete>', '<BS><Delete>')
-- km.set('i', '<C-x>', '<C-n>')
km.set('i', '<M-a>', '<C-o>$;')
km.set('i', '<C-z>', '<Esc>[s1z=A')
km.set('n', 'x', 'i')
km.set('n', 'X', 'I')
km.set('i', '<M-e>', 'X<bs><C-o>:E<CR>')
km.set('i', '<M-r>', 'X<bs><C-o>:R<CR>')
km.set('i', '<M-g>', 'X<bs><C-o>:D<CR>')
km.set('i', '<M-j>', 'X<bs><C-o>:J<CR>')
km.set('i', '<C-m>', '$$<Left>')

-- $keymaps:terminal
km.set('t', '<C-q>', '<C-\\><C-n>')

-- $keymaps:navigation
km.set('n', '<Up>', 'gk')
km.set('n', '<Down>', 'gj')
km.set({'n', 'v'}, '<M-Up>', '5k')
km.set('v', '<S-M-Up>', '5k')
km.set({'n', 'v'}, '<M-Down>', '5j')
km.set('v', '<S-M-Down>', '5j')
km.set({'n', 'v'}, '<M-Left>', '5h')
km.set({'n', 'v'}, '<M-Right>', '5l')
km.set('i', '<M-up>', '<C-o>5k')
km.set('i', '<M-down>', '<C-o>5j')
km.set('i', '<M-left>', '<C-o>5h')
km.set('i', '<M-right>', '<C-o>5l')
km.set({'n', 'v'}, '<M-a>', '%')
km.set('n', 'n', 'nzz')
km.set('n', 'N', 'Nzz')
km.set('n', '<C-M-Left>', '<C-^>')
km.set('t', '<C-M-Left>', '<C-\\><C-n><C-^>')
km.set('n', '<C-M-Right>', 'gf')

km.set('n', '<C-c>', '<cmd>q!<CR>')

vim.o.swapfile = false
vim.o.wrap = false
vim.o.linebreak = true
vim.o.list = false
vim.o.breakat = '   '
vim.opt.autochdir=true
vim.o.shell = '/usr/bin/env zsh'
vim.wo.number = true
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
vim.o.expandtab = true
vim.o.compatible = false
vim.o.hlsearch = false
vim.o.incsearch = true
vim.o.synmaxcol = 0
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.clipboard = 'unnamedplus'
vim.o.undofile = true
vim.o.cursorline = false
vim.g.neovide_transparency = 0.9


local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
	callback = function()
		vim.highlight.on_yank()
	end,
	group = highlight_group,
	pattern = '*',
})

vim.api.nvim_create_autocmd({ 'CursorMoved' }, {
	pattern = '*',
	callback = function()
        vim.cmd("g/^$/d")
	end,
})

vim.api.nvim_create_user_command('START', function ()
    vim.cmd("silent write! /tmp/kitty_scrollback_buffer")
    vim.cmd("te cat /tmp/kitty_scrollback_buffer -")
    vim.cmd("set ma")
    vim.cmd("normal G")
    print("hi!")
    -- vim.cmd("g/^$/d")
end, {})
