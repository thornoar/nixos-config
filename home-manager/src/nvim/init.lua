vim.loader.enable()

local km = vim.keymap
vim.g.mapleader = ';'
km.set('n', 'ec', ':e $NIXOS_CONFIG/home-manager/src/nvim/init.lua<CR>')
local autosave = false

-- $install
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system {
        'git',
        'clone',
        '--filter=blob:none',
        'https://github.com/folke/lazy.nvim.git',
        '--branch=stable',
        lazypath,
    }
end
vim.opt.rtp:prepend(lazypath)
require('lazy').setup({
    defaults = { lazy = true },
    'tpope/vim-rhubarb',
    'tpope/vim-surround',
    'tpope/vim-repeat',
    'tpope/vim-fugitive',
    'sagarrakshe/toggle-bool',
    'farmergreg/vim-lastplace',
    'sirver/ultisnips',
    'neovimhaskell/haskell-vim',
    'numToStr/Comment.nvim',
    'ap/vim-css-color',
    'nanozuki/tabby.nvim',
    'lewis6991/gitsigns.nvim',
    'hjson/vim-hjson',
    'dkarter/bullets.vim',
    'chrisbra/csv.vim',
    'JuliaEditorSupport/julia-vim',
    {
        "folke/trouble.nvim",
        opts = {}, -- for default options, refer to the configuration section for custom setup.
        cmd = "Trouble",
        keys = {
            {
                "<M-S-d>",
                "<cmd>Trouble diagnostics toggle<cr>",
                desc = "Diagnostics (Trouble)",
            },
            {
                "<M-d>",
                "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
                desc = "Buffer Diagnostics (Trouble)",
            },
        },
    },
    {
        'nvim-lualine/lualine.nvim',
    },
    {
        'github/copilot.vim',
        cmd = 'Copilot'
    },
    'mbbill/undotree',
    { 'akinsho/toggleterm.nvim', version = '*', config = true },
    {
        'kdheepak/lazygit.nvim',
        dependencies = {
            'nvim-lua/plenary.nvim',
        },
        keys = {
            { '<M-g>', ':LazyGit<CR>', desc = 'LazyGit' }
        },
    },
    {
        'kaarmu/typst.vim',
        ft = 'typst',
        lazy = false,
    },
    {
        'nvim-telescope/telescope.nvim', tag = '0.1.6',
        dependencies = { 'nvim-lua/plenary.nvim' }
    },
    {
        'davvid/telescope-git-grep.nvim',
        dependencies = { "nvim-telescope/telescope.nvim" }
    },
    {
        'altermo/ultimate-autopair.nvim',
        event = { 'InsertEnter','CmdlineEnter' },
        branch = 'v0.6',
        opts = {
            space2 = { enable = true },
            tabout = { enable = true },
            fastwarp = {
                enable = true,
                enable_normal = true,
                enable_reverse = true,
                hopout = false,
                faster = false,
                map = '<M-/>',
        },
        internal_pairs={-- *ultimate-autopair-pairs-default-pairs*
            {'[',']',fly=true,dosuround=true,newline=true,space=true},
            {'(',')',fly=true,dosuround=true,newline=true,space=true},
            {'{','}',fly=true,dosuround=true,newline=true,space=true},
            {'"','"',suround=true,multiline=false},
            {'`','`', nft={'tex'},multiline=false},
            {'``',"''",ft={'tex'}},
            {'```','```',newline=true,ft={'markdown'}},
            {'<!--','-->',ft={'markdown','html'},space=true},
            {'"""','"""',newline=true,ft={'python'}},
            {"'''","'''",newline=true,ft={'python'}},
        },
    },
    },
    {
        'navarasu/onedark.nvim',
        priority = 1000,
    },
    {
        'lukas-reineke/indent-blankline.nvim',
        version = '3.5.4',
        main = 'ibl',
        opts = {},
    },
    {
        'nvim-treesitter/nvim-treesitter',
        dependencies = {
            'nvim-treesitter/nvim-treesitter-textobjects',
        },
        build = ':TSUpdate',
    },
    {
        'neovim/nvim-lspconfig',
        dependencies = {
            'hrsh7th/cmp-nvim-lsp',
        },
    },
    { 'simrat39/rust-tools.nvim' },
    {
        'hrsh7th/nvim-cmp',
        dependencies = {
            'L3MON4D3/LuaSnip',
        },
    },
    {
        "quangnguyen30192/cmp-nvim-ultisnips",
        config = function()
            require("cmp_nvim_ultisnips").setup({})
        end,
    },
    { 'hrsh7th/cmp-buffer', },
    { 'hrsh7th/cmp-path', },
    { 'https://github.com/octaltree/cmp-look' },
    { 'folke/neodev.nvim', opts = {} },
    -- 'thornoar/nvim-subfiles',
}, {})

require("setup-neodev")
require("setup-commands")

-- $keymaps:text
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
km.set('n', '<C-\\>', 'yy\\\\p')
km.set({'n', 'i', 'v'}, '<C-s>', function () vim.cmd('silent write') end)
km.set('n', '<C-x>', 'dd')

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
-- km.set('i', '<Up>', '<C-o>gk')
-- km.set('i', '<Down>', '<C-o>gj')
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

-- $keymaps:window
km.set('n', '<C-Left>', '<C-w>h')
km.set('n', '<C-Down>', '<C-w>j')
km.set('n', '<C-Up>', '<C-w>k')
km.set('n', '<C-Right>', '<C-w>l')
km.set('n', '<C-w><Left>', '<C-w>H')
km.set('n', '<C-w><Right>', '<C-w>L')
km.set('n', '<C-w><Up>', '<C-w>K')
km.set('n', '<C-w><Down>', '<C-w>J')
km.set('n', '<C-M-o>', '<C-w>+')
km.set('n', '<C-M-l>', '<C-w>-')
km.set('n', '<C-M-i>', '<C-w>>')
km.set('n', '<C-M-u>', '<C-w><')
km.set('n', '<C-M-k>', '<C-w>=')
km.set('n', '<C-M-w>', '<C-w>o')
km.set('n', '<C-Delete>', '<cmd>wqa<CR>')
km.set('n', '<C-c>', function()
    if (#vim.api.nvim_list_wins() < 2) then
        for _, ui in pairs(vim.api.nvim_list_uis()) do
            if ui.chan and not ui.stdout_tty then
                vim.fn.chanclose(ui.chan)
            end
        end
    else
        local buf = vim.api.nvim_win_get_buf(0)
        if vim.bo[buf].readonly or (vim.api.nvim_buf_is_loaded(buf) and vim.api.nvim_buf_get_name(buf) == '') then
            vim.cmd('quit!')
        else
            vim.cmd('wqa')
        end
    end
end, { noremap = true })

-- $keymaps:command
km.set('n', '<M-t>', function () vim.cmd('silent !$TERMINAL --title \'Terminal\' &') end)
km.set('n', '<M-b>', function () vim.cmd('silent !$TERMINAL --title \'Viewer\' -e zsh -c \'nvim-server; br\'&') end)
km.set('n', '<leader>ek', function () vim.cmd('edit $NIXOS_CONFIG/home-manager/home.nix') end)
km.set('n', '<leader>eK', function () vim.cmd('tabnew $NIXOS_CONFIG/home-manager/home.nix') end)
km.set('n', '<leader>el', function () vim.cmd('edit $NIXOS_CONFIG/home-manager/src/nvim/init.lua') end)
km.set('n', '<leader>eL', function () vim.cmd('tabnew $NIXOS_CONFIG/home-manager/src/nvim/init.lua') end)
km.set('n', '<leader>en', function () vim.cmd('edit $NIXOS_LOCAL/home-local.nix') end)
km.set('n', '<leader>eN', function () vim.cmd('tabnew $NIXOS_LOCAL/system-local.nix') end)
km.set('n', '<leader>ee', function ()
    local ft = vim.bo.filetype
    vim.cmd('sp $NIXOS_CONFIG/home-manager/src/nvim/UltiSnips/' .. ft .. '.snippets')
end)
km.set('n', '<leader>cd', function () vim.cmd('Copilot disable') end)
km.set('n', '<leader>ce', function () vim.cmd('Copilot enable') end)
km.set('n', 'Z', function () vim.cmd('ToggleBool') end)
km.set('n', '<M-s>', function () vim.cmd('silent Gitsigns preview_hunk_inline') end)

-- $telescope
local telescope = require('telescope.builtin')
km.set('n', '<M-/>', function () telescope.live_grep({ search_dirs = { vim.fn.expand('%') } }) end)
km.set('n', '<C-/>', function () telescope.grep_string{ search_dirs = { vim.fn.expand('%') }, shorten_path = true, word_match = "-w", only_sort_text = true, search = '' } end)
km.set('n', '<C-M-/>', function () require('git_grep').live_grep() end)
km.set('n', '<M-e>', telescope.jumplist)
km.set('n', '<leader>/', telescope.search_history)
km.set('n', '<leader>?', telescope.command_history)
km.set('n', '<leader>:', telescope.commands)
km.set('n', '<M-h>', telescope.help_tags)
km.set('n', '<M-q>', telescope.builtin)
km.set('n', '<M-g>', telescope.git_files)
km.set('n', '<M-f>', telescope.find_files)
km.set('n', '<M-x>', function ()
    telescope.buffers({ previewer = true })
end)
local state = require('telescope.state')
local action_state = require('telescope.actions.state')
local slow_scroll = function(prompt_bufnr, direction)
    local previewer = action_state.get_current_picker(prompt_bufnr).previewer
    local status = state.get_status(prompt_bufnr)
    if type(previewer) ~= 'table' or previewer.scroll_fn == nil or status.preview_win == nil then
        return
    end
    previewer:scroll_fn(1 * direction)
end
vim.api.nvim_create_user_command('CS', telescope.colorscheme, {})
require('telescope').setup({
    defaults = {
        color_devicons = false,
        mappings = {
            i = {
                ['<C-Down>'] = function(bufnr) slow_scroll(bufnr, 1) end,
                ['<C-Up>'] = function(bufnr) slow_scroll(bufnr, -1) end,
            },
        },
    },
    extensions = {
        git_grep = {
            cwd = '%:h:p',
            regex = 'extended',
            skip_binary_files = true,
            use_git_root = true
        }
    },
    pickers = {
        git_files = { theme = "ivy" },
        find_files = { theme = "ivy" },
        commands = { theme = "ivy" },
        command_history = { theme = "ivy" },
        search_history = { theme = "ivy" },
        help_tags = { theme = "ivy" },
        builtin = { theme = "ivy" },
        jumplist = { theme = "ivy" },
        live_grep = { theme = "ivy" },
        grep_string = { theme = "ivy" },
        colorscheme = {
            enable_preview = true,
        },
    },
})
require('telescope').load_extension('git_grep')

-- $comment
require('Comment').setup({
    ignore = nil,
    padding = true,
    sticky = true,
    toggler = {
        line = '\\\\',
        block = '||',
    },
    opleader = {
        line = '\\',
        block = '|',
    },
    extra = {
        above = '<C-\\>O',
        below = '<C-\\>o',
        eol = '<C-S-a>',
    },
    mappings = {
        basic = true,
        extra = true,
    },
    pre_hook = nil,
    post_hook = nil,
})
local ft = require('Comment.ft')
ft.set('asy', { '//%s', '/*%s*/' })
ft.set('hjson', { '#%s' })

-- $gitsigns
require('gitsigns').setup({
    numhl = true,
    current_line_blame = false,
    current_line_blame_opts = {
        delay = 1000,
        ignore_whitespace = true,
    },
    max_file_length = 10000,
    on_attach = function(bufnr)
        local gitsigns = require('gitsigns')

        local function map (mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            km.set(mode, l, r, opts)
        end

        map('n', ']c', function ()
            if vim.wo.diff then
                vim.cmd.normal({']c', bang = true})
            else
                gitsigns.nav_hunk('next')
            end
        end)
        map('n', '[c', function ()
            if vim.wo.diff then
                vim.cmd.normal({'[c', bang = true})
            else
                gitsigns.nav_hunk('prev')
            end
        end)
        map('v', '<leader>hs', function() gitsigns.stage_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end)
        map('v', '<leader>hr', function() gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') }) end)
        map('n', '<leader>hb', function() gitsigns.blame_line({ full=true }) end)
        map('n', '<leader>hD', function() gitsigns.diffthis('~') end)
        map('n', '<leader>hs', gitsigns.stage_hunk)
        map('n', '<leader>hr', gitsigns.reset_hunk)
        map('n', '<leader>hS', gitsigns.stage_buffer)
        map('n', '<leader>hu', gitsigns.undo_stage_hunk)
        map('n', '<leader>hR', gitsigns.reset_buffer)
        map('n', '<leader>hp', gitsigns.preview_hunk)
        map('n', '<leader>hd', gitsigns.diffthis)
        map('n', '<leader>td', gitsigns.toggle_deleted)
        map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
    end,
})

-- $treesitter
require('nvim-treesitter.configs').setup({
    auto_install = false,
    modules = {},
    sync_install = true,
    ignore_install = {},
    ensure_installed = { 'c', 'lua', 'python', 'vimdoc', 'vim', 'hjson', 'java', 'markdown_inline', 'typst', 'html', 'css' },
    highlight = { enable = true },
    indent = { enable = false },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = '<C-space>',
            node_incremental = '<C-space>',
            scope_incremental = '<C-s>',
            node_decremental = '<M-space>',
        },
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = { query = '@class.inner', desc = 'Select inner part of a class region' },
                ['as'] = { query = '@scope', query_group = 'locals', desc = 'Select language scope' },
            },
            selection_modes = {
                ['@parameter.outer'] = 'v',
                ['@function.outer'] = 'V',
                ['@class.outer'] = '<C-v>',
            },
            include_surrounding_whitespace = false,
        },
    },
})

-- $toggleterm
require('toggleterm').setup({
    size = function(term)
        if term.direction == 'horizontal' then
            return 20
        elseif term.direction == 'vertical' then
            return vim.o.columns * 0.4
        end
    end,
    shade_terminals = false,
    open_mapping = [[<S-M-t>]],
    hide_numbers = true,
    autochdir = true,
    terminal_mappings = true,
    direction = 'float',
    shell = vim.o.shell,
    border = 'single'
})

-- $ibl
require('ibl').setup({
    indent = {
        -- char = '┊',
        char = '|',
    },
    whitespace = {
        remove_blankline_trail = true,
    },
})

-- $lsp
vim.diagnostic.config({
    virtual_text = false
})
local symbols = { Error = "E", Info = "I", Hint = "H", Warn = "W" }
for name, icon in pairs(symbols) do
    local hl = "DiagnosticSign" .. name
    vim.fn.sign_define(hl, { text = icon, numhl = hl, texthl = hl })
end
km.set('n', '<C-S-d>', function () vim.diagnostic.goto_prev() end)
km.set('n', '<C-d>', function () vim.diagnostic.goto_next() end)
vim.api.nvim_create_autocmd('LspAttach', {
    desc = 'LSP actions',
    callback = function(event)
        local opts = { buffer = event.buf }
        km.set('n', '<C-Space>', function () vim.lsp.buf.hover() end, opts)
        -- km.set('n', '<C-M-CR>', function () vim.lsp.buf.references() end, opts)
        km.set('n', '<M-S-CR>', function () vim.lsp.buf.references() end, opts)
        km.set({ 'n', 'x' }, '<leader>cf', function () vim.lsp.buf.format({ async = true }) end, opts)
        km.set('n', '<leader>ca', function () vim.lsp.buf.code_action() end, opts)
    end
})

local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
local hl_name = "FloatBorder"
-- local border = { '+', '-', '+', '|', '+', '-', '+', '|' }
local border = {
    { "╭", hl_name },
    { "─", hl_name },
    { "╮", hl_name },
    { "│", hl_name },
    { "╯", hl_name },
    { "─", hl_name },
    { "╰", hl_name },
    { "│", hl_name },
}
local handlers =  {
    ["textDocument/hover"] =  vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
    ["textDocument/signatureHelp"] =  vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
}

local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')
local util = require 'lspconfig.util'
local lspbasicconfig = {
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
}

if not configs.asy_ls then
    configs.asy_ls = {
        default_config = {
            cmd = {'asy', '-lsp'},
            filetypes = {'asy'},
            root_dir = function(fname)
                return util.find_git_ancestor(fname)
            end,
            single_file_support = true,
            settings = {},
        },
    }
end
lspconfig.asy_ls.setup(lspbasicconfig)

vim.diagnostic.config({
    float = { border = "rounded", },
})

lspconfig.texlab.setup(lspbasicconfig)
lspconfig.ts_ls.setup(lspbasicconfig)
lspconfig.pyright.setup(lspbasicconfig)
lspconfig.clojure_lsp.setup(lspbasicconfig)
-- lspconfig.nil_ls.setup(lspbasicconfig)
lspconfig.nixd.setup({
    handlers = handlers,
    capabilities = lsp_capabilities,
    cmd = { "nixd" },
    settings = {
        nixd = {
            nixpkgs = {
                expr = "import <nixpkgs { }>",
            },
            formatting = {
                command = { "alejandra" },
            },
            options = {
                nixos = {
                    expr = "(builtins.getFlake \"/home/ramak/projects/nixos-config\").nixosConfigurations.master.options",
                },
            },
        },
    },
})
lspconfig.r_language_server.setup(lspbasicconfig)
lspconfig.hls.setup(lspbasicconfig)
lspconfig.clangd.setup(lspbasicconfig)
lspconfig.bashls.setup(lspbasicconfig)

lspconfig.tinymist.setup({
    -- settings = {
    --     exportPdf = "never",
    -- },
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
})

lspconfig.rust_analyzer.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    settings = {
        ['rust-analyzer'] = {
            check = {
                command = "clippy"
            },
            diagnostics = {
                enable = true
            }
        }
    }
})
local rt = require("rust-tools")
rt.setup({
    server = {
        on_attach = function(_, bufnr)
            -- Hover actions
            vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
            -- Code action groups
            vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
        end,
    },
})

lspconfig.lua_ls.setup({
    autostart = true,
    capabilities = lsp_capabilities,
    handlers = handlers,
    root_dir = function (_)
        return vim.loop.cwd()
    end,
    settings = {
        Lua = {
            diagnostics = {
                globals = { 'vim' }
            }
        }
    }
})

vim.api.nvim_create_user_command('LA', 'LspStart', {})
vim.api.nvim_create_user_command('LD', 'LspStop', {})
local luasnip = require('luasnip')
local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")
local cmp = require('cmp')
cmp.setup({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'buffer' },
        { name = 'ultisnips' },
        {
            name = 'look',
            keyword_length = 2,
            option = {
                convert_case = true,
                loud = true,
                dict = os.getenv('WORDLIST')
            }
        },
        -- {
        --     name = "html-css",
        --     option = {
        --         enable_on = {
        --             'html'
        --         },
        --     },
        -- },
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<Down>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<Up>'] = cmp.mapping(function (fallback)
            cmp.close()
            fallback()
        end, { 'i' }),
        ['<C-x>'] = cmp.mapping.select_next_item({}),
        ['<C-Down>'] = cmp.mapping.select_next_item({}),
        ['<C-a>'] = cmp.mapping.select_prev_item({}),
        ['<C-Up>'] = cmp.mapping.select_prev_item({}),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-Right>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(1) then
                luasnip.jump(1)
            else
                cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
        end, { 'i', 's' }),
        ['<C-Left>'] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(-1) then
                luasnip.jump(-1)
            else
                cmp_ultisnips_mappings.jump_backwards(fallback)
            end
        end, { 'i', 's' }),
    }),
    snippet = {
        expand = function(args)
            -- luasnip.lsp_expand(args.body)
            vim.fn["UltiSnips#Anon"](args.body)
        end,
    },
})

-- $settings
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
-- vim.o.colorcolumn = "20"
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

-- vim.g.python3_host_prog = "/etc/profiles/per-user/ramak/bin/python"

-- vim.g.vimtex_complete_enabled = 1
-- vim.g.vimtex_quickfix_enabled = 0
-- -- vim.g.vimtex_view_method = 'zathura'
-- -- vim.g.latex_view_general_viewer = 'zathura'
-- -- vim.g.vimtex_compiler_progname = 'nvr'
-- vim.g.vimtex_view_general_options = '-reuse-instance -forward-search @tex @line @pdf'
-- vim.g.vimtex_mappings_prefix = '\\'
-- vim.cmd([[let g:vimtex_compiler_latexmk = {'continuous': 0, 'aux_dir': '.aux', 'options': ['-verbose', '-synctex=1', '-interaction=nonstopmode', '-file-line-error']}]])

vim.cmd([[
    let tex_flavor="latex"
    " set shiftwidth=4 smarttab
    let g:omni_sql_no_default_maps = 1
    autocmd BufEnter * set formatoptions-=cro
    autocmd BufEnter * setlocal formatoptions-=cro
]])

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})

vim.api.nvim_create_autocmd('vimLeave', {
    pattern = '*',
    callback = function () vim.opt.guicursor = { 'a:ver25' } end
})

vim.g.typst_embedded_languages = {'haskell', 'c', 'java'}

-- $UltiSnips setup
-- vim.g.UltiSnipsExpandTrigger='<C-Right>'
-- vim.g.UltiSnipsJumpForwardTrigger='<C-Right>'
-- vim.g.UltiSnipsJumpBackwardTrigger='<C-Left>'
vim.g.UltiSnipsEditSplit='horizontal'

-- $markdown setup
-- vim.o.vim_markdown_folding_level = 6
-- vim.o.vim_markdown_folding_style_pythonic = 1

package.path = package.path .. ';'..os.getenv('PROJECTS')..'/nvim-subfiles/lua/?.lua'
require('nvim-subfiles').setup({
    bindings = { },
    opts = {
        jump_to_file = false,
    }
})

local colors = require("colors")

require('tabby.tabline').set(function(line)
    return {
        line.tabs().foreach(function(tab)
            local hl = tab.is_current() and { fg = colors.colorMagenta0, bg = colors.bgColor0 } or { fg = colors.colorWhite3, bg = colors.bgColor0 }
            return {
                line.sep("", hl, { bg = colors.bgColor0 }),
                tab.name(),
                line.sep("", hl, { bg = colors.bgColor0 }),
                hl = hl,
                margin = ' ',
            }
        end),
        hl = { bg = colors.bgColor0 },
    }
end)

require('onedark').setup  {
    style = 'dark',
    colors = {
        bg0 = colors.bgColor0,
        bg1 = colors.bgColor1,
        bg2 = colors.bgColor2,
        bg3 = colors.bgColor3,
    },
    transparent = true,
    term_colors = true,
    ending_tildes = false,
    cmp_itemkind_reverse = false,
    toggle_style_key = '<leader>wt',
    toggle_style_list = { 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer', 'light' },
    code_style = {
        comments = 'italic',
        keywords = 'none',
        functions = 'none',
        strings = 'none',
        variables = 'none'
    },
    lualine = {
        transparent = false,
    },
    highlights = {},
}

vim.cmd.colorscheme 'onedark'

vim.cmd( [[
    highlight Function guifg=burlywood
    highlight Number guifg=lightsteelblue
    highlight Include guifg=orchid
    highlight Type guifg=lightseagreen
    highlight Constant guifg=palevioletred gui=italic cterm=italic
    highlight Operator guifg=aquamarine
    highlight Keyword guifg=plum
]])

local function keymap()
    if vim.opt.iminsert:get() > 0 and vim.b.keymap_name then
        return vim.b.keymap_name
    end
    return 'en'
end
require('lualine').setup{
    options = {
        icons_enabled = true,
        theme = {
            normal = {
                a = { fg = colors.bgColor0, bg = colors.colorMagenta1, gui = 'bold' },
                b = { fg = colors.colorWhite3, bg = colors.bgColor0 },
                c = { fg = colors.colorWhite3, bg = colors.bgColor0 },
            },
            command = { a = { fg = colors.bgColor0, bg = colors.colorYellow1, gui = 'bold' } },
            insert = { a = { fg = colors.bgColor0, bg = colors.colorRed1, gui = 'bold' } },
            visual = { a = { fg = colors.bgColor0, bg = colors.colorMagenta0, gui = 'bold' } },
            terminal = { a = { fg = colors.bgColor0, bg = colors.colorCyan, gui = 'bold' } },
            replace = { a = { fg = colors.bgColor0, bg = colors.colorMagenta1, gui = 'bold' } },
            inactive = {
                a = { fg = colors.bgColor0, bg = colors.bgColor0, gui = 'bold' },
                b = { fg = colors.bgColor0, bg = colors.bgColor0 },
                c = { fg = colors.bgColor0, bg = colors.bgColor0 },
            },
        },
        component_separators = '|',
        section_separators = "",
    },
    sections = {
        lualine_a = { 'mode' },
        lualine_b = { 'branch', 'diff', 'diagnostics' },
        lualine_c = { 'filename', keymap },
        lualine_x = { 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' },
    },
}
