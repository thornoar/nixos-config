local autosave = false

-- $commands
local compilefunc = {
    ['asy'] = function (name) return ('!asy -noV -nosafe ' .. name) end,
    ['r'] = function (name) return ('!Rscript ' .. name) end,
    ['julia'] = function (name) return ('!julia ' .. name) end,
    ['python'] = function (name) return ('!python ' .. name) end,
    ['c'] = function (name) return ('!gcc ' .. name .. ' -o a.out && ./a.out') end,
    ['cpp'] = function (name) return ('!g++ -Wall ' .. name .. ' -o cpp.out && ./cpp.out') end,
    ['rust'] = function (name) return ('!rustc ' .. name .. ' -o rust.out && ./rust.out') end,
    ['haskell'] = function (name) return ('!runhaskell ' .. name) end,
    ['tex'] = function (name)
        return autosave and ('!latexmk -g -pdf -synctex=1 -verbose -auxdir=./.aux ./' .. name) or ''
    end,
    ['typst'] = function (_) return ('') end,
    ['lua'] = function (name) return ('!lua ' .. name) end,
    ['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}
local daemonfunc = {
    ['typst'] = function (name) return ('terminal typst watch ' .. name .. ' --root ..') end,
    -- ['typst'] = function (_) return 'TypstWatch' end,
    ['tex'] = function (name) return ('terminal latexmk -g -pdf -pvc -synctex=1 -auxdir=./.aux ./' .. name) end,
}
local compile = function (daemon, silent)
    return function ()
        local compilecmd = (daemon and daemonfunc or compilefunc)[vim.bo.filetype]
        if not compilecmd then
            print('not set to compile')
        else
            if vim.bo.modified then vim.cmd('write') end
            vim.cmd((silent and 'silent ' or '') .. compilecmd('%:t'))
        end
    end
end
vim.api.nvim_create_user_command('Compile', compile(false, false), {})
vim.keymap.set('n', '<leader>o', ':Compile<CR>')
vim.keymap.set({'n', 'i'}, '<C-a>', compile(false, true))
vim.keymap.set({'n', 'i'}, '<C-M-a>', compile(true, false))

local defaultoutputname = 'out'
local view_output = function (args)
    local ext, flags = (args and args['args'] or 'pdf'):match"^(%S+)%s+(.+)"
    if not ext then ext = (args and args['args'] or 'pdf') end
    if not flags then flags = '' else flags = ' '..flags end
    if io.open(vim.fn.expand('%:r')..'.'..ext, 'r') ~= nil then
        vim.cmd('silent !xdg-open %:r.'..ext..'&')
    elseif io.open(defaultoutputname..'.'..ext, 'r') ~= nil then
        vim.cmd('silent !xdg-open '..defaultoutputname..'.'..ext..'&')
    else
        vim.cmd('silent !blkcmd '..ext..' xdg-open'..flags..'&')
    end
end
vim.api.nvim_create_user_command('View', view_output, { nargs = '?' })
vim.keymap.set('n', '<leader>vp', function () view_output({ ['args'] = 'pdf' }) end)
vim.keymap.set('n', '<leader>vs', function () view_output({ ['args'] = 'svg' }) end)
vim.keymap.set('n', '<leader>vg', function () view_output({ ['args'] = 'png' }) end)

vim.api.nvim_create_user_command('E', function () vim.bo.keymap = '' end, {})
vim.api.nvim_create_user_command('R', function () vim.bo.keymap = 'russian-jcuken' end, {})
vim.api.nvim_create_user_command('D', function () vim.bo.keymap = 'german-qwertz' end, {})
vim.api.nvim_create_user_command('J', function () vim.bo.keymap = 'kana' end, {})
vim.api.nvim_create_user_command('SPELL', function () vim.wo.spell = not vim.wo.spell end, {})
vim.api.nvim_create_user_command('S', function () vim.cmd('silent write') end, {})
vim.api.nvim_create_user_command('WRAP', function () vim.o.wrap = not vim.o.wrap end, {})
vim.api.nvim_create_user_command('T', function (args)
    local dir = args and args['args'] or '.'
    vim.cmd('silent !$TERMINAL --title \'Terminal\' -e zsh -c \'cd '..dir..'; zsh\' &')
end, { nargs = '?' })

-- $autocommands
vim.api.nvim_create_user_command('AS', function()
    autosave = not autosave
    print('autosave is ' .. (autosave and 'enabled' or 'disabled'))
end, {})
-- local autosavepattern = {
--     '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt',
--     '*.r', '*.snippets', '*.nix', '*.hjson', '*.vim', '*.sh',
--     '*.html', '*.css', '*.c', '*.jl', '*.yml', '*.conf', '*.rs', '*.cabal'
-- }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
    pattern = '*.*',
    callback = function()
        if autosave and
        not vim.bo[vim.api.nvim_win_get_buf(0)].readonly and
        vim.bo[vim.api.nvim_win_get_buf(0)].buftype == '' then
            vim.cmd('silent write')
        end
    end
})

vim.api.nvim_create_autocmd({ 'BufEnter' }, {
    pattern = { '*.tex', '*.typ', '*.md' },
    callback = function ()
        vim.o.wrap = true
    end
})

vim.api.nvim_create_autocmd({ 'BufEnter' }, {
    pattern = "*.*",
    callback = function ()
        local ft = vim.bo.filetype
        if ft == 'haskell' or ft == 'typst' or ft == "markdown" then
            vim.o.tabstop = 2
            vim.o.shiftwidth = 2
            vim.o.expandtab = true
            vim.o.softtabstop = 2
        else
            vim.o.tabstop = 4
            vim.o.shiftwidth = 4
            vim.o.expandtab = true
            vim.o.softtabstop = 4
        end
    end
})
