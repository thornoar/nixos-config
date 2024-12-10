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
