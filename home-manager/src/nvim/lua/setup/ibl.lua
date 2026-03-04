return {
    "lukas-reineke/indent-blankline.nvim",
    -- version = "3.5.4",
    main = "ibl",
    opts = {},
    config = function ()
        require('ibl').setup({
            indent = {
                char = '|',
                -- highlight = "Comment",
            },
            scope = {
                enabled = false,
            },
            whitespace = {
                remove_blankline_trail = true,
            },
        })
    end
}
