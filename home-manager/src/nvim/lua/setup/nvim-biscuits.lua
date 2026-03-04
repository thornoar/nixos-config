return {
    "code-biscuits/nvim-biscuits",
    requires = {
        "nvim-treesitter/nvim-treesitter",
        -- run = ":TSUpdate"
    },
    config = function ()
        require("nvim-biscuits").setup({
            toggle_keybind = "<C-b>",
            show_on_start = false,
            max_file_size = "100kb",
            max_distance = 5,
            cursor_line_only = true,
            prefix_string = "^ "
        })
    end
}
