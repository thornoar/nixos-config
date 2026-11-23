return {
    "Julian/lean.nvim",
    event = { "BufReadPre *.lean", "BufNewFile *.lean" },

    dependencies = {
        -- optional dependencies:

        -- "nvim-telescope/telescope.nvim", -- for Lean-specific pickers
        -- "andymass/vim-matchup",          -- for enhanced % motion behavior
        -- "andrewradev/switch.vim",        -- for switch support
        -- "tomtom/tcomment_vim",           -- for commenting
    },

    config = function()
        require("lean").setup({
            -- Enable suggested mappings?
            --
            -- false by default, true to enable
            mappings = false,

            -- Enable the Lean language server(s)?
            --
            -- false to disable, otherwise should be a table of options to pass to `leanls`
            --
            -- See :help vim.lsp.Config for details.
            lsp = {
                -- lean.nvim replaces some default LSP handlers with enhanced versions.
                -- These can be individually disabled if they interfere with other plugins.
                enhanced_handlers = {
                    -- Replace the default hover with an interactive popup where
                    -- subexpressions are clickable (press K or <CR> to see a type,
                    -- gd to jump to a definition, etc.)
                    hover = true,

                    -- Replace the default diagnostics handler with one which filters
                    -- silent diagnostics and renders multi-line diagnostic signs.
                    diagnostics = true,
                },

                init_options = {
                    -- See Lean.Lsp.InitializationOptions for details and further options.

                    -- Time (in milliseconds) which must pass since latest edit until elaboration begins.
                    -- Lower values may make editing feel faster at the cost of higher CPU usage.
                    -- Note that lean.nvim changes the Lean default for this value!
                    editDelay = 3,

                    -- Whether to signal that widgets are supported.
                    hasWidgets = true,
                }
            },

            ft = {
                -- A list of patterns which will be used to protect any matching
                -- Lean file paths from being accidentally modified (by marking the
                -- buffer as `nomodifiable`).
                nomodifiable = {
                    -- by default, this list includes the Lean standard libraries,
                    -- as well as files within dependency directories (e.g. `_target`)
                    -- Set this to an empty table to disable.
                }
            },

            -- Abbreviation support
            abbreviations = {
                -- Enable expanding of unicode abbreviations?
                enable = true,
                -- additional abbreviations:
                extra = {
                    -- Add a \wknight abbreviation to insert ♘
                    --
                    -- Note that the backslash is implied, and that you of
                    -- course may also use a snippet engine directly to do
                    -- this if so desired.
                    wknight = '♘',
                },
                -- Change if you don't like the backslash
                -- (comma is a popular choice on French keyboards)
                leader = '\\',
            },

            -- Terminal graphics configuration.
            -- When enabled, lean.nvim renders rich content like images and SVGs
            -- via the Kitty graphics protocol in terminals that support it.
            graphics = {
                enabled = false,
            },

            -- Infoview support
            infoview = {
                -- Automatically open an infoview on entering a Lean buffer?
                -- Should be a function that will be called anytime a new Lean file
                -- is opened. Return true to open an infoview, otherwise false.
                -- Setting this to `true` is the same as `function() return true end`,
                -- i.e. autoopen for any Lean file, or setting it to `false` is the
                -- same as `function() return false end`, i.e. never autoopen.
                autoopen = true,

                -- Set the initial size (in columns/lines) of the infoview's window.
                -- Windows open horizontally or vertically based on available space.
                -- Values less than 1 are treated as a percentage of the current
                -- buffer's max columns or lines.
                width = 1/3,
                height = 1/3,

                -- Set the infoviews' orientation to be dynamic based on screen layout
                -- or fixed to a vertical or horizontal orientation
                -- auto | vertical | horizontal
                orientation = "horizontal",

                -- Put the infoview on the top or bottom when horizontal?
                -- top | bottom
                horizontal_position = "bottom",

                -- Always open the infoview window in a separate tabpage.
                -- Might be useful if you are using a screen reader and don't want too
                -- many dynamic updates in the terminal at the same time.
                -- Note that `height` and `width` will be ignored in this case.
                separate_tab = false,

                -- Show indicators for pin locations when entering an infoview window?
                -- always | never | auto (= only when there are multiple pins)
                indicators = "auto",
            },

            -- Imports-out-of-date
            on_imports_out_of_date = function(bufnr)
                -- A callback which will be called in the event that a file's imports
                -- have changed and the file must be rebuilt.
                --
                -- See https://github.com/leanprover/vscode-lean4/blob/master/vscode-lean4/manual/manual.md#file-restarting
                -- or the lean.nvim manual for further details.
                --
                -- The default will prompt you to confirm you wish to restart the file,
                -- but you can replace this implementation to customize how to handle
                -- imports being out of date by being either more or less aggressive with
                -- automatic restarting by explicitly calling `lean.lsp.restart_file(bufnr)`.
            end,

            -- Progress bar support
            progress_bars = {
                -- Enable the progress bars?
                -- By default, this is `true` if satellite.nvim is not installed, otherwise
                -- it is turned off, as when satellite.nvim is present this information would
                -- be duplicated.
                enable = true,  -- see above for default
                -- What character should be used for the bars?
                character = '│',
                -- Use a different priority for the signs
                priority = 10,
            },

            -- Customize the goal markers in the sign column and virtual text
            -- Can be set to an empty string to disable.
            goal_markers = {
                unsolved = '⚒',    -- shown inline in incomplete proofs
                accomplished = '✓', -- shown in the sign column for completed proofs
            },

            -- Diagnostic signs in the sign column.
            -- When enabled, multi-line diagnostics show guide characters marking
            -- their full range. Disable to restore vim.diagnostic's default signs.
            signs = {
                enabled = true,
            },

            -- Redirect Lean's stderr messages somewhere (to a buffer by default)
            stderr = {
                enable = true,
                -- height of the window
                height = 8,
                -- a callback which will be called with (multi-line) stderr output
                -- e.g., use:
                --   on_lines = function(lines) vim.notify(lines) end
                -- if you want to redirect stderr to `vim.notify`.
                -- The default implementation will redirect to a dedicated stderr
                -- window.
                on_lines = nil,
            },

            -- Debugging and introspection for lean.nvim internals
            debug = {
                -- A logging handler called with (log_level, data) for internal log messages
                log = function() end,
                -- Keep a ring buffer of this many RPC request records per session,
                -- queryable via require('lean.rpc').sessions() and .history(uri).
                -- 0 to disable.
                rpc_history = 0,
            },
        })
    end,
}
