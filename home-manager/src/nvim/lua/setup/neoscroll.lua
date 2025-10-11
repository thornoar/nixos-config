local ns = require("neoscroll")
ns.setup({
  -- mappings = {                 -- Keys to be mapped to their corresponding default scrolling animation
  --   '<PageUp>', '<PageDown>',
  --   '<C-PageUp>', '<C-PageDown>',
  --   '<C-y>', '<C-e>',
  --   'zt', 'zz', 'zb',
  -- },
  mappings = {},
  hide_cursor = true,          -- Hide cursor while scrolling
  stop_eof = true,             -- Stop at <EOF> when scrolling downwards
  respect_scrolloff = false,   -- Stop scrolling when the cursor reaches the scrolloff margin of the file
  cursor_scrolls_alone = true, -- The cursor will keep on scrolling even if the window cannot scroll further
  duration_multiplier = 1.0,   -- Global duration multiplier
  easing = 'linear',           -- Default easing function
  pre_hook = nil,              -- Function to run before the scrolling animation starts
  post_hook = nil,             -- Function to run after the scrolling animation ends
  performance_mode = false,    -- Disable "Performance Mode" on all buffers.
  ignored_events = {           -- Events ignored while scrolling
      'WinScrolled', 'CursorMoved'
  },
})

local opts = { duration = 100 }

vim.keymap.set("n", "<C-PageDown>", function() ns.scroll(0.2, opts) end)
vim.keymap.set("n", "<C-PageUp>", function() ns.scroll(-0.2, opts) end)
