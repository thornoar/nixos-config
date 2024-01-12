local emptycompile = 'echo \"not set to compile\"';
local defaultoutputname = 'output'
local compilecmd = {
	['asy'] = 'asy -nosafe -noV',
	['r'] = 'Rscript',
	['py'] = 'python',
	['c'] = 'gcc',
	['cpp'] = 'g++ -o cpp.out',
    ['rust'] = 'rustc -o rust.out',
	['hs'] = 'runhaskell',
	['tex'] = 'latexmk -g -pdf',
	['lua'] = 'lua',
	['lol'] = 'lci',
	['java'] = 'javac',
    ['pdf'] = 'zathura',
    ['nix'] = 'nix eval --file',
}
local compilefunc = {
	['asy'] = function (name) return ('!asy -noV -nosafe ' .. name) end,
	['r'] = function (name) return ('!Rscript ' .. name) end,
	['python'] = function (name) return ('!python ' .. name) end,
	['c'] = function (name) return ('!gcc ' .. name .. ' && ./a.out') end,
	['cpp'] = function (name) return ('!g++ -Wall ' .. name .. ' -o cpp.out && ./cpp.out') end,
    ['rust'] = function (name) return ('!rustc ' .. name .. ' -o rust.out && ./rust.out') end,
	['haskell'] = function (name) return ('!runhaskell ' .. name) end,
	['tex'] = function (name) return ('!latexmk -g -pdf ' .. name) end,
	['lua'] = function (name) return ('!lua ' .. name) end,
	['lolcode'] = function (name) return ('!lci ' .. name) end,
	['java'] = function (name) return ('!javac ' .. name .. ' && java Main') end,
    ['pdf'] = function (name) return ('!nohup zathura ' .. name .. '&') end,
    ['nix'] = function (name) return ('!nix eval --file ' .. name) end,
}

newcmd("AS", function() autosave = not autosave end)
newcmd('C', function () vim.cmd('tabclose') end)
newcmd('Compile', function () 
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and emptycompile or ccmd('%:t'))
end)
newcmd('CompileSilent', function () 
	local ccmd = compilefunc[vim.bo.filetype]
	vim.cmd(not ccmd and '' or 'silent '..ccmd('%:t'))
end)

vim.api.nvim_create_user_command('BC', function (ext)
	local cmd = compilecmd[ext['args']]
	if not cmd then
		print('invalid file extension provided')
	else
		vim.cmd('!bulkcompile '..ext['args']..' '..'\"'..cmd..'\"')
	end
end, { nargs='?' })

local autocompile = false
local autocompilepattern = { '*.asy', '*.cpp', '*.tex', '*.hs' }
newcmd('AC', function ()
	autocompile = not autocompile
	print('autocompile: '..tostring(autocompile))
end)
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI' }, {
	pattern = autocompilepattern,
	callback = function ()
		if autocompile then vim.cmd('CompileSilent') end
	end
})

newcmd('ViewPdf', function ()
	if io.open(vim.fn.expand('%:r')..'.pdf', 'r') ~= nil then
		vim.cmd('silent !zathura %:r.pdf&')
	elseif io.open(defaultoutputname..'.pdf', 'r') ~= nil then
		vim.cmd('silent !$READER '..defaultoutputname..'.pdf&')
	else
		vim.cmd('silent !pdfviewall')
	end
end)
newcmd('E', function () vim.bo.keymap = '' end)
newcmd('R', function () vim.bo.keymap = 'russian-jcuken' end)
newcmd('J', function () vim.bo.keymap = 'kana' end)
newcmd('G', function () vim.bo.keymap = 'german-qwertz' end)
newcmd('L', function () vim.cmd('Lazy') end)
newcmd('S', function () vim.wo.spell = not vim.wo.spell end)
newcmd('NS', function () vim.cmd('set nospell') end)

-- Autocommands
autosave = true
autosavepattern = { '*.tex', '*.asy', '*.md', '*.lua', '*.cpp', '*.py', '*.hs', '*.txt', '*.lol', '*.r', '*.snippets', '*.java', '*.nix' }
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI', 'TextChangedP' }, {
	pattern = autosavepattern,
	callback = function()
		if autosave then vim.cmd('silent write') end
	end
})
vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = {'*.md', '*.txt'},
	command = 'setlocal spell! spelllang=en_us'
})
