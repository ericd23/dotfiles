-- load standard vis module, providing parts of the Lua API
require('vis')

vis.events.subscribe(vis.events.INIT, function()
	-- global configuration options
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
	-- per window configuration options
	vis:command('set tw 2')
	vis:command('set ai')
	vis:command('set nu')
	vis:command('set cul')
	vis:command('set cc 80')
	vis:command('set theme default-256')
	vis:command('set ic')
end)
