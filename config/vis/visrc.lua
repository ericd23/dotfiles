require('vis')

local plug = require('plugins/vis-plug')

vis.events.subscribe(vis.events.INIT, function()
    -- global configuration options
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    -- per window configuration options
    vis:command('set tw 4')
    vis:command('set ai')
    vis:command('set et')
    vis:command('set cul')
    vis:command('set cc 80')
    vis:command('set ic')
end)

local plugins = {
    { 'lutobler/vis-commentary' },
    -- { 'git.sr.ht/~emg/vis-cscope' },
    { 'repo.or.cz/vis-pairs.git' },
    { 'erf/vis-sneak' },
    { 'repo.or.cz/vis-surround.git' },
    { 'przmv/base16-vis', theme = true, file = 'themes/base16-ashes' }
}

plug.init(plugins, true)
