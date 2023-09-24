hs.alert.show("Hammerspoon config loaded")

hyper = {"cmd", "alt", "ctrl", "shift"}

-- bind reload at start in case of error later in config
hs.hotkey.bind(hyper, "R", hs.reload)
hs.hotkey.bind(hyper, "Y", hs.toggleConsole)

function inspect(value)
  hs.alert.show(hs.inspect(value))
end

arch = io.popen('uname -p', 'r'):read('*l')
path = arch == 'arm' and '/opt/homebrew' or nil
hs.ipc.cliInstall(path)
hs.ipc.cliSaveHistory(true)

fennel = require("fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)
fennel.dofile("init.fnl") -- exports into global namespace

-- Apps with multiple windows
function launchOrNewWindow(name, menuPath)
   return function ()
      local app = hs.application.find(name)
      if app then
	 app:selectMenuItem(menuPath)
      else
	 hs.application.launchOrFocus(name)
      end
   end
end

hs.hotkey.bind(
   {"cmd","alt","ctrl"}, "B",
   launchOrNewWindow("Brave Browser", {"File", "New Window"})
)

hs.hotkey.bind(
   {"cmd","alt","ctrl"}, "T",
   launchOrNewWindow("iTerm", {"Shell", "New Window"})
)

-- spaces
function findAEmptySpace()
   local sp, sc = hs.spaces, hs.screen
   local allSpaces = sp.allSpaces()[sc.mainScreen():getUUID()]
   for _, sid in pairs(allSpaces) do
      wids = sp.windowsForSpace(sid)
      -- A newly created space will have 2 windows
      -- guessing they're bar and dock
      if #wids == 2 then
	 return sid
      end
   end
   return nil
end

-- TODO: handle when app already running
function launchToNewSpace(name)
   local sp = hs.spaces
   return function ()
      local emptySpace = findAEmptySpace()
      if not emptySpace then sp.addSpaceToScreen() end
      -- TODO: opt, use set diff
      emptySpace = findAEmptySpace()
      hs.alert.show(emptySpace)
      sp.gotoSpace(emptySpace)
      -- Add some delay to prevent launching on the previous space
      hs.timer.doAfter(1, function()
	 hs.application.launchOrFocus(name)
      end)
   end
end

hs.hotkey.bind(
   {"cmd","alt","ctrl"}, "W",
   launchToNewSpace("WeChat")
)

hs.hotkey.bind(
   {"cmd","alt","ctrl"}, "N",
   function ()
      hs.spaces.addSpaceToScreen()
   end
)
