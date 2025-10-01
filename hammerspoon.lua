local hyper = {"cmd", "alt", "ctrl", "shift"}
local mash = {"cmd", "alt", "ctrl"}
local cmdc = {"ctrl", "cmd"}
local cmds = {"shift", "cmd"}
local ca = {"ctrl", "alt"}
local sa = {"shift", "alt"}
local alt = "alt"
local cmd = "cmd"

-- bind reload at start in case of error later in config
hs.hotkey.bind(hyper, "R", hs.reload)
hs.hotkey.bind(hyper, "Y", hs.toggleConsole)

hs.ipc.cliInstall('/opt/homebrew')
hs.ipc.cliSaveHistory(true)

--- Workspace management
-- Workspaces logic is coming from AeroSpace:
-- * WS can be bound to a screen, so
--    * if screen is available, switch opens WS on that screen
--    * if screen is unavailable, switch opens WS on a first (!) screen
-- * Switch to WS hides all other windows on a target screen
-- * Apps can be "assigned" to a screen, that only has effect on startup

local workspaces = {
  corners={}, -- screen to corner location
  appinit={}, -- config of apps to workspaces they are attached to
  winlocs={}, -- original window coords when hiding them
  current={}, -- screen index to current workspace index
}

_G.workspaces = workspaces

--- Utils

--- Internal APIs

function workspaces:_hideWindow(window)
  self.winlocs[window] = window:topLeft()
  window:setTopLeft(self.corners[window:screen():id()])
    --{x=20000,y=20000}
end

function workspaces:_showWindow(window)
  window:setTopLeft(self.winlocs[window])
end

--- API

function workspaces:register(n, screen, apps) -- screen is a 1-based index
  self[n] = {screen=screen, windows={}}
  if not workspaces.current[screen] then
    workspaces.current[screen] = n
  end

  for _, app in ipairs(apps) do
    workspaces.appinit[app] = n
  end
end

function workspaces:switch(ws)
  local cfg = self[ws]
  local screens = hs.screen.allScreens()
  local targetScreen = (screens[cfg.screen] and cfg.screen or 1)
  local outgoingWS = self.current[targetScreen]
  local screen = screens[targetScreen]

  self.current[targetScreen] = ws
  local outgoing = self[outgoingWS]
  outgoing.windows = {}

  for _, window in ipairs(hs.window.visibleWindows()) do
    if window:screen() == screen then
      table.insert(outgoing.windows, window)
      workspaces:_hideWindow(window)
    end
  end

  for _, window in ipairs(cfg.windows) do
    workspaces:_showWindow(window)
  end
end

function workspaces:readjust()
  -- handle screen count increase/decrease
end

function workspaces:start()
  hs.window.animationDuration = 0
  for _, screen in pairs(hs.screen.allScreens()) do
    self.corners[screen:id()] = screen:fullFrame().bottomright:move{x=-1,y=-1}
  end
  -- push apps out to respective workspaces
end

--- Workspace config

workspaces:register(1, 1, {"Emacs"})
workspaces:register(2, 1, {"Firefox"})
workspaces:register(3, 1, {"Terminal"})
workspaces:register(4, 1, {"Slack", "Telegram"})
workspaces:register(5, 1, {"zoom.us", "Transmission"})
workspaces:register(6, 1, {})
workspaces:register(7, 1, {})
workspaces:register(8, 1, {})
workspaces:register(9, 1, {})
workspaces:register('q', 2, {"Logseq"})
workspaces:register('w', 2, {"OBS"})
workspaces:register('e', 2, {})
workspaces:register('r', 2, {})
workspaces:start()

--- App Switch

function runApp(appName)
  return function() hs.application.launchOrFocus(appName) end
end

function bindApp(mod, key, app)
  return hs.hotkey.bind(mod, key, runApp(app))
end

--hs.hotkey.bind(hyper, "i", runApp("Music"))
-- bindApp(hyper, "i", "Music")
-- bindApp(hyper, ";", "Slack")
-- bindApp(hyper, "'", "Telegram")
-- bindApp(cmdc, "\\", "Quip")
-- bindApp(cmdc, "/", "Bear")
-- bindApp(cmds, "/", "UlyssesMac")
-- bindApp(hyper, "t", 'Terminal')
-- bindApp(hyper, "b", 'Google Chrome')
-- bindApp(hyper, "z", 'zoom.us')
-- bindApp(hyper, "e", 'Emacs')


--- window focus

-- hs.window.filter.ignoreAlways['TotalSpacesCrashWatcher'] = true
-- local switcher_space = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true))
-- hs.hotkey.bind(alt, 'tab', function() switcher_space:next() end)
-- hs.hotkey.bind(sa, 'tab', function() switcher_space:previous() end)


--- Various stuff

-- show song name and copy it to clipboard
hs.hotkey.bind(mash, "i", function()
                 local artist = hs.itunes.getCurrentArtist()
                 local track = hs.itunes.getCurrentTrack()
                 if artist then
                   local song = artist .. " - " .. track
                   hs.pasteboard.writeObjects(song)
                   hs.alert.show(song .. " copied", 1)
                 else
                   hs.alert.show("Nothing is playing", 1)
                 end
end)


--- Focus screen

function isInScreen(screen, win)
  return win:screen() == screen
end

function focusScreen(screen)
  -- Get windows within screen, ordered from front to back.
  -- If no windows exist, bring focus to desktop. Otherwise, set focus on
  -- front-most application window.
  local windows = hs.fnutils.filter(
    hs.window.orderedWindows(),
    hs.fnutils.partial(isInScreen, screen))
  local windowToFocus = #windows > 0 and windows[1] or hs.window.desktop()
  windowToFocus:focus()

  -- Move mouse to center of screen
  local pt = hs.geometry.rectMidPoint(screen:fullFrame())
  hs.mouse.setAbsolutePosition(pt)
end

hs.hotkey.bind("alt", "`",
               function() focusScreen(hs.window.focusedWindow():screen():next()) end)
