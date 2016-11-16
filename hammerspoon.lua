local mash = {"cmd", "alt", "ctrl"}
local cmdc = {"ctrl", "cmd"}
local cmds = {"shift", "cmd"}
local ca = {"ctrl", "alt"}


--- Window management

hs.window.animationDuration = 0

local resize = function(sizechange)
    return function()
        local win = hs.window.focusedWindow()
        local f = win:frame()
        local screen = win:screen()
        local max = screen:frame()

        sizechange(f, max)
        win:setFrame(f)
    end
end

hs.hotkey.bind(cmds, "m", resize(function(f, max)
        f.x = max.x
        f.y = max.y
        f.w = max.w
        f.h = max.h
end))

hs.hotkey.bind(mash, "left", resize(function(f, max)
        f.x = max.x
        f.y = max.y
        f.w = max.w / 2
        f.h = max.h
end))

hs.hotkey.bind(mash, "right", resize(function(f, max)
        f.x = max.x + (max.w / 2)
        f.y = max.y
        f.w = max.w / 2
        f.h = max.h
end))

hs.hotkey.bind(mash, "up", resize(function(f, max)
        f.x = max.x
        f.y = max.y
        f.w = max.w
        f.h = max.h / 2
end))

hs.hotkey.bind(mash, "down", resize(function(f, max)
        f.x = max.x
        f.y = max.y + (max.h / 2)
        f.w = max.w
        f.h = max.h / 2
end))


--- App Switch

hs.hotkey.bind(cmdc, "i",
    function() hs.application.launchOrFocus("iTunes") end)

hs.hotkey.bind(cmdc, ";",
    function() hs.application.launchOrFocus("Slack") end)

hs.hotkey.bind(cmdc, "'",
    function() hs.application.launchOrFocus("Telegram") end)

hs.hotkey.bind(cmdc, "down",
    function() hs.window.focusedWindow():moveOneScreenSouth() end)

hs.hotkey.bind(cmdc, "up",
    function() hs.window.focusedWindow():moveOneScreenNorth() end)

hs.hotkey.bind(cmds, "down",
    function() focusScreen(hs.window.focusedWindow():screen():next()) end)

hs.hotkey.bind(cmds, "up",
    function() focusScreen(hs.window.focusedWindow():screen():previous()) end)


--- Various stuff

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


--- Config reload

function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Hammerspoon config loaded", 1)


--- Outlook

archiveScript = io.popen('osadecompile ~/conf/scripts/outlook-archive.scpt'):read('*all')
archiveMessage = hs.hotkey.new({'alt'}, 'a', function()
    hs.osascript.applescript(archiveScript)
end)


hs.window.filter.new('Microsoft Outlook')
  :subscribe(hs.window.filter.windowFocused, function()
               archiveMessage:enable()
            end)
  :subscribe(hs.window.filter.windowUnfocused, function()
               archiveMessage:disable()
            end)
