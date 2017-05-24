local mash = {"cmd", "alt", "ctrl"}
local cmdc = {"ctrl", "cmd"}
local cmds = {"shift", "cmd"}
local ca = {"ctrl", "alt"}


--- Switch keyboard layout

hs.hotkey.bind({}, "F16", function()
    hs.keycodes.currentSourceID("com.apple.keylayout.US")
end)

hs.hotkey.bind({}, "F17", function()
    hs.keycodes.currentSourceID("org.sil.ukelele.keyboardlayout.cyrillic- normal.russian-normal")
end)

hs.hotkey.bind({}, "F18", function()
    hs.keycodes.currentSourceID("org.sil.ukelele.keyboardlayout.cyrillic- normal.ukrainian-normal")
end)

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

--- Window movement

hs.hotkey.bind(cmdc, "down",
    function() hs.window.focusedWindow():moveOneScreenSouth() end)

hs.hotkey.bind(cmdc, "up",
    function() hs.window.focusedWindow():moveOneScreenNorth() end)

hs.hotkey.bind("alt", "`",
    function() focusScreen(hs.window.focusedWindow():screen():next()) end)

--- App Switch

hs.hotkey.bind(cmdc, "i",
    function() hs.application.launchOrFocus("iTunes") end)

hs.hotkey.bind(cmdc, ";",
    function() hs.application.launchOrFocus("Slack") end)

hs.hotkey.bind(cmdc, "'",
    function() hs.application.launchOrFocus("Telegram") end)


--- Terminal

function TermHotkeyHandler()
  local term = hs.application.get('Terminal')
  if (term) then
    local window = term:mainWindow()
    if not window then
      if term:selectMenuItem('New Window') then
        window = term:mainWindow()
      end
      return
    end
    if term:isFrontmost() then
      term:hide()
    else
      local s = hs.window.focusedWindow():screen()
      window:moveToScreen(s, false, true)
      window:focus()
    end
  end
end
hs.hotkey.bind('ctrl', "`", TermHotkeyHandler)


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
    for _, file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end

local configWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Hammerspoon config loaded", 1)
