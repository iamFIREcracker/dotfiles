function whenWorking(thenDo, otherwiseDo)
  return function()
    output, status = hs.execute('ps aux | grep ssh.*sock5 | grep -v grep')
    if status ~= nil then
      thenDo()
    else
      otherwiseDo()
    end
  end
end

--- Quick open applications
local hyper = {"ctrl", "cmd", "alt", "shift"}

function switchTo(name)
  return function()
    local frontmost = hs.application.frontmostApplication()
    if name == frontmost:name() then
      -- Already focused... cycle the group
      -- I had to enable Hammerspoon under System Preferences > Security
      -- & Privacy > Privacy > Accessibility
      -- https://github.com/Hammerspoon/hammerspoon/issues/1984#issuecomment-481272133
      hs.eventtap.keyStroke({"cmd"}, "`")
    else
      -- Ohterwise... launch or focus
      hs.application.launchOrFocus(name)
      -- if name == 'Finder' then
      --   hs.appfinder.appFromName(name):activate()
      -- end
    end
  end
end

function launchNew(name)
    return function()
        hs.appfinder.appFromName(name):activate()
        hs.eventtap.keyStroke({"cmd"}, "n")
    end
end

hs.hotkey.bind(hyper, "delete", function()
  hs.reload()
end)
-- hs.hotkey.bind(hyper, ";", switchTo("/usr/local/bin/gimp"))
hs.hotkey.bind(hyper, "e", switchTo("Finder"))
hs.hotkey.bind(hyper, "o", switchTo("Spotify"))
hs.hotkey.bind(hyper, "k", switchTo("Google Chrome"))
-- hs.hotkey.bind(hyper, "k", switchTo("Brave Browser"))
-- hs.hotkey.bind(hyper, "k", switchTo("Firefox"))
-- Keep it simple for now:
-- - one for the plan file
-- - one for...everything else
-- hs.hotkey.bind(hyper, "h", switchTo("Alacritty"))
hs.hotkey.bind(hyper, "1", launchNew("Alacritty-main"))
hs.hotkey.bind(hyper, "p", switchTo("Alacritty-main"))
hs.hotkey.bind(hyper, "j", switchTo("Alacritty-fullscreen"))

hs.hotkey.bind(hyper, "m", whenWorking(switchTo("Microsoft Outlook")))
hs.hotkey.bind(hyper, "i", whenWorking(switchTo("Microsoft Teams"), switchTo("Discord")))

--- Resize windows
hs.window.animationDuration = 0

local hyper2 = {"ctrl", "shift"}
function resize_pct(x, y, width, height)
  return function()
    local win = hs.window.focusedWindow()
    local frame = win:frame()
    local work_area = win:screen():frame()
    local dock_width = win:screen():fullFrame().w - work_area.w

    frame.x = dock_width + work_area.w * x
    frame.y = work_area.h * y
    frame.w = work_area.w * width
    frame.h = work_area.h * height
    win:setFrame(frame)
  end
end

-- Left: main, half, small
hs.hotkey.bind(hyper2, "y", resize_pct(0, 0, 3/4, 1))
hs.hotkey.bind(hyper2, "h", resize_pct(0, 0, 1/2, 1))
hs.hotkey.bind(hyper2, "n", resize_pct(0, 0, 1/4, 1))
-- Right: main, half, small
hs.hotkey.bind(hyper2, "u", resize_pct(1/4, 0, 3/4, 1))
hs.hotkey.bind(hyper2, "j", resize_pct(1/2, 0, 1/2, 1))
hs.hotkey.bind(hyper2, "m", resize_pct(3/4, 0, 1/4, 1))
-- Maximize
hs.hotkey.bind(hyper2, "k", resize_pct(0, 0, 1, 1))

---
hs.alert.show("HS init config... reloaded")
