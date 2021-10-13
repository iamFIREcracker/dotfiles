function whenWorking(thenDo, otherwiseDo)
  return function()
    output, status = hs.execute('ps aux | grep pisa299-sock5 | grep -v grep')
    if status ~= nil then
      thenDo()
    else
      otherwiseDo()
    end
  end
end

--- Quick open applications
local hyper = {"ctrl", "cmd", "alt", "shift"}

function open(name)
  return function()
    local frontmost = hs.application.frontmostApplication()
    if name == frontmost:name() then
      -- Already focused... cycle the group
      -- I had to enable Hammerspoon under System Preferences > Security
      -- & Privacy > Privacy > Accessibility
      -- https://github.com/Hammerspoon/hammerspoon/issues/1984#issuecomment-481272133
      hs.eventtap.keyStroke({"cmd"}, "`")
    else
      -- Ohterwise... lkaunch or focus
      hs.application.launchOrFocus(name)
      if name == 'Finder' then
        hs.appfinder.appFromName(name):activate()
      end
    end
  end
end

hs.hotkey.bind(hyper, "delete", function()
  hs.reload()
end)
hs.hotkey.bind(hyper, ";", open("/usr/local/bin/gimp"))
hs.hotkey.bind(hyper, "e", open("Finder"))
hs.hotkey.bind(hyper, "o", open("Spotify"))
hs.hotkey.bind(hyper, "k", open("Brave Browser"))
hs.hotkey.bind(hyper, "p", open("Alacritty-main"))
hs.hotkey.bind(hyper, "j", open("Alacritty-fullscreen"))
hs.hotkey.bind(hyper, "h", open("Alacritty"))

hs.hotkey.bind(hyper, "m", whenWorking(open("Microsoft Outlook")))
hs.hotkey.bind(hyper, "i", whenWorking(open("Microsoft Teams"), open("Discord")))

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

hs.hotkey.bind(hyper2, "k", resize_pct(0, 0, 1, 1))
hs.hotkey.bind(hyper2, "j", resize_pct(1/5, 1/10, 3/5, 1))

hs.hotkey.bind(hyper2, "h", resize_pct(0, 0, 1/2, 1))
hs.hotkey.bind(hyper2, "l", resize_pct(1/2, 0, 1/2, 1))

hs.hotkey.bind(hyper2, "y", resize_pct(0, 0, 1/4, 1))
hs.hotkey.bind(hyper2, "o", resize_pct(1/4, 0, 3/4, 1))

hs.hotkey.bind(hyper2, "i", resize_pct(0, 0, 1, 1/2))
hs.hotkey.bind(hyper2, "m", resize_pct(0, 1/2, 1, 1/2))

hs.hotkey.bind(hyper2, "u", resize_pct(1/8, 0, 3/4, 1))
hs.hotkey.bind(hyper2, "n", resize_pct(1/6, 0, 2/3, 1))
hs.hotkey.bind(hyper2, "7", resize_pct(1/4, 0, 1/4, 1))
hs.hotkey.bind(hyper2, "8", resize_pct(1/2, 0, 1/4, 1))

---
hs.alert.show("HS init config... reloaded")
