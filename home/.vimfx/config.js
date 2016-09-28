let set = (pref, valueOrFunction) => {
    let value = typeof valueOrFunction === 'function'
        ? valueOrFunction(vimfx.getDefault(pref))
        : valueOrFunction
    vimfx.set(pref, value)
}

let map = (shortcuts, command, custom=false, mode='normal') => {
    vimfx.set(`${custom ? 'custom.' : ''}mode.${mode}.${command}`, shortcuts)
}

// options
set('prevent_autofocus', true)

// Faster please
set('smoothScroll.lines.spring-constant', '4000')


// commands
vimfx.addCommand({
    name: 'search_selected_text',
    description: 'Search for the selected text'
}, ({vim}) => {
    vimfx.send(vim, 'getSelection', null, selection => {
        let inTab = true // Change to `false` if you’d like to search in current tab.
        vim.window.BrowserSearch.loadSearch(selection, inTab)
    })
})
map('s', 'search_selected_text', true)


// Reload config file
map(',sv', 'reload_config_file')

// Left/Right shifts to swiwtch tabs
map('(', 'tab_select_previous')
map(')', 'tab_select_next')

// Come on!!!
map('<c-d>', 'scroll_half_page_down')
map('<c-u>', 'scroll_half_page_up')
map('d', 'tab_close')
map('u', 'tab_restore')

map('Y', 'copy_selection_and_exit', false, 'caret')
map('y', 'copy_current_url')

vimfx.addCommand({
  name: 'toggle_dev_tools',
  description: 'Web console',
}, ({vim}) => {
  vim.window.gDevToolsBrowser.toggleToolboxCommand(vim.window.gBrowser)
})
map('gd', 'toggle_dev_tools', true)
