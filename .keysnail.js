// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
prompt.displayDelayTime = 50;

plugins.options["K2Emacs.editor"] = "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient";


// stop searching by hit Enter on search box
function emacslike_search(ev){
    if(ev.ctrlKey && ev.charCode == 115){ // C-s
        gFindBar.onFindAgainCommand(false);
    }
    if(ev.keyCode == 13){ // Enter
        gFindBar.onFindAgainCommand(true);
        gFindBar.close();
    }
    //TODO: save searching start point and back to it when searching is finished with C-g
}

if ('gFindBar' in window) {
    gFindBar.getElement("findbar-textbox")
        .addEventListener("keypress", emacslike_search, false);
}


ext.add("list-closed-tabs", function () {
    const fav = "chrome://mozapps/skin/places/defaultFavicon.png";
    var ss   = Cc["@mozilla.org/browser/sessionstore;1"].getService(Ci.nsISessionStore);
    var json = Cc["@mozilla.org/dom/json;1"].createInstance(Ci.nsIJSON);
    var closedTabs = [[tab.image || fav, tab.title] for each (tab in json.decode(ss.getClosedTabData(window)))];

    if (!closedTabs.length)
        return void display.echoStatusBar("No closed tabs", 2000);

    prompt.selector(
        {
            message    : "select tab to undo:",
            collection : closedTabs,
            flags      : [ICON | IGNORE, 0],
            callback   : function (i) { if (i >= 0) window.undoCloseTab(i); }
        });
}, "List closed tabs");

plugins.options["hok.hint_base_style"] = {
  position : 'absolute',
  zIndex : '2147483647',
  color : '#000',
  fontSize : '12px',
  fontFamily : 'sans',
  fontWeight : 'bold',
  lineHeight : '12px',
  padding : '3px',
  margin : '0px',
  textTransform : 'uppercase'
};

plugins.options["hok.hint_color_link"] = 'rgba(0, 255, 0, 1)';
plugins.options["hok.hint_color_form"] = 'rgba(157, 82, 255, 1)';
plugins.options["hok.hint_color_focused"] = 'rgba(255, 0, 255, 1)';
plugins.options["hok.hint_color_candidates"] = 'rgba(255, 100, 255, 1)';

plugins.options['hok.selector'] = 'a, input:not([type="hidden"]), textarea, iframe, area, select, button, embed, *[onclick], *[onmouseover], *[onmousedown], *[onmouseup], *[oncommand], *[role="link"], *[role="button"]';


//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }
    command.closeFindBar();
    var marked = command.marked(aEvent);
    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});


// ============================== Black list =============================== //

hook.addToHook("LocationChange", function (aNsURI) {
    var URL = aNsURI ? aNsURI.spec : null;
    key.suspendWhenMatched(URL, key.blackList);
});

key.blackList = [
    'mail.google.com',
    'http://www.google.com/reader/view/'
];

// ============================= Key bindings ============================== //

key.setGlobalKey('M-x', function (ev, arg) {
    ext.select(arg, ev);
}, 'List exts and execute selected one', true);

key.setGlobalKey('M-:', function (ev) {
    command.interpreter();
}, 'Command interpreter', true);

key.setGlobalKey(['<f1>', 'b'], function (ev) {
    key.listKeyBindings();
}, 'List all keybindings');

key.setGlobalKey('C-m', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RETURN, true);
}, 'Generate the return key code');

key.setGlobalKey(['C-x', 'l'], function (ev) {
    command.focusToById("urlbar");
}, 'Focus to the location bar', true);

key.setGlobalKey(['C-x', 'g'], function (ev) {
    command.focusToById("searchbar");
}, 'Focus to the search bar', true);

key.setGlobalKey(['C-x', 't'], function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, 'Focus to the first textarea', true);

key.setGlobalKey(['C-x', 's'], function (ev) {
    command.focusElement(command.elementsRetrieverButton, 0);
}, 'Focus to the first button', true);

key.setGlobalKey(['C-x', 'K'], function (ev) {
    closeWindow(true);
}, 'Close the window');

key.setGlobalKey(['C-x', 'n'], function (ev) {
    OpenBrowserWindow();
}, 'Open new window');

key.setGlobalKey(['C-x', 'C-c'], function (ev) {
    goQuitApplication();
}, 'Exit Firefox', true);

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    command.focusOtherFrame(arg);
}, 'Select next frame');

key.setGlobalKey(['C-x', '1'], function (ev) {
    window.loadURI(ev.target.ownerDocument.location.href);
}, 'Show current frame only', true);

key.setGlobalKey(['C-x', 'C-f'], function (ev) {
    BrowserOpenFileWindow();
}, 'Open the local file', true);

key.setGlobalKey(['C-x', 'C-s'], function (ev) {
    saveDocument(window.content.document);
}, 'Save current page to the file', true);

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs like incremental search forward', true);

key.setGlobalKey('C-r', function (ev) {
    command.iSearchBackwardKs(ev);
}, 'Emacs like incremental search backward', true);

key.setGlobalKey(['C-c', 'u'], function (ev) {
    undoCloseTab();
}, 'Undo closed tab');

key.setGlobalKey(['C-c', 'C-c', 'C-v'], function (ev) {
    toJavaScriptConsole();
}, 'Display JavaScript console', true);

key.setGlobalKey(['C-c', 'C-c', 'C-c'], function (ev) {
    command.clearConsole();
}, 'Clear Javascript console', true);

key.setGlobalKey('C-.', function (ev, arg) {
    ext.exec("tanything", arg, ev);
}, 'view all tabs ', true);

key.setGlobalKey('C-\'', function (ev, arg) {
    ext.exec("bmany-list-all-bookmarks", arg, ev);
}, 'bmany - List all bookmarks', true);

key.setViewKey([['M-<']], function (ev) {
    goDoCommand("cmd_scrollTop");
}, 'Scroll to the top of the page', true);

key.setViewKey([['M->']], function (ev) {
    goDoCommand("cmd_scrollBottom");
}, 'Scroll to the bottom of the page', true);

key.setViewKey(':', function (ev, arg) {
    shell.input(null, arg);
}, 'List and execute commands', true);

key.setViewKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all', true);

key.setViewKey([['C-x', 'k'], ['t', 'k']], function () {
    BrowserCloseTabOrWindow();
}, 'Close tab/window');

key.setViewKey(['t', 'l'], function () {
    ext.exec("list-closed-tabs");
}, 'List closed tabs');

key.setViewKey(['t', 'u'], function () {
    undoCloseTab();
}, 'Undo closed tab');

key.setViewKey([['t', 't'], ['e']], function (ev) {
    command.setClipboardText(ev.target.ownerDocument.location.href);
}, 'Copy current page URL');

key.setViewKey('f', function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, 'Focus to the first textarea', true);

key.setViewKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, 'Focus to the next button');

key.setViewKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, 'Focus to the previous button');

key.setViewKey('o', function (ev, arg) {
    ext.exec("hok-start-background-mode", arg, ev);
}, 'Start Hit a Hint background mode', true);

key.setViewKey('C-o', function (ev, arg) {
    ext.exec("hok-start-continuous-mode", arg, ev);
}, 'Start Hit a Hint extended mode', true);

key.setViewKey(['C-c', 'o'], function (ev, arg) {
    ext.exec("hok-start-extended-mode", arg, ev);
}, 'Start Hit a Hint extended mode', true);

key.setEditKey(['C-x', 'h'], function (ev) {
    command.selectAll(ev);
}, 'Select whole text', true);

key.setEditKey(['C-x', 'r', 'd'], function (ev, arg) {
    command.replaceRectangle(ev.originalTarget, "", false, !arg);
}, 'Delete text in the region-rectangle', true);

key.setEditKey(['C-x', 'r', 't'], function (ev) {
    prompt.read("String rectangle: ", function (aStr, aInput) {command.replaceRectangle(aInput, aStr);}, ev.originalTarget);
}, 'Replace text in the region-rectangle with user inputted string', true);

key.setEditKey(['C-x', 'r', 'o'], function (ev) {
    command.openRectangle(ev.originalTarget);
}, 'Blank out the region-rectangle, shifting text right', true);

key.setEditKey(['C-x', 'r', 'k'], function (ev, arg) {
    command.kill.buffer = command.killRectangle(ev.originalTarget, !arg);
}, 'Delete the region-rectangle and save it as the last killed one', true);

key.setEditKey(['C-x', 'r', 'y'], function (ev) {
    command.yankRectangle(ev.originalTarget, command.kill.buffer);
}, 'Yank the last killed rectangle with upper left corner at point', true);

key.setEditKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'Set the mark', true);

key.setEditKey('C-o', function (ev) {
    command.openLine(ev);
}, 'Open line');

key.setEditKey('C-\\', function (ev) {
    display.echoStatusBar("Redo!", 2000);
    goDoCommand("cmd_redo");
}, 'Redo');

key.setEditKey('C-a', function (ev) {
    command.beginLine(ev);
}, 'Beginning of the line');

key.setEditKey('C-e', function (ev) {
    command.endLine(ev);
}, 'End of the line');

key.setEditKey('C-f', function (ev) {
    command.nextChar(ev);
}, 'Forward char');

key.setEditKey('C-b', function (ev) {
    command.previousChar(ev);
}, 'Backward char');

key.setEditKey('M-f', function (ev) {
    command.forwardWord(ev);
}, 'Next word');

key.setEditKey('M-b', function (ev) {
    command.backwardWord(ev);
}, 'Previous word');

key.setEditKey('C-n', function (ev) {
    command.nextLine(ev);
}, 'Next line');

key.setEditKey('C-p', function (ev) {
    command.previousLine(ev);
}, 'Previous line');

key.setEditKey('C-v', function (ev) {
    command.pageDown(ev);
}, 'Page down');

key.setEditKey([['M-v'], ['C-y']], command.yank, 'Paste (Yank)');

key.setEditKey('M-<', function (ev) {
    command.moveTop(ev);
}, 'Beginning of the text area');

key.setEditKey('M->', function (ev) {
    command.moveBottom(ev);
}, 'End of the text area');

key.setEditKey('C-d', function (ev) {
    goDoCommand("cmd_deleteCharForward");
}, 'Delete forward char');

key.setEditKey('C-h', function (ev) {
    goDoCommand("cmd_deleteCharBackward");
}, 'Delete backward char');

key.setEditKey('M-d', function (ev) {
    command.deleteForwardWord(ev);
}, 'Delete forward word');

key.setEditKey([['C-<backspace>'], ['M-<delete>']], function (ev) {
    command.deleteBackwardWord(ev);
}, 'Delete backward word');

key.setEditKey('M-u', function (ev, arg) {
    command.wordCommand(ev, arg, command.upcaseForwardWord, command.upcaseBackwardWord);
}, 'Convert following word to upper case');

key.setEditKey('C-k', function (ev) {
    command.killLine(ev);
}, 'Kill the rest of the line');

key.setEditKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, 'Focus to the next text area');

key.setEditKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, false, true);
}, 'Focus to the previous text area');

key.setEditKey('C-M-e', function (ev, arg) {
    ext.exec("edit_text", arg, ev);
}, 'edit by external editor', true);
