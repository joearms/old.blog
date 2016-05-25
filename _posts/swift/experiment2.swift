// experiment2.swift
// run with:
//  $ swift experiment2.swift

import Cocoa

class AppDelegate: NSObject, NSApplicationDelegate
{
    let window = NSWindow()
    
    func applicationDidFinishLaunching(aNotification: NSNotification)
    {
        window.setContentSize(NSSize(width:600, height:200))
        window.styleMask = NSTitledWindowMask | NSClosableWindowMask |
                           NSMiniaturizableWindowMask |
                           NSResizableWindowMask
        
        window.opaque = false
        window.center();
        window.title = "Experiment 2"
        window.makeKeyAndOrderFront(window)
        window.level = 1
    }
}

let app = NSApplication.sharedApplication()
let controller = AppDelegate()

app.delegate = controller
app.run()
