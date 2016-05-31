// experiment6.swift
// run with:
//  $ swift experiment6.swift

import Cocoa

class AppDelegate: NSObject, NSApplicationDelegate
{
    let window = NSWindow()
    
    func applicationDidFinishLaunching(aNotification: NSNotification)
    {
        window.setContentSize(NSSize(width:500, height:260))
        window.styleMask = NSTitledWindowMask | NSClosableWindowMask |
                           NSMiniaturizableWindowMask |
                           NSResizableWindowMask
        
        window.opaque = false
        window.center();
        window.title = "Experiment 6"

        // load and display an image
        let path = "../images/ada.png"
        let img = NSImage(contentsOfFile: path)!
        let imgView = NSImageView(frame: NSMakeRect(10, 10, 480, 240))
        imgView.image = img
        window.contentView!.addSubview(imgView)
        
        window.makeKeyAndOrderFront(window)
        window.level = 1
    }
}

let app = NSApplication.sharedApplication()
let controller = AppDelegate()

app.delegate = controller
app.run()
