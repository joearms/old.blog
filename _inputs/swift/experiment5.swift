// experiment5.swift
// run with:
//  $ swift experiment5.swift

import Cocoa

class AppDelegate: NSObject, NSApplicationDelegate
{
    var text = NSTextView(frame: NSMakeRect(20, 150, 180, 30))
    let window = NSWindow()

    @IBAction func myAction(sender: AnyObject) {
        text.string = "Wow I've been clicked"
    }
    
    func applicationDidFinishLaunching(aNotification: NSNotification)
    {
        window.setContentSize(NSSize(width:600, height:200))
        window.styleMask = NSTitledWindowMask | NSClosableWindowMask |
                           NSMiniaturizableWindowMask |
                           NSResizableWindowMask
        
        window.opaque = false
        window.center();
        window.title = "Experiment 5"

        let button = NSButton(frame: NSMakeRect(20, 100, 280, 30))
        button.bezelStyle =  .ThickSquareBezelStyle
        button.title = "Click me and the text will change"
        button.target = self;
        button.action = "myAction:" // Note the colon 
        window.contentView!.addSubview(button)

        text.string = "Some Text"
        text.editable = false
        text.backgroundColor = window.backgroundColor
        text.selectable = false
        window.contentView!.addSubview(text)
        
        window.makeKeyAndOrderFront(window)
        window.level = 1
    }
}

let app = NSApplication.sharedApplication()
let controller = AppDelegate()

app.delegate = controller
app.run()
