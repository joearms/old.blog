// experiment3.swift
// run with:
//  $ swift experiment3.swift

import Cocoa




func add_label_and_entry(
    window: NSWindow,
    x:CGFloat, y:CGFloat,
    s: String) -> NSTextView {
    
    let label = NSTextView(frame: NSMakeRect(x, y, 20, 20))
    label.string = s
    label.editable = false
    label.backgroundColor = window.backgroundColor
    label.selectable = false
    window.contentView!.addSubview(label)

    let entry = NSTextView(frame: NSMakeRect(x+30, y, 40, 20))
    entry.editable = false
    entry.selectable = false
    window.contentView!.addSubview(entry)
    return entry
}

    
class AppDelegate: NSObject, NSApplicationDelegate
{
    

    @IBOutlet weak var t2: NSTextView!

    let window = NSWindow()
    @IBOutlet weak var t1 = add_label_and_entry(window, x:10.0, y:150.0, s:"A")
    

    
    func applicationDidFinishLaunching(aNotification: NSNotification)
    {
        window.setContentSize(NSSize(width:600, height:200))
        window.styleMask = NSTitledWindowMask | NSClosableWindowMask |
                           NSMiniaturizableWindowMask |
                           NSResizableWindowMask
        
        window.opaque = false
        window.center();
        window.title = "Experiment 3"

       
        t2 = add_label_and_entry(window, x:10.0, y:120.0, s:"B")
    
        let button = NSButton(frame: NSMakeRect(10, 80, 180, 30))
        button.bezelStyle =  .ThickSquareBezelStyle
        button.title = "Compute A+B"
        button.target = self
        button.action = "myAdder:"
        
        window.contentView!.addSubview(button)

        window.makeKeyAndOrderFront(window)
        window.level = 1
    }


    @IBAction func myAction(sender: AnyObject) {
    // NSLog("a=%d", t1)
    }

    
}

let app = NSApplication.sharedApplication()
let controller = AppDelegate()

app.delegate = controller
app.run()
