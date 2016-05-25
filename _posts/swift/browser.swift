#!/usr/bin/swift  

import WebKit  
let application = NSApplication.sharedApplication()  
application.setActivationPolicy(NSApplicationActivationPolicy.Regular)  
let window = NSWindow(contentRect: NSMakeRect(0, 0, 960, 720),
                      styleMask: NSTitledWindowMask | NSClosableWindowMask |
                                 NSMiniaturizableWindowMask,
                      backing: .Buffered, `defer`: false)  
window.center()  
window.title = "Minimal Swift WebKit Browser"  
window.makeKeyAndOrderFront(window)  
class WindowDelegate: NSObject, NSWindowDelegate {  
    func windowWillClose(notification: NSNotification) {  
        NSApplication.sharedApplication().terminate(0)  
    }  
}  
let windowDelegate = WindowDelegate()  
window.delegate = windowDelegate  
class ApplicationDelegate: NSObject, NSApplicationDelegate {  
    var _window: NSWindow  
    init(window: NSWindow) {  
        self._window = window  
    }  
    func applicationDidFinishLaunching(notification: NSNotification) {  
        let webView = WebView(frame: self._window.contentView!.frame)  
        self._window.contentView!.addSubview(webView)  
        webView.mainFrame.loadRequest(NSURLRequest(URL: NSURL(string: "https://forums.developer.apple.com/thread/5137")!))  
    }  
}  
let applicationDelegate = ApplicationDelegate(window: window)  
application.delegate = applicationDelegate  
application.activateIgnoringOtherApps(true)  
application.run()  
