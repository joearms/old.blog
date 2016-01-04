class Demo {
    var call: () -> Bool = {() -> Bool in true}
}

let x = Demo()
print(x.call())
var i = 10
var f = {() -> Bool in print("hello I'm a callback and i =", i); return true}
x.call = f
print(x.call())
i = 20
print(x.call())
