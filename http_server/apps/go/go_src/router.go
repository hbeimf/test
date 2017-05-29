package main



func setRouter() {
    setDemo()
    setList()
    setCurl()
    setIconv()
}

func setDemo() {
    var ctrl_demo DemoController
    addRoute("demo", &ctrl_demo)
}

func setList() {
    var ctrl ListController
    addRoute("list", &ctrl)
}

func setCurl() {
    var ctrl CurlController
    addRoute("curl", &ctrl)
}

func setIconv() {
    var ctrl IconvController
    addRoute("iconv", &ctrl)
}


