from erlport import Port, Protocol
from lib.Curl import Curl

class PyPort(Protocol):

    def handle_echo(self, msg):
        # print msg
        new_msg = 'python say: %s' % msg
        return new_msg

    def handle_add(self, x, y):
        return x + y

    def handle_curl(self, link):
        curl = Curl()
        html = curl.get(link)
        return html

if __name__ == '__main__':
    PyPort().run(Port(packet=4, use_stdio=True))
