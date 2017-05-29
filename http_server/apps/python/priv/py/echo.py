from erlport import Port, Protocol

class Echo(Protocol):

    def handle_echo(self, msg):
        # print msg
        new_msg = 'reply xx: %s' % msg
        return new_msg

    def handle_add(self, x, y):
        return x + y



if __name__ == '__main__':
    Echo().run(Port(packet=4, use_stdio=True))
