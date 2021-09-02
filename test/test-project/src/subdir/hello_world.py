from time import sleep

def hello_world():
    print("HELLO WORLD")

def main():
    for i in range(3):
        hello_world()
    for i in range(1000):
        print("Sleep %d ..." % i)
        sleep(1)

if __name__ == "__main__":
    main()
