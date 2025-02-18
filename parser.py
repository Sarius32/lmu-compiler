
def read_file(file_path) -> list[str]:

    try:
        with open(file_path, 'r') as file:
            content = file.readlines()
        return content
    except FileNotFoundError:
        print(f"Error: File at '{file_path}' not found.")
        exit(-1)
        return None
    
def get_ebnf(input: str) -> dict[str, list[list[str]]]:
    return_dict = dict[str, list[list[str]]]()
    lines = input


    for line in lines:
        line = line.strip()
        if line.startswith("#") or line == "": #Comment
            continue

        (key, _ , value) = line.partition("=")
        key = key.strip()

        options = value.split("|")
        value = list[list[str]]()
        for o in options:
            value.append(o.strip().split(" "))


        return_dict[key] = value

    return return_dict


def space_line(content: str) -> str:
    content = content.replace("(", " ( ")
    content = content.replace(")", " ) ")
    content = content.replace("{", " { ")
    content = content.replace("}", " } ")
    content = content.replace("[", " [ ")
    content = content.replace("]", " ] ")
    content = content.replace(";", " ; ")
    content = content.replace(",", " , ")
    content = content.replace(".", " . ")
    content = content.replace("=", " = ")

    return content


def get_next_word(program_str: list[str]) -> str:
    line = ""

    while line == "":
        if len(program_str) < 1:
            print("missing line in programm")
            exit(-1)

        line = program_str.pop(0)
        line = space_line(line)
        line = line.strip()
    

    word_idx = line.find(" ")
    if word_idx != -1:
        word = line[0:word_idx]
        program_str.insert(0, line[word_idx:])
    else: 
        word = line



    return word


def read_next_word(program_str: list[str]) -> str:
    line = ""

    while line == "":
        if len(program_str) < 1:
            print("missing line in programm")
            exit(-1)

        line = program_str.pop(0)
        line = space_line(line)
        line = line.strip()
    

    word_idx = line.find(" ")
    if word_idx != -1:
        word = line[0:word_idx]
    else: 
        word = line

    program_str.insert(0, line)


    return word


def is_value(word: str) -> bool:
    return word.isnumeric()

def is_name(word: str) -> bool:
    return word[0].isalpha()



class ParseTreeNode:
    def __init__(self, token, value):
        self.token: str = token
        self.value: str = value
        self.current_child = 0
        self.children = []
    
    def add_child(self, child):
        if isinstance(child, ParseTreeNode):
            self.children.append(child)
        else:
            raise TypeError("Child must be an instance of TreeNode")
    
    def __repr__(self, level=0):
        ret = "\t" * level + str(self.token) + " | " + str(self.value) + "\n"
        for child in self.children:
            ret += child.__repr__(level + 1)
        return ret

class Parser:
    def __init__(self, program_str: list[str], ebnf_dict: dict[str, list[list[str]]], start_token):
        self.program_str = program_str
        self.ebnf_dict = ebnf_dict
        self.root = ParseTreeNode(start_token, None)
    
    def next_token(self, root: ParseTreeNode, parent: ParseTreeNode) -> bool:

        token = root.token
        if token[0] == '"':
            if token[-1] == '"':
                #Keyword
                word = read_next_word(self.program_str)
                if word == token[1:-1]:
                    get_next_word(self.program_str)
                    return True
                
                return False
            
            print(f"Token Error: {token}")
            exit(-1)

        elif token == "value" or token == "type" or token == "name":
            word = read_next_word(self.program_str)

            if token == "value":
                if is_value(word):
                    parent.add_child(ParseTreeNode(token, get_next_word(self.program_str)))
                    return True
                return False
            
            if token == "type" or token == "name":
                if is_name(word):
                    parent.add_child(ParseTreeNode(token, get_next_word(self.program_str)))
                    return True
                return False
        
        else:
            #Non-Terminal symbol
            options = self.ebnf_dict[token]
            for option in options:
                is_first_symbol = True

                for token in option:
                    if token[-1] == "*":
                        token = token[:-1]
                        
                        while True:
                            node = ParseTreeNode(token, None)
                            if self.next_token(node, root):
                                if is_first_symbol:
                                    parent.add_child(root)
                                    is_first_symbol = False
                            else:
                                break
                        continue
                    else:
                        node = ParseTreeNode(token, None)
                        if self.next_token(node, root):
                            if is_first_symbol:
                                parent.add_child(root)
                                is_first_symbol = False

                        elif is_first_symbol:
                            break
                        else:
                            print(f"Parse Error Token: {token}, word: {read_next_word(self.program_str)}")
                            exit(-1)

                if is_first_symbol:
                    continue

                return True
            
            return False
                    

        print(f"Unreachable Statment Parsing")
        exit(-1)
    
    def parse(self):
        n = ParseTreeNode("program", None)
        #self.root.add_child(n)
        if self.next_token(n, self.root):
            print("Successfull Parsed \n")
            print(self.root.__repr__())
        
        else:
            print("Could not Parse")


def pase_program(program_path: str, ebnf_path: str):
    start_token = "START"

    ebnf_dict = get_ebnf(read_file(ebnf_path))
    program_str = read_file(program_path)

    parser = Parser(program_str, ebnf_dict, start_token)
    parser.parse()


if __name__ == "__main__":
    pase_program("./program.txt", "./language.ebnf")




