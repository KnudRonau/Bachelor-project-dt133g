from customtkinter import *
import CoreFunctionality as cf

# pip install langchain, langchain_openai, GitPython, customtkinter

class GUI:
    temperature = 0.0

    def __init__(self, master):
        self.master = master
        master.title("SPAC-B")
        set_appearance_mode("dark")
        set_default_color_theme("dark-blue")
        
        # Chat Window
        self.chat_frame = CTkFrame(master)
        self.chat_frame.pack(fill=BOTH, expand=True)

        self.chat_log = CTkTextbox(self.chat_frame)
        self.chat_log.pack(side=LEFT, fill=BOTH, expand=True)

        self.scrollbar = CTkScrollbar(self.chat_frame, command=self.chat_log.yview)
        self.scrollbar.pack(side=RIGHT, fill=Y)
        self.chat_log.configure(yscrollcommand=self.scrollbar.set)

        self.chat_input = CTkEntry(master)
        self.chat_input.pack(fill=X)

        self.send_button = CTkButton(master, text="Send message", command=self.send_message)
        self.send_button.pack()

        # enter Git path
        self.url_label = CTkLabel(master, text="Enter GIT URL or local path and select model afterwards:")
        self.url_label.pack()
        self.url_input = CTkEntry(master)
        self.url_input.pack(fill=X)

        self.url_button = CTkButton(master, text="select repository", command=self.select_repository)
        self.url_button.pack()

        self.branch_label = CTkLabel(master, text="Enter branch name:")
        self.branch_label.pack()
        self.branch_input = CTkEntry(master)
        self.branch_input.pack(fill=X)

        self.label = CTkLabel(master, text="Temperature: 0.0")
        self.label.pack()

        self.scale = CTkSlider(master, from_=0, to=1, number_of_steps=10, orientation="horizontal", command=self.on_scale)
        self.scale.set(0)
        self.scale.pack()

        # File Locator
        self.file_button = CTkButton(master, text="Select model", command=self.locate_file)
        self.file_button.pack()

    # Query the LLM with inputted text and display the response
    def send_message(self):
        message = self.chat_input.get()
        self.chat_input.delete(0, END)
        self.chat_log.insert(END, f"Your question: \n{message}\n")
        self.chat_log.insert(END, f"Response: \n{cf.query_model(message)}\n\n")
    
    # Update the temperature value
    def on_scale(self, value):
        self.temperature = float(value)
        self.label.configure(text = 'Temperature: ' + str(round(value, 1)))
    
    def select_repository(self):
        self.chat_log.insert(END, cf.return_branches(self.url_input.get()))

    # call setup method in WorkingVersion1.py with GIT path and model path
    def locate_file(self):
        #file_path = filedialog.askopenfilename()
        #if file_path:
        self.chat_log.insert(END, f"Trying to load repository and LLM...\n")
        self.chat_log.insert(END, cf.setup(self.url_input.get(), self.branch_input.get(), self.temperature)+"\n")
            

def main():
    root = CTk()
    root.geometry("800x600")
    gui = GUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()