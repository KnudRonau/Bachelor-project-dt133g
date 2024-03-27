from customtkinter import *
import CoreFunctionality as cf


# pip install langchain, langchain_openai, GitPython, customtkinter, faiss-cpu

class GUI:
    temperature = 0.0
    branches = []
    local_repo_path = ""

    # Initialize the GUI
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
        self.url_label = CTkLabel(master, text="Enter Git URL or local path to select repository. Set desired temperature and choose branch:")
        self.url_label.pack()
        self.url_input = CTkEntry(master)
        self.url_input.pack(fill=X)

        self.url_button = CTkButton(master, text="select repository", command=self.on_select_repository)
        self.url_button.pack()

        # temperature slider
        self.label = CTkLabel(master, text="Temperature: 0.0")
        self.label.pack()

        self.scale = CTkSlider(master, from_=0, to=1, number_of_steps=10, orientation="horizontal", command=self.on_scale)
        self.scale.set(0)
        self.scale.pack()

        # select branch
        self.branches_combobox = CTkComboBox(master, values=self.branches, command=self.on_branch_select)
        self.branches_combobox.pack()

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
    
    # Select the repository and update the branches combobox
    def on_select_repository(self):
        self.branches, self.local_repo_path = cf.return_branches(self.url_input.get())
        self.branches_combobox.configure(values=self.branches)
        self.chat_log.insert(END, cf.return_branches(self.url_input.get()))

    # Select the branch
    def on_branch_select(self, value):
        self.chat_log.insert(END, f"Selected branch: {value}\n")
        cf.setup(self.local_repo_path, value, self.temperature)
            

def main():
    root = CTk()
    root.geometry("800x600")
    gui = GUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()