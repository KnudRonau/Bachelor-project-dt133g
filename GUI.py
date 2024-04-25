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

        self.chat_log = CTkTextbox(self.chat_frame, font=("Consolas", 15), text_color="white", width=800)
        self.chat_log.insert(END, "Welcome to SPAC-B!\n\nPlease enter a Git repository path, choose a temperature and select a branch.\n\n")
        self.chat_log.pack(side=LEFT, fill=BOTH, expand=True, pady=8, padx=8)

        self.chat_input = CTkEntry(master, width=900, height=36, font=("Arial", 14))
        self.chat_input.pack()

        self.send_button = CTkButton(master, text="Send message", command= self.send_message)
        self.send_button.pack(pady=8)

        # enter Git path
        self.url_input = CTkEntry(master, placeholder_text="Enter Git repository path here", width=500, height=32, font=("Arial", 14))
        self.url_input.pack()

        self.url_button = CTkButton(master, text="select repository", command= self.on_select_repository)
        self.url_button.pack(pady=8)

        # temperature slider
        self.label = CTkLabel(master, text="Temperature: 0.0")
        self.label.pack()

        self.scale = CTkSlider(master, from_=0, to=1, number_of_steps=10, orientation="horizontal", command=self.on_scale)
        self.scale.set(0)
        self.scale.pack()

        # select branch
        self.branches_combobox = CTkComboBox(master, values=self.branches, command=self.on_branch_select)
        self.branches_combobox.set("Select branch")
        self.branches_combobox.pack(pady=8)

    # Query the LLM with inputted text and display the response
    def send_message(self):
        message = self.chat_input.get()
        self.chat_input.delete(0, END)
        self.chat_log.insert(END, f"--- Your question:\n\n")
        self.chat_log.insert(END, (message))
        self.chat_log.insert(END, f"\n\n--- Reponse:\n\n")
        self.chat_log.insert(END, "Loading...\n\n")

        self.master.update()
        self.chat_log.insert(END, cf.query_model(message) + "\n\n")
    
    # Update the temperature value
    def on_scale(self, value):
        self.temperature = float(value)
        self.label.configure(text = 'Temperature: ' + str(round(value, 1)))
    
    # Select the repository and update the branches combobox
    def on_select_repository(self):
        try:
            self.branches, self.local_repo_path = cf.return_branches(self.url_input.get())
        except:
            self.chat_log.insert(END, "Invalid repository path\n")
            return    
        self.branches_combobox.configure(values=self.branches)
        self.chat_log.insert(END, f"Repository succesfully loaded from: {self.url_input.get()}\n\n")

    # Select the branch
    def on_branch_select(self, value):
        self.chat_log.insert(END, "Loading repository...\n")
        self.master.update()
        cf.setup(self.local_repo_path, value, self.temperature)
        self.chat_log.delete(1.0, END)
        self.chat_log.insert(END, f"Loaded Repository: {self.url_input.get()} - Selected branch: {value} - Ask me anything about this project! \n\n")
        self.chat_input.configure(placeholder_text="Enter your question here...")
            

def main():
    root = CTk()
    root.geometry("1000x900")
    gui = GUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()