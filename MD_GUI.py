import sys
from PyQt5.QtWidgets import QApplication, QMainWindow, QTextEdit, QPushButton, QLabel, QSlider, QComboBox, QLineEdit, QVBoxLayout, QWidget, QHBoxLayout 
from PyQt5.QtCore import Qt
import CoreFunctionality as cf

class GUI(QMainWindow):
    temperature = 0.0
    branches = []
    local_repo_path = ""
    markdowntext = ""

    def __init__(self):
        super().__init__()
        self.setWindowTitle("SPAC-B")

        # Set up the central widget and layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        main_layout = QVBoxLayout()
        repo_layout = QHBoxLayout()
        query_layout = QHBoxLayout()



        # Chat Window
        self.chat_log = QTextEdit(self)
        self.chat_log.setReadOnly(True)
        self.chat_log.setMarkdown("""
# Welcome to SPAC-B!

### Please enter a Git repository path, choose a temperature and select a branch.
""")   
        self.chat_log.setMinimumHeight(600)

        # Chat input
        self.chat_input = QLineEdit(self)
        self.chat_input.setFixedHeight(32)
        self.chat_input.returnPressed.connect(self.send_message)

        # Send button
        self.send_button = QPushButton("Send message", self)
        self.send_button.clicked.connect(self.send_message)
        self.send_button.setFixedHeight(32)
        self.send_button.setFixedWidth(120)

        # Enter Git path
        self.url_input = QLineEdit(self)
        self.url_input.setPlaceholderText("Enter Git repository path here")
        self.url_input.setFixedHeight(28)

        self.url_button = QPushButton("Select repository", self)
        self.url_button.clicked.connect(self.on_select_repository)
        self.url_button.setFixedHeight(28)
        self.url_button.setFixedWidth(120)

        # Temperature slider
        self.label = QLabel("Temperature: 0.0", self)

        self.scale = QSlider(self)
        self.scale.setMinimum(0)
        self.scale.setMaximum(10)
        self.scale.setOrientation(1)
        self.scale.setFixedWidth(300)
        self.scale.valueChanged.connect(self.on_scale)

        # Select branch
        self.branches_combobox = QComboBox(self)       
        self.branches_combobox.setFixedWidth(250)
        self.branches_combobox.activated.connect(self.on_branch_select)
        self.branches_combobox.setToolTip("Select a branch from the repository")
        self.branches_combobox.setPlaceholderText("Select branch")
        
        #Add widgets to layout
        main_layout.addWidget(self.chat_log)

        query_layout.addWidget(self.chat_input)
        query_layout.addWidget(self.send_button)
        main_layout.addLayout(query_layout)

        repo_layout.addWidget(self.url_input)
        repo_layout.addWidget(self.url_button)
        main_layout.addLayout(repo_layout)
        
        main_layout.addWidget(self.label)
        main_layout.setAlignment(self.label, Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(self.scale)
        main_layout.setAlignment(self.scale, Qt.AlignmentFlag.AlignCenter)
        
        main_layout.addWidget(self.branches_combobox)
        main_layout.setAlignment(self.branches_combobox, Qt.AlignmentFlag.AlignCenter)

        central_widget.setLayout(main_layout)
        
    # Query the LLM with inputted text and display the response
    def send_message(self):
        message = self.chat_input.text()
        self.chat_input.clear()
        self.markdowntext += """
## Your question:
#### {message}
## Response:
*Loading...*

""".format(message=message)

        self.chat_log.setMarkdown(self.markdowntext)
        self.chat_log.repaint()
        self.markdowntext += cf.query_model(message)
        self.chat_log.setMarkdown(self.markdowntext)
        
    # Update the temperature value
    def on_scale(self, value):
        self.temperature = value / 10
        self.label.setText(f'Temperature: {self.temperature}')

    # Select the repository and update the branches combobox
    def on_select_repository(self):
        try:
            self.branches, self.local_repo_path = cf.return_branches(self.url_input.text())
        except:
            self.chat_log.setMarkdown("### Invalid repository path. Please try again.")
            return
        self.branches_combobox.clear()
        self.branches_combobox.setPlaceholderText("Select branch")
        #self.branches_combobox.addItem("Select branch")
        self.branches_combobox.addItems(self.branches)
        self.chat_log.setMarkdown(""" ### Repository successfully loaded from: 
#### {path}""".format(path=self.url_input.text()))

    # Select the branch
    def on_branch_select(self, value):
        value = self.branches_combobox.currentText()
        self.chat_log.setMarkdown("*Loading repository...*")
        self.chat_log.repaint()
        cf.setup(self.local_repo_path, value, self.temperature)
        self.chat_log.clear()
        self.chat_log.setMarkdown("""### Loaded Repository from:
#### {path}

### Selected branch:
#### {value}

### Ask me anything about this project!**""".format(path=self.url_input.text(), value=value))
        self.chat_input.setPlaceholderText("Enter your question here...")

def main():
    app = QApplication(sys.argv)
    gui = GUI()
    gui.setGeometry(100, 100, 1000, 900)
    gui.show()
    sys.exit(app.exec_())

if __name__ == "__main__":
    main()
