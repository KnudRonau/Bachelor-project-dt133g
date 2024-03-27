# Program Name

Software Project Artificial Consultation Bot (SPAC-B)

## Introduction

SPAC-B is a small GenAI RAG-tool that let's users ask questions about a given Git repository. 

## Installation

- Ensure you have the latest version of Python installed with pip
- Clone this repository.
- Create virtual environment. 
- Use pip to install following dependencies.
    - ``langchain``
    - ``langchain_openai``
    - ``GitPython``
    - ``customtkinter``
    - ``faiss-cpu``
 - Program should now be able to run.

## Usage

- If you do not already have one, create a new API key from OpenAI at https://platform.openai.com/api-keys
- Run ``GUI.PY`` using Python.
- Input your secret API key.
- Input a repository you want to ask questions about.
- Press "select repository".
- (Optional) set temperature for LLM.
- Choose branch from repository.
- Enter a question about the repository you want answered.